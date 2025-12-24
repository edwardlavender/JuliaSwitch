#' @title `JuliaConnectoR` wrappers
#' @description These internal functions wrap `JuliaConnectoR` routines.
#' @param name,value Arguments for [`juliaSend()`].
#' * `name` is a `character` that defines the object name in `Julia`.
#' * `value` is the `R` object.
#' @param x Arguments for [`juliaClass()`] and [`juliaReceive()`].
#' * `x` is a `character` that defines the name of an object in `Julia`.
#' @details
#' * [`juliaInitialise()`] starts `Julia` via [`JuliaConnectoR::startJuliaServer()`];
#' * [`juliaSend()`] is a [`julia_send()`] equivalent:
#'    - The default method wraps [`JuliaConnectoR::juliaCall()`];
#'    - The `data.frame` method translates `data.frame` inputs to `DataFrame`s;
#'    - The `SpatRaster` method translates [`terra::SpatRaster`]s to `GeoArray`s;
#' * [`juliaClass()`] extracts the type of a `Julia` object as an R `class`:
#'    - This is used for method dispatch in [`juliaReceive()`];
#' * [`juliaReceive()`] is a [`JuliaCall::julia_eval()`] equivalent:
#'    - The default method wraps  [`JuliaConnectoR::juliaEval()`];
#'    - For some object types, this may return an `JuliaProxy` object;
#'    - The `data.frame` method translates `JuliaProxy` `DataFrame`s to `data.frame`s via [`JuliaConnectoR::as.data.frame.JuliaProxy()`];
#'  * [`juliaTerminate()`] stops `Julia` via [`JuliaConnectoR::stopJulia()`];
#' @author Edward Lavender
#' @name JuliaConnectoR-wrappers
NULL

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaInitialise <- function(...) {
  # Start server
  julia <- startJuliaServer(...)
  # Load __r_assign__
  # (Used to send objects from R to Julia)
  invisible(julia)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend <- function(name, value) {
  UseMethod("juliaSend", value)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend.default <- function(name, value) {
  # assign_expr <- juliaCall("Expr",
  #                          juliaCall("Symbol", "="),
  #                          juliaCall("Symbol", name),
  #                          value)
  # juliaCall("eval", assign_expr)
  juliaCall("__assign_from_JuliaConnectoR__", name, value)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Send POSIXct vectors to Julia
juliaSend.POSIXct <- function(name, value) {
  juliaEval("import Dates")
  juliaSend(name, as.numeric(value))
  julia_cmd(glue("{name} = Dates.unix2datetime.({name})"))
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend.data.frame <- function(name, value) {
  juliaEval("import DataFrames")
  # Send individual columns
  # - For each column, an appropriate juliaSend method is used
  # - This handles timestamp columns
  for (col in names(value)) {
    juliaSend(col, value[[col]])
  }
  # Build DataFrame (correct Julia string syntax)
  cols <- paste0(
    "Symbol(\"", names(value), "\") => ", names(value),
    collapse = ", "
  )
  julia_cmd(glue::glue("{name} = DataFrame({cols})"))
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend.list <- function(name, value) {

  # Send unnamed lists a Any[] (to match JuliaCall)
  # (This behaviour matches JuliaCall)
  nms <- names(value)
  if (is.null(nms)) {
    # JuliaConnectoR handles empty lists as Any[] automatically
    if (length(value) == 0L) {
      juliaSend.default(name, value)
    } else {
      # Otherwise, use an appropriate juliaSend method e.g., for a data.frame
      julia_cmd_line(glue("{name} = Vector{{Any}}()"))
      for (i in seq_along(value)) {
        tmp <- paste0(name, "_", i)
        juliaSend(tmp, value[[i]])
        julia_cmd_line(glue("push!({name}, {tmp})"))
      }
    }

  } else {

    # Send named lists as OrderedCollections  (to match JuliaCall)
    julia_import("OrderedCollections")
    julia_cmd_line(glue("{name} = OrderedCollections.OrderedDict{{Symbol, Any}}()"))
    for (i in seq_along(value)) {
      key <- nms[i]
      tmp <- paste0(name, "_", key)
      juliaSend(tmp, value[[i]])
      julia_cmd_line(glue("{name}[:{key}] = {tmp}"))
    }

  }

  nothing()

}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend.SpatRaster <- function(name, value) {
  julia_send_SpatRaster(name, value, juliaEval)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaClass <- function(x) {
  # type <- juliaEval(glue('string(nameof(typeof({x})))'))
  type <- juliaEval(glue('string(typeof({x}))'))
  type <- julia_class_parse(type)
  structure(list(), class = type)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaReceive <- function(x) {
  UseMethod("juliaReceive", juliaClass(x))
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaReceive.default <- function(x) {
  x |>
    juliaEval() |>
    drop_attr_JuliaConnectoR()
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Receive a single DateTime from Julia
juliaReceive.DateTime <- function(x) {
  julia_import("Dates")
  x <- juliaEval(glue('Dates.datetime2unix({x})'))
  as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Receive a Vector of Date Times
# * We use Dates.datetime2unix vectorised
juliaReceive.VectorDateTime <- function(x) {
  julia_import("Dates")
  x <- juliaEval(glue('Dates.datetime2unix.({x})'))
  x <- as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
  drop_attr_JuliaConnectoR(x)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaReceive.DataFrame <- function(x) {

  # Use juliaReceive() on each column & bind
  # This ensures appropriate methods e.g., for timestamps get dispatched
  # (as.data.frame(juliaEval(x) only works if the dataframe does not contain timestamps)

  # Collect columns in a list
  headings <- juliaEval(glue("Base.names({x})"))
  columns <- lapply(headings, \(heading) juliaReceive(glue("{x}[:, :{heading}]")))
  names(columns) <- headings

  # Build data.frame
  columns |>
    dplyr::bind_cols() |>
    as.data.frame() |>
    drop_attr_JuliaConnectoR()
}


#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Recursively handle Vector{Any} objects
juliaReceive.VectorAny <- function(x) {
  n <- juliaEval(glue("length({x})"))
  lapply(seq_len(n), function(i) {
    juliaReceive(glue("{x}[{i}]"))
  })
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Recursively handle NamedTuple objects
juliaReceive.NamedTuple <- function(x) {
  nms <- juliaEval(glue("collect(String.(keys({x})))"))
  lapply(nms, function(nm) {
    juliaReceive(glue("getfield({x}, Symbol(\"{nm}\"))"))
    }) |>
    stats::setNames(nms) |>
    drop_attr_JuliaConnectoR()
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaTerminate <- function() {
  stopJulia()
}
