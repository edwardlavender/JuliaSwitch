#' @title `JuliaConnectoR` wrappers
#' @description These internal functions wrap `JuliaConnectoR` routines.
#' @param name,value Arguments for [`juliaSend()`].
#' * `name` is a `character` that defines the object name in `Julia`.
#' * `value` is the `R` object.
#' @param x Arguments for `juliaClass()` and [`juliaReceive()`].
#' * `x` is a `character` that defines the name of an object in `Julia`.
#' @details
#' * [`juliaInitialise()`] starts `Julia` via [`JuliaConnectoR::startJuliaServer()`];
#' * [`juliaSend()`] is a [`julia_send()`] equivalent:
#'    - The default method wraps [`JuliaConnectoR::juliaCall()`];
#'    - The `data.frame` method translates `data.frame` inputs to `DataFrame`s;
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
  julia <- startJuliaServer(...)
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
  if (lubridate::tz(value) == "") {
    abort("Time zone is assumed to be UTC.")
    lubridate::tz(value) <- "UTC"
  }
  if (lubridate::tz(value) != "UTC") {
    abort("Only the UTC time zone is supported.")
  }
  juliaSend(name, as.numeric(value))
  julia_cmd(glue("{name} = Dates.unix2datetime.({name})"))
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend.data.frame <- function(name, value) {
  julia_import("DataFrames")
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
#' @noRd

juliaClass <- function(x) {
  # Define type
  type <- juliaEval(glue('string(typeof({x}))'))
  # Recode Vector{Float64} etc. as VectorSimple
  # * This cannot be done in julia_class_parse() b/c we need juliaEval
  #   which is JuliaConnectoR specific
  if (startsWith(type, "Vector")) {
    type <- ifelse(
      juliaEval(paste0("isa(", x, ", AbstractVector{<:Union{Number, AbstractString, Bool, Char, Missing}})")),
      "VectorSimple",
      type)
  }
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

# Default receive method (uses TCP)
juliaReceive.default <- function(x) {
  x |>
    juliaEval() |>
    juliaReceiveMemCleanAttr()
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Receive a single DateTime from Julia (uses TCP)
juliaReceive.DateTime <- function(x) {
  julia_import("Dates")
  x <- juliaEval(glue('Dates.datetime2unix({x})'))
  as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Receive a 'simple' vector (e.g. Vector{Float64})
juliaReceive.VectorSimple <- function(x) {
  juliaReceiveSwitch(x,
                     juliaReceiveMemVectorSimple,
                     juliaReceiveFeatherVectorSimple)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Receive a Vector of Date Times
juliaReceive.VectorDateTime <- function(x) {
  juliaReceiveSwitch(x,
                     juliaReceiveMemVectorDateTime,
                     juliaReceiveFeatherVectorDateTime)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaReceive.DataFrame <- function(x) {
  juliaReceiveSwitch(x, juliaReceiveMemDataFrame, juliaReceiveFeatherDataFrame)
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
    stats::setNames(nms)
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Receive objects via memory or via feather
# > We only pass small objects via memory
juliaReceiveSwitch <- function(x, receive_via_mem, receive_via_feather) {
  bytes <- juliaEval(glue("Base.summarysize({x})"))
  if (bytes < 1000) {
    receive_via_mem(x)
  } else {
    receive_via_feather(x)
  }
}

# General function for receiving functions via memory
# Specific functions provided below

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Supporting function used to drop JuliaConnectoR attributes e.g., JLDIM, JLTYPE
# > This causes objects from Julia not to match R counterparts
# > This cases issues in tests e.g., with expect_equal(... ignore_attr = FALSE)
juliaReceiveMemCleanAttr <- function(x) {
  for (att in c("JLDIM", "JLTYPE")) {
    if (!is.null(attr(x, att, exact = TRUE))) {
      attr(x, att) <- NULL
    }
  }
  x
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# General function to receive objects via feather (for bigger objects)
juliaReceiveFeather <- function(x) {
  tmp_feather <- file.path(tempdir(), "tmp.feather")
  julia_push("tmp_feather", tmp_feather)
  julia_cmd_line(glue('Arrow.write(tmp_feather, {x})'))
  on.exit(unlink(tmp_feather), add = TRUE)
  # Read data.frame
  # As arrow assumes UTC time stamps, this is set
  tmp_feather |>
    arrow::read_feather() |>
    mutate(across(where(~inherits(.x, "POSIXt")), ~lubridate::with_tz(.x, "UTC"))) |>
    as.data.frame()
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Receive simple vectors via TCP
juliaReceiveMemVectorSimple <- function(x) {
  x |>
    juliaEval() |>
    juliaReceiveMemCleanAttr()
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Receive simple Vectors via feather
juliaReceiveFeatherVectorSimple <- function(x) {
  julia_import("DataFrames")
  julia_cmd_line(glue('tmp_df = DataFrame(x = {x})'))
  x <- juliaReceiveFeather("tmp_df")
  x$x
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Receive a Vector of DateTimes via TCP
juliaReceiveMemVectorDateTime <- function(x) {
  # Use Dates.datetime2unix vectorised
  julia_import("Dates")
  juliaEval(glue('Dates.datetime2unix.({x})')) |>
    as.POSIXct(origin = "1970-01-01", tz = "UTC") |>
    juliaReceiveMemCleanAttr()
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Receive a Vector of DateTimes via feather
juliaReceiveFeatherVectorDateTime <- function(x) {
  x |>
    juliaReceiveFeatherVectorSimple() |>
    lubridate::with_tz("UTC")
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Receive a DataFrame via TCP
juliaReceiveMemDataFrame <- function(x) {
  # Collect columns in a list
  # > Use juliaReceive() on each column & bind
  # > This ensures appropriate methods e.g., for timestamps get dispatched
  # > (as.data.frame(juliaEval(x) only works if the dataframe does not contain timestamps)
  julia_import("DataFrames")
  headings <- juliaEval(glue("Base.names({x})"))
  columns <- lapply(headings, \(heading) juliaReceive(glue("{x}[:, :{heading}]")))
  names(columns) <- headings
  # Build data.frame
  columns |>
    dplyr::bind_cols() |>
    as.data.frame() |>
    juliaReceiveMemCleanAttr()
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Receive a DataFrame via feather
juliaReceiveFeatherDataFrame <- function(x) {
  juliaReceiveFeather(x)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaTerminate <- function() {
  stopJulia()
}
