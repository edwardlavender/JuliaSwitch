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
  startJuliaServer(...)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend <- function(name, value) {
  UseMethod("juliaSend", value)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend.default <- function(name, value) {
  assign_expr <- juliaCall("Expr",
                           juliaCall("Symbol", "="),
                           juliaCall("Symbol", name),
                           value)
  juliaCall("eval", assign_expr)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Send POSIXct vectors to Julia
juliaSend.POSIXct <- function(name, value) {
  juliaEval("using Dates")
  juliaSend(name, as.numeric(value))
  julia_cmd(glue("{name} = Dates.unix2datetime.({name})"))
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend.data.frame <- function(name, value) {
  juliaEval("using DataFrames")
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

juliaSend.SpatRaster <- function(name, value) {
  julia_send_SpatRaster(name, value, juliaEval)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaClass <- function(x) {
  # type <- juliaEval(glue('string(nameof(typeof({x})))'))
  type <- juliaEval(glue('string(typeof({x}))'))
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
  juliaEval(x)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

# Receive a single DateTime from Julia
juliaReceive.DateTime <- function(x) {
  julia_using("Dates")
  x <- juliaEval(glue('Dates.datetime2unix({x})'))
  as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
}

#' @rdname JuliaConnectoR-wrappers
#' @noRd

# Receive a Vector of Date Times
# * We need @noRd for this as Vector{DateTime} causes issues
# * We use Dates.datetime2unix vectorised
`juliaReceive.Vector{DateTime}` <- function(x) {
  julia_using("Dates")
  x <- juliaEval(glue('Dates.datetime2unix.({x})'))
  as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaReceive.DataFrame <- function(x) {
  as.data.frame(juliaEval(x))
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaTerminate <- function() {
  stopJulia()
}
