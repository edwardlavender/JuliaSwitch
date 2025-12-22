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

juliaSend.data.frame <- function(name, value) {
  juliaEval("using DataFrames")
  value <- juliaCall("DataFrame", value)
  juliaSend(name, value)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaSend.SpatRaster <- function(name, value) {
  julia_send_SpatRaster(name, value, juliaEval)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaClass <- function(x) {
  type <- juliaEval(glue('string(nameof(typeof({x})))'))
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

juliaReceive.DataFrame <- function(x) {
  as.data.frame(juliaEval(x))
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaTerminate <- function() {
  stopJulia()
}
