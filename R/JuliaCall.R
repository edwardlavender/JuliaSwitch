#' @title `JuliaCall` wrappers
#' @description These internal functions wrap `JuliaCall` routines.
#' @details
#' * [`julia_initialise`] starts `Julia` via [`JuliaCall::julia_setup()`];
#' * [`julia_send()`] wraps [`JuliaCall::julia_assign()`]:
#'    - The default method simply calls [`JuliaCall::julia_assign()`];
#'    - The default method handles `data.frame` objects;
#'    - The `SpatRaster` method translates [`terra::SpatRaster`]s to `GeoArray`s;
#' * [`julia_class()`] extracts the type of a `Julia` object as an R `class`:
#'    - This is used for method dispatch in [`juliaReceive()`];
#' * [`julia_receive()`] wraps [`JuliaCall::julia_eval()`] wrapper:
#'    - The default method handles wraps [`JuliaCall::julia_eval()`] and handles [`data.frame`]s;
#' * [`julia_terminate()`] is a placeholder equivalent for [`juliaTerminate()`] ([`JuliaConnectoR::stopJulia()`]);
#'
#' @author Edward Lavender
#' @name JuliaCall-wrappers
NULL

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_initialise <- function(...) {
  julia_setup(...)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_send <- function(name, value) {
  UseMethod("julia_send", value)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_send.default <- function(name, value) {
  julia_assign(name, value)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_send.SpatRaster <- function(name, value) {
  julia_send_SpatRaster(name, value, julia_command)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_class <- function(x) {
  # type <- julia_eval(glue('string(nameof(typeof({x})))'))
  type <- julia_eval(glue('string(typeof({x}))'))
  structure(list(), class = type)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_receive <- function(x) {
  UseMethod("julia_receive", julia_class(x))
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_receive.default <- function(x) {
  julia_eval(x)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_terminate <- function() {
  message("JuliaCall does not support termination of the Julia session.")
  nothing()
}
