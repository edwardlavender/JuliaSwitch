#' @title `JuliaCall` wrappers
#' @description These internal functions wrap `JuliaCall` routines.
#' @details
#' * [julia_terminate()] is a placeholder equivalent for [`JuliaConnectoR::stopJulia()`].
#' * [`julia_allot()`] is a [`JuliaCall::julia_assign()`] wrapper:
#'    - The default method simply calls [`JuliaCall::julia_assign()`];
#'    - The default method handles `data.frame` objects;
#'    - The `SpatRaster` method translates [`terra::SpatRaster`]s to `GeoArray`s;
#' @author Edward Lavender
#' @name JuliaCall-wrappers
NULL

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_terminate <- function() {
  message("JuliaCall does not support termination of the Julia session.")
  nothing()
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_allot <- function(name, value) {
  UseMethod("julia_allot", value)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_allot.default <- function(name, value) {
  julia_assign(name, value)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_allot.SpatRaster <- function(name, value) {
  julia_allot_SpatRaster(name, value, julia_command)
}
