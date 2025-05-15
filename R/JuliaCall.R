#' @title `JuliaCall` wrappers
#' @description These internal functions wrap `JuliaCall` routines.
#' @details
#' * [julia_terminate()] is a placeholder equivalent for [`JuliaConnectoR::stopJulia()`].
#' @author Edward Lavender
#' @name JuliaCall-wrappers
NULL

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_terminate <- function() {
  message("JuliaCall does not support termination of the Julia session.")
  nothing()
}
