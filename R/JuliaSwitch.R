#' @title Julia interface
#' @description A common `R`--`Julia` interface syntax.
#' @param backend A `character` string that defines the `Julia` backend (`"JuliaCall"` or `"JuliaConnectoR"`).
#' @param file For [`julia_include()`], `file` is a `character` string that defines the name of a `Julia` script to source.
#' @param name,value For [`julia_push()`]:
#' * `name` is a `character` that defines the object name in `Julia`.
#' * `value` is the `R` object.
#' @param ... Arguments passed to `JuliaCall` or `JuliaConnectoR` routines.
#' @details
#'
#' * [`julia_backend()`] sets the `Julia` backend. This is simply a wrapper for `options(JuliaSwitch.backend = backend)`. The output is returned invisibly. Other functions use this option to `switch` between `JuliaCall` and `JuliaConnectoR` routines.
#'
#' * [`julia_start()`] and [`julia_stop()`] start/stop a `Julia` session.
#'    - [`julia_start()`] wraps [`JuliaCall::julia_setup()`] and [`JuliaConnectoR::startJuliaServer()`].
#'    - [`julia_stop()`] wraps [`julia_terminate()`] and [`JuliaConnectoR::stopJulia()`], respectively. Note that [`julia_terminate()`] is simply a placeholder that does not terminate the `Julia` connection.
#'
#' * [`julia_cmd_line()`] and [`julia_cmd_block()`] run lines/blocks of `Julia` code, provided as `character` strings.
#'    - [`julia_cmd_line()`] wraps [`JuliaCall::julia_command()`] or [`JuliaConnectoR::juliaEval()`] and returns `invisible(NULL)`.
#'    - [`julia_cmd_block()`] wraps [`julia_include()`] and returns `invisible(NULL)`.
#'
#' * [`julia_include()`] sources a `Julia` script.
#'    - This runs the `Julia` code `include(file)` via [`JuliaCall::julia_command()`] or [`JuliaConnectoR::juliaEval()`].
#'
#' * [`julia_push()`] and [`julia_pull()`] push/pull `R` objects to/from `Julia`.
#'    - [`julia_push()`] wraps [`JuliaCall::julia_assign()`] or [`juliaAssign()`]. These functions expect a `name`--`value` argument pair.
#'    - [`julia_pull()`] wraps [`JuliaCall::julia_eval()`] or [`juliaTranslate()`]. These functions expect a `character` string of `Julia` code or the name of an `object` in `Julia` that is pulled to `R`.
#'
#' @example man/examples/example-JuliaSwitch.R
#' @author Edward Lavender
#' @name JuliaSwitch-interface
NULL

#' @rdname JuliaSwitch-interface
#' @export

# Set the Julia backend
julia_backend <- function(backend = c("JuliaCall", "JuliaConnectoR")) {
  backend <- match.arg(backend)
  op <- options(JuliaSwitch.backend = backend)
  invisible(op)
}

#' @rdname JuliaSwitch-interface
#' @export

# Start Julia
julia_start <- function(...) {
  .julia_start <- julia_switch(julia_setup, startJuliaServer)
  .julia_start(...)
}

#' @rdname JuliaSwitch-interface
#' @export

# Stop Julia
julia_stop <- function(...) {
  .julia_stop <- julia_switch(julia_terminate, stopJulia)
  .julia_stop(...)
}

#' @rdname JuliaSwitch-interface
#' @export

# Run a one-line command in Julia
julia_cmd_line <- function(...) {
  .julia_cmd_line <- julia_switch(julia_command, juliaEval)
  .julia_cmd_line(...)
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

# Run a larger piece of Julia code
julia_cmd_block <- function(...) {
  file <- tempfile(fileext = ".jl")
  on.exit(unlink(file), add = TRUE)
  writeLines(..., file)
  # readLines(file)
  julia_include(file)
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

# Include (source) Julia code
julia_include <- function(file) {
  julia_cmd_line(glue('include("{file}")'))
}

#' @rdname JuliaSwitch-interface
#' @export

# Push R objects to Julia
julia_push <- function(name, value) {
  .julia_push <- julia_switch(julia_assign, juliaAssign)
  .julia_push(name, value)
}

#' @rdname JuliaSwitch-interface
#' @export

# Pull objects from Julia
julia_pull <- function(...) {
  .julia_pull <- julia_switch(julia_eval, juliaTranslate)
  .julia_pull(...)
}
