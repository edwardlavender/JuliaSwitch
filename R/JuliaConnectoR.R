#' @title `JuliaConnectoR` wrappers
#' @description These internal functions wrap `JuliaConnectoR` routines.
#' @param name,value Arguments for [`juliaAssign()`].
#' * `name` is a `character` that defines the object name in `Julia`.
#' * `value` is the `R` object.
#' @param x Arguments for [`juliaClass()`] and [`juliaTranslate()`].
#' * `x` is a `character` that defines the name of an object in `Julia`.
#' @details
#' * [`juliaAssign()`] is a [`JuliaCall::julia_assign()`] equivalent.
#'    - The default method wraps [`JuliaConnectoR::juliaCall()`];
#'    - The `data.frame` method translates `data.frame` inputs to `DataFrame`s;
#'
#' * [`juliaClass()`] extracts the type of a `Julia` object as an R `class`. This is used for method dispatch in [`juliaTranslate()`].
#'
#' * [`juliaTranslate()`] is a [`JuliaCall::julia_eval()`] equivalent.
#'    - The default method wraps  [`JuliaConnectoR::juliaEval()`];
#'    - For some object types, this may return an `JuliaProxy` object;
#'    - The `data.frame` method translates `JuliaProxy` `DataFrame`s to `data.frame`s via [`JuliaConnectoR::as.data.frame.JuliaProxy()`];
#' @author Edward Lavender
#' @name JuliaConnectoR-wrappers
NULL

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaAssign <- function(name, value) {
  UseMethod("juliaAssign", value)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaAssign.default <- function(name, value) {
  assign_expr <- juliaCall("Expr",
                           juliaCall("Symbol", "="),
                           juliaCall("Symbol", name),
                           value)
  juliaCall("eval", assign_expr)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaAssign.data.frame <- function(name, value) {
  juliaEval("using DataFrames")
  value <- juliaCall("DataFrame", value)
  juliaAssign(name, value)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaClass <- function(x) {
  type <- juliaEval(glue('typeof({x})'))
  structure(list(), class = type)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaTranslate <- function(x) {
  UseMethod("juliaTranslate", juliaClass(x))
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaTranslate.default <- function(x) {
  juliaEval(x)
}

#' @rdname JuliaConnectoR-wrappers
#' @keywords internal

juliaTranslate.DataFrame <- function(x) {
  as.data.frame(juliaEval(x))
}
