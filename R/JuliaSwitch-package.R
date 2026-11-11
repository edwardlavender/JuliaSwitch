#' @title [`JuliaSwitch`]
#' @name JuliaSwitch
#' @details
#' For a list of exported functions, see [`JuliaSwitch-interface`].
#'
#' # Julia dependencies
#'
#' Calling [`julia_start()`] starts the selected backend and triggers
#' installation of its `Julia` dependencies in the base (default) `Julia`
#' environment:
#'
#' * `JuliaCall` installs `RCall`, `Revise` and `Suppressor`;
#' * `JuliaConnectoR` installs `Tables`.
#'
#' After starting the backend, [`julia_start()`] activates `JULIA_PROJ`, if
#' supplied, and installs or loads the `JuliaSwitch` dependencies `Arrow`,
#' `DataFrames`, `Dates`, `GeoArrays` and `OrderedCollections` in that local
#' environment. If `JULIA_PROJ` is not supplied, these dependencies use the
#' default environment. `Dates` is included in the `Julia` standard library.
"_PACKAGE"

#' @importFrom dplyr across mutate where
#' @importFrom glue glue
#' @importFrom JuliaCall julia_assign julia_command julia_eval julia_setup
#' @importFrom JuliaConnectoR juliaCall juliaEval startJuliaServer stopJulia

NULL
