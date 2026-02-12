try_julia_start <- function(JULIA_PROJ) {
  # Try to start Julia
  julia <- tryCatch(julia_start(JULIA_PROJ = JULIA_PROJ),
                    error = function(e) e)
  # If julia_start fails, return a warning and skip tests
  if (inherits(julia, "error")) {
    unlink(JULIA_PROJ, recursive = TRUE)
    warning(conditionMessage(julia), call. = FALSE)
    warning(paste0("`julia_start()` failed with the ",
                   getOption("JuliaSwitch.backend"),  " backend."))
    return(FALSE)
  }
  TRUE
}
