# Internal proj.build functions
# These are defined locally to reduce package dependencies

nothing <- function() {
  invisible(NULL)
}

# Simple rlang::check_installed() replacement
check_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(glue("This function requires the `{pkg}` package.", call. = FALSE))
  }
}
