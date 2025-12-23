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

# Add ; to the end of a string
str_add_semicolon <- function(x) {
  if (!endsWith(trimws(x, "right"), ";")) {
    x <- paste0(x, ";")
  }
  x
}

# Check if a string has multiple lines (TRUE/FALSE)
str_multiline <- function(x) {
  str_nlines(x) > 1L
}

# Count the number of lines in a string
str_nlines <- function(x) {
  sum(charToRaw(x) == as.raw(10)) + 1L
}
