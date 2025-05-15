.onLoad <- function(libname, pkgname) {
  # Collect options
  op <- options()
  # List default JuliaSwitch options
  op_JuliaSwitch <- list(JuliaSwitch.backend = "JuliaCall")
  # Set options
  bool <- !(names(op_JuliaSwitch) %in% names(op))
  if (any(bool)) {
    options(op_JuliaSwitch[bool])
  }
  invisible(NULL)
}

.onUnload <- function(libpath){
  options("JuliaSwitch.backend" = NULL)
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  msg_startup <- paste0("This is {JuliaSwitch} v.", utils::packageVersion("JuliaSwitch"), ". Use `julia_backend()` to set the `Julia` backend.")
  packageStartupMessage(msg_startup)
}
