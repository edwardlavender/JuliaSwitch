# A generic routine for switching between JuliaCall and JuliaConnectoR
julia_switch <- function(JuliaCall, JuliaConnectoR) {
  backend <- getOption("JuliaSwitch.backend")
  if (!(backend %in% c("JuliaCall", "JuliaConnectoR"))) {
    stop("`JuliaSwitch.backend` must be 'JuliaCall' or 'JuliaConnectoR'.")
  }
  switch(backend,
         JuliaCall = JuliaCall,
         JuliaConnectoR = JuliaConnectoR)
}


# Get the value of a julia option, if specified
# (copied from patter)
julia_option <- function(VALUE) {

  OPTION <- deparse(substitute(VALUE))

  #### Get VALUE(s)
  # Get inputted value
  if (missing(VALUE)) {
    VALUE <- NULL
  }
  value_input  <- VALUE
  # Get global option value (set or NULL)
  value_option <- getOption(OPTION, default = NULL)
  # Get environmental variable value (set or NULL)
  value_env    <- Sys.getenv(OPTION)
  if (!nzchar(value_env)) {
    value_env <- NULL
  }

  #### Validate VALUEs
  if (!all(is.null(value_input), is.null(value_option), is.null(value_env))) {
    if (length(unique(c(value_input, value_option, value_env))) != 1L) {
      warn("There are multiple values for `{OPTION}`.", .envir = environment())
    }
  }

  #### Set VALUE
  # Use VALUE, if specified
  # Otherwise, try global option & then environment variable
  if (is.null(VALUE) && !is.null(value_option)) {
    VALUE <- value_option
  }
  if (is.null(VALUE) && !is.null(value_env)) {
    VALUE <- value_env
  }

  #### Return VALUE
  VALUE

}


# Set JULIA_NUM_THREADS
# (copied from patter)
set_JULIA_NUM_THREADS <- function(JULIA_NUM_THREADS) {
  # Get JULIA_NUM_THREADS
  JULIA_NUM_THREADS <- julia_option(JULIA_NUM_THREADS)
  # On unix, set JULIA_NUM_THREADS = "auto"
  # (Otherwise, Julia is launched with one thread only)
  if (os_unix() & is.null(JULIA_NUM_THREADS)) {
    JULIA_NUM_THREADS <- "auto"
  }
  # On Windows, JULIA_NUM_THREADS should be NULL or set system-wide
  if (os_windows()) {
    if (is.null(JULIA_NUM_THREADS)) {
      msg("On Windows, set `JULIA_NUM_THREADS` system-wide to improve performance (see https://github.com/edwardlavender/patter/issues/11).")
    } else {
      # Verify that JULIA_NUM_THREADs is set system-wide (not only in R)
      # (System-wide variables are captured by Sys.getenv())
      try({
        # Obtain system-wide environment variables via registry
        # Note this check appears to work on some, but not all, systems
        # * Hence we use a message not a warning
        # (system("cmd.exe /c set", intern = TRUE) inherits environment variables from R)
        SET <- system("reg query \"HKLM\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment\" /s", intern = TRUE)
        if (!any(grepl("JULIA_NUM_THREADS", SET))) {
          msg("On Windows, `JULIA_NUM_THREADS` should be set system-wide (see https://github.com/edwardlavender/patter/issues/11).")
        }
      }, silent = TRUE)
    }
  }
  # Update JULIA_NUM_THREADS setting
  # * This is implemented on unix only
  # * On Windows, JULIA_NUM_THREADS is already set if relevant
  if (os_unix() & !is.null(JULIA_NUM_THREADS)) {
    Sys.setenv(JULIA_NUM_THREADS = JULIA_NUM_THREADS)
  }
  invisible(JULIA_NUM_THREADS)
}


# Find the path to a Julia Project
# (copied from patter)
julia_proj_path <- function(JULIA_PROJ) {
  # Get JULIA_PROJ
  JULIA_PROJ <- julia_option(JULIA_PROJ)
  if (is.null(JULIA_PROJ)) {
    msg("Using Julia's global environment.")
  } else {
    # Normalise path
    # * This is required for correct parsing on windows in downstream functions:
    # * julia_proj_generate()
    # * julia_proj_activate()
    JULIA_PROJ <- normalizePath(JULIA_PROJ, winslash = "/", mustWork = FALSE)
  }
  JULIA_PROJ
}


# Generate a Julia project (if required)
# (modified from patter)
julia_pkg_generate <- function(JULIA_PROJ) {
  if (!dir.exists(JULIA_PROJ) &&
      !file.exists(file.path(JULIA_PROJ, "Project.toml"))) {
    julia_cmd(glue('Pkg.generate("{JULIA_PROJ}");'))
  }
  nothing()
}


# Activate a Julia Project
# This is an exported function
# julia_pkg_activate <- function(JULIA_PROJ) {
#   # julia_cmd("using Revise")
#   julia_cmd(glue('Pkg.activate("{JULIA_PROJ}"; temp = false);'))
#   nothing()
# }


# List dependencies
# (copied from patter)
julia_pkg_list_full <- function(.pkg_install) {
  pkg_required  <- julia_pkg_list_req()
  pkg_requested <- .pkg_install
  pkg_installed <- julia_pkg_list_installed()
  sort(unique(c(pkg_required, pkg_requested, pkg_installed)))
}


# List required Julia dependencies
# (modified from patter)
julia_pkg_list_req <- function() {
  c("Arrow",
    "DataFrames",
    "Dates",
    "GeoArrays",
    "OrderedCollections",
    "Pkg")
}


# List all installed Julia dependencies
# (copied from patter)
julia_pkg_list_installed <- function() {
  julia_import("Pkg")
  sort(julia_pull('collect(keys(Pkg.project().dependencies))'))
}


# List Julia dependencies for update
# (copied from patter)
julia_pkg_list_update <- function(.pkg_update) {
  if (inherits(.pkg_update, "logical")) {
    if (isFALSE(.pkg_update)) {
      return(NULL)
    } else {
      return(julia_pkg_list_full(NULL))
    }
  }
  .pkg_update
}


# List Julia dependencies for loading
# (copied from patter)
julia_pkg_list_load <- function(.pkg_load) {
  pkg_required <- julia_pkg_list_req()
  if (is.null(.pkg_load) | isFALSE(.pkg_load)) {
    pkg_extra    <- NULL
  } else {
    pkg_extra <- julia_pkg_list_full(NULL)
  }
  sort(unique(c(pkg_required, pkg_extra)))
}


# Install additional Julia packages
# (modified from patter)
julia_pkg_install_deps <- function(.pkg_install, .pkg_update) {
  # Iteratively install & update dependencies as required
  lapply(.pkg_install, function(.pkg) {
    # Choose whether or not to install packages
    # * For packages in Julia's standard library (e.g., Random)
    #   julia_pkg_installed() returns FALSE
    # * But these packages do not require install & this is suppressed (for speed)
    if (.pkg %in% c("Pkg", "Random")) {
      install <- FALSE
    } else {
      install  <- !julia_pkg_installed(.pkg)
    }
    update   <- ifelse(isFALSE(install) & .pkg %in% .pkg_update, TRUE, FALSE)
    # Run installation/update
    if (install) {
      # NB it appears Pkg.add() can remove the temporary directory
      # This then breaks the loop when julia_pkg_installed is used for the next file
      # because this uses tempfile() internally
      # Hence julia_pkg_add() contains tempdir(check = TRUE) to resolve this issue
      julia_pkg_add(.pkg)
    }
    if (update) {
      julia_pkg_update(.pkg)
    }
    NULL
  })
  nothing()
}


# Load Julia packages
# (copied from patter)
julia_pkg_library <- function(.pkg_load) {
  lapply(.pkg_load, \(.pkg) julia_using(.pkg))
  nothing()
}


# Handle (install/update/load) Julia packages
# (modified from patter)
julia_pkg_setup <- function(.pkg_install,
                            .pkg_update,
                            .pkg_load) {
  # List Julia packages for install/update
  pkg_install <- julia_pkg_list_full(.pkg_install = .pkg_install)

  pkg_update  <- julia_pkg_list_update(.pkg_update)
  # Install and optionally update dependencies
  julia_pkg_install_deps(.pkg_install = pkg_install,
                         .pkg_update  = pkg_update)
  # Load relevant Julia packages
  # (Run julia_pkg_list_load() at this point to pick up newly installed Julia packages)
  pkg_load <- julia_pkg_list_load(.pkg_load = .pkg_load)
  julia_pkg_library(pkg_load)
  nothing()
}


# Load Julia helper functions
julia_helpers <- function() {
  if (getOption("JuliaSwitch.backend") == "JuliaConnectoR") {
    julia_cmd(
      '
    function __assign_from_JuliaConnectoR__(name::String, value)
      Core.eval(Main, Expr(:(=), Symbol(name), value))
      return nothing
    end
  ')
  }
}


# Get the number of threads used by Julia
julia_threads <- function(JULIA_NUM_THREADS) {
  nthreads <- julia_pull("Threads.nthreads()")
  if (!is.null(JULIA_NUM_THREADS) && JULIA_NUM_THREADS != "auto" && nthreads != JULIA_NUM_THREADS) {
    warn("`JULIA_NUM_THREADS` could not be set.")
  }
  invisible(nthreads)
}


# Assign SpatRasters
julia_send_SpatRaster <- function(name, value, command) {
  # Checks
  check_installed("terra")
  stopifnot(inherits(value, "SpatRaster"))
  # Define file
  file <- terra::sources(value)
  if (file == "") {
    file <- tempfile(fileext = ".tif")
    terra::writeRaster(value, file)
  }
  # Set env
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  command('import GeoArrays')
  command(glue::glue('{name} = GeoArrays.read("{file}");'))
  nothing()
}


# Parse Julia 'classes' into syntactic R names
julia_class_parse <- function(string) {

  if (startsWith(string, "DataFrames.DataFrame")) {
    "DataFrame"
  } else if (startsWith(string, "@NamedTuple")) {
    "NamedTuple"
  } else if (startsWith(string, "OrderedCollections.OrderedDict")) {
    "OrderedDict"
  } else if (startsWith(string, "Vector{Any}")) {
    "VectorAny"
  } else if (
    startsWith(string, "Vector{DateTime}") ||
    startsWith(string, "Vector{Dates.DateTime}")) {
    "VectorDateTime"
  }  else {
    string
  }
}
