#' @title Julia interface
#' @description A common `R`--`Julia` interface syntax.
#' @param backend A `character` string that defines the `Julia` backend (`"JuliaCall"` or `"JuliaConnectoR"`).
#' @param file For [`julia_include()`], `file` is a `character` string that defines the name of a `Julia` script to source.
#' @param name,value For [`julia_push()`]:
#' * `name` is a `character` that defines the object name in `Julia`.
#' * `value` is the `R` object.
#' @param pkg A `character` vector of `Julia` package name(s).
#' @param s A `character` that specifies the directory of a `Julia` environment.
#' @param string A `character` string of `Julia` code (or the name of an object for [`julia_println()`].
#' @param fname A `character` string that defines the name of a `Julia` function.
#' @param ... Arguments passed to `JuliaCall` or `JuliaConnectoR` routines.
#' @details
#'
#' # Interface
#'
#' * [`julia_backend()`] sets the `Julia` backend. This is simply a wrapper for `options(JuliaSwitch.backend = backend)`. The output is returned invisibly. Other functions use this option to `switch` between `JuliaCall` and `JuliaConnectoR` routines.
#'
#' * [`julia_start()`] and [`julia_stop()`] start/stop a `Julia` session.
#'    - [`julia_start()`] wraps [`JuliaCall::julia_setup()`] and [`JuliaConnectoR::startJuliaServer()`].
#'    - [`julia_stop()`] wraps [`julia_terminate()`] and [`JuliaConnectoR::stopJulia()`], respectively. Note that [`julia_terminate()`] is simply a placeholder that does not terminate the `Julia` connection.
#'
#' * [`julia_cmd()`] runs arbitrary `Julia` code provided as `character` strings. This uses:
#'    - [`julia_cmd_line()`] runs a single line of code, wrapping [`JuliaCall::julia_command()`] or [`JuliaConnectoR::juliaEval()`] and returning `invisible(NULL)`.
#'    - [`julia_cmd_block()`] runs a block of code, wrapping [`julia_include()`] and returning `invisible(NULL)`.
#'
#' * [`julia_include()`] sources a `Julia` script.
#'    - This runs the `Julia` code `include(file)` via [`JuliaCall::julia_command()`] or [`JuliaConnectoR::juliaEval()`].
#'
#' * [`julia_push()`] and [`julia_pull()`] push/pull `R` objects to/from `Julia`.
#'    - [`julia_push()`] wraps [`julia_send()`] or [`juliaSend()`]. These functions expect a `name`--`value` argument pair. [`julia_push()`] returns `invisible(NULL)`.
#'    - [`julia_pull()`] wraps [`JuliaCall::julia_eval()`] or [`juliaReceive()`]. These functions expect a `character` string of `Julia` code or the name of an `object` in `Julia` that is pulled to `R`.
#'
#' # Helpers
#'
#' The following helper routines are also exported:
#' * [`julia_pkg_activate()`] runs `Pkg.activate()`;
#' * [`julia_pkg_add()`] and [`julia_pkg_update()`] run `Pkg.add()` and `Pkg.update()`;
#' * [`julia_pkg_installed()`] checks if a package is installed (`TRUE`/`FALSE`);
#' * [`julia_using()`] and [`julia_import()`] runs `using {pkg}` and `import {Pkg}`;
#' * [`julia_defined()`] checks if an object is defines (`TRUE`/`FALSE`);
#' * [`julia_println()`] runs `println()`;
#' * [`julia_helpfile()`] prints the help file for a function;
#' * [`julia_save()`] and [`julia_load()`] run `using JLD2` plus `@save {file} {name}` or `@load {file} {name}`:
#'    - [`julia_save()`] returns the absolute file path for `file`;
#'    - [`julia_load()`] returns `invisible(NULL)`;
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
  .julia_start <- julia_switch(julia_initialise, juliaInitialise)
  .julia_start(...)
}

#' @rdname JuliaSwitch-interface
#' @export

# Stop Julia
julia_stop <- function() {
  .julia_stop <- julia_switch(julia_terminate, juliaTerminate)
  .julia_stop()
}

#' @rdname JuliaSwitch-interface
#' @export

# Run arbitrary (one line or multi-line) Julia commands
julia_cmd <- function(string){
  if (str_multiline(string)) {
    julia_cmd_block(string)
  } else {
    julia_cmd_line(string)
  }
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

# Run a one-line command in Julia
julia_cmd_line <- function(string) {
  # Add semi-colon
  # * This unifies behaviour of julia_command and juliaEval
  # * And improves speed in juliaEval() with large objects
  #   b/c NULL not the object is returned
  string <- str_add_semicolon(string)
  # Run command
  .julia_cmd_line <- julia_switch(julia_command, juliaEval)
  .julia_cmd_line(string)
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

# Run a larger piece of Julia code
julia_cmd_block <- function(string) {
  file <- tempfile(fileext = ".jl")
  on.exit(unlink(file), add = TRUE)
  writeLines(string, file)
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
  .julia_push <- julia_switch(julia_send, juliaSend)
  .julia_push(name, value)
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

# Pull objects from Julia
julia_pull <- function(...) {
  .julia_pull <- julia_switch(julia_receive, juliaReceive)
  .julia_pull(...)
}

#' @rdname JuliaSwitch-interface
#' @export

# Check if Julia object defined
julia_defined <- function(name) {
  stopifnot(inherits(name, "character"))
  julia_pull(glue('isdefined(Main, Symbol("{name}"))'))
}

#' @rdname JuliaSwitch-interface
#' @export

julia_using <- function(pkg) {
  sapply(pkg, function(p) {
    julia_cmd_line(glue('using {p}'))
  })
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

julia_import <- function(pkg) {
  sapply(pkg, function(p) {
    julia_cmd_line(glue('import {p}'))
  })
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

julia_pkg_activate <- function(s = ".") {
  julia_cmd_line('import Pkg')
  julia_cmd_line(glue('Pkg.activate("{s}")'))
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

julia_pkg_add <- function(pkg) {
  julia_cmd_line('import Pkg')
  sapply(pkg, function(p) {
    julia_cmd_line(glue('Pkg.add("{p}")'))
  })
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

julia_pkg_update <- function(pkg) {
  julia_cmd_line('import Pkg')
  sapply(pkg, function(p) {
    julia_cmd_line(glue('Pkg.update("{p}")'))
  })
  nothing()
}

#' @rdname JuliaSwitch-interface
#' @export

# Check if Julia package(s) are installed (TRUE/FALSE)
julia_pkg_installed <- function(pkg) {
  # Code modified from JuliaCall/setup.jl function installed(name)
  # TO DO Define inst/julia/JuliaSwitch.jl module with functions that are sourced in setup
  julia_cmd_line('import Pkg')
  # Push package names & force vector
  julia_push("package_names", pkg)
  if (length(pkg) == 1L) {
    julia_cmd_line('package_names = [package_names]')
  }
  julia_cmd_block(
    '
    installed = Vector{Bool}(undef, length(package_names))
    deps = values(Pkg.dependencies())
    for (i, name) in pairs(package_names)
      installed[i] = false
        for p in deps
          if p.name == name && p.is_direct_dep
            installed[i] = true
            break
          end
      end
    end
    '
  )
  julia_pull('installed')
}

#' @rdname JuliaSwitch-interface
#' @export

# Print an object or the output of a line of Julia code
julia_println <- function(string) {
  julia_cmd_line(glue('println({string})'))
}

#' @rdname JuliaSwitch-interface
#' @export

julia_helpfile <- function(fname) {
  julia_cmd_line('import Markdown')
  julia_cmd_line(glue('println(Markdown.plain(@doc {fname}))'))
}

#' @rdname JuliaSwitch-interface
#' @export

julia_save <- function(name, file = name) {
  julia_using("JLD2")
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  file <- glue("{tools::file_path_sans_ext(file)}.jld2")
  julia_cmd_line(glue('@save "{file}" {name};'))
  tools::file_path_as_absolute(file)
}

#' @rdname JuliaSwitch-interface
#' @export

julia_load <- function(file, name = basename(tools::file_path_sans_ext(file))) {
  julia_using("JLD2")
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  julia_cmd_line(glue('@load "{file}" {name};'))
  nothing()
}
