#' @title `JuliaCall` wrappers
#' @description These internal functions wrap `JuliaCall` routines.
#' @details
#' * [`julia_initialise`] starts `Julia` via [`JuliaCall::julia_setup()`];
#' * [`julia_send()`] wraps [`JuliaCall::julia_assign()`]:
#'    - The default method simply calls [`JuliaCall::julia_assign()`];
#'    - The default method handles `data.frame` objects;
#'    - The `SpatRaster` method translates [`terra::SpatRaster`]s to `GeoArray`s;
#' * [`julia_receive()`] wraps [`JuliaCall::julia_eval()`] wrapper:
#'    - The default method handles wraps [`JuliaCall::julia_eval()`] and handles [`data.frame`]s;
#' * [`julia_terminate()`] is a placeholder equivalent for [`juliaTerminate()`] ([`JuliaConnectoR::stopJulia()`]);
#'
#' @author Edward Lavender
#' @name JuliaCall-wrappers
NULL

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_initialise <- function(...) {
  julia_setup(...)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_send <- function(name, value) {
  UseMethod("julia_send", value)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal
#' @exportS3Method

julia_send.default <- function(name, value) {
  julia_assign(name, value)
}

# Send POSIXct vectors to Julia
#' @exportS3Method
julia_send.POSIXct <- function(name, value) {
  # JuliaCall may convert larger POSIXct vectors to integer epoch seconds;
  # convert explicitly so timestamps are always Julia DateTime values.
  julia_command("import Dates")
  julia_send(name, as.numeric(value))
  julia_cmd(glue("{name} = Dates.unix2datetime.({name})"))
}

#' @exportS3Method
julia_send.data.frame <- function(name, value) {
  julia_import("DataFrames")
  for (col in names(value)) {
    julia_send(col, value[[col]])
  }
  cols <- paste0(
    "Symbol(\"", names(value), "\") => ", names(value),
    collapse = ", "
  )
  julia_cmd(glue::glue("{name} = DataFrame({cols})"))
}

#' @rdname JuliaCall-wrappers
#' @keywords internal
#' @exportS3Method

julia_send.SpatRaster <- function(name, value) {
  julia_send_SpatRaster(name, value, julia_command)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_class <- function(x) {
  # type <- julia_eval(glue('string(nameof(typeof({x})))'))
  type <- julia_eval(glue('string(typeof(begin {x} end))'))
  type <- julia_class_parse(type)
  structure(list(), class = type)
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_receive <- function(x) {
  UseMethod("julia_receive", julia_class(x))
}

#' @rdname JuliaCall-wrappers
#' @keywords internal
#' @exportS3Method

julia_receive.default <- function(x) {
  julia_eval(x)
}

julia_receive_DateTime <- function(value) {
  if (!inherits(value, "POSIXct")) {
    value <- as.POSIXct(value, origin = "1970-01-01", tz = "UTC")
  }
  lubridate::with_tz(value, "UTC")
}

# Receive a DateTime
#' @exportS3Method
julia_receive.DateTime <- function(x) {
  julia_receive_DateTime(julia_eval(x))
}

# Receive a Vector of DateTimes
#' @exportS3Method
julia_receive.VectorDateTime <- function(x) {
  julia_receive_DateTime(julia_eval(x))
}

# Receive a DataFrame
#' @exportS3Method
julia_receive.DataFrame <- function(x) {
  value <- julia_eval(x)
  value[] <- lapply(value, function(column) {
    if (inherits(column, "POSIXct")) {
      column <- julia_receive_DateTime(column)
    }
    column
  })
  value
}

#' @rdname JuliaCall-wrappers
#' @keywords internal

julia_terminate <- function() {
  message("JuliaCall does not support termination of the Julia session.")
  nothing()
}
