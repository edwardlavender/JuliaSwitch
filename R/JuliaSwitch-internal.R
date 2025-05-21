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

# Assign SpatRasters
julia_allot_SpatRaster <- function(name, value, command) {
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
