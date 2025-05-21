# The following suggested packages are required for these examples
library(curl)
library(terra)

# Run examples if internet is available
# This is required to install selected Julia packages (below)
if (curl::has_internet()) {

  # Define temporary Julia project
  temp <- file.path(tempdir(), "JuliaSwitch")

  # Run Julia code with each backend
  lapply(c("JuliaCall", "JuliaConnectoR"), function(backend) {

    #### Set Julia backend
    # julia_backend("JuliaCall")
    # julia_backend("JuliaConnectoR")
    julia_backend(backend)

    #### Start Julia
    # Start Julia, activate local environment & add DataFrames package
    dir.create(temp, showWarnings = FALSE)
    julia <- julia_start()
    julia_pkg_activate(temp)
    julia_pkg_add("DataFrames")
    julia_pkg_add("GeoArrays")

    #### Run one-line commands
    julia_cmd_line("x = 1")
    julia_println("x")

    #### Run larger commands
    julia_cmd_block(
      '
      x = 2
      y = 3
    '
    )
    julia_println("x")
    julia_println("y")

    #### Push objects to Julia
    # Push a matrix
    m1 <- matrix(1:4)
    julia_push("m1", m1)
    julia_println("m1")
    # Push a data.frame
    d1 <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
    julia_push("d1", d1)
    julia_println("d1")
    # Push a SpatRaster
    r1 <- terra::rast(system.file("ex", "elev.tif",
                                  package = "terra", mustWork = TRUE))
    julia_push("r1", r1)
    julia_println("r1")

    #### Pull objects from Julia
    # Pull the matrix
    m2 <- julia_pull('m1')
    stopifnot(identical(m1, m2))
    # Pull the data.frame
    d2 <- julia_pull('d1')
    stopifnot(identical(d1, d2))

    #### Close Julia session
    julia_stop()

    # Clean up
    unlink(temp, recursive = TRUE)

  })

}
