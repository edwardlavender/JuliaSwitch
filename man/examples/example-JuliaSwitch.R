# Load packages
library(glue)

# Define temporary Julia project
temp <- file.path(tempdir(), "JuliaSwitch")
dir.create(temp)

# Run Julia code with each backend
lapply(c("JuliaCall", "JuliaConnectoR"), function(backend) {

  #### Set Julia backend
  # julia_backend("JuliaCall")
  # julia_backend("JuliaConnectoR")
  julia_backend(backend)

  #### Start Julia
  # Start Julia
  julia <- julia_start()
  # Activate local environment
  # * Add DataFrames package for DataFrames example
  julia_cmd_line('import Pkg')
  julia_cmd_line(glue('Pkg.activate("{temp}")'))
  julia_cmd_line('Pkg.add("DataFrames")')

  #### Run one-line commands
  julia_cmd_line("x = 1")
  julia_cmd_line("println(x)")

  #### Run larger commands
  julia_cmd_block(
    '
  x = 2
  y = 3
  '
  )
  julia_cmd_line("println(x)")
  julia_cmd_line("println(y)")

  #### Push objects to Julia
  # Push a matrix
  m1 <- matrix(1:4)
  julia_push("m1", m1)
  julia_cmd_line('m1')
  # Push a data.frame
  d1 <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  julia_push("d1", d1)
  julia_cmd_line('println(d1)')

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

