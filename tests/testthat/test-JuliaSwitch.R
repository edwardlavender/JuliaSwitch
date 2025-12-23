test_that("JuliaSwitch works", {

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

      #### Test julia_cmd
      # Test single line
      julia_cmd('x = 1')
      expect_equal(1, julia_pull("x"))
      julia_cmd(
        '
        a = 1
        b = 2
        c = 3
        '
      )
      expect_equal(1, julia_pull("a"))
      expect_equal(2, julia_pull("b"))
      expect_equal(3, julia_pull("c"))

      #### Test julia_pkg_installed()
      expect_true(julia_pkg_installed("DataFrames"))
      expect_false(julia_pkg_installed("blah"))
      expect_identical(julia_pkg_installed(c("DataFrames", "blah")),
                       c(TRUE, FALSE))

      #### Test julia_push() and julia_pull() handle time series
      # Define timeline
      timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                      as.POSIXct("2016-01-01 03:18:00", tz = "UTC"),
                      by = "2 mins")
      # Test for a single timestamp
      julia_push("timestamp", timeline[1])
      expect_equal(timeline[1], julia_pull("timestamp"))
      # Test for a vector of timestamps
      julia_push("timeline", timeline)
      expect_equal(timeline, julia_pull("timeline"))
      # Test for a one-row dataframe
      d <- data.frame(timestamp = timeline[1], timestep = 1)
      julia_push("d", d)
      expect_equal(d$timestamp[1], julia_pull('d.timestamp[1]'))
      expect_equal(d$timestamp, julia_pull('d.timestamp'), ignore_attr = TRUE)
      expect_equal(d, julia_pull("d"), ignore_attr = TRUE)
      # Test for a multi-row data.frame
      d <- data.frame(timestamp = timeline[1:5], timestep = 1:5)
      julia_push("d", d)
      expect_equal(d$timestamp[1], julia_pull('d.timestamp[1]'))
      expect_equal(d$timestamp, julia_pull('d.timestamp'), ignore_attr = TRUE)
      expect_equal(d, julia_pull("d"), ignore_attr = TRUE)

      #### Test julia_push() and julia_pull() handle Dictionaries
      # TO DO

      # Clean up
      julia_stop()
      unlink(temp, recursive = TRUE)

    })

  }

})

