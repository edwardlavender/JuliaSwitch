test_that("JuliaSwitch works", {

  if (curl::has_internet()) {

    # Define temporary Julia project
    temp <- file.path(tempdir(), "JuliaSwitch")

    # Run Julia code with each backend
    lapply(c("JuliaCall", "JuliaConnectoR"), function(backend) {

      #### Set Julia backend
      # backend <- "JuliaCall"
      # julia_backend(backend)
      # backend <- "JuliaConnectoR"
      julia_backend(backend)

      #### Start Julia
      # Start Julia, activate local environment & add DataFrames package
      dir.create(temp, showWarnings = FALSE)
      julia <- julia_start(JULIA_PROJ = temp)
      julia_pkg_activate(temp)
      # julia_pkg_add("DataFrames")
      # julia_pkg_add("Dates")
      # julia_pkg_add("OrderedCollections")
      julia_using("DataFrames")
      julia_using("Dates")
      julia_using("OrderedCollections")


      #### -----------------------------------------------------------------####
      #### Test julia_cmd

      # Test single line
      julia_cmd('x = 1')
      expect_equal(1, julia_pull("x"))

      # Test multi-line
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


      #### -----------------------------------------------------------------####
      #### Test julia_pkg_installed()

      expect_true(julia_pkg_installed("DataFrames"))
      expect_false(julia_pkg_installed("blah"))
      expect_identical(julia_pkg_installed(c("DataFrames", "blah")),
                       c(TRUE, FALSE))


      #### -----------------------------------------------------------------####
      #### Test julia_push() and julia_pull() handle time series

      # Define timeline
      timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                      as.POSIXct("2016-01-01 03:18:00", tz = "UTC"),
                      by = "2 mins")

      # Test for a single timestamp
      # > Note that tzone is maintained
      julia_push("timestamp", timeline[1])
      expect_equal(timeline[1], julia_pull("timestamp"))
      expect_equal(attr(timeline[1], "tzone"), "UTC")
      expect_equal(attr(julia_pull("timestamp"), "tzone"), "UTC")

      # Test for a vector of timestamps
      julia_push("timeline", timeline)
      expect_equal(timeline, julia_pull("timeline"))

      # Test for a one-row dataframe
      d <- data.frame(timestamp = timeline[1], timestep = 1)
      julia_push("d", d)
      expect_equal(d$timestamp[1], julia_pull('d.timestamp[1]'))
      expect_equal(d$timestamp, julia_pull('d.timestamp'), ignore_attr = FALSE)
      expect_equal(d, julia_pull("d"), ignore_attr = FALSE)

      # Test for a multi-row data.frame
      d <- data.frame(timestamp = timeline[1:5], timestep = 1:5)
      julia_push("d", d)
      expect_equal(d$timestamp[1], julia_pull('d.timestamp[1]'))
      expect_equal(d$timestamp, julia_pull('d.timestamp'), ignore_attr = FALSE)
      expect_equal(d, julia_pull("d"), ignore_attr = FALSE)

      # NB with big dataframes tzone is not maintained
      # TO DO TO FIX


      #### -----------------------------------------------------------------####
      #### Test julia_push() handles lists

      ## Test empty list
      julia_push("a", list())
      expect_true(julia_pull("a == Any[]"))
      expect_equal(julia_pull("a"), list())

      ## Test simple unnamed list
      a <- list(1, c(1, 2))
      julia_push("a", a)
      expect_true(julia_pull("a == Any[1.0, [1.0, 2.0]]"))
      expect_equal(julia_pull("a"), a)

      ## Test unnamed list of data.frames
      # Define unnamed list & send to Julia
      ModelObsAcousticLogisTrunc <- data.frame(
        timestamp = as.POSIXct(c("2016-01-01 00:00:00", "2016-01-01 00:00:00"), tz = "UTC"),
        obs = c(0L, 0L),
        sensor_id = c(1L, 2L),
        receiver_x = c(709142.1, 698042.1),
        receiver_y = c(6266607.0, 6267507.0),
        receiver_alpha = c(4, 4),
        receiver_beta = c(-0.01, -0.01),
        receiver_gamma = c(750, 750)
      )
      ModelObsDepthUniformSeabed <- data.frame(
        timestamp = as.POSIXct(c("2016-01-01 00:00:00", "2016-01-01 00:02:00"), tz = "UTC"),
        obs = c(27.79555, 36.54936),
        sensor_id = c(1L, 1L),
        depth_shallow_eps = c(10, 10),
        depth_deep_eps = c(10, 10)
      )
      yobs <- list(ModelObsAcousticLogisTrunc, ModelObsDepthUniformSeabed)
      julia_push("yobs_vect", yobs)
      julia_println("yobs_vect")
      # Define expected structure in Julia
      julia_cmd(
        '
        yobs_vect_expected = [
        DataFrame(
            timestamp = [DateTime(2016,1,1,0,0), DateTime(2016,1,1,0,0)],
            obs = [0, 0],
            sensor_id = [1, 2],
            receiver_x = [709142.1, 698042.1],
            receiver_y = [6266607.0, 6267507.0],
            receiver_alpha = [4.0, 4.0],
            receiver_beta = [-0.01, -0.01],
            receiver_gamma = [750.0, 750.0]
          ),
          DataFrame(
            timestamp = [DateTime(2016,1,1,0,0), DateTime(2016,1,1,0,2)],
            obs = [27.79555, 36.54936],
            sensor_id = [1, 1],
            depth_shallow_eps = [10.0, 10.0],
            depth_deep_eps = [10.0, 10.0]
          )
        ]
        '
      )
      # Expect match
      # julia_println("yobs_vect_expected")
      # julia_println("yobs_vect")
      expect_true(julia_pull("yobs_vect == yobs_vect_expected"))

      ## Test simple named list
      a <- list(a = 1, b = c(1, 2))
      julia_push("a", a)
      expect_true(julia_pull("a == OrderedCollections.OrderedDict{Symbol, Any}(:a => 1.0, :b => [1.0, 2.0])"))
      # expect_equal(julia_pull("a"), a) # TO DO Implement OrderedCollections.OrderedDict method

      ## Test named list with DataFrames
      # Define list in R & send to Julia
      yobs <- list(
        ModelObsAcousticLogisTrunc = ModelObsAcousticLogisTrunc,
        ModelObsDepthUniformSeabed = ModelObsDepthUniformSeabed)
      julia_push("yobs_vect", yobs)
      # Define expected structure in Julia
      julia_cmd(
        '
        yobs_vect_expected = OrderedCollections.OrderedDict{Symbol,Any}(
          :ModelObsAcousticLogisTrunc => DataFrame(
            timestamp = [DateTime(2016,1,1,0,0), DateTime(2016,1,1,0,0)],
            obs = [0, 0],
            sensor_id = [1, 2],
            receiver_x = [709142.1, 698042.1],
            receiver_y = [6266607.0, 6267507.0],
            receiver_alpha = [4.0, 4.0],
            receiver_beta = [-0.01, -0.01],
            receiver_gamma = [750.0, 750.0]
          ),
          :ModelObsDepthUniformSeabed => DataFrame(
            timestamp = [DateTime(2016,1,1,0,0), DateTime(2016,1,1,0,2)],
            obs = [27.79555, 36.54936],
            sensor_id = [1, 1],
            depth_shallow_eps = [10.0, 10.0],
            depth_deep_eps = [10.0, 10.0]
          )
        )
        '
      )
      # Expect match
      # julia_println("yobs_vect_expected")
      # julia_println("yobs_vect")
      expect_true(julia_pull("yobs_vect == yobs_vect_expected"))


      #### -----------------------------------------------------------------####
      #### Test julia_pull() handles lists

      ## Test nested list of dataframes
      # Define nested list in R
      df1 <- data.frame(timestamp = as.POSIXct(c("2020-01-01 00:00:00",
                                                 "2020-01-01 00:01:00",
                                                 "2020-01-01 00:02:00"),
                                               tz = "UTC"),
                        obs = c(0, 1, 0))
      df2 <- data.frame(timestamp = as.POSIXct(c("2020-01-01 00:00:00",
                                                 "2020-01-01 00:02:00",
                                                 "2020-01-01 00:04:00"),
                                               tz = "UTC"),
                        value = c(10, 20, 30))
      x <- list(list(df1), list(df2))
      # Define equivalent nested Vector in Julia
      julia_cmd(
        '
        df1 = DataFrame(timestamp = DateTime(2020):Minute(1):DateTime(2020,1,1,0,2),
                        obs = [0, 1, 0])
        df2 = DataFrame(timestamp = DateTime(2020):Minute(2):DateTime(2020,1,1,0,4),
                        value = [10.0, 20.0, 30.0])
        x = Vector{Any}(undef, 2)
        x[1] = Vector{Any}([df1])
        x[2] = Vector{Any}([df2])
        '
      )
      expect_equal(x, julia_pull("x"))

      #### Test julia_pull handles named tuples

      ## Define NamedTuple in Julia
      julia_cmd(
        '
      states = DataFrame(
          path_id = [1, 1],
          timestep = [1, 2],
          timestamp = [DateTime(2016,1,1,0,0), DateTime(2016,1,1,0,2)],
          map_value = [34.1, 33.2],
          x = [7.11e5, 7.10e5],
          y = [6.26e6, 6.2601e6])


      diagnostics = DataFrame(
        timestep = [1, 2],
        timestamp = [DateTime(2016,1,1,0,0), DateTime(2016,1,1,0,2)],
        ess = [31054.9, 29879.2],
        maxlp = [-3.11, -2.99])

      callstats = DataFrame(
        timestamp = [DateTime(2025,12,24,15,7,55)],
        routine = ["filter: forward"],
        n_particle = [50000],
        n_iter = [1],
        loglik = [-418.9],
        convergence = [true],
        time = [1.44])

      nt = (states = states, diagnostics = diagnostics, callstats = callstats)
      ')

      ## Define expected object in R
      states <- data.frame(path_id = c(1L, 1L),
                           timestep = c(1L, 2L),
                           timestamp = as.POSIXct(
                             c("2016-01-01 00:00:00", "2016-01-01 00:02:00"),
                             tz = "UTC"),
                           map_value = c(34.1, 33.2),
                           x = c(711000, 710000),
                           y = c(6260000, 6260100))
      diagnostics <- data.frame(timestep = c(1L, 2L),
                                timestamp = as.POSIXct(
                                  c("2016-01-01 00:00:00", "2016-01-01 00:02:00"),
                                  tz = "UTC"),
                                ess = c(31054.9, 29879.2),
                                maxlp = c(-3.11, -2.99))
      callstats <- data.frame(timestamp = as.POSIXct("2025-12-24 15:07:55", tz = "UTC"),
                              routine = "filter: forward",
                              n_particle = 50000L,
                              n_iter = 1L,
                              loglik = -418.9,
                              convergence = TRUE,
                              time = 1.44)
      pf_particles <- list(states = states,
                           diagnostics = diagnostics,
                           callstats = callstats)
      expect_equal(pf_particles, julia_pull("nt"), ignore_attr = TRUE)


      #### -----------------------------------------------------------------####
      # Test handling of in-memory & arrow transfers (JuliaConnectoR)

      # Test for small & big vectors
      lapply(c(10L, 1e5L), function(n) {

        # Integer vector
        # > Following implementation of Arrow, the big vector should be relatively fast
        n      <- as.integer(n)
        input  <- runif(n)
        julia_push("x", input)
        expect_equal(input, julia_pull("x"))

        # Integer vector with missing
        julia_push("n", n)
        julia_cmd('x = [rand(n); fill(missing, 2)]')
        expect_equal(c(NA_real_, NA_real_), tail(julia_pull("x"), 2))

        # Character
        input  <- sample(c("a", "b", "c"), n, replace = TRUE)
        julia_push("x", input)
        expect_equal(input, julia_pull("x"))

        # String
        input  <- sample(c("apple", "pear", "orange"), n, replace = TRUE)
        julia_push("x", input)
        expect_equal(input, julia_pull("x"))

        # Boolian
        input  <- sample(c(TRUE, FALSE), n, replace = TRUE)
        julia_push("x", input)
        expect_equal(input, julia_pull("x"))

        # Timeline
        # * NB With arrow, we lose the timestamp attribute (UTC -> GMT)
        input <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC") + seq_len(n) * 60
        julia_push("x", input)
        expect_equal(input, julia_pull("x"), ignore_attr = TRUE)

        # Data.frame
        input <- data.frame(timestamp = as.POSIXct("2025-01-01 00:00:00", tz = "UTC") + seq_len(n) * 60,
                            value = runif(n))
        output <- julia_push("x", input)
        expect_equal(input, julia_pull("x"), ignore_attr = TRUE)

      })


      #### -----------------------------------------------------------------####
      #### Clean up

      julia_stop()
      unlink(temp, recursive = TRUE)

    })

  }

})
