##########################
##########################
#### benchmark.R

#### Aims
# (1) Benchmark JuliaCall, JuliaConnectoR and JuliaSwitch

#### Prerequisites
# (1) NA


##########################
##########################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Load essential packages
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(glue)
library(JuliaCall)
library(JuliaConnectoR)
library(JuliaSwitch)
library(testthat)
library(tibble)
library(tictoc)
library(proj.verse)

#### Start Julia
# JuliaCall
julia_setup()
# JuliaConnectoR
# > Start server
startJuliaServer()
# Set backend for JuliaSwitch functions
julia_backend("JuliaConnectoR")
# > Load packages normally loaded by JuliaSwitch::julia_start()
julia_using("Arrow")
julia_using("DataFrames")
julia_using("Random")
# Source JuliaConnectoR helpers
JuliaSwitch:::julia_helpers()

#### Define helpers

# Inform
inform <- function(...) {
  print(paste(Sys.time(), ":", ...))
}

# Compute elapsed time
elapsed <- function(x) {
  system.time(x)[["elapsed"]]
}

# Pull an object using a 'full' function & time the operation
xelapsed <- function(x, pull) {
  t1 <- Sys.time()
  y  <- pull(x)
  t2 <- Sys.time()
  list(x = y, t = as.numeric(difftime(t2, t1, units = "secs")))
}

# Minimal juliaEvalDataFrame function
# > Pull a DataFrame via TCP
# > This assumes the columns a simple e.g., not time stamps
#   (so they can be handled by juliaEval)
juliaEvalDataFrame <- function(x) {
  # Collect columns
  headings <- juliaEval(glue("Base.names({x})"))
  columns <- lapply(headings, \(heading) juliaEval(glue("{x}[:, :{heading}]")))
  names(columns) <- headings
  # Build data.frame
  columns |>
    dplyr::bind_cols() |>
    as.data.frame()
}

# Minimal juliaFeatherDataFrame function
# > Pull a DataFrame via feather
juliaFeatherDataFrame <- function(x) {
  inform("Checking bytes...")
  bytes       <- juliaEval(glue("Base.summarysize({x})"))
    inform("Defining tmp_feather")
    tmp_feather <- file.path(tempdir(), "tmp.feather")
    julia_push("tmp_feather", tmp_feather)
    inform("Arrow.write...")
    julia_cmd_line(glue('Arrow.write(tmp_feather, {x})'))
    on.exit(unlink(tmp_feather), add = TRUE)
    inform("arrow::read_feather...")
    arrow::read_feather(tmp_feather) |>
      as.data.frame()
}


##########################
##########################
#### Benchmark R-side behaviour

#### Benchmark numeric -> as.POSIXct
# This step is required for JuliaConnectoR in juliaReceive.DataTime
# > This step is fast
nt <- 1e7L
tin  <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq_len(nt) - 1
tnum <- as.numeric(tin)
tic()
tout <- as.POSIXct(tnum, origin = "1970-01-01", tz = "UTC")
toc()


##########################
##########################
#### Benckmark packages
# Benchmark package push/pull speeds for different object sizes

#### Build benchmark data.table (~126 s)
# For patter, we may have > 20000 time steps * 1000 particles * 5 fields
tic()
ns <- as.integer(c(1, 10, 20, 100, 500, 1e3, 1e4, 1e5, 2.5e5, 5e5, 1e6))
bm_pp <-
  cl_lapply(ns, function(n) {

  # Define object
  inform(paste("-------", n, "---------"))
  x <- runif(n)

  # JuliaCall push (0.088 s [n = 1e6])
  tic()
  t1 <- elapsed(julia_assign("x", x))
  toc()

  # JuliaCall pull (0.021 s [n = 1e6])
  tic()
  t2 <- xelapsed("x", julia_eval)
  toc()

  # JuliaConnectoR push (0.038 s [n = 1e6])
  tic()
  t3 <- elapsed(juliaCall("__assign_from_JuliaConnectoR__", "x", x))
  toc()

  # JuliaConnectoR pull (~27.689 s [n = 1e6])
  tic()
  t4 <- xelapsed("x", juliaEval)
  toc()

  # JuliaSwitch push (~0.004 s [n = 1e6])
  tic()
  t5 <- elapsed(julia_push("x", x))
  toc()

  # JuliaSwitch pull (~27.796 [n = 1e6])
  tic()
  t6 <- xelapsed("x", julia_pull)
  toc()

  # Checks
  expect_equal(t2$x, t4$x)
  expect_equal(t4$x, t6$x)

  # Record times
  tribble(
    ~package,                     ~operation, ~n, ~time,
    "JuliaCall",                  "push",       n, t1,
    "JuliaCall",                  "pull",       n, t2$t,
    "JuliaConnectoR",             "push",       n, t3,
    "JuliaConnectoR",             "pull",       n, t4$t,
    "JuliaSwitch-JuliaConnectoR", "push",       n, t5,
    "JuliaSwitch-JuliaConnectoR", "pull",       n, t6$t
  )

}) |> rbindlist() |>
  mutate(package = factor(package, levels = c("JuliaCall",
                                              "JuliaConnectoR",
                                              "JuliaSwitch-JuliaConnectoR")),
         operation = factor(operation, levels = c("push", "pull"))) |>
  as.data.table()
toc()

#### Plot the growth in computation time for each package
png("./dev/benchmark-packages.png",
    height = 10, width = 10, units = "in", res = 600)
p <-
  bm_pp |>
  ggplot(aes(n, time, colour = package)) +
  geom_line() +
  geom_point() +
  xlab("n") + ylab("Elapsed time (s)") +
  facet_wrap(~operation, scales = "free_y")
print(p)
dev.off()
plotly::ggplotly(p)


##########################
##########################
#### Benchmark juliaEval
# For JuliaConnectoR, benchmark juliaEval vs. feather for different object sizes

#### Build benchmark data.table (~ 57 s)
tic()
bm_df <-
  lapply(ns, function(n) {

    print(paste(" ----------- On n", n, "-----------"))

    # Define DataFrame in Julia
    inform("Pushing n")
    n <- as.integer(n)
    julia_push("n", n)
    inform("Defining df")
    julia_cmd('df = DataFrame(x = rand(n))')
    # julia_println('first(df, 6)')
    print("Printing nrow(df)...")
    julia_println('nrow(df)')

    # Pull via juliaEvalDataFrame()
    inform("Pulling df via juliaEvalDataFrame()...")
    t1  <- Sys.time()
    df1 <- juliaEvalDataFrame("df")
    t2  <- Sys.time()
    td1 <- as.numeric(difftime(t2, t1, "secs"))

    # Pull via juliaFeatherDataFrame
    inform("Pulling df via juliaFeatherDataFrame()...")
    t1  <- Sys.time()
    df2 <- juliaFeatherDataFrame("df")
    t2  <- Sys.time()
    td2 <- as.numeric(difftime(t2, t1, "secs"))

    # Pull via julia_pull -> juliaReceive()
    # * This should maintain reasonable speed for small/big datasets
    #   as JuliaSwitch swaps between juliaEval() and Arrow depending on data size
    inform("Pulling df via julia_pull()...")
    t1  <- Sys.time()
    df3 <- julia_pull("df")
    t2  <- Sys.time()
    td3 <- as.numeric(difftime(t2, t1, "secs"))

    # Checks
    expect_equal(df1, df2, ignore_attr = TRUE)
    expect_equal(df2, df3, ignore_attr = TRUE)

    # Collate times
    tribble(
      ~package,         ~operation,     ~n, ~time,
      "JuliaConnectoR", "juliaEvalDataFrame",      n, td1,
      "JuliaConnectoR", "juliaFeatherDataFrame",   n, td2,
      "JuliaConnectoR", "juliaReceive",            n, td3
    )

}) |> rbindlist()
toc()

#### Plot the growth in computation time for juliaEval vs. juliaFeather
png("./dev/benchmark-feather.png",
    height = 10, width = 10, units = "in", res = 600)
p <-
  bm_df |>
  ggplot(aes(n, time, colour = operation, lty = operation)) +
  geom_line() +
  geom_point() +
  xlab("n") + ylab("Elapsed time (s)")
print(p)
dev.off()
plotly::ggplotly(p)

# > juliaFeatherDataFrame seems to be faster by > 100 rows & certainly by 1000 rows


#### End of code.
##########################
##########################
