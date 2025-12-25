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
library(JuliaCall)
library(JuliaConnectoR)
library(JuliaSwitch)
library(tibble)
library(tictoc)
library(proj.verse)

#### Start Julia
julia_setup()
startJuliaServer()
julia_backend("JuliaConnectoR")
JuliaSwitch:::julia_helpers()

#### Define helpers
# inform
inform <- function(...) {
  print(paste(Sys.time(), ":", ...))
}
# Compute elaspsed time
elapsed <- function(x) {
  system.time(x)[["elapsed"]]
}
# juliaReceive 'df' via feather
juliaFeather <- function() {
  inform("Checking bytes...")
  bytes       <- julia_pull("Base.summarysize(df)")
  if (bytes > 1L) {
    inform("Defining tmp_feather")
    tmp_feather <- file.path(tempdir(), "tmp.feather")
    julia_push("tmp_feather", tmp_feather)
    inform("Arrow.write...")
    julia_cmd_line('Arrow.write(tmp_feather, df)')
    on.exit(unlink(tmp_feather), add = TRUE)
    inform("arrow::read_feather...")
    arrow::read_feather(tmp_feather)
    inform("Unlinking tmp_feather...")
  }
  nothing()
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
  x <- runif(n)

  # JuliaCall push (0.088 s [n = 1e6])
  tic()
  t1 <- julia_assign("x", x) |> elapsed()
  toc()

  # JuliaCall pull (0.021 s [n = 1e6])
  tic()
  t2 <- julia_eval("x") |> elapsed()
  toc()

  # JuliaConnectoR push (0.038 s [n = 1e6])
  tic()
  t3 <- juliaCall("__assign_from_JuliaConnectoR__", "x", x) |> elapsed()
  toc()

  # JuliaConnectoR pull (~27.689 s [n = 1e6])
  tic()
  t4 <- juliaEval("x") |> elapsed()
  toc()

  # JuliaSwitch push (~0.004 s [n = 1e6])
  tic()
  t5 <- julia_push("x", x) |> elapsed()
  toc()

  # JuliaSwitch pull (~27.796 [n = 1e6])
  tic()
  t6 <- julia_pull("x") |> elapsed()
  toc()

  # Record times
  tribble(
    ~package,                     ~operation, ~n, ~time,
    "JuliaCall",                  "push",       n, t1,
    "JuliaCall",                  "pull",       n, t2,
    "JuliaConnectoR",             "push",       n, t3,
    "JuliaConnectoR",             "pull",       n, t4,
    "JuliaSwitch-JuliaConnectoR", "push",       n, t5,
    "JuliaSwitch-JuliaConnectoR", "pull",       n, t6
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
julia_using("Arrow")
julia_using("DataFrames")
julia_using("Random")
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

    # Pull via julia_pull() and ultimately juliaEval()
    # (julia_pull() is needed for proper data.table translation)
    inform("Pulling df via julia_pull()...")
    tic()
    t1 <- julia_pull("df") |> elapsed()
    toc()

    # Pull via feather
    inform("Pulling df() via juliaFeather()...")
    tic()
    t2 <- juliaFeather() |> elapsed()
    toc()

    tribble(
      ~package,        ~operation,     ~n, ~time,
      "JuliaConnectoR","juliaEval",      n, t1,
      "JuliaConnectoR","juliaFeather",   n, t2
    )

}) |> rbindlist()
toc()

#### Plot the growth in computation time for juliaEval vs. juliaFeather
png("./dev/benchmark-feather.png",
    height = 10, width = 10, units = "in", res = 600)
p <-
  bm_df |>
  ggplot(aes(n, time, colour = operation)) +
  geom_line() +
  geom_point() +
  xlab("n") + ylab("Elapsed time (s)")
print(p)
dev.off()
plotly::ggplotly(p)

# > juliaFeather seems to be faster by > 100 rows & certainly by 1000 rows


#### End of code.
##########################
##########################
