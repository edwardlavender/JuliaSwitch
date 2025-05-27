
# `JuliaSwitch`

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/patter)](https://CRAN.R-project.org/package=patter)

`JuliaSwitch` is a high-level `R`–`Julia` interface. The package
provides a common syntax that enables users to switch between multiple
backends for `R`–`Julia` communication without changes to the code base.

- **Run `Julia` code** via a `julia_*()` helper function or
  `julia_cmd_line()`/`julia_cmd_block()`;
- **Push objects to `Julia`** using `julia_push()`;
- **Pull objects back** via `julia_pull()`;

# Motivation

`JuliaSwitch` was motivated our work on animal tracking with `patter`.
This package provides an `R` interface to `Patter.jl`, which fit
state-space models to animal-tracking data in `Julia`.

> When `patter` was first developed, we used
> [`JuliaCall`](https://github.com/JuliaInterop/JuliaCall) as a
> `R`–`Julia` interface. That package connects `R` and `Julia` via a `C`
> interface, which is fast but can be unstable across `Julia` versions
> and platforms. (We experienced particular challenges on Linux when
> using `R` and `Julia` packages that require the same dynamic
> libraries.)

> [`JuliaConnectoR`](https://github.com/stefan-m-lenz/JuliaConnectoR) is
> another `R`–`Julia` option. Unlike `JuliaCall`, this package
> interfaces `R` and `Julia` using Transmission Control Protocol. This
> is a looser form of integration which trades speed for stability.

`JuliaSwitch` became the solution to toggle between these two options.

# Installation

`JuliaSwitch` can be installed with:

``` r
install.packages("devtools")
devtools::install_github("edwardlavender/JuliaSwitch", 
                         dependencies = TRUE)
```

# Functionality

- Run `julia_backend("JuliaCall")` or `julia_backend("JuliaConnectoR")`
  to set the `Julia` backend.

- Start a `Julia` session via `julia_start()`.

- Run arbitrary `Julia` code via `julia_cmd_line()` and
  `julia_cmd_block()`.

- For selected operations, helper functions are provided:

  - `julia_pkg_activate()` activates a local environment;
  - `julia_pkg_add()` adds packages;
  - `julia_using()` and `julia_import()` load and import packages;
  - `julia_include()` sources `Julia` scripts;
  - `julia_println()` prints lines;
  - `julia_save()` and `julia_load()` save and load files;

- To push objects from `R` to `Julia`, use `julia_push()`.

- To pull objects back from `Julia` to `R`, use `julia_pull()`.

- Stop a `Julia` session via `julia_stop()`.

# `S3` methods

`JuliaSwitch` uses `S3` methods to handle object transfers between `R`
and `Julia`. Method dispatch is implemented by the internal functions
`julia_allot()`, `juliaAllot()` and `juliaTranslate()`.The default
methods call the relevant `JuliaCall` or `JuliaConnectoR` routines.
Special cases (such as `data.frame`s and `terra::SpatRaster`s) are
handled by custom methods. Specify additional methods for other special
cases.

# Code of conduct

Please note that the `JuliaSwitch` project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
