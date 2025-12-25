##########################
##########################
#### investigate.R

#### Aims
# (1) Investigate JuliaSwitch/JuliaCall/JuliaConnectoR behaviour

#### Prerequisites
# (1) NA


##########################
##########################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(JuliaSwitch)


##########################
##########################
#### SpatRaster -> Matrix (for Wahoo.jl)

# Push raster to Julia
r <- patter::dat_gebco()
julia_push("r", r)

# Push matrix to Julia
m <- terra::as.matrix(r, wide = TRUE)
julia_push("m", m)
julia_push("m2", t(m))

# Create matrix in Julia
julia_cmd_line('m3 = Matrix(r)')

# Are the two matrices the same?
# > No
julia_println('summary(m)')
julia_println('summary(m2)')
julia_println('summary(m3)')

julia_pull('m2[1]')
julia_pull('m3[1]')


#### End of code.
##########################
##########################
