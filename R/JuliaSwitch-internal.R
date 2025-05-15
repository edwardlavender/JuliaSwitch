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
