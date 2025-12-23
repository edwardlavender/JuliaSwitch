# JuliaSwitch 0.0.2

* Export new functions:
    - `julia_cmd()`, to run arbitrary code;
    - `julia_pkg_installed()`, to check if package(s) are installed;
    - `julia_pkg_update()`, to update package(s);
    - `julia_helpfile()`, to print a help file;
    
* Improve flexibility:
    - `julia_pkg_*()` functions support multiple packages;
    
* Update internal functions
    - Add `julia_initalise()` and `juliaInitialise()` wrappers;
    - Rename `julia_allot()` and `juliaAllot()` to `julia_send()` and `JuliaSend()`;
    - Add/rename `julia_receive()` and `juliaReceiver()` (formerly `juliaTranslate()`);

# JuliaSwitch 0.0.1

This is the initial version of the package, used in Lavender, E., Albert, C., & Scheidegger, A. (2025). Animal geolocation with convolution algorithms in Julia and R via Wahoo.jl. Methods in Ecology and Evolution, 00, 1–8. https://doi.org/10.1111/2041-210x.70185
