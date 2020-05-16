

cirq <- NULL

.onLoad <- function(libname, pkgname) {

    cirq <<- reticulate::import("cirq", delay_load = list(
      priority = 10,
      environment = "r-tensorflow"
    ))

}
