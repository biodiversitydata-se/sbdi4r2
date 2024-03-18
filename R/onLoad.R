.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is a wrapper for the package galah-R designed to work with data hosted by the Swedish Biodiversity Data Infrastructure (SBDI).")
}

#' Set-up for galah during loading
#' @noRd
#@import conflicts
#' @keywords Internal
.onLoad <- function(libname, pkgname) {
  if (pkgname == "sbdi4r2") {
    # brew(.pkg = "galah")
    library(galah)
    # conflicts()
    sbdi_config(atlas = "Sweden") # to cache defaults
  }
}

