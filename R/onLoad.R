# .onAttach <- function(libname, pkgname) {
#   
# }

#' Set-up for sbdi4r during loading
#' @noRd
#' @import galah
#' @keywords Internal
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("This is a wrapper for the package galah-R designed to work with data hosted by the Swedish Biodiversity Data Infrastructure (SBDI).")
  if (pkgname == "sbdi4r2") {
    requireNamespace("galah")
    sbdi_config(atlas = "Sweden") # to cache defaults
  }
}

