
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "www",
    system.file(
      "www",
      package = "dischargecriteria"
    )
  )
}

.onUnload <- function(libname, pkgname) {
   shiny::removeResourcePath("www")
}