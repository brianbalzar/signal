# R/zzz.R
# Package load hook — registers the Signal www resource path so Shiny can
# serve styles.css via "signal-www/styles.css" when the package is installed.

.onLoad <- function(libname, pkgname) {
  www <- system.file("www", package = "signal", mustWork = FALSE)
  if (nchar(www) > 0 && dir.exists(www)) {
    shiny::addResourcePath("signal-www", www)
  }
}
