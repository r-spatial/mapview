.onLoad = function(libname, pkgname) {
  if (sf::sf_extSoftVersion()[["GDAL"]] >= "3.1.0") {
    mapviewOptions(fgb = TRUE)
  }
}

.onUnload = function(libpath) {
  mapview::stopWatching()
}

.onAttach = function(libname, pkgname) {
  msg = sprintf(
    "GDAL version >= 3.1.0 | setting mapviewOptions(fgb = TRUE)"
  )
  if (sf::sf_extSoftVersion()[["GDAL"]] >= "3.1.0") {
    packageStartupMessage(msg)
    mapviewOptions(fgb = TRUE)
  }
}

.onDetach = function(libpath) {
  mapview::stopWatching()
}
