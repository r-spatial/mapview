.onLoad = function(libname, pkgname) {
  if (useFgb()) {
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
  if (useFgb()) {
    packageStartupMessage(msg)
    mapviewOptions(fgb = TRUE)
  }
}

.onDetach = function(libpath) {
  mapview::stopWatching()
}
