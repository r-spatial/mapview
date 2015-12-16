### This functionality is a modified version of rasterOptions.
### (c) Robert J. Hijmans

#' Global options for the mapview package
#'
#' @description
#' To permanently set any of these options, you can add them to
#' <your R installation>/etc/Rprofile.site>. For example,
#' to change the default number of pixels to be visualised for Raster* objects,
#' add a line like this: options(mapviewMaxPixels = 700000) to that file.
#'
#' @param platform character. The platform to be used (currently only "leaflet"
#' is allowed)
#' @param basemaps character. The basemaps to be used for rendering data. See
#' \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for possible
#' values
#' @param raster.size numeric. see the maxBytes argument in \code{\link{addRasterImage}}
#' @param maxpixels numeric. The maximum amount of pixels allowed for Raster*
#' objects to be rendered. Defaults to 500000. Set this higher if you have a
#' potent machine or are patient enough to wait a little
#' @param maxpolygons numeric. Maximum number of polygons allowed for leaflet overlay
#' rendering. If this number is exceeded rendering will be done
#' using special functionality which will provide much more speed and better handling.
#' This means that standard functionality is reduced.
#' For example adding layers via "+" is not possible anymore.
#' @param maxpoints numeric. Maximum number of points allowed for leaflet overlay
#' rendering. If this number is exceeded rendering will be done
#' using special functionality which will provide much more speed and better handling.
#' This means that standard functionality is reduced.
#' For example adding layers via "+" is not possible anymore.
#' @param maxlines numeric. Maximum number of lines allowed for leaflet overlay
#' rendering. If this number is exceeded rendering will be done
#' using special functionality which will provide much more speed and better handling.
#' This means that standard functionality is reduced.
#' For example adding layers via "+" is not possible anymore.
#' @param raster.palette a color palette function for raster visualisation.
#' Should be a function that takes an integer as input and returns a vector of colors.
#' See \code{\link{colorRampPalette}} for details.
#' @param vector.palette a color palette function for vector visualisation.
#' Should be a function that takes an integer as input and returns a vector of colors.
#' See \code{\link{colorRampPalette}} for details.
#' @param verbose logical. Many functions in mapview provide details about their
#' behaviour. Set this to TRUE if you want to see these printed to the console
#' @param na.color character. The default color to be used for NA values.
#' This is relevant for Raster* objects
#' @param default logical. If TRUE all options are set to their default values
#' @param console logical. Should the options be printed to the console
#' @param layers.control.pos character. Where should the layer control be
#' placed? One of "topleft", "topright", "bottomleft", "bottomright".
#'
#' @author
#' Tim Appelhans
#'
#' @return
#' list of the current options (invisibly).
#' If no arguments are provided the options are printed.
#'
#' @seealso
#' \code{\link{rasterOptions}}, \code{\link{options}}
#'
#' @examples
#' \dontrun{
#' mapviewOptions()
#' mapviewOptions(na.color = "pink")
#' mapviewOptions()
#'
#' mapviewGetOption("platform")
#' }
#'
#'
#' @export mapviewOptions
#' @name mapviewOptions
#' @rdname mapviewOptions
#' @aliases mapviewOptions

mapviewOptions <- function(platform,
                           basemaps,
                           raster.size,
                           maxpixels,
                           maxpoints,
                           maxpolygons,
                           maxlines,
                           raster.palette,
                           vector.palette,
                           verbose,
                           na.color,
                           layers.control.pos,
                           default = FALSE,
                           console = TRUE) {

  ## platform
  setPlatform <- function(platform) {
    if (!platform %in% c("leaflet")) {
      warning("currently only platform leaflet is allowed")
      options(mapviewPlatform = "leaflet")
    } else {
      options(mapviewPlatform = platform)
    }
  }

  ## basemaps
  setBasemaps <- function(basemaps) {
    options(mapviewBasemaps = basemaps)
  }

  ## raster.size
  setRasterSize <- function(raster.size) {
    options(mapviewRasterSize = raster.size)
  }

  ## maxpixels
  setMaxPixels <- function(maxpixels) {
    options(mapviewMaxPixels = maxpixels)
  }

  ## maxpolygons
  setMaxPolygons <- function(maxpolygons) {
    options(mapviewMaxPolygons = maxpolygons)
  }

  ## maxpoints
  setMaxPoints <- function(maxpoints) {
    options(mapviewMaxPoints = maxpoints)
  }

  ## maxlines
  setMaxLines <- function(maxlines) {
    options(mapviewMaxLines = maxlines)
  }

  ## raster.palette
  setRasterPalette <- function(raster.palette) {
    options(mapviewRasterPalette = raster.palette)
  }

  ## vector.palette
  setVectorPalette <- function(vector.palette) {
    options(mapviewVectorPalette = vector.palette)
  }

  ## verbose
  setVerbose <- function(verbose) {
    options(mapviewVerbose = verbose)
  }

  ## na.color
  setNAColor <- function(na.color) {
    options(mapviewNAColor = na.color)
  }

  ## layers control position
  setLayersControlPos <- function(layers.control.pos) {
    options(mapviewLayersControlPos = layers.control.pos)
  }

  cnt <- 0

  if (default) {
    cnt <- 1
    options(mapviewPlatform = "leaflet")
    options(mapviewBasemaps = c("OpenStreetMap",
                                "Esri.WorldImagery",
                                "Thunderforest.Landscape",
                                "OpenTopoMap"))
    options(mapviewraster.size = 8 * 1024 * 1024)
    options(mapviewMaxPixels = 500000)
    options(write.table = 30000)
    options(mapviewMaxPoints = 20000)
    options(mapviewMaxLines = 30000)
    options(mapviewRasterPalette = mapviewPalette)
    options(mapviewVectorPalette = mapviewPalette)
    options(mapviewVerbose = FALSE)
    options(mapviewNAColor = "transparent")
    options(mapviewLayersControlPos = "topleft")
  }


  if (!missing(platform)) { setPlatform(platform); cnt <- cnt + 1 }
  if (!missing(basemaps)) { setBasemaps(basemaps); cnt <- cnt + 1 }
  if (!missing(raster.size)) { setRasterSize(raster.size); cnt <- cnt + 1 }
  if (!missing(maxpixels)) { setMaxPixels(maxpixels); cnt <- cnt + 1 }
  if (!missing(maxpolygons)) { setMaxPolygons(maxpolygons); cnt <- cnt + 1 }
  if (!missing(maxpoints)) { setMaxPoints(maxpoints); cnt <- cnt + 1 }
  if (!missing(maxlines)) { setMaxLines(maxlines); cnt <- cnt + 1 }
  if (!missing(raster.palette)) {
    setRasterPalette(raster.palette); cnt <- cnt + 1 }
  if (!missing(vector.palette)) {
    setVectorPalette(vector.palette); cnt <- cnt + 1 }
  if (!missing(verbose)) { setVerbose(verbose); cnt <- cnt + 1 }
  if (!missing(na.color)) { setNAColor(na.color); cnt <- cnt + 1 }
  if (!missing(layers.control.pos)) {
    setLayersControlPos(layers.control.pos); cnt <- cnt + 1 }


  lst <- list(platform = .platform(),
              basemaps = .basemaps(),
              raster.size = .rasterSize(),
              maxpixels = .maxpixels(),
              maxpolygons = .maxpolygons(),
              maxpoints = .maxpoints(),
              maxlines = .maxlines(),
              raster.palette = .rasterPalette(),
              vector.palette = .vectorPalette(),
              verbose = .verbose(),
              na.color = .naColor(),
              layers.control.pos = .layersControlPos())



  if (console) {
    if (cnt == 0) {
      cat('platform            :', lst$platform, '\n' )
      cat('basemaps            :', lst$basemaps, '\n')
      cat('raster.size         :', lst$raster.size, '\n')
      cat('maxpixels           :', lst$maxpixels, '\n')
      cat('maxpolygons         :', lst$maxpolygons, '\n')
      cat('maxpoints           :', lst$maxpoints, '\n')
      cat('maxlines            :', lst$maxlines, '\n')
      cat('raster.palette      : \n')
      print(.rasterPalette())
      cat("\n")
      cat('vector.palette      : \n')
      print(.vectorPalette())
      cat("\n")
      cat('verbose             :', lst$verbose, '\n')
      cat('na.color            :', lst$na.color, '\n')
      cat('layers.control.pos  :', lst$layers.control.pos, '\n')
    }
  }

  invisible(lst)

}


.platform <- function() {
  default <- "leaflet"
  pf <- getOption('mapviewPlatform')
  if (is.null(pf)) {
    return(default)
  } else {
    return(pf)
  }
}


.basemaps <- function() {
  default <- c("OpenStreetMap",
               "Esri.WorldImagery",
               "Thunderforest.Landscape",
               "OpenTopoMap")

  bm <- getOption('mapviewBasemaps')
  if (is.null(bm)) {
    return(default)
  } else {
    return(bm)
  }
}


.rasterSize <- function() {
  default <- 8 * 1024 * 1024
  rs <- getOption('mapviewRasterSize')
  if (is.null(rs)) {
    return(default)
  } else {
    return(rs)
  }
}


.maxpixels <- function() {
  default <- 500000
  mp <- getOption('mapviewMaxPixels')
  if (is.null(mp)) {
    return(default)
  } else {
    return(mp)
  }
}


.maxpolygons <- function() {
  default <- 30000
  mp <- getOption('mapviewMaxPolygons')
  if (is.null(mp)) {
    return(default)
  } else {
    return(mp)
  }
}


.maxpoints <- function() {
  default <- 20000
  mp <- getOption('mapviewMaxPoints')
  if (is.null(mp)) {
    return(default)
  } else {
    return(mp)
  }
}


.maxlines <- function() {
  default <- 30000
  ml <- getOption('mapviewMaxLines')
  if (is.null(ml)) {
    return(default)
  } else {
    return(ml)
  }
}

.rasterPalette <- function() {
  default <- mapviewPalette
  rp <- getOption('mapviewRasterPalette')
  if (is.null(rp)) {
    return(default)
  } else {
    return(rp)
  }
}


.vectorPalette <- function() {
  default <- mapviewPalette
  rp <- getOption('mapviewVectorPalette')
  if (is.null(rp)) {
    return(default)
  } else {
    return(rp)
  }
}


.verbose <- function() {
  default <- FALSE
  vb <- getOption('mapviewVerbose')
  if (is.null(vb)) {
    return(default)
  } else {
    return(as.logical(vb))
  }
}


.naColor <- function() {
  default <- "transparent"
  nc <- getOption('mapviewNAColor')
  if (is.null(nc)) {
    return(default)
  } else {
    return(nc)
  }
}


.layersControlPos <- function() {
  default <- "topleft"
  lcp <- getOption('mapviewLayersControlPos')
  if (is.null(lcp)) {
    return(default)
  } else {
    return(lcp)
  }
}


#' query single mapviewOption parameters
#' @describeIn mapviewOptions query single mapviewOption parameters
#' @param param character. parameter to be queried.
#' @export mapviewGetOption
mapviewGetOption <- function(param) {
  mapviewOptions(console = FALSE)[[param]]
}

