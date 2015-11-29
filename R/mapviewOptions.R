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
#' @param rastersize numeric. see the maxBytes argument in \code{\link{addRasterImage}}
#' @param maxpixels numeric. The maximum amount of pixels allowed for Raster*
#' objects to be rendered. Defaults to 500000. Set this higher if you have a
#' potent machine or are patient enough to wait a little
#' @param verbose logical. Many functions in mapview provide details about their
#' behaviour. Set this to TRUE if you want to see these printed to the console
#' @param nacolor character. The default color to be used for NA values.
#' This is relevant for Raster* objects
#' @param default logical. If TRUE all options are set to their default values
#' @param console logical. Should the options be printed to the console
#' @param layerscontrolpos character. Where should the layer control be
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
#' mapviewOptions(nacolor = "pink")
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
                           rastersize,
                           maxpixels,
                           verbose,
                           nacolor,
                           layerscontrolpos,
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

  ## rastersize
  setRasterSize <- function(rastersize) {
    options(mapviewRasterSize = rastersize)
  }

  ## maxpixels
  setMaxPixels <- function(maxpixels) {
    options(mapviewMaxPixels = maxpixels)
  }

  ## verbose
  setVerbose <- function(verbose) {
    options(mapviewVerbose = verbose)
  }

  ## nacolor
  setNAColor <- function(nacolor) {
    options(mapviewNAColor = nacolor)
  }

  setLayersControlPos <- function(layerscontrolpos) {
    options(layersControlPos = layerscontrolpos)
  }

  cnt <- 0

  if (default) {
    cnt <- 1
    options(mapviewPlatform = "leaflet")
    options(mapviewBasemaps = c("OpenStreetMap",
                                "Esri.WorldImagery",
<<<<<<< HEAD
                                "Thunderforest.Landscape",
                                "OpenTopoMap"))
=======
                                "Thunderforest.Landscape"))
>>>>>>> 698828d8e8d794953de77451f8b39d132271918e
    options(mapviewRasterSize = 8 * 1024 * 1024)
    options(mapviewMaxPixels = 500000)
    options(mapviewVerbose = FALSE)
    options(mapviewNAColor = "transparent")
    options(layersControlPos = "topleft")
  }


  if (!missing(platform)) { setPlatform(platform); cnt <- cnt+1 }
  if (!missing(basemaps)) { setBasemaps(basemaps); cnt <- cnt+1 }
  if (!missing(rastersize)) { setRasterSize(rastersize); cnt <- cnt+1 }
  if (!missing(maxpixels)) { setMaxPixels(maxpixels); cnt <- cnt+1 }
  if (!missing(verbose)) { setVerbose(verbose); cnt <- cnt+1 }
  if (!missing(nacolor)) { setNAColor(nacolor); cnt <- cnt+1 }
  if (!missing(layerscontrolpos)) { setNAColor(layerscontrolpos); cnt <- cnt+1 }


  lst <- list(platform = .platform(),
              basemaps = .basemaps(),
              rastersize = .rastersize(),
              maxpixels = .maxpixels(),
              verbose = .verbose(),
              nacolor = .nacolor(),
              layerscontrolpos = .layerscontrolpos())



  if (console) {
    if (cnt == 0) {
      cat('platform           :', lst$platform, '\n' )
      cat('basemaps           :', lst$basemaps, '\n')
      cat('rastersize         :', lst$rastersize, '\n')
      cat('maxpixels          :', lst$maxpixels, '\n')
      cat('verbose            :', lst$verbose, '\n')
      cat('nacolor            :', lst$nacolor, '\n')
      cat('layerscontrolpos   :', lst$layerscontrolpos, '\n')
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
<<<<<<< HEAD
               "Thunderforest.Landscape",
               "OpenTopoMap")
=======
               "Thunderforest.Landscape")
>>>>>>> 698828d8e8d794953de77451f8b39d132271918e
  bm <- getOption('mapviewBasemaps')
  if (is.null(bm)) {
    return(default)
  } else {
    return(bm)
  }
}


.rastersize <- function() {
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


.verbose <- function() {
  default <- FALSE
  vb <- getOption('mapviewVerbose')
  if (is.null(vb)) {
    return(default)
  } else {
    return(as.logical(vb))
  }
}


.nacolor <- function() {
  default <- "transparent"
  nc <- getOption('mapviewNAColor')
  if (is.null(nc)) {
    return(default)
  } else {
    return(nc)
  }
}


.layerscontrolpos <- function() {
  default <- "topleft"
  lcp <- getOption('layersControlPos')
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

