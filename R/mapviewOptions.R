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
#' @param platform character. The rendering platform to be used.
#' Current options are "leaflet" and "mapdeck".
#' @param basemaps character. The basemaps to be used for rendering data. See
#' \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for possible
#' values
#' @param raster.palette a color palette function for raster visualisation.
#' Should be a function that takes an integer as input and returns a vector of colors.
#' See \code{\link{colorRampPalette}} for details.
#' @param vector.palette a color palette function for vector visualisation.
#' Should be a function that takes an integer as input and returns a vector of colors.
#' See \code{\link{colorRampPalette}} for details.
#' @param verbose logical. Many functions in mapview provide details about their
#' behaviour. Set this to TRUE if you want to see these printed to the console.
#' @param na.color character. The default color to be used for NA values.
#' @param legend logical. Whether or not to show a legend for the layer(s).
#' @param legend.pos Where should the legend be placed?
#' One of "topleft", "topright", "bottomleft", "bottomright".
#' @param default logical. If TRUE all options are set to their default values
#' @param console logical. Should the options be printed to the console
#' @param layers.control.pos character. Where should the layer control be
#' placed? One of "topleft", "topright", "bottomleft", "bottomright".
#' @param leafletWidth,leafletHeight height and width of the htmlwidget in px.
#' @param raster.size numeric. see the maxBytes argument in \code{\link{addRasterImage}}
#' @param mapview.maxpixels numeric. The maximum amount of pixels allowed for Raster*
#' objects to be rendered with \code{mapview}. Defaults to 500000.
#' Set this higher if you have a potent machine or are patient enough to wait a little.
#' @param plainview.maxpixels numeric. The maximum amount of pixels allowed for Raster*
#' objects to be rendered with \code{plainview}. Defaults to 10000000.
#' Set this higher if you have a potent machine or are patient enough to wait a little.
#' @param use.layer.names whether to use layer names when plotting raster layers.
#' @param trim should the raster be trimmed in case there are NAs on the edges.
#' @param method for raster data only (raster/stars). Method used to compute
#' values for the resampled layer that is passed on to leaflet. mapview does
#' projection on-the-fly to ensure correct display and therefore needs to know
#' how to do this projection. The default is 'bilinear' (bilinear interpolation),
#' which is appropriate for continuous variables. The other option, 'ngb'
#' (nearest neighbor), is useful for categorical variables. Ignored if the raster
#' layer is of class \code{factor} in which case "ngb" is used.
#' @param query.type for raster methods only. Whether to show raster value query
#' on \code{'mousemove'} or \code{'click'}. Ignored if \code{label = FALSE}.
#' @param query.digits for raster methods only. The amount of digits to be shown
#' by raster value query. Ignored if \code{label = FALSE}.
#' @param query.position for raster methods only. The position of the raster
#' value query info box. See \code{position} argument of \code{\link{addLegend}}
#' for possible values. Ignored if \code{label = FALSE}.
#' @param query.prefix for raster methods only. a character string to be shown
#' as prefix for the layerId. Ignored if \code{label = FALSE}.
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
#' mapviewOptions()
#' mapviewOptions(na.color = "pink")
#' mapviewOptions()
#'
#' mapviewGetOption("platform")
#'
#' mapviewOptions(default = TRUE)
#' mapviewOptions()
#'
#'
#' @export mapviewOptions
#' @name mapviewOptions
#' @rdname mapviewOptions
#' @aliases mapviewOptions
mapviewOptions <- function(platform,
                           basemaps,
                           raster.palette,
                           vector.palette,
                           verbose,
                           na.color,
                           legend,
                           legend.pos,
                           layers.control.pos,
                           leafletWidth,
                           leafletHeight,
                           raster.size,
                           mapview.maxpixels,
                           plainview.maxpixels,
                           use.layer.names,
                           trim,
                           method,
                           query.type,
                           query.digits,
                           query.position,
                           query.prefix,
                           maxpoints,
                           maxpolygons,
                           maxlines,
                           default = FALSE,
                           console = TRUE) {

  ### 1. global options -----


  ## platform
  setPlatform <- function(platform) {
    if (!platform %in% c("leaflet", "mapdeck")) {
      warning(
        sprintf(
          "currently only platforms %s & %s are allowed!
          \nUsing default platform %s", "'leaflet'", "'mapdeck'", "'leaflet'"
        )
        , call. = FALSE
      )
      platform = "leaflet"
    }
    options(mapviewPlatform = platform)
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

  ## basemaps
  setBasemaps <- function(basemaps) {
    options(mapviewBasemaps = basemaps)
  }

  .basemaps <- function() {
    default <- c("CartoDB.Positron",
                 "CartoDB.DarkMatter",
                 "OpenStreetMap",
                 "Esri.WorldImagery",
                 "OpenTopoMap")

    bm <- getOption('mapviewBasemaps')
    if (is.null(bm)) {
      return(default)
    } else {
      return(bm)
    }
  }

  ## verbose
  setVerbose <- function(verbose) {
    options(mapviewVerbose = verbose)
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

  ## na.color
  setNAColor <- function(na.color) {
    options(mapviewNAColor = na.color)
  }

  .naColor <- function() {
    default <- "#BEBEBE80"
    nc <- getOption('mapviewNAColor')
    if (is.null(nc)) {
      return(default)
    } else {
      return(nc)
    }
  }

  ## legend
  setLegend <- function(legend) {
    options(mapviewLegend = legend)
  }

  .Legend <- function() {
    default <- TRUE
    sl <- getOption('mapviewLegend')
    if (is.null(sl)) {
      return(default)
    } else {
      return(sl)
    }
  }

  ## legend.pos
  setLegendPos <- function(legend.pos) {
    options(mapviewLegendPos = legend.pos)
  }

  .legendPos <- function() {
    default <- "topright"
    lp <- getOption('mapviewLegendPos')
    if (is.null(lp)) {
      return(default)
    } else {
      return(lp)
    }
  }

  ## layers control position
  setLayersControlPos <- function(layers.control.pos) {
    options(mapviewLayersControlPos = layers.control.pos)
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

  ## leaflet() height
  setleafletHeight <- function(leafletHeight) {
    options(leafletHeight = leafletHeight)
  }

  .leafletHeight <- function() {
    default <- NULL #"800px"
    lhgt <- getOption('leafletHeight')
    if (is.null(lhgt)) {
      return(default)
    } else {
      return(lhgt)
    }
  }

  ## leaflet() width
  setleafletWidth <- function(leafletWidth) {
    options(leafletWidth = leafletWidth)
  }

  .leafletWidth <- function() {
    default <- NULL #"100%"
    lwth <- getOption('leafletWidth')
    if (is.null(lwth)) {
      return(default)
    } else {
      return(lwth)
    }
  }

  ### 2. raster relevant options -----


  ## raster.palette
  setRasterPalette <- function(raster.palette) {
    options(mapviewRasterPalette = raster.palette)
  }

  .rasterPalette <- function() {
    default <- mapviewPalette(name = "mapviewRasterColors")
    rp <- getOption('mapviewRasterPalette')
    if (is.null(rp)) {
      return(default)
    } else {
      return(rp)
    }
  }

  ## raster.size
  setRasterSize <- function(raster.size) {
    options(mapviewRasterSize = raster.size)
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

  ## mapview maxpixels
  setMapviewMaxPixels <- function(mapview.maxpixels) {
    options(mapviewMaxPixels = mapview.maxpixels)
  }

  .mapviewMaxpixels <- function() {
    default <- 500000
    mmp <- getOption('mapviewMaxPixels')
    if (is.null(mmp)) {
      return(default)
    } else {
      return(mmp)
    }
  }

  ## plainview maxpixels
  setPlainviewMaxPixels <- function(plainview.maxpixels) {
    options(plainviewMaxPixels = plainview.maxpixels)
  }

  .plainviewMaxpixels <- function() {
    default <- 10000000
    pmp <- getOption('plainviewMaxPixels')
    if (is.null(pmp)) {
      return(default)
    } else {
      return(pmp)
    }
  }

  ## layer names
  setUseLayerNames = function(use.layer.names) {
    options(mapviewUseLayerNames = use.layer.names)
  }

  .useLayerNames = function() {
    default = FALSE
    lyrnms = getOption("mapviewUseLayerNames")
    if (is.null(lyrnms)) {
      return(default)
    } else {
      return(lyrnms)
    }
  }

  ## trim rasters
  setTrim = function(trim) {
    options(mapviewTrim = trim)
  }

  .trim = function() {
    default = TRUE
    trm = getOption("mapviewTrim")
    if (is.null(trm)) {
      return(default)
    } else {
      return(trm)
    }
  }

  ## method interpolate raster
  setInterpolationMethod = function(method) {
    if (!method %in% c("bilinear", "ngb")) {
      warning(
        sprintf(
          "invalid interpolation method option %s",
          method,
          "\n using default method 'bilinear'"
        )
        , call. = FALSE
      )
      method = "bilinear"
    }
    options(mapviewMethod = method)
  }

  .method = function() {
    default = "bilinear"
    mthd = getOption("mapviewMethod")
    if (is.null(mthd)) {
      return(default)
    } else {
      return(mthd)
    }
  }

  ## query type
  setQueryType = function(query.type) {
    if (!query.type %in% c("mousemove", "click")) {
      warning(
        sprintf(
          "invalid query type option %s",
          query.type,
          "\n using default query type 'mousemove'"
        )
        , call. = FALSE
      )
      query.type = "mousemove"
    }
    options(mapviewQueryType = query.type)
  }

  .queryType = function() {
    default = "mousemove"
    qry = getOption("mapviewQueryType")
    if (is.null(qry)) {
      return(default)
    } else {
      return(qry)
    }
  }

  ## query digits
  setQueryDigits = function(query.digits) {
    options(mapviewQueryDigits = query.digits)
  }

  .queryDigits = function() {
    default = getOption("digits")
    dgts = getOption("mapviewQueryDigits")
    if (is.null(dgts)) {
      return(default)
    } else {
      return(dgts)
    }
  }

  ## query position
  setQueryPosition = function(query.position) {
    allowed = c("topright", "topleft", "bottomleft", "bottomright")
    if (!query.position %in% allowed) {
      warning(
        sprintf(
          "invalid query position option %s",
          query.position,
          "\n using default query position 'topright'"
        )
        , call. = FALSE
      )
      query.position = "topright"
    }
    options(mapviewQueryPosition = query.position)
  }

  .queryPosition = function() {
    default = "topright"
    pos = getOption("mapviewQueryPosition")
    if (is.null(pos)) {
      return(default)
    } else {
      return(pos)
    }
  }

  ## query prefix
  setQueryPrefix = function(query.prefix) {
    options(mapviewQueryPrefix = query.prefix)
  }

  .queryPrefix = function() {
    default = "Layer"
    prfx = getOption("mapviewQueryPrefix")
    if (is.null(prfx)) {
      return(default)
    } else {
      return(prfx)
    }
  }

  ### 3. vector relevant options -----


  ## maxpolygons
  setMaxPolygons <- function(maxpolygons) {
    options(mapviewMaxPolygons = maxpolygons)
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

  ## maxpoints
  setMaxPoints <- function(maxpoints) {
    options(mapviewMaxPoints = maxpoints)
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

  ## maxlines
  setMaxLines <- function(maxlines) {
    options(mapviewMaxLines = maxlines)
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

  ## vector.palette
  setVectorPalette <- function(vector.palette) {
    options(mapviewVectorPalette = vector.palette)
  }

  .vectorPalette <- function() {
    default <- mapviewPalette(name = "mapviewVectorColors")
    rp <- getOption('mapviewVectorPalette')
    if (is.null(rp)) {
      return(default)
    } else {
      return(rp)
    }
  }


  ### 4. default options -----


  cnt <- 0

  if (default) {
    cnt <- 1

    ## global
    options(mapviewPlatform = "leaflet")
    options(mapviewBasemaps = c("CartoDB.Positron",
                                "CartoDB.DarkMatter",
                                "OpenStreetMap",
                                "Esri.WorldImagery",
                                "OpenTopoMap"))
    options(mapviewRasterPalette = mapviewPalette(name = "mapviewRasterColors"))
    options(mapviewVectorPalette = mapviewPalette(name = "mapviewVectorColors"))
    options(mapviewVerbose = FALSE)
    options(mapviewNAColor = "#BEBEBE80")
    options(mapviewLegend = TRUE)
    options(mapviewLegendPos = "topright")
    options(mapviewLayersControlPos = "topleft")
    options(mapviewleafletWidth = NULL)
    options(mapviewleafletHeight = NULL)

    ## raster
    options(mapviewraster.size = 8 * 1024 * 1024)
    options(mapviewMaxPixels = 500000)
    options(plainviewMaxPixels = 10000000)
    options(mapviewUseLayerNames = FALSE)
    options(mapviewTrim = TRUE)
    options(mapviewMethod = "bilinear")
    options(mapviewQueryType = "mousemove")
    options(mapviewQueryDigits = getOption("digits"))
    options(mapviewQueryPostion = "topright")
    options(mapviewQueryPrefix = "Layer")

    ## vector
    options(mapviewMaxPolygons = 30000)
    options(mapviewMaxPoints = 20000)
    options(mapviewMaxLines = 30000)

  }


  ## global
  if (!missing(platform)) { setPlatform(platform); cnt <- cnt + 1 }
  if (!missing(basemaps)) { setBasemaps(basemaps); cnt <- cnt + 1 }
  if (!missing(raster.palette)) {
    setRasterPalette(raster.palette); cnt <- cnt + 1
  }
  if (!missing(vector.palette)) {
    setVectorPalette(vector.palette); cnt <- cnt + 1
  }
  if (!missing(verbose)) { setVerbose(verbose); cnt <- cnt + 1 }
  if (!missing(na.color)) { setNAColor(na.color); cnt <- cnt + 1 }
  if (!missing(legend)) { setLegend(legend); cnt <- cnt + 1 }
  if (!missing(legend.pos)) { setLegendPos(legend.pos); cnt <- cnt + 1 }
  if (!missing(layers.control.pos)) {
    setLayersControlPos(layers.control.pos); cnt <- cnt + 1
  }
  if (!missing(leafletWidth)) { setleafletWidth(leafletWidth); cnt <- cnt + 1 }
  if (!missing(leafletHeight)) { setleafletHeight(leafletHeight); cnt <- cnt + 1 }

  ## raster
  if (!missing(raster.size)) { setRasterSize(raster.size); cnt <- cnt + 1 }
  if (!missing(mapview.maxpixels)) {
    setMapviewMaxPixels(mapview.maxpixels); cnt <- cnt + 1
  }
  if (!missing(plainview.maxpixels)) {
    setPlainviewMaxPixels(plainview.maxpixels); cnt <- cnt + 1
  }
  if (!missing(use.layer.names)) {
    setUseLayerNames(use.layer.names); cnt <- cnt + 1
  }
  if (!missing(trim)) { setTrim(trim); cnt <- cnt + 1 }
  if (!missing(method)) { setInterpolationMethod(method); cnt <- cnt + 1 }
  if (!missing(query.type)) { setQueryType(query.type); cnt <- cnt + 1 }
  if (!missing(query.digits)) { setQueryDigits(query.digits); cnt <- cnt + 1 }
  if (!missing(query.position)) { setQueryPosition(query.position); cnt <- cnt + 1 }
  if (!missing(query.prefix)) { setQueryPrefix(query.prefix); cnt <- cnt + 1 }

  ## vector
  if (!missing(maxpolygons)) { setMaxPolygons(maxpolygons); cnt <- cnt + 1 }
  if (!missing(maxpoints)) { setMaxPoints(maxpoints); cnt <- cnt + 1 }
  if (!missing(maxlines)) { setMaxLines(maxlines); cnt <- cnt + 1 }

  lst <- list(

    ## global
    platform = .platform()
    , basemaps = .basemaps()
    , raster.palette = .rasterPalette()
    , vector.palette = .vectorPalette()
    , verbose = .verbose()
    , na.color = .naColor()
    , legend = .Legend()
    , legend.pos = .legendPos()
    , layers.control.pos = .layersControlPos()
    , leafletWidth = .leafletWidth()
    , leafletHeight = .leafletHeight()

    ## raster
    , raster.size = .rasterSize()
    , mapview.maxpixels = .mapviewMaxpixels()
    , plainview.maxpixels = .plainviewMaxpixels()
    , use.layer.names = .useLayerNames()
    , trim = .trim()
    , method = .method()
    , query.type = .queryType()
    , query.digits = .queryDigits()
    , query.position = .queryPosition()
    , query.prefix = .queryPrefix()

    ## vector
    , maxpolygons = .maxpolygons()
    , maxpoints = .maxpoints()
    , maxlines = .maxlines()

  )



  if (console) {
    if (cnt == 0) {

      ## global
      cat('platform            :', lst$platform, '\n' )
      cat('basemaps            :', lst$basemaps, '\n')
      cat('raster.palette      :', format(.rasterPalette())[1], '\n')
      cat('vector.palette      :', format(.vectorPalette())[1], '\n')
      cat('verbose             :', lst$verbose, '\n')
      cat('na.color            :', lst$na.color, '\n')
      cat('legend              :', lst$legend, '\n')
      cat('legend.pos          :', lst$legend.pos, '\n')
      cat('layers.control.pos  :', lst$layers.control.pos, '\n')
      cat('leafletWidth        :', lst$leafletWidth, '\n')
      cat('leafletHeight       :', lst$leafletHeight, '\n')

      ## raster
      cat('raster.size         :', lst$raster.size, '\n')
      cat('mapview.maxpixels   :', lst$mapview.maxpixels, '\n')
      cat('plainview.maxpixels :', lst$plainview.maxpixels, '\n')
      cat('use.layer.names     :', lst$use.layer.names, '\n')
      cat('trim                :', lst$trim, '\n')
      cat('method              :', lst$method, '\n')
      cat('query.type          :', lst$query.type, '\n')
      cat('query.digits        :', lst$query.digits, '\n')
      cat('query.position      :', lst$query.position, '\n')
      cat('query.prefix        :', lst$query.prefix, '\n')

      ## vector
      cat('maxpolygons         :', lst$maxpolygons, '\n')
      cat('maxpoints           :', lst$maxpoints, '\n')
      cat('maxlines            :', lst$maxlines, '\n')

    }
  }

  invisible(lst)

}

#' query single mapviewOption parameters
#' @describeIn mapviewOptions query single mapviewOption parameters
#' @param param character. parameter to be queried.
#' @export mapviewGetOption
mapviewGetOption <- function(param) {
  mapviewOptions(console = FALSE)[[param]]
}

