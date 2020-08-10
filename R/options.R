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
#' \url{https://leaflet-extras.github.io/leaflet-providers/preview/} for possible
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
#' @param legend.opacity opacity of the legend.
#' @param legend.pos Where should the legend be placed?
#' One of "topleft", "topright", "bottomleft", "bottomright".
#' @param default logical. If TRUE all options are set to their default values
#' @param console logical. Should the options be printed to the console
#' @param layers.control.pos character. Where should the layer control be
#' placed? One of "topleft", "topright", "bottomleft", "bottomright".
#' @param leafletWidth,leafletHeight height and width of the htmlwidget in px.
#' @param viewer.suppress whether to render the map in the browser (\code{TRUE})
#' or the RStudio viewer (\code{FALSE}).
#' @param homebutton logical, whether to add a zoom-to-layer button to the map.
#' @param homebutton.pos character. Where should the homebutton(s) be
#' placed? One of "topleft", "topright", "bottomleft", "bottomright".
#' @param native.crs logical whether to reproject to web map coordinate
#' reference system (web mercator - epsg:3857) or render using native CRS of
#' the supplied data (can also be NA). Default is FALSE which will render in
#' web mercator. If set to TRUE now background maps will be drawn (but rendering
#' may be much quicker as no reprojecting is necessary).
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
#' @param pane name of the map pane in which to render features. See
#' \code{\link{addMapPane}} for details. Currently only supported for vector layers.
#' Ignored if \code{canvas = TRUE}. The default \code{"auto"} will create different panes
#' for points, lines and polygons such that points overlay lines overlay polygons.
#' Set to \code{NULL} to get default leaflet behaviour where allfeatures
#' are rendered in the same pane and layer order is determined automatically/sequentially.
#' @param cex numeric or attribute name(s) or column number(s) in attribute table
#' of the column(s) to be used for defining the size of circles.
#' @param alpha opacity of lines.
#' @param watch whether to watch a certain environment and automatically
#' render changes to the list of spatial data in that environment. See
#' \link{mapviewWatcher} for details.
#' @param fgb if set to \code{TRUE} mapview will not use 'clasical' leaflet/htmlwidgets
#' rendering (which embeds data directly in the html) but leverage the speed of
#' a file format called flatgeobuf (hence, fgb). This has the added benefit that
#' data is being streamed onto the map, which makes for a pleasant user experience.
#' It should also help to visualise larger data sets due to a reduced memeory footprint.
#' A note of warning, data will be attached to the html
#' via a <src=...> call which means that the html is not selfcontained anymore
#' (so it cannot be used without an accompanying folder).
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
                           legend.opacity,
                           legend.pos,
                           layers.control.pos,
                           leafletWidth,
                           leafletHeight,
                           viewer.suppress,
                           homebutton,
                           homebutton.pos,
                           native.crs,
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
                           pane,
                           cex,
                           alpha,
                           default = FALSE,
                           console = TRUE,
                           watch = FALSE,
                           fgb) {

  ### 1. global options =====

  ## platform ----
  setPlatform <- function(platform) {
    if (!platform %in% c("leaflet", "leafgl", "mapdeck")) {
      warning(
        sprintf(
          "currently only platforms %s & %s & %s are allowed!
          \nUsing default platform %s"
          , "'leaflet'"
          , "'leafgl'"
          , "'mapdeck'"
          , "'leaflet'"
        )
        , call. = FALSE
      )
      platform = "leaflet"
    }
    if (isTRUE(mapviewGetOption("fgb")) && platform != "leaflet") {
      warning(
        sprintf(
          "option 'fgb' cannot (yet) be used with platform '%s'. Ignoring 'fgb'."
         , platform
        )
        , call. = FALSE
      )
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

  ## basemaps ----
  setBasemaps <- function(basemaps) {
    options(mapviewBasemaps = basemaps)
  }

  .basemaps <- function() {
    pf <- getOption('mapviewPlatform')
    if (is.null(pf) || pf %in% c("leaflet", "leafgl")) {
      default <- c(
        "CartoDB.Positron"
        , "CartoDB.DarkMatter"
        , "OpenStreetMap"
        , "Esri.WorldImagery"
        , "OpenTopoMap"
      )
    } else if (pf == "mapdeck") {
      default = c(
        mapdeck::mapdeck_style("light")
        , mapdeck::mapdeck_style("dark")
      )
    }

    bm <- getOption('mapviewBasemaps')
    if (is.null(bm)) {
      return(default)
    } else {
      return(bm)
    }
  }

  ## verbose ----
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

  ## na.color ----
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

  ## legend ----
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

  ## legend.opacity ----
  etLegendOpacity = function(legend.opacity) {
    options(mapviewLegendOpacity = legend.opacity)
  }

  .legenOpacity = function() {
    default = 1
    lo = getOption("mapviewLegendOpacity")
    if (is.null(lo)) {
      return(default)
    } else {
      return(lo)
    }
  }

  ## legend.pos ----
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

  ## layers control position ----
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

  ## leaflet() height ----
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

  ## leaflet() width ----
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

  ## viewer suppress ----
  setViewerSuppress = function(viewer.suppress) {
    options(mapviewViewerSuppress = viewer.suppress)
  }

  .viewerSuppress = function() {
    default = FALSE
    vwrspprs = getOption("mapviewViewerSuppress")
    if (is.null(vwrspprs)) {
      return(default)
    } else {
      return(vwrspprs)
    }
  }

  ## homebutton ----
  setHomebutton = function(homebutton) {
    options(mapviewHomebutton = homebutton)
  }

  .homebutton = function() {
    default = TRUE
    hbtn = getOption("mapviewHomebutton")
    if (is.null(hbtn)) {
      return(default)
    } else {
      return(hbtn)
    }
  }

  ## homebutton.pos ----
  setHomebuttonPos = function(homebutton.pos) {
    options(mapviewHomebuttonPos = homebutton.pos)
  }

  .homebuttonPos = function() {
    default = "bottomright"
    hbtn = getOption("mapviewHomebuttonPos")
    if (is.null(hbtn)) {
      return(default)
    } else {
      return(hbtn)
    }
  }

  ## native.crs ----
  setNativeCRS = function(native.crs) {
    options(mapviewNativeCRS = native.crs)
  }

  .nativeCRS = function() {
    default = FALSE
    ntvcrs = getOption("mapviewNativeCRS")
    if (is.null(ntvcrs)) {
      return(default)
    } else {
      return(ntvcrs)
    }
  }

  ## watch ----
  setWatcher = function(watch) {
    options(mapviewWatcher = watch)
  }

  .watch = function() {
    default = FALSE
    wtch = getOption("mapviewWatcher")
    if (is.null(wtch)) {
      return(default)
    } else {
      return(wtch)
    }
  }

  ## fgb ----
  setFgb <- function(fgb) {
    if (mapviewGetOption("platform") %in% c("leafgl", "mapdeck") &&
        isTRUE(fgb)) {
      warning(
        sprintf(
          "option 'fgb' currently only works on platform %s. Setting fgb = FALSE"
          , "'leaflet'"
        )
        , call. = FALSE
      )
      fgb = FALSE
    }
    v = sf::sf_extSoftVersion()
    if (unname(v[names(v) == "GDAL"] <= "3.0.4")) {
      warning(
        sprintf(
          "option 'fgb' requires GDAL >= 3.1.0! Your version is %s. Setting fgb = FALSE"
          , v[names(v) == "GDAL"]
        )
        , call. = FALSE
      )
      fgb = FALSE
    }
    options(mapviewFgb = fgb)
  }

  .fgb <- function() {
    default <- FALSE
    fgb <- getOption('mapviewFgb')
    if (is.null(fgb)) {
      return(default)
    } else {
      return(fgb)
    }
  }

  ### 2. raster relevant options =====

  ## raster.palette -----
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

  ## raster.size -----
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

  ## mapview maxpixels -----
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

  ## plainview maxpixels -----
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

  ## layer names -----
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

  ## trim rasters -----
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

  ## method interpolate raster -----
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

  ## query type -----
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

  ## query digits -----
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

  ## query position -----
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

  ## query prefix -----
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

  ### 3. vector relevant options =====

  ## maxpolygons -----
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

  ## maxpoints -----
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

  ## maxlines -----
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

  ## vector.palette -----
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

  ## pane -----
  setPane = function(pane) {
    options(mapviewPane = pane)
  }

  .pane = function() {
    default = "auto"
    pn = getOption("mapviewPane")
    if (is.null(pn)) {
      return(default)
    } else {
      return(pn)
    }
  }

  ## cex -----
  setCex = function(cex) {
    options(mapviewCex = cex)
  }

  .cex = function() {
    default = 6
    cx = getOption("mapviewCex")
    if (is.null(cx)) {
      return(default)
    } else {
      return(cx)
    }
  }

  ## alpha -----
  setAlpha = function(alpha) {
    options(mapviewAlpha = alpha)
  }

  .alpha = function() {
    default = 0.9
    alph = getOption("mapviewAlpha")
    if (is.null(alph)) {
      return(default)
    } else {
      return(alph)
    }
  }


  ### 4. default options =====


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
    options(mapviewLegendOpacity = 1)
    options(mapviewLegendPos = "topright")
    options(mapviewLayersControlPos = "topleft")
    options(mapviewleafletWidth = NULL)
    options(mapviewleafletHeight = NULL)
    options(mapviewViewerSuppress = FALSE)
    options(mapviewHomebutton = TRUE)
    options(mapviewHomebuttonPos = "bottomright")
    options(mapviewNativeCRS = FALSE)
    options(mapviewWatcher = FALSE)
    options(mapviewFgb = FALSE)

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
    options(mapviewPane = "auto")
    options(mapviewCex = 0.6)
    options(mapviewAlpha = 0.9)

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
  if (!missing(legend.opacity)) { setLegend(legend.opacity); cnt <- cnt + 1 }
  if (!missing(legend.pos)) { setLegendPos(legend.pos); cnt <- cnt + 1 }
  if (!missing(layers.control.pos)) {
    setLayersControlPos(layers.control.pos); cnt <- cnt + 1
  }
  if (!missing(leafletWidth)) { setleafletWidth(leafletWidth); cnt <- cnt + 1 }
  if (!missing(leafletHeight)) { setleafletHeight(leafletHeight); cnt <- cnt + 1 }
  if (!missing(viewer.suppress)) {
    setViewerSuppress(viewer.suppress); cnt <- cnt + 1
  }
  if (!missing(homebutton)) { setHomebutton(homebutton); cnt <- cnt + 1 }
  if (!missing(homebutton.pos)) { setHomebuttonPos(homebutton.pos); cnt <- cnt + 1 }
  if (!missing(native.crs)) { setNativeCRS(native.crs); cnt <- cnt + 1 }
  if (!missing(watch)) { setWatcher(watch); cnt <- cnt + 1 }
  if (!missing(fgb)) { setFgb(fgb); cnt <- cnt + 1 }


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
  if (!missing(pane)) { setPane(pane); cnt <- cnt + 1 }
  if (!missing(cex)) { setCex(cex); cnt <- cnt + 1 }
  if (!missing(alpha)) { setAlpha(alpha); cnt <- cnt + 1 }

  lst <- list(

    ## global
    platform = .platform()
    , basemaps = .basemaps()
    , raster.palette = .rasterPalette()
    , vector.palette = .vectorPalette()
    , verbose = .verbose()
    , na.color = .naColor()
    , legend = .Legend()
    , legend.opacity = .legenOpacity()
    , legend.pos = .legendPos()
    , layers.control.pos = .layersControlPos()
    , leafletWidth = .leafletWidth()
    , leafletHeight = .leafletHeight()
    , viewer.suppress = .viewerSuppress()
    , homebutton = .homebutton()
    , homebutton.pos = .homebuttonPos()
    , native.crs = .nativeCRS()
    , watch = .watch()
    , fgb = .fgb()

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
    , pane = .pane()
    , cex = .cex()
    , alpha = .alpha()

  )



  if (console) {
    if (cnt == 0) {

      ## global
      cat("\n global options: \n\n")
      cat('platform            :', lst$platform, '\n' )
      cat('basemaps            :', lst$basemaps, '\n')
      cat('raster.palette      :', format(.rasterPalette())[1], '\n')
      cat('vector.palette      :', format(.vectorPalette())[1], '\n')
      cat('verbose             :', lst$verbose, '\n')
      cat('na.color            :', lst$na.color, '\n')
      cat('legend              :', lst$legend, '\n')
      cat('legend.opacity      :', lst$legend.opacity, '\n')
      cat('legend.pos          :', lst$legend.pos, '\n')
      cat('layers.control.pos  :', lst$layers.control.pos, '\n')
      cat('leafletWidth        :', lst$leafletWidth, '\n')
      cat('leafletHeight       :', lst$leafletHeight, '\n')
      cat('viewer.suppress     :', lst$viewer.suppress, '\n')
      cat('homebutton          :', lst$homebutton, '\n')
      cat('homebutton.pos      :', lst$homebutton.pos, '\n')
      cat('native.crs          :', lst$native.crs, '\n')
      cat('watch               :', lst$watch, '\n')
      cat('fgb                 :', lst$fgb, '\n')

      ## raster
      cat("\n raster data related options: \n\n")
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
      cat("\n vector data realted options: \n\n")
      cat('maxpolygons         :', lst$maxpolygons, '\n')
      cat('maxpoints           :', lst$maxpoints, '\n')
      cat('maxlines            :', lst$maxlines, '\n')
      cat('pane                :', lst$pane, '\n')
      cat('cex                 :', lst$cex, '\n')
      cat('alpha               :', lst$alpha, '\n')

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

