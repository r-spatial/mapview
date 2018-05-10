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
#' @param platform character. The platform to be used.
#' Current options are "leaflet" and "quickmapr".
#' @param basemaps character. The basemaps to be used for rendering data. See
#' \url{http://leaflet-extras.github.io/leaflet-providers/preview/} for possible
#' values
#' @param raster.size numeric. see the maxBytes argument in \code{\link{addRasterImage}}
#' @param mapview.maxpixels numeric. The maximum amount of pixels allowed for Raster*
#' objects to be rendered with \code{mapview}. Defaults to 500000.
#' Set this higher if you have a potent machine or are patient enough to wait a little.
#' @param plainview.maxpixels numeric. The maximum amount of pixels allowed for Raster*
#' objects to be rendered with \code{plainview}. Defaults to 10000000.
#' Set this higher if you have a potent machine or are patient enough to wait a little.
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
mapviewOptions = function(x, zcol = NULL, ...) {
  out = switch(
    getSimpleClass(x),
    "rst" = mapviewRasterOptions(x, ...),
    "vec" = mapviewVectorOptions(x, zcol, ...)
  )

  out[!duplicated(names(out))]

}


mapviewRasterOptions = function(x, ...) {
  dots = list(...)

  if (missing(x)) {
    rstopts = list(
      maxpixels = 5e5,
      use.layer.names = FALSE,
      values = NULL,
      trim = TRUE,
      raster.size = 8 * 1024 * 1024,
      query.position = "topright",
      query.type = "mousemove",
      query.digits = unname(unlist(options("digits"))),
      query.prefix = "Layer"
    )
  } else {
    rstopts = list(
      maxpixels = 5e5,
      use.layer.names = ifelse(inherits(x, "RasterStackBrick"), TRUE, FALSE),
      values = NULL,
      trim = TRUE,
      raster.size = 8 * 1024 * 1024,
      query.position = "topright",
      query.type = "mousemove",
      query.digits = unname(unlist(options("digits"))),
      query.prefix = "Layer"
    )
  }

  rstopts = utils::modifyList(rstopts, dots, keep.null = TRUE)

  rstopts = Reduce(
    append,
    list(
      rstopts,
      mapviewLayoutOptions(zcol = 1, ...),
      mapviewGlobalOptions(...)
    )
  )

  rstopts[!duplicated(names(rstopts))]
}


mapviewVectorOptions = function(x,
                                zcol = NULL,
                                alpha.regions = 0.7,
                                alpha = 0.9,
                                lwd = 2,
                                ...) {
  dots = list(...)

  vctopts = list(
    pane = "auto",
    canvas = FALSE,
    burst = FALSE,
    popup = if (!missing(x)) popupTable(x) else NULL,
    layer.name = NULL,
    label = if (!missing(x)) makeLabels(x, zcol) else NULL
  )

  vctopts = append(
    vctopts,
    list(
      highlight = mapviewHighlightOptions(
        x,
        alpha.regions,
        alpha,
        lwd
      )
    )
  )

  vctopts = utils::modifyList(vctopts, dots, keep.null = TRUE)

  vctopts = Reduce(
    append,
    list(
      vctopts,
      mapviewLayoutOptions(zcol, ...),
      mapviewGlobalOptions(...)
    )
  )

  vctopts[!duplicated(names(vctopts))]
}


mapviewLayoutOptions = function(zcol = NULL, ...) {
  dots = list(...)

  layopts = list(
    homebutton = TRUE,
    homebutton.pos = "bottomright",
    legend = ifelse(is.null(zcol), FALSE, TRUE),
    legend.opacity = 1,
    legend.pos = "topright",
    layers.control.pos = "topleft",
    scalebar = TRUE,
    scalebar.pos = "bottomleft",
    label = TRUE,
    map.types = c(
      "CartoDB.Positron",
      "CartoDB.DarkMatter",
      "OpenStreetMap",
      "Esri.WorldImagery",
      "OpenTopoMap"
    ),
    leaflet.width = NULL,
    leaflet.height = NULL
  )

  layopts = utils::modifyList(layopts, dots, keep.null = TRUE)

  return(layopts)
}


mapviewGlobalOptions = function(...) {
  dots = list(...)

  glbopts = list(
    platform = "leaflet",
    verbose = FALSE,
    native.crs = FALSE,
    plainview.maxpixels = 1e7
  )

  glbopts = utils::modifyList(glbopts, dots, keep.null = TRUE)

  return(glbopts)
}


extractOptions = function(options, dots, which = c("leaflet", "mapview")) {
  which = match.arg(which)

  switch(which,
         "leaflet" = dots[setdiff(names(dots), names(options))],
         "mapview" = options[setdiff(names(options), names(dots))])
}

