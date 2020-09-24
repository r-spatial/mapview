#' mapview version of leaflet::color* functions
#'
#' @param x Spatial* or Raster* object
#' @param zcol the column to be colored
#' @param colors color vector to be used for coloring the levels
#' specified in at
#' @param at numeric vector giving the breakpoints for the colors
#' @param na.color the color for NA values.
#' @param ... additional arguments passed on to \code{\link{level.colors}}
#'
#' @author
#' Tim Appelhans
#'
#' @seealso
#' \code{\link{level.colors}}
#'
#' @name mapviewColors
#' @export mapviewColors
#' @aliases mapviewColors
mapviewColors <- function(x,
                          zcol = NULL,
                          colors = mapviewGetOption("vector.palette"),
                          at = NULL,
                          na.color = mapviewGetOption("na.color"),
                          ...) {

  stnd_col <- "#6666ff"

  if (typeof(x) == "list") {
    if (is.function(colors)) colors <- colors(length(x))
    return(col2Hex(colors))
  }

  if (inherits(x, "Spatial") && is.null(zcol)) {
    if (is.function(colors)) colors <- stnd_col #colors(1)
    return(col2Hex(colors))
  }

  if (!isMultiFeature(x)) {
    colors <- stnd_col
    return(col2Hex(colors))
  }

  if (isMultiFeature(x) && is.null(zcol)) {
    if (is.function(colors)) colors <- stnd_col #colors(1)
    return(col2Hex(colors))
  }

  if (inherits(x, "Spatial") && !is.null(zcol)) x <- x@data[, zcol]

  if (is.character(x)) x <- factor(x)
  x <- as.numeric(x)

  if (length(unique(x)) == 1) {
    if (is.function(colors)) colors <- colors(1)
    return(col2Hex(colors[1]))
  } else if (is.null(at)) {
    at <- lattice::do.breaks(range(x, na.rm = TRUE),
                             length(unique(x)))
    cols <- lattice::level.colors(x,
                                  at = at,
                                  col.regions = colors,
                                  ...)
    cols[is.na(cols)] <- na.color
    return(col2Hex(cols))
  } else {
    cols <- lattice::level.colors(x,
                                  at = at,
                                  col.regions = colors,
                                  ...)
    cols[is.na(cols)] <- na.color
    return(col2Hex(cols))
  }

  # attributes(f) <- list(colorType = "bin",
  #                       colorArgs = list(bins = at,
  #                                        na.color = na.color))
}


## raster colors
rasterColors <- function(col.regions,
                         at,
                         na.color) {

  f <- function(x) {

    cols <- lattice::level.colors(x,
                                  at = at,
                                  col.regions = col.regions)
    #cols <- col2Hex(cols)
    cols[is.na(cols)] <- na.color

    return(col2Hex(cols, alpha = TRUE))

  }

  attributes(f) <- list(colorType = "bin",
                        colorArgs = list(bins = at,
                                         na.color = na.color))

  return(f)

}


## hex color conversion
col2Hex <- function(col, alpha = FALSE) {

  mat <- grDevices::col2rgb(col, alpha = TRUE)
  if (alpha) {
    hx <- grDevices::rgb(mat[1, ]/255, mat[2, ]/255,
                         mat[3, ]/255, mat[4, ]/255)
  } else {
    hx <- grDevices::rgb(mat[1, ]/255, mat[2, ]/255, mat[3, ]/255)
  }
  return(hx)

}


## vector colors
vectorColors <- function(x, # a sp or sf object
                         zcol = NULL,
                         colors = mapviewGetOption("vector.palette"),
                         at = NULL,
                         na.color = mapviewGetOption("na.color"),
                         ...) {

  stnd_col <- standardColor(x) #"#333333"

  if (getGeometryType(x) == "ln") {
    if (!is.null(zcol)) {
      if (!inherits(x[[zcol]], c("numeric", "integer")) & !is.null(at)) {
        warning("ignoring 'at' which is only supported for numeric/integer values")
        at <- NULL
      }
      col <- zcolColors(x[[zcol]],
                        colors = colors,
                        at = at,
                        na.color = na.color,
                        ...)
    } else if (is.null(zcol) & is.function(colors)) {
      col <- standardColor(x)
    } else col <- colors
  } else if (is.function(colors)) {
    col <- stnd_col
  } else col <- colors

  return(col2Hex(col))

}


vectorColRegions <- function(x, # a sp or sf object
                             zcol = NULL,
                             col.regions = mapviewGetOption("vector.palette"),
                             at = NULL,
                             na.color = mapviewGetOption("na.color"),
                             ...) {

  stnd_col <- standardColRegions(x) #"#aaaaff" #"#CCCCCC"

  if (!is.null(zcol)) {
    if (!inherits(x[[zcol]], c("numeric", "integer")) & !is.null(at)) {
      warning("ignoring 'at' which is only supported for numeric values")
      at <- NULL
    }
    col <- zcolColors(x[[zcol]],
                      colors = col.regions,
                      at = at,
                      na.color = na.color,
                      ...)
  } else if (is.null(zcol) & is.function(col.regions)) {
    col <- stnd_col
  } else col <- col.regions

  return(col2Hex(col))

}

### if zcol is set, we use this function to produce colors according to
### levels of zcol or as provided by at
zcolColors <- function(x, # a vector, not a sp or sf object
                       colors = mapviewGetOption("vector.palette"),
                       at = NULL,
                       na.color = mapviewGetOption("na.color"),
                       return.sorted = FALSE,
                       ...) {

  ## if interger convert to numeric
  if (inherits (x, "integer")) {
    x = as.numeric(x)
  }

  ## if character convert to factor
  if (inherits(x, "character")) {
    x = as.factor(x)
  }

  if (!(is.function(colors))) {
    # if (length(colors) == length(x)) {
    #   return(col2Hex(colors))
    # }

    if (is.character(colors) &
        length(colors) == 1) {
      return(col2Hex(colors))
    }

    if (inherits(x, "numeric")) {
      if (is.null(at) & length(colors) < length(unique(x))) {
        warning(
          sprintf(
            "Found less unique colors (%s) than unique zcol values (%s)! \nInterpolating color vector to match number of zcol values."
            , length(colors)
            , length(unique(x))
          )
          , call. = FALSE
        )
        colors = grDevices::colorRampPalette(colors)
      }

      if (!is.null(at) & length(colors) < length(at)) {
        warning(
          sprintf(
            "Found less unique colors (%s) than unique zcol values (%s)! \nInterpolating color vector to match number of zcol values."
            , length(colors)
            , length(at)
          )
          , call. = FALSE
        )
        colors = grDevices::colorRampPalette(colors)
      }

      if (length(colors) > length(unique(x))) {
        warning(
          sprintf(
            "Found more unique colors (%s) than unique zcol values (%s)! \nTrimming color vector to match number of zcol values."
            , length(colors)
            , length(unique(x))
          )
          , call. = FALSE
        )
        colors = grDevices::colorRampPalette(colors[1:length(x)])
      }
    }

    if (inherits(x, "factor")) {
      if (length(colors) < length(unique(x))) {
        if (length(unique(colors)) < length(unique(x))) {
          warning(
            sprintf(
              "Found less unique colors (%s) than unique zcol values (%s)! \nRecycling color vector."
              , length(colors)
              , length(unique(x))
            )
            , call. = FALSE
          )
        }
        if (length(unique(colors)) > length(unique(x))) {
          warning(
            sprintf(
              "Found more unique colors (%s) than unique zcol values (%s)! \nTrimming colors to match number of unique zcol values."
              , length(unique(colors))
              , length(unique(x))
            )
            , call. = FALSE
          )
        }
        colors = rep_len(colors, length(levels(x)))
      }

      if (length(colors) > length(x)) {
        warning(
          sprintf(
            "Found more colors (%s) than zcol values (%s)! \nTrimming colors to match number of zcol values."
            , length(colors)
            , length(x)
          )
          , call. = FALSE
        )
        colors = colors[1:length(levels(x))]
      }
    }
  }

  if (is.factor(x)) {
    nint = length(levels(x))
    rng = range(seq_along(levels(x)), na.rm = TRUE)
  } else {
    nint = length(unique(x))
    rng = range(x, na.rm = TRUE)
  }

  x <- as.numeric(x)

  if (length(unique(x)) == 1) {
    cols <- colors(1) #"#ffffff" #"#6666ff"
  } else {
    if (is.null(at)) {
      at <- lattice::do.breaks(extendLimits(rng), nint)
    }

    if (length(colors) != length(x) | is.function(colors)) {
      cols <- lattice::level.colors(x,
                                    at = at,
                                    col.regions = colors,
                                    ...)
    } else {
      cols = colors
    }
    if (return.sorted) cols <- cols[order(x)]

    cols[is.na(cols)] <- na.color
  }
  return(col2Hex(cols))

}


standardColor <- function(x) {
  if (missing(x)) return("#6666ff")
  switch(getGeometryType(x),
         "pt" = "#333333", #"#66b3ff",
         "ln" = "#6666ff", #"#66b3ff",
         "pl" = "#333333",
         "gc" = "#333333") #"#66b3ff")
}


standardColRegions <- function(x) {
  if (missing(x)) return("#6666ff")
  switch(getGeometryType(x),
         "pt" = "#6666ff", #"#66b3ff",
         "ln" = "#6666ff", #"#66b3ff",
         "pl" = "#6666ff",
         "gc" = "#6666ff") #"#66b3ff")
}


# from https://en.wikipedia.org/wiki/Relative_luminance
luminence <- function(color) {
  rgbcols <- as.numeric(grDevices::col2rgb(color))
  (0.2126 * rgbcols[1] + 0.7152 * rgbcols[2] + 0.0722 * rgbcols[3]) / 255
}



#' Color palettes for mapview
#'
#' @param name Name of the color palette to be used. One of
#' "mapviewVectorColors" (default), "mapviewRasterColors",
#' "mapviewSpectralColors" or "mapviewTopoColors".
#'
#' @seealso
#' \code{\link{colorRampPalette}}
#'
#' @name mapviewPalette
#' @export mapviewPalette
#' @aliases mapviewPalette
#' @rdname mapviewColors

mapviewPalette <- function(name = "mapviewVectorColors") {

  switch(name,
         mapviewRasterColors = function(n) {
           grDevices::hcl.colors(n, palette = "Inferno")
         },
         mapviewVectorColors = function(n) {
           grDevices::hcl.colors(n, palette = "viridis")
         },
         mapviewSpectralColors =
           grDevices::colorRampPalette(c("#ebeaf7", "#92b9db",
                                         "#7db7c4", "#7dbbaa",
                                         "#7FB972", "#abb342",
                                         "#d6a62c", "#E68E34",
                                         "#E6642C", "#D92120",
                                         "#460000")),
         mapviewTopoColors =
           grDevices::colorRampPalette(c("#00555f", "#058353",
                                         "#7a8139", "#c1923b",
                                         "#ca9b7b", "#99D6D1",
                                         "#edf8f7"))
  )
}

## mapViewPalette =========================================================

#' @aliases mapViewPalette
#' @export mapViewPalette
#' @rdname mapviewColors

mapViewPalette <- function(name) {
  mapviewPalette(name)
}


# setClassPalette <- function(simple.class) {
#
#   pkgs <- c("viridis")
#   avl <- sapply(pkgs, "requireNamespace",
#                 quietly = TRUE, USE.NAMES = FALSE)
#
#   if (simple.class == "rst") {
#
#     function(type) {
#
#       if (type == "factor") {
#         if (avl) viridis::magma else mapviewPalette
#       } else {
#         if (avl) viridis::inferno else mapviewPalette
#       }
#
#     }
#
#   } else {
#
#     function(type) {
#
#       if (type == "factor") {
#         if (avl) viridis::magma else mapviewPalette
#       } else {
#         if (avl) viridis::inferno else mapviewPalette
#       }
#
#     }
#   }
#
# }
