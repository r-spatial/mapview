#' Convert an RGB RasterBrick/Stack to use with spplot
#'
#' @description
#' This function takes a red-green-blue 'RasterStack' (or 'RasterBrick') object
#' and produces a list with color information that can be passed on to the
#' \code{sp.layout} argument from \code{\link[sp]{spplot}}.
#'
#' @param x a RasterBrick or RasterStack
#' @param quantiles Upper and lower quantiles used for color stretching.
#' @param alpha Level of transparency.
#'
#' @seealso \code{\link{plotRGB}}
#'
#' @author
#' Tim Appelhans, Florian Detsch
#'
#' @examples
#' library(raster)
#' library(sp)
#'
#' b <- brick(system.file("external/rlogo.grd", package="raster"))
#'
#' ## using plotRGB
#' plotRGB(b)
#'
#' ## convert brick to list
#' lout <- rgb2spLayout(b)
#' lout_alph <- rgb2spLayout(b, alpha = 0.5)
#'
#' ## create random spatial points for plotting
#' df <- data.frame(dat = rnorm(100, 2, 1),
#'                  x = rnorm(100, 50, 20),
#'                  y = rnorm(100, 50, 25))
#' coordinates(df) <- ~x+y
#'
#' ## plot spatial points with rgb background
#' spplot(df, sp.layout = lout)
#' spplot(df, sp.layout = lout_alph)
#'
#' @export rgb2spLayout
#' @aliases rgb2spLayout

rgb2spLayout <- function(x,
                         quantiles = c(0.02, 0.98),
                         alpha = 1) {

  if (!isTRUE(class(x) %in% c("RasterBrick", "RasterStack"))) {
    stop("x needs to be of class 'RasterBrick' or 'RasterStack'")
  }

  colim.recl <- raster::reclassify(x, cbind(NA, 1))

  val <- raster::getValues(colim.recl)
  val[val < 0] <- 1
  colim.recl <- raster::setValues(colim.recl, val)

  ### use downloaded map for sp raster layout definition
  mat <- raster::as.matrix(colim.recl)

  for(i in seq(ncol(mat))){
    z <- mat[, i]
    lwr <- stats::quantile(z, quantiles[1], na.rm = TRUE)
    upr <- stats::quantile(z, quantiles[2], na.rm = TRUE)
    z <- (z - lwr) / (upr - lwr)
    z[z < 0] <- 0
    z[z > 1] <- 1
    mat[, i] <- z
  }

  cols <- grDevices::rgb(mat[, ], alpha = 1)

  map.cols <- matrix(cols,
                     nrow = raster::nrow(colim.recl),
                     ncol = raster::ncol(colim.recl))

  attr(map.cols, "class") <- c("ggmap", "raster")
  attr(map.cols, "bb") <- data.frame(ll.y = raster::ymin(colim.recl),
                                     ll.x = raster::xmin(colim.recl),
                                     ur.y = raster::ymax(colim.recl),
                                     ur.x = raster::xmax(colim.recl))

  bbMap <- attr(map.cols, 'bb')
  latCenter <- with(bbMap, ll.y + ur.y) / 2
  lonCenter <- with(bbMap, ll.x + ur.x) / 2
  height <- with(bbMap, ur.y - ll.y)
  width <- with(bbMap, ur.x - ll.x)

  ## Use sp.layout of spplot: a list with the function and its arguments
  sp.raster <- list('grid.raster', map.cols,
                    x = lonCenter, y = latCenter,
                    width = width, height = height,
                    default.units = 'native',
                    first = TRUE)

  return(sp.raster)
}
