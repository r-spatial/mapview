

raster2PNG <- function(x,
                       colors,
                       na.color,
                       maxpixels) {

  x <- rasterCheckSize(x, maxpixels = maxpixels)

  x_rsc <- suppressWarnings(raster::calc(x, fun = function(y) {
    scales::rescale(y, to = c(0, 1))
  }))
  mat <- raster::as.matrix(x_rsc)

  clrs <- leaflet::colorNumeric(colors, domain = NULL,
                                na.color = na.color, alpha = TRUE)

  cols <- clrs(t(mat))
  png_dat <- as.raw(grDevices::col2rgb(cols, alpha = TRUE))
  dim(png_dat) <- c(4, ncol(x), nrow(x))

  return(png_dat)
}


rgbStack2PNG <- function(x, r = 3, g = 2, b = 1,
                         na.color = "transparent",
                         quantiles = c(0.02, 0.98),
                         maxpixels) {

  x <- rasterCheckSize(x, maxpixels = maxpixels)

  x3 <- raster::subset(x, c(r, g, b))

  mat <- cbind(x[[r]][],
               x[[g]][],
               x[[b]][])

  for(i in seq(ncol(mat))){
    z <- mat[, i]
    lwr <- stats::quantile(z, quantiles[1], na.rm = TRUE)
    upr <- stats::quantile(z, quantiles[2], na.rm = TRUE)
    z <- (z - lwr) / (upr - lwr)
    z[z < 0] <- 0
    z[z > 1] <- 1
    mat[, i] <- z
  }

  na_indx <- apply(mat, 1, anyNA)
  cols <- mat[, 1]
  cols[na_indx] <- na.color
  cols[!na_indx] <- grDevices::rgb(mat[!na_indx, ], alpha = 1)
  png_dat <- as.raw(grDevices::col2rgb(cols, alpha = TRUE))
  dim(png_dat) <- c(4, ncol(x), nrow(x))

  return(png_dat)
}
