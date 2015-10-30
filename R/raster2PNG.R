

raster2PNG <- function(x, path = NULL) {

  x_rsc <- raster::calc(x, fun = function(y) scales::rescale(y, c(0, 1)))
  mat <- raster::as.matrix(x_rsc)

  fl <- paste0(tempfile(), ".png")
  png::writePNG(mat, fl)

  return(fl)
}


stack2RGB <- function(x, r = 3, g = 2, b = 1,
                      na.color = "transparent",
                      quantiles = c(0.02, 0.98)) {

  x3 <- raster::subset(x, subset = c(r, g, b))

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
  png_dat <- as.raw(col2rgb(cols, alpha = TRUE))
  dim(png_dat) <- c(4, ncol(x), nrow(x))

  fl <- paste0(tempfile(), ".png")
  png::writePNG(png_dat, fl)

  return(fl)
}
