library(mapview)

??cubeView

#example with RasterBrick Poppendorf
cubeView_RasterBrick(poppendorf)

#example with RasterStack Poppendorf
rs <- stack(poppendorf)
cubeView_RasterStack(rs)


#example with synthetic data
x_size <- 41
y_size <- 97
z_size <- 13
xyz_size <- x_size*y_size*z_size
r <- (c(0:(xyz_size-1))/43)%%256
g <- (c(0:(xyz_size-1))/7)%%256
b <- (c(0:(xyz_size-1))/1)%%256

cubeViewRaw(red=r, green=g, blue=b, x_size=x_size, y_size=y_size, z_size=z_size)

#example with data of Poppendorf
library(mapview)
p <- poppendorf
v <- as.integer(p@data@values)
v <- as.integer(v %/% (max(v)%/%255))

x_size <- p@ncols
y_size <- p@nrows
z_size <- p@data@nlayers

cubeViewRaw(grey=v, x_size=x_size, y_size=y_size, z_size=z_size)

#example with replicated data of Poppendorf
library(mapview)
library(raster)
p <- stack(poppendorf)
p <- stack(replicate(25, p))
v <- as.integer(values(p))
v <- as.integer(v %/% (max(v)%/%255))

x_size <- ncol(p)
y_size <- nrow(p)
z_size <- nlayers(p)

cubeViewRaw(grey=v, x_size=x_size, y_size=y_size, z_size=z_size)

