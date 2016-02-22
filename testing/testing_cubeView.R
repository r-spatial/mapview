library(mapview)

?cubeView

x_size <- 41
y_size <- 97
z_size <- 13
xyz_size <- x_size*y_size*z_size
r <- c(0:(xyz_size-1))/43
g <- c(0:(xyz_size-1))/7
b <- c(0:(xyz_size-1))/1

cubeView(red=r, green=g, blue=b, x_size=x_size, y_size=y_size, z_size=z_size)
