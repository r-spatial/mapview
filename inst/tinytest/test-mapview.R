### bbox -----
library(sf)
library(mapview)

## mapview
map = mapview(st_bbox(franconia))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(st_bbox(franconia))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))


### data frame -----
library(sf)

## mapview
map = mapview(as.data.frame(franconia), xcol = "SHAPE_AREA", ycol = "SHAPE_LEN")

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(as.data.frame(franconia), xcol = "SHAPE_AREA", ycol = "SHAPE_LEN")

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### list -----
library(sf)

## mapview
map = mapview(list(franconia, breweries))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sf"))

## mapView
map = mapView(list(breweries, franconia))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sf"))

### missing -----
library(sf)

## mapview
map = mapview()

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_null(map@object[[1]])

## mapView
map = mapView()

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_null(map@object[[1]])

### NULL -----
library(sf)

## mapview
map = mapview(NULL)

expect_null(map)

## mapView
map = mapView(NULL)

expect_null(map)

### numeric -----
library(sf)

## mapview
map = mapview(x = franconia$SHAPE_AREA, y = franconia$SHAPE_LEN)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(x = franconia$SHAPE_AREA, y = franconia$SHAPE_LEN)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### raster layer -----
library(raster)

## mapview
map = mapview(plainview::poppendorf[[4]])

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "Raster"))

## mapView
map = mapView(plainview::poppendorf[[2]])

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "Raster"))

### raster stack/brick -----
library(raster)

## mapview
map = mapview(plainview::poppendorf)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "RasterStackBrick"))

## mapView
map = mapView(plainview::poppendorf)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "RasterStackBrick"))

### sf -----
## polygons
## mapview
map = mapview(franconia)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(franconia)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))


## polylines
## mapview
map = mapview(trails)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))


## mapView
map = mapView(trails)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))


## points
## mapview
map = mapview(breweries)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(breweries)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### sfc -----
library(sf)

## polygons
## mapview
map = mapview(st_geometry(franconia))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(st_geometry(franconia))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## polylines
## mapview
map = mapview(st_geometry(trails))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(st_geometry(trails))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## points
## mapview
map = mapview(st_geometry(breweries))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(st_geometry(breweries))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### SpatialGridDataFrame -----
library(sp)

## mapview
map = mapview(as(plainview::poppendorf[[1]], "SpatialGridDataFrame"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "SpatialPixelsDataFrame"))

## mapView
map = mapView(as(plainview::poppendorf[[1]], "SpatialGridDataFrame"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "SpatialPixelsDataFrame"))

### SpatialPixelsDataFrame -----
library(sp)

## mapview
map = mapview(as(plainview::poppendorf[[1]], "SpatialPixelsDataFrame"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "SpatialPixelsDataFrame"))

## mapView
map = mapView(as(plainview::poppendorf[[1]], "SpatialPixelsDataFrame"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "SpatialPixelsDataFrame"))

### SpatialLines -----
library(sp)
library(leaflet)

## mapview
map = mapview(as(atlStorms2005, "SpatialLines"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(as(atlStorms2005, "SpatialLines"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### SpatialLinesDataFrame -----
library(sp)
library(leaflet)

## mapview
map = mapview(atlStorms2005)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(atlStorms2005)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### SpatialPoints -----
library(sp)
library(leaflet)

## mapview
map = mapview(as(breweries91, "SpatialPoints"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(as(breweries91, "SpatialPoints"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### SpatialPointsDataFrame -----
library(sp)
library(leaflet)

## mapview
map = mapview(breweries91)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(breweries91)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### SpatialPolygons -----
library(sp)
library(leaflet)

## mapview
map = mapview(as(gadmCHE, "SpatialPolygons"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(as(gadmCHE, "SpatialPolygons"))

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### SpatialPolygonsDataFrame -----
library(sp)
library(leaflet)

## mapview
map = mapview(gadmCHE)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(gadmCHE)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

### stars -----
if (utils::packageVersion("stars") >= "0.4-1") {
  library(stars)

  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  x1 = read_stars(tif)

  ## mapview
  map = mapview(x1)

  expect_true(inherits(map, "mapview"))
  expect_true(inherits(map@map, "leaflet"))
  expect_equal(typeof(map@object), "list")
  expect_true(inherits(map@object[[1]], "stars"))

  ## mapView
  map = mapView(x1)

  expect_true(inherits(map, "mapview"))
  expect_true(inherits(map@map, "leaflet"))
  expect_equal(typeof(map@object), "list")
  expect_true(inherits(map@object[[1]], "stars"))
}

### XY(ZM) -----
library(sf)

pxy = st_point(c(1, 2))
pxyz = st_point(c(1, 2, 3))
pxym = st_point(c(1, 2, 3), "XYM")
pxyzm = st_point(c(1, 2, 3, 4), "XYZM")

## XY
## mapview
map = mapview(pxy)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(pxy)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## XYZ
## mapview
map = mapview(pxyz)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(pxyz)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## XYM
## mapview
map = mapview(pxym)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(pxym)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## XYZM
## mapview
map = mapview(pxyzm)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

## mapView
map = mapView(pxyzm)

expect_true(inherits(map, "mapview"))
expect_true(inherits(map@map, "leaflet"))
expect_equal(typeof(map@object), "list")
expect_true(inherits(map@object[[1]], "sfc"))

