context("mapview")


### bbox -----
test_that("mapview() runs without any issues for bbox", {
  library(sf)

  ## mapview
  map = mapview(st_bbox(franconia))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(st_bbox(franconia))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### character -----
# test_that("mapview() runs without any issues for character", {
#   library(sf)
#   tmpfl = tempfile(fileext = ".gpkg")
#   st_write(franconia, tmpfl, delete_dsn = TRUE, quiet = TRUE)
#
#   ## mapview
#   map = mapview(tmpfl)
#
#   expect_s4_class(map, "mapview")
#   expect_s3_class(map@map, "leaflet")
#   expect_type(map@object, "list")
#   expect_type(map@object[[1]], "character")
#
#   ## mapView
#   map = mapView(tmpfl)
#
#   expect_s4_class(map, "mapview")
#   expect_s3_class(map@map, "leaflet")
#   expect_type(map@object, "list")
#   expect_type(map@object[[1]], "character")
# })

### data frame -----
test_that("mapview() runs without any issues for data frames", {
  library(sf)

  ## mapview
  map = mapview(as.data.frame(franconia), xcol = "SHAPE_AREA", ycol = "SHAPE_LEN")

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(as.data.frame(franconia), xcol = "SHAPE_AREA", ycol = "SHAPE_LEN")

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### list -----
test_that("mapview() runs without any issues for list", {
  library(sf)

  ## mapview
  map = mapview(list(franconia, breweries))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sf")
  expect_s3_class(map@object[[2]], "sf")

  ## mapView
  map = mapView(list(breweries, franconia))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sf")
  expect_s3_class(map@object[[2]], "sf")
})

### missing -----
test_that("mapview() runs without any issues for missing", {
  library(sf)

  ## mapview
  map = mapview()

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_null(map@object[[1]])

  ## mapView
  map = mapView()

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_null(map@object[[1]])
})

### NULL -----
test_that("mapview() runs without any issues for NULL", {
  library(sf)

  ## mapview
  map = mapview(NULL)

  expect_null(map)

  ## mapView
  map = mapView(NULL)

  expect_null(map)
})

### numeric -----
test_that("mapview() runs without any issues for numeric", {
  library(sf)

  ## mapview
  map = mapview(x = franconia$SHAPE_AREA, y = franconia$SHAPE_LEN)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(x = franconia$SHAPE_AREA, y = franconia$SHAPE_LEN)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### raster layer -----
test_that("mapview() runs without any issues for raster layer", {
  library(raster)

  ## mapview
  map = mapview(plainview::poppendorf[[4]])

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s4_class(map@object[[1]], "Raster")

  ## mapView
  map = mapView(plainview::poppendorf[[2]])

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s4_class(map@object[[1]], "Raster")
})

### raster stack/brick -----
test_that("mapview() runs without any issues for raster stack/brick", {
  library(raster)

  ## mapview
  map = mapview(plainview::poppendorf)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s4_class(map@object[[1]], "RasterStackBrick")

  ## mapView
  map = mapView(plainview::poppendorf)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s4_class(map@object[[1]], "RasterStackBrick")
})

### sf -----
test_that("mapview() runs without any issues for sf", {
  ## polygons
  ## mapview
  map = mapview(franconia)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(franconia)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## polylines
  ## mapview
  map = mapview(trails)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(trails)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## points
  ## mapview
  map = mapview(breweries)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(breweries)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### sfc -----
test_that("mapview() runs without any issues for sfc", {
  library(sf)

  ## polygons
  ## mapview
  map = mapview(st_geometry(franconia))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(st_geometry(franconia))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## polylines
  ## mapview
  map = mapview(st_geometry(trails))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(st_geometry(trails))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## points
  ## mapview
  map = mapview(st_geometry(breweries))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(st_geometry(breweries))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### SpatialGridDataFrame -----
test_that("mapview() runs without any issues for SpatialGridDataFrame", {
  library(sp)

  ## mapview
  map = mapview(as(plainview::poppendorf[[1]], "SpatialGridDataFrame"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s4_class(map@object[[1]], "SpatialPixelsDataFrame")

  ## mapView
  map = mapView(as(plainview::poppendorf[[1]], "SpatialGridDataFrame"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s4_class(map@object[[1]], "SpatialPixelsDataFrame")
})

### SpatialPixelsDataFrame -----
test_that("mapview() runs without any issues for SpatialPixelsDataFrame", {
  library(sp)

  ## mapview
  map = mapview(as(plainview::poppendorf[[1]], "SpatialPixelsDataFrame"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s4_class(map@object[[1]], "SpatialPixelsDataFrame")

  ## mapView
  map = mapView(as(plainview::poppendorf[[1]], "SpatialPixelsDataFrame"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s4_class(map@object[[1]], "SpatialPixelsDataFrame")
})

### SpatialLines -----
test_that("mapview() runs without any issues for SpatialLines", {
  library(sp)
  library(leaflet)

  ## mapview
  map = mapview(as(atlStorms2005, "SpatialLines"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(as(atlStorms2005, "SpatialLines"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### SpatialLinesDataFrame -----
test_that("mapview() runs without any issues for SpatialLinesDataFrame", {
  library(sp)
  library(leaflet)

  ## mapview
  map = mapview(atlStorms2005)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(atlStorms2005)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### SpatialPoints -----
test_that("mapview() runs without any issues for SpatialLines", {
  library(sp)
  library(leaflet)

  ## mapview
  map = mapview(as(breweries91, "SpatialPoints"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(as(breweries91, "SpatialPoints"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### SpatialPointsDataFrame -----
test_that("mapview() runs without any issues for SpatialPointsDataFrame", {
  library(sp)
  library(leaflet)

  ## mapview
  map = mapview(breweries91)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(breweries91)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### SpatialPolygons -----
test_that("mapview() runs without any issues for SpatialPolygons", {
  library(sp)
  library(leaflet)

  ## mapview
  map = mapview(as(gadmCHE, "SpatialPolygons"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(as(gadmCHE, "SpatialPolygons"))

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### SpatialPolygonsDataFrame -----
test_that("mapview() runs without any issues for SpatialPolygonsDataFrame", {
  library(sp)
  library(leaflet)

  ## mapview
  map = mapview(gadmCHE)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(gadmCHE)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

### stars -----
test_that("mapview() runs without any issues for stars", {
  library(stars)

  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  x1 = read_stars(tif)

  ## mapview
  map = mapview(x1)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "stars")

  ## mapView
  map = mapView(x1)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "stars")
})

### XY(ZM) -----
test_that("mapview() runs without any issues for XY(ZM)", {
  library(sf)

  pxy = st_point(c(1, 2))
  pxyz = st_point(c(1, 2, 3))
  pxym = st_point(c(1, 2, 3), "XYM")
  pxyzm = st_point(c(1, 2, 3, 4), "XYZM")

  ## XY
  ## mapview
  map = mapview(pxy)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(pxy)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## XYZ
  ## mapview
  map = mapview(pxyz)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(pxyz)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")

  ## XYM
  ## mapview
  map = mapview(pxym)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(pxym)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## XYZM
  ## mapview
  map = mapview(pxyzm)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")

  ## mapView
  map = mapView(pxyzm)

  expect_s4_class(map, "mapview")
  expect_s3_class(map@map, "leaflet")
  expect_type(map@object, "list")
  expect_s3_class(map@object[[1]], "sfc")
})

