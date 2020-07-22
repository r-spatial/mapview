context("mapviewControls")


### isAvailableInLeaflet() -----
test_that("isAvailableInLeaflet() runs without any issues", {
  avl <- isAvailableInLeaflet()

  expect_true(avl$lab)
  expect_true(avl$scl)
})


### mapview2leaflet -----
test_that("mapview2leaflet() runs without any issues", {
  m <- mapview(breweries)
  expect_true(inherits(m, "mapview"))

  l <- mapview2leaflet(m)
  expect_true(inherits(l, c("leaflet", "htmlwidget")))
})


### getSimpleClass -----
test_that("getSimpleClass() runs without any issues", {
  library(plainview)

  expect_equal(getSimpleClass(leaflet::breweries91), "vec") # sf
  expect_equal(getSimpleClass(plainview::poppendorf), "rst") # raster
  expect_equal(getSimpleClass(franconia), "vec") # sp
})

### makeLabels -----
test_that("makeLabels() runs without any issues", {
  expect_equal(makeLabels(sf::st_geometry(breweries)[[1]]), "1")
  expect_equal(makeLabels(head(sf::st_geometry(breweries))),
               as.character(1:6))
  expect_equal(makeLabels(franconia), rownames(franconia))
  expect_equal(makeLabels(franconia, zcol = "district"),
               franconia[["district"]])
})

### makeLabelsSP -----
test_that("makeLabelsSP() runs without issues", {
  expect_length(makeLabelsSP(leaflet::breweries91$brewery),
                nrow(leaflet::breweries91@data))
  expect_type(makeLabelsSP(leaflet::breweries91$founded), "character")
})

### getFeatureIds -----
test_that("getFeatureIds() runs without issues", {
  expect_equal(getFeatureIds(franconia), row.names(franconia))
  expect_equal(getFeatureIds(leaflet::gadmCHE), row.names(leaflet::gadmCHE))
  expect_equal(getFeatureIds(sf::st_geometry(breweries)),
               1:length(sf::st_geometry(breweries)))
})

### createExtent -----
test_that("createExtent() runs without issues", {
  expect_s4_class(createExtent(breweries), "Extent")
})

### isMultiFeature -----
test_that("isMultiFeature() runs without issues", {
  expect_equal(isMultiFeature(sf::st_geometry(franconia)[[1]]), TRUE)
  expect_equal(isMultiFeature(sf::st_geometry(breweries)[[1]]), FALSE)
  expect_false(isMultiFeature("Test"))
})

### getSFClass -----
test_that("getSFClass() runs without issues", {
  expect_equal(getSFClass(breweries), "sf")
  expect_equal(getSFClass(sf::st_geometry(franconia)), "sfc_MULTIPOLYGON")
  expect_equal(getSFClass(sf::st_geometry(breweries)[[1]]), "POINT")
})

### getGeometryType -----
test_that("getGeometryType() runs without issues", {
  expect_type(getGeometryType(franconia), "character")
  expect_equal(getGeometryType(breweries), "pt")
  expect_equal(getGeometryType(sf::st_geometry(breweries)), "pt")
  expect_equal(getGeometryType(trails), "ln")
  expect_equal(getGeometryType(franconia), "pl")
})

### lineWidth -----
test_that("lineWidth() runs without issues", {
  expect_type(lineWidth(franconia), "double")
  expect_equal(lineWidth(trails), 2)
  expect_equal(lineWidth(franconia), 0.5)
})

### regionOpacity -----
test_that("regionOpacity() runs without issues", {
  expect_type(regionOpacity(franconia), "double")
  expect_equal(regionOpacity(trails), 1)
  expect_equal(regionOpacity(franconia), 0.6)
})

### basemaps -----
test_that("basemaps() runs without issues", {
  expect_type(basemaps("#0000ff"), "character")
  expect_equal(basemaps("black")[1], "CartoDB.Positron")
  expect_equal(basemaps("white")[1], "CartoDB.DarkMatter")
})

### getProjection -----
test_that("getProjection() runs without issues", {
  expect_type(getProjection(franconia), "character")
  # expect_equal(getProjection(franconia), "+proj=longlat +datum=WGS84 +no_defs")
})

### createFileId -----
test_that("createFileId() runs without issues", {
  expect_type(createFileId(), "character")
  expect_equal(nchar(createFileId(8)), 8)
})

### extendLimits -----
test_that("extendLimits() runs without issues", {
  expect_length(extendLimits(c(1, 10)), 2)
  expect_equal(extendLimits(c(1, 10)), c(0.37, 10.63))
  expect_type(extendLimits(c(1, 10)), "double")
})

### circleRadius -----
test_that("circleRadius() runs without issues", {
  expect_length(circleRadius(breweries), 1)
  expect_length(circleRadius(breweries, "founded"), nrow(breweries))
  expect_type(circleRadius(breweries, "founded"), "double")
  expect_equal(max(circleRadius(breweries, "founded", max.rad = 22)), 22)
  expect_equal(min(circleRadius(breweries, "founded", min.rad = 2)), 2) # NAs will have radius 2!!
})

### extentOverlap -----
test_that("extentOverlap() runs without issues", {
  expect_true(suppressMessages(extentOverlap(breweries, franconia)))
  expect_false(suppressMessages(extentOverlap(franconia[1, ], franconia[2, ])))
})

### makeLayerName -----
test_that("makeLayerName() runs without issues", {
  expect_type(makeLayerName(franconia), "character")
})

### makeListLayerNames -----
test_that("makeListLayerNames() runs without issues", {
  expect_type(makeListLayerNames(list(franconia, breweries), "test"), "list")
  expect_equal(makeListLayerNames(list(f = franconia, b = breweries), "test"),
               list("f", "b"))
})

### paneName -----
test_that("paneName runs without issues", {
  expect_equal(paneName(breweries), "point")
})

### zIndex -----
test_that("zIndex() runs without issues", {
  expect_equal(zIndex(franconia), 420)
})

### useCanvas -----
test_that("useCanvas() runs without issues", {
  expect_false(useCanvas(breweries))
})

### is_literally_false ----
test_that("is_literally_false() runs without issues", {
  expect_true(is_literally_false(FALSE))
  tst = NULL
  expect_false(is_literally_false(tst))
})
