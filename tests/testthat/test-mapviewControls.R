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
  expect_equal(getSimpleClass(breweries91), "vec") # sf
  expect_equal(getSimpleClass(poppendorf), "rst") # raster
  expect_equal(getSimpleClass(gadmCHE), "vec") # sp
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

### getFeatureIds -----
test_that("getFeatureIds() runs without issues", {
  expect_equal(getFeatureIds(franconia), row.names(franconia))
  expect_equal(getFeatureIds(gadmCHE), row.names(gadmCHE))
  expect_equal(getFeatureIds(sf::st_geometry(breweries)),
               1:length(sf::st_geometry(breweries)))
})

### createExtent -----
test_that("createExtent() runs without issues", {
  expect_equal(createExtent(poppendorf[[4]]),
               raster::extent(poppendorf[[4]]))
})

### isMultiFeature -----
test_that("isMultiFeature() runs without issues", {
  expect_equal(isMultiFeature(franconia[1, ]), TRUE)
  expect_equal(isMultiFeature(breweries[1, ]), FALSE)
})
