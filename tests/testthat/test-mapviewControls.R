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
  expect_equal(getSimpleClass(breweries91), "vec")
  expect_warning(getSimpleClass(breweries)) # dual-class 'sf' object

  expect_equal(getSimpleClass(poppendorf), "rst")
})
