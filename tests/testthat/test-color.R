context("color")

### col2Hex -----
test_that("col2Hex() runs without any issues", {
  expect_equal(col2Hex("black"), "#000000")
  expect_equal(col2Hex("black", alpha = TRUE), "#000000FF")
})

### standardColor -----
test_that("standardColor() runs without any issues", {
  expect_equal(standardColor(breweries), "#333333")
  expect_equal(standardColor(trails), "#6666ff")
})

### luminence -----
test_that("luminence() runs without any issues", {
  expect_equal(luminence("black"), 0)
  expect_equal(luminence("white"), 1)
})
