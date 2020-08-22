library(mapview)

### col2Hex -----
expect_equal(mapview:::col2Hex("black"), "#000000")
expect_equal(mapview:::col2Hex("black", alpha = TRUE), "#000000FF")

### standardColor -----
expect_equal(mapview:::standardColor(breweries), "#333333")
expect_equal(mapview:::standardColor(trails), "#6666ff")

### luminence -----
expect_equal(mapview:::luminence("black"), 0)
expect_equal(mapview:::luminence("white"), 1)
