# library(mapview)
devtools::load_all()

mapviewOptions(platform = "mapgl")

mapview(trails)
mapview(franconia)
mapview(breweries, cex = 2)

# Working
mapview(breweries, cex = 2) +
  mapview(franconia)

# Not working
# TODO: Need to find a way how to inherit the second layer's
# settings
mapview(franconia) +
  mapview(breweries, cex = 2)

# Not working
# How to add third layer?
mapview(franconia) +
  mapview(breweries) +
  mapview(trails)
