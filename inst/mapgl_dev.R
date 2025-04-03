# library(mapview)
devtools::load_all()

mapviewOptions(platform = "mapgl")

mapview(franconia)
mapview(breweries)
mapview(trails)
