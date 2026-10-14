# 5. extra functionality

**mapview** provides a couple of special convenience functions for all
supported spatial data types:

- `viewExtent` to view extents/bounding boxes of objects
- `sync`ing of multiple maps

There are also a few *raster-only* specialty functions:

- `viewRGB` to view RGB (raster) images

For the following:

- `plainview` to view raster images without map background
- `slideview` to compare two raster/RGB images
- `cubeview` to view 3D raster cubes

please refer to the individual help pages (e.g. `?cubeview`)

## All types

### viewExtent

This can be handy to check whether features overlap or to just check
where things are roughly located (e.g. in case we have big data sets,
e.g. raster stacks).

``` r

library(mapview)
library(leaflet)
library(plainview)
library(leafsync)

viewExtent(poppendorf, color = "red") + viewExtent(breweries)
```

------------------------------------------------------------------------

### sync & latticeView

`sync` can be used to produce small multiples that are synchronised.
Meaning that if we zoom into one panel, all other panels will also zoom.
Additionally, the mouse position is also synced (indicated via a red
circle). `sync` provides a very convenient way to compare multiple
attributes of the same feature geometry set.

We can `sync` all panels:

``` r

m1 <- mapview(breweries, zcol = "village", map.types = "Esri.WorldImagery")
m2 <- mapview(breweries, zcol = "brewery", col.regions = heat.colors)
m3 <- mapview(breweries, zcol = "founded", legend = TRUE)
m4 <- leaflet() %>% addTiles() %>% addCircleMarkers(data = breweries)

sync(m1, m2, m3, m4)
```

  

Any combination of panels:

``` r

sync(list(m1, m2, m3, m4),
     sync = list(c(1, 4),
                 c(2, 3)))
```

  

We can also control the layout by specifying how many columns we want to
have (rows will be automatically set):

``` r

sync(list(m1, m2, m3), ncol = 1)
```

  

`latticeView` will render small multiples without synchronising

``` r

latticeView(m1, m3)
```

  

------------------------------------------------------------------------

## Raster-only

### viewRGB

`viewRB` can be used to view true- or false-color raster images. It
expects a `RasterStack` or `RasterBrick` and the default setting will
render layer combination 3-2-1

``` r

library(plainview)

viewRGB(poppendorf, r = 4, g = 3, b = 2)
```

    ## Warning in CPL_crs_from_input(x): GDAL Message 1: +init=epsg:XXXX syntax is
    ## deprecated. It might return a CRS with a non-EPSG compliant axis order.

  

``` r

viewRGB(poppendorf, 5, 4, 3)
```
