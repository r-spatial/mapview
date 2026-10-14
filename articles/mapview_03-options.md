# 3. mapview options

`mapviewOptions` can be used to set several plotting parameters
(arguments) globally, meaning that they will be valid for as long as the
session is active.

A call to
[`mapviewOptions()`](https://r-spatial.github.io/mapview/reference/mapviewOptions.md)
will show all options and their default values in the console

``` r

library(mapview)
mapviewOptions()
```

    ## 
    ##  global options: 
    ## 
    ## platform                : leaflet 
    ## basemaps                : Stadia.AlidadeSmooth Stadia.AlidadeSmoothDark OpenStreetMap Esri.WorldImagery OpenTopoMap 
    ## basemaps.color.shuffle  : TRUE 
    ## raster.palette          : function (n)  
    ## vector.palette          : function (n)  
    ## verbose                 : FALSE 
    ## na.color                : #BEBEBE80 
    ## legend                  : TRUE 
    ## legend.opacity          : 1 
    ## legend.pos              : topright 
    ## layers.control.pos      : topleft 
    ## leafletWidth            : 
    ## leafletHeight           : 
    ## viewer.suppress         : FALSE 
    ## homebutton              : TRUE 
    ## homebutton.pos          : bottomright 
    ## native.crs              : FALSE 
    ## watch                   : FALSE 
    ## 
    ##  raster data related options: 
    ## 
    ## raster.size             : 8388608 
    ## mapview.maxpixels       : 5e+05 
    ## plainview.maxpixels     : 1e+07 
    ## use.layer.names         : FALSE 
    ## trim                    : TRUE 
    ## method                  : bilinear 
    ## query.type              : mousemove 
    ## query.digits            : 7 
    ## query.position          : topright 
    ## query.prefix            : Layer 
    ## georaster               : FALSE 
    ## 
    ##  vector data related options: 
    ## 
    ## maxpolygons             : 30000 
    ## maxpoints               : 20000 
    ## maxlines                : 30000 
    ## pane                    : auto 
    ## cex                     : 6 
    ## alpha                   : 0.9 
    ## fgb                     : FALSE

------------------------------------------------------------------------

In detail, these are:

- `platform` - the rendering platform: `leaflet` (default), `mapdeck`,
  or `leafgl`
- `basemaps` - the default `map.types` used by **mapview**
- `raster.size` - the maximum number of bytes to allow for the projected
  raster image rendering (before base64 encoding) -\> default 8MB
- `mapview.maxpixels` - the maximum number of pixels to allow for raster
  image rendering on the map background -\> default 500k
- `plainview.maxpixels` - the maximum number of pixels to allow for
  plain raster image rendering without the map background -\> default 1
  Million
- `maxpolygons` - the maximum number of polygons to allow for polygon
  rendering using standard **leaflet** (package) implementation. If an
  object has more features than this threshold, special canvas shader
  rendering is used. Note that this will result in less flexible
  rendering as many of the standard arguments will not be available
  (e.g. color mapping). -\> default 30k
- `maxpoints` - as for `maxpolygons` but for point features -\> default
  20k
- `maxlines` - as for `maxpolygons` but for line features -\> default
  30k
- `raster.palette` - the default palette used for rendering raster
  images -\> default `hcl.colors(n, palette = "Inferno")`
- `vector.palette` - the default palette used for rendering vector
  features -\> default `hcl.colors(n, palette = "viridis")`
- `verbose` - whether to print more verbose information to the console
  during the rendering -\> default `FALSE`
- `na.color` - the color to be used for rendering NA values -\> default
  `#BEBEBE80`
- `legend` - whether to plot a legend -\> default `FALSE`
- `legend.pos` - where to position the legend if shown -\> default
  `topright`
- `layers.control.pos` - where to position the layers control on the map
  -\> default `topleft`
- `leafletWidth`, `leafletHeight` - the width and height of the map
  widget -\> default `NULL` which means that the browser will be filled
  if possible

------------------------------------------------------------------------

To get the current value of a certain option use
[`mapviewGetOption()`](https://r-spatial.github.io/mapview/reference/mapviewOptions.md)

``` r

mapviewGetOption("basemaps")
```

    ## [1] "Stadia.AlidadeSmooth"     "Stadia.AlidadeSmoothDark"
    ## [3] "OpenStreetMap"            "Esri.WorldImagery"       
    ## [5] "OpenTopoMap"

To set certain options use
[`mapviewOptions()`](https://r-spatial.github.io/mapview/reference/mapviewOptions.md)

``` r

mapviewOptions(basemaps = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"),
               raster.palette = grey.colors,
               vector.palette = colorRampPalette(c("snow", "cornflowerblue", "grey10")),
               na.color = "magenta",
               layers.control.pos = "topright")
```

------------------------------------------------------------------------

``` r

mapviewOptions()
```

    ## 
    ##  global options: 
    ## 
    ## platform                : leaflet 
    ## basemaps                : Esri.WorldShadedRelief OpenStreetMap.DE 
    ## basemaps.color.shuffle  : TRUE 
    ## raster.palette          : function (n, start = 0.3, end = 0.9, gamma = 2.2, alpha, rev = FALSE)  
    ## vector.palette          : function (n)  
    ## verbose                 : FALSE 
    ## na.color                : magenta 
    ## legend                  : TRUE 
    ## legend.opacity          : 1 
    ## legend.pos              : topright 
    ## layers.control.pos      : topright 
    ## leafletWidth            : 
    ## leafletHeight           : 
    ## viewer.suppress         : FALSE 
    ## homebutton              : TRUE 
    ## homebutton.pos          : bottomright 
    ## native.crs              : FALSE 
    ## watch                   : FALSE 
    ## 
    ##  raster data related options: 
    ## 
    ## raster.size             : 8388608 
    ## mapview.maxpixels       : 5e+05 
    ## plainview.maxpixels     : 1e+07 
    ## use.layer.names         : FALSE 
    ## trim                    : TRUE 
    ## method                  : bilinear 
    ## query.type              : mousemove 
    ## query.digits            : 7 
    ## query.position          : topright 
    ## query.prefix            : Layer 
    ## georaster               : FALSE 
    ## 
    ##  vector data related options: 
    ## 
    ## maxpolygons             : 30000 
    ## maxpoints               : 20000 
    ## maxlines                : 30000 
    ## pane                    : auto 
    ## cex                     : 6 
    ## alpha                   : 0.9 
    ## fgb                     : FALSE

------------------------------------------------------------------------

``` r

mapview(breweries, zcol = "founded", legend = TRUE)
```

------------------------------------------------------------------------

These options will now be available until the current session is closed.
i.e. you quit R

``` r

library(plainview)

mapview(poppendorf[[5]])
```

------------------------------------------------------------------------

To change these options permanently, i.e. across sessions, you could
place a call to `mapviewOptions` in your `.Rprofile` so that they will
be set everytime you start R

To revert to factory settings use `default = TRUE`

``` r

mapviewOptions(default = TRUE)
mapview(franconia)
```

------------------------------------------------------------------------
