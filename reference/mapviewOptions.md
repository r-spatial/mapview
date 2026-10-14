# Global options for the mapview package

To permanently set any of these options, you can add them to your
Rprofile.site. For example, to change the default number of pixels to be
visualised for Raster\* objects, add a line like this:
options(mapviewMaxPixels = 700000) to that file.

## Usage

``` r
mapviewOptions(
  platform,
  basemaps,
  basemaps.color.shuffle,
  raster.palette,
  vector.palette,
  verbose,
  na.color,
  legend,
  legend.opacity,
  legend.pos,
  layers.control.pos,
  leafletWidth,
  leafletHeight,
  viewer.suppress,
  homebutton,
  homebutton.pos,
  native.crs,
  raster.size,
  mapview.maxpixels,
  plainview.maxpixels,
  use.layer.names,
  trim,
  method,
  query.type,
  query.digits,
  query.position,
  query.prefix,
  maxpoints,
  maxpolygons,
  maxlines,
  pane,
  cex,
  alpha,
  default = FALSE,
  console = TRUE,
  watch = FALSE,
  fgb,
  georaster
)

mapviewGetOption(param)
```

## Arguments

- platform:

  character. The rendering platform to be used. Current options are
  "leaflet", "mapdeck", and "leafgl".

- basemaps:

  character. The basemaps to be used for rendering data. See
  <https://leaflet-extras.github.io/leaflet-providers/preview/> for
  possible values

- basemaps.color.shuffle:

  logical. Should basemaps order be changed to enhance contrast based on
  layer coloring. Set to FALSE if you supply custom basemaps or want to
  ensure that "CartoDB.Positron" is always the default.

- raster.palette:

  a color palette function for raster visualisation. Should be a
  function that takes an integer as input and returns a vector of
  colors. See
  [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html) for
  details.

- vector.palette:

  a color palette function for vector visualisation. Should be a
  function that takes an integer as input and returns a vector of
  colors. See
  [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html) for
  details.

- verbose:

  logical. Many functions in mapview provide details about their
  behaviour. Set this to TRUE if you want to see these printed to the
  console.

- na.color:

  character. The default color to be used for NA values.

- legend:

  logical. Whether or not to show a legend for the layer(s).

- legend.opacity:

  opacity of the legend.

- legend.pos:

  Where should the legend be placed? One of `"topleft"`, `"topright"`,
  `"bottomleft"`, `"bottomright"`.

- layers.control.pos:

  character. Where should the layer control be placed? One of
  `"topleft"`, `"topright"`, `"bottomleft"`, `"bottomright"`.

- leafletWidth, leafletHeight:

  height and width of the htmlwidget in px.

- viewer.suppress:

  whether to render the map in the browser (`TRUE`) or the RStudio
  viewer (`FALSE`).

- homebutton:

  logical, whether to add a zoom-to-layer button to the map.

- homebutton.pos:

  character. Where should the homebutton(s) be placed? One of
  `"topleft"`, `"topright"`, `"bottomleft"`, `"bottomright"`.

- native.crs:

  logical whether to reproject to web map coordinate reference system
  (web Mercator - epsg:3857) or render using native CRS of the supplied
  data (can also be NA). Default is FALSE which will render in web
  Mercator. If set to TRUE now background maps will be drawn (but
  rendering may be much quicker as no reprojecting is necessary).

- raster.size:

  numeric. see the maxBytes argument in
  [`addRasterImage`](https://rstudio.github.io/leaflet/reference/addRasterImage.html)

- mapview.maxpixels:

  numeric. The maximum amount of pixels allowed for Raster\* objects to
  be rendered with `mapview`. Defaults to 500000. Set this higher if you
  have a potent machine or are patient enough to wait a little.

- plainview.maxpixels:

  numeric. The maximum amount of pixels allowed for Raster\* objects to
  be rendered with `plainview`. Defaults to 10000000. Set this higher if
  you have a potent machine or are patient enough to wait a little.

- use.layer.names:

  whether to use layer names when plotting raster layers.

- trim:

  should the raster be trimmed in case there are NAs on the edges.

- method:

  for raster data only (raster/stars). Method used to compute values for
  the resampled layer that is passed on to leaflet. mapview does
  projection on-the-fly to ensure correct display and therefore needs to
  know how to do this projection. The default is 'bilinear' (bilinear
  interpolation), which is appropriate for continuous variables. The
  other option, 'ngb' (nearest neighbor), is useful for categorical
  variables. Ignored if the raster layer is of class `factor` in which
  case "ngb" is used.

- query.type:

  for raster methods only. Whether to show raster value query on
  `'mousemove'` or `'click'`. Ignored if `label = FALSE`.

- query.digits:

  for raster methods only. The amount of digits to be shown by raster
  value query. Ignored if `label = FALSE`.

- query.position:

  for raster methods only. The position of the raster value query info
  box. See `position` argument of
  [`addLegend`](https://rstudio.github.io/leaflet/reference/addLegend.html)
  for possible values. Ignored if `label = FALSE`.

- query.prefix:

  for raster methods only. a character string to be shown as prefix for
  the layerId. Ignored if `label = FALSE`.

- maxpoints:

  numeric. Maximum number of points allowed for leaflet overlay
  rendering. If this number is exceeded rendering will be done using
  special functionality which will provide much more speed and better
  handling. This means that standard functionality is reduced. For
  example adding layers via "+" is not possible anymore.

- maxpolygons:

  numeric. Maximum number of polygons allowed for leaflet overlay
  rendering. If this number is exceeded rendering will be done using
  special functionality which will provide much more speed and better
  handling. This means that standard functionality is reduced. For
  example adding layers via "+" is not possible anymore.

- maxlines:

  numeric. Maximum number of lines allowed for leaflet overlay
  rendering. If this number is exceeded rendering will be done using
  special functionality which will provide much more speed and better
  handling. This means that standard functionality is reduced. For
  example adding layers via "+" is not possible anymore.

- pane:

  name of the map pane in which to render features. See
  [`addMapPane`](https://rstudio.github.io/leaflet/reference/addMapPane.html)
  for details. Currently only supported for vector layers. Ignored if
  `canvas = TRUE`. The default `"auto"` will create different panes for
  points, lines and polygons such that points overlay lines overlay
  polygons. Set to `NULL` to get default leaflet behaviour where all
  features are rendered in the same pane and layer order is determined
  automatically/sequentially.

- cex:

  numeric or attribute name(s) or column number(s) in attribute table of
  the column(s) to be used for defining the size of circles.

- alpha:

  opacity of lines.

- default:

  logical. If TRUE all options are set to their default values

- console:

  logical. Should the options be printed to the console

- watch:

  whether to watch a certain environment and automatically render
  changes to the list of spatial data in that environment. See
  [mapviewWatcher](https://r-spatial.github.io/mapview/reference/mapviewWatcher.md)
  for details.

- fgb:

  if set to `TRUE` mapview will not use 'classical' leaflet/htmlwidgets
  rendering (which embeds data directly in the html) but leverage the
  speed of a file format called flatgeobuf (hence, fgb). This has the
  added benefit that data is being streamed onto the map, which makes
  for a pleasant user experience. It should also help to visualise
  larger data sets due to a reduced memory footprint. A note of warning,
  data will be attached to the html via a \<src=...\> call which means
  that the html is not selfcontained anymore (so it cannot be used
  without an accompanying folder).

- georaster:

  whether to use
  [`addGeoRaster`](https://r-spatial.github.io/leafem/reference/addGeoRaster.html)
  instead of
  [`addRasterImage`](https://rstudio.github.io/leaflet/reference/addRasterImage.html).
  If set to `TRUE` raster image visualisation will be more performant
  for large raster data, but given the nearest neighbor resampling
  results may be slightly distorted.

- param:

  character. parameter(s) to be queried.

## Value

list of the current options (invisibly). If no arguments are provided
the options are printed.

## Functions

- `mapviewGetOption()`: query mapviewOptions parameters.

## See also

[`rasterOptions`](https://rdrr.io/pkg/raster/man/rasterOptions.html),
[`options`](https://rdrr.io/r/base/options.html)

## Author

Tim Appelhans

## Examples

``` r
mapviewOptions()
#> 
#>  global options: 
#> 
#> platform                : leaflet 
#> basemaps                : Stadia.AlidadeSmooth Stadia.AlidadeSmoothDark OpenStreetMap Esri.WorldImagery OpenTopoMap 
#> basemaps.color.shuffle  : TRUE 
#> raster.palette          : function (n)  
#> vector.palette          : function (n)  
#> verbose                 : FALSE 
#> na.color                : #BEBEBE80 
#> legend                  : TRUE 
#> legend.opacity          : 1 
#> legend.pos              : topright 
#> layers.control.pos      : topleft 
#> leafletWidth            : 
#> leafletHeight           : 
#> viewer.suppress         : FALSE 
#> homebutton              : TRUE 
#> homebutton.pos          : bottomright 
#> native.crs              : FALSE 
#> watch                   : FALSE 
#> 
#>  raster data related options: 
#> 
#> raster.size             : 8388608 
#> mapview.maxpixels       : 5e+05 
#> plainview.maxpixels     : 1e+07 
#> use.layer.names         : FALSE 
#> trim                    : TRUE 
#> method                  : bilinear 
#> query.type              : mousemove 
#> query.digits            : 7 
#> query.position          : topright 
#> query.prefix            : Layer 
#> georaster               : FALSE 
#> 
#>  vector data related options: 
#> 
#> maxpolygons             : 30000 
#> maxpoints               : 20000 
#> maxlines                : 30000 
#> pane                    : auto 
#> cex                     : 6 
#> alpha                   : 0.9 
#> fgb                     : FALSE 
mapviewOptions(na.color = "pink")
mapviewOptions()
#> 
#>  global options: 
#> 
#> platform                : leaflet 
#> basemaps                : Stadia.AlidadeSmooth Stadia.AlidadeSmoothDark OpenStreetMap Esri.WorldImagery OpenTopoMap 
#> basemaps.color.shuffle  : TRUE 
#> raster.palette          : function (n)  
#> vector.palette          : function (n)  
#> verbose                 : FALSE 
#> na.color                : pink 
#> legend                  : TRUE 
#> legend.opacity          : 1 
#> legend.pos              : topright 
#> layers.control.pos      : topleft 
#> leafletWidth            : 
#> leafletHeight           : 
#> viewer.suppress         : FALSE 
#> homebutton              : TRUE 
#> homebutton.pos          : bottomright 
#> native.crs              : FALSE 
#> watch                   : FALSE 
#> 
#>  raster data related options: 
#> 
#> raster.size             : 8388608 
#> mapview.maxpixels       : 5e+05 
#> plainview.maxpixels     : 1e+07 
#> use.layer.names         : FALSE 
#> trim                    : TRUE 
#> method                  : bilinear 
#> query.type              : mousemove 
#> query.digits            : 7 
#> query.position          : topright 
#> query.prefix            : Layer 
#> georaster               : FALSE 
#> 
#>  vector data related options: 
#> 
#> maxpolygons             : 30000 
#> maxpoints               : 20000 
#> maxlines                : 30000 
#> pane                    : auto 
#> cex                     : 6 
#> alpha                   : 0.9 
#> fgb                     : FALSE 

mapviewGetOption("platform")
#> [1] "leaflet"

mapviewOptions(default = TRUE)
mapviewOptions()
#> 
#>  global options: 
#> 
#> platform                : leaflet 
#> basemaps                : CartoDB.Positron CartoDB.DarkMatter OpenStreetMap Esri.WorldImagery OpenTopoMap 
#> basemaps.color.shuffle  : TRUE 
#> raster.palette          : function (n)  
#> vector.palette          : function (n)  
#> verbose                 : FALSE 
#> na.color                : #BEBEBE80 
#> legend                  : TRUE 
#> legend.opacity          : 1 
#> legend.pos              : topright 
#> layers.control.pos      : topleft 
#> leafletWidth            : 
#> leafletHeight           : 
#> viewer.suppress         : FALSE 
#> homebutton              : TRUE 
#> homebutton.pos          : bottomright 
#> native.crs              : FALSE 
#> watch                   : FALSE 
#> 
#>  raster data related options: 
#> 
#> raster.size             : 8388608 
#> mapview.maxpixels       : 5e+05 
#> plainview.maxpixels     : 1e+07 
#> use.layer.names         : FALSE 
#> trim                    : TRUE 
#> method                  : bilinear 
#> query.type              : mousemove 
#> query.digits            : 7 
#> query.position          : topright 
#> query.prefix            : Layer 
#> georaster               : FALSE 
#> 
#>  vector data related options: 
#> 
#> maxpolygons             : 30000 
#> maxpoints               : 20000 
#> maxlines                : 30000 
#> pane                    : auto 
#> cex                     : 0.6 
#> alpha                   : 0.9 
#> fgb                     : FALSE 

```
