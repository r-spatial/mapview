# 1. mapview basics

**mapview** is intended to provide a quick and easy way to
visualize/plot spatial data in an interactive manner. As such, a
one-liner is enough to produce an interactive map view of the data.
Methods are defined for all objects from packages **sf**, **sp**,
**raster** and **satellite**.

## Vector data

**mapview** includes 3 vector type data sets:

- `breweries` - a selection of (micro-) breweries in Franconia, Bavaria.
- `trails` - slected hiking trails in Franconia (not used in this
  vignette).
- `franconia` - administrative regions of Franconia at the district
  level.

For vector data a call to `mapview` without any further arguments will
produce a default map view including:

- a layer control to switch between 5 different background maps
- a scale bar
- information on mouse cursor position and zoom level of the current
  view
- a zoom-to-layer button to easily navigate to the displayed layer
- popups listing all attribute entries of the respective features (when
  clicked)
- labels of the feature IDs (when hovered over)
- zoom control buttons provided by the underlying **leaflet** map
- attribution information of the active map layer in the bottom right
  corener of the map

Here’s an example for each vector type:

``` r

library(mapview)
mapview(breweries)
```

------------------------------------------------------------------------

``` r

mapview(franconia)
```

------------------------------------------------------------------------

### Controlling attribute columns

If we only want to plot certain columns of the attribute table we can
use argument `zcol`. Each column will be rendered as a separate layer.
These layers will be colored according to the selected attributes.

``` r

mapview(breweries, zcol = c("brewery", "village", "founded"))
```

------------------------------------------------------------------------

### Including legends

We can also include legends for the layers. Be careful, though, as it is
currently not possible to link legends to layers so we end up with too
many legends to fit the viewer window if we want to visualize many
layers. This is also the reason why legends are not shown by default.

``` r

mapview(breweries, zcol = "founded", legend = TRUE)
```

------------------------------------------------------------------------

### Automatic background map detection

Based on the mean luminence of the layer(s) to be rendered, **mapview**
will automatically decide which default map to use. For layers with very
light colors of `CartoDB.Darkmatter` is used.

``` r

mapview(breweries)
```

  

Here we also adjust line color and width (using `lwd`)

``` r

mapview(franconia, color = "cyan", col.regions = "white", lwd = 3)
```

------------------------------------------------------------------------

## Raster data

Here’s an example for raster data:

### Raster Layer

``` r

library(plainview)

mapview(poppendorf[[5]], legend = TRUE)
```

------------------------------------------------------------------------

### RasterStack/Brick

`mapview(<RasterStack/Brick>)` will produce one map view layer for each
layer in the stack/brick. Use the layers control to switch between the
layers. By default all layers are shown (see chapter advanced for an
example on how to hide all but one layer).

The `kiliNDVI` data can be loaded like this:

``` r

kili_data <- system.file("extdata", "kiliNDVI.tif", package = "mapview")
kiliNDVI <- raster::stack(kili_data)

## view the first 3 layers
mapview(kiliNDVI[[1:3]])
```
