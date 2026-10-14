# Red-Green-Blue map view of a multi-layered Raster object

Make a Red-Green-Blue plot based on three layers (in a RasterBrick,
RasterStack). Three layers (sometimes referred to as "bands" because
they may represent different bandwidths in the electromagnetic spectrum)
are combined such that they represent the red, green and blue channel.
This function can be used to make 'true (or false) color images' from
Landsat and other multi-band satellite images. Note, this text is
plagiarized, i.e. copied from
[`raster::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html).

## Usage

``` r
viewRGB(
  x,
  r = 3,
  g = 2,
  b = 1,
  quantiles = c(0.02, 0.98),
  map = NULL,
  maxpixels = mapviewGetOption("mapview.maxpixels"),
  map.types = mapviewGetOption("basemaps"),
  na.color = mapviewGetOption("na.color"),
  layer.name = NULL,
  method = c("bilinear", "ngb"),
  ...
)
```

## Arguments

- x:

  a RasterBrick, RasterStack

- r:

  integer. Index of the Red channel/band, between 1 and nlayers(x)

- g:

  integer. Index of the Green channel/band, between 1 and nlayers(x)

- b:

  integer. Index of the Blue channel/band, between 1 and nlayers(x)

- quantiles:

  the upper and lower quantiles used for color stretching. If set to
  NULL, no stretching is applied.

- map:

  the map to which the layer should be added

- maxpixels:

  integer \> 0. Maximum number of cells to use for the plot. If
  maxpixels \< `ncell(x)`, sampleRegular is used before plotting.

- map.types:

  character spcifications for the base maps. see
  <https://leaflet-extras.github.io/leaflet-providers/preview/> for
  available options.

- na.color:

  the color to be used for NA pixels

- layer.name:

  the name of the layer to be shown on the map

- method:

  Method used to compute values for the resampled layer that is passed
  on to leaflet. mapview does projection on-the-fly to ensure correct
  display and therefore needs to know how to do this projection. The
  default is 'bilinear' (bilinear interpolation), which is appropriate
  for continuous variables. The other option, 'ngb' (nearest neighbor),
  is useful for categorical variables.

- ...:

  additional arguments passed on to
  [`mapView`](https://r-spatial.github.io/mapview/reference/mapView.md)

## Author

Tim Appelhans

## Examples

``` r
if (interactive()) {
  library(raster)
  library(plainview)

  viewRGB(plainview::poppendorf, 4, 3, 2) # true-color
  viewRGB(plainview::poppendorf, 5, 4, 3) # false-color
}
```
