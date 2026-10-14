# mapview version of leaflet::color\* functions

mapview version of leaflet::color\* functions

Color palettes for mapview

## Usage

``` r
mapviewColors(
  x,
  zcol = NULL,
  colors = mapviewGetOption("vector.palette"),
  at = NULL,
  na.color = mapviewGetOption("na.color"),
  ...
)

mapviewPalette(name = "mapviewVectorColors")

mapViewPalette(name)
```

## Arguments

- x:

  Spatial\* or Raster\* object

- zcol:

  the column to be colored

- colors:

  color vector to be used for coloring the levels specified in at

- at:

  numeric vector giving the breakpoints for the colors

- na.color:

  the color for NA values.

- ...:

  additional arguments passed on to
  [`level.colors`](https://rdrr.io/pkg/lattice/man/level.colors.html)

- name:

  Name of the color palette to be used. One of "mapviewVectorColors"
  (default), "mapviewRasterColors", "mapviewSpectralColors" or
  "mapviewTopoColors".

## See also

[`level.colors`](https://rdrr.io/pkg/lattice/man/level.colors.html)

[`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html)

## Author

Tim Appelhans
