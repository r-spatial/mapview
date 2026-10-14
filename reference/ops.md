# mapview + mapview adds data from the second map to the first

mapview + mapview adds data from the second map to the first

mapview + data adds spatial data (raster\*, sf\*, sp\*) to a mapview map

mapview + NULL returns the LHS map

[...](https://rdrr.io/r/base/dots.html)

mapview \| mapview provides a slider in the middle to compare two maps.

mapview \| NULL returns the LHS map

NULL \| mapview returns the RHS map

## Usage

``` r
# S4 method for class 'mapview,mapview'
e1 + e2

# S4 method for class 'mapview,ANY'
e1 + e2

# S4 method for class 'mapview,NULL'
e1 + e2

# S4 method for class 'mapview,character'
e1 + e2

# S4 method for class 'mapview,NULL'
e1 | e2

# S4 method for class 'NULL,mapview'
e1 | e2
```

## Arguments

- e1:

  a leaflet or mapview map, or NULL.

- e2:

  a leaflet or mapview map, or NULL.

## Examples

``` r
  m1 <- mapView(franconia, col.regions = "red")
  m2 <- mapView(breweries)

  ### add two mapview objects
  m1 + m2

  ### add layers to a mapview object
  if (interactive()) {
    library(plainview)
    m1 + breweries + plainview::poppendorf[[4]]
  }

  m1 <- mapView(franconia, col.regions = "red")
  m2 <- mapView(breweries)

  ### add two mapview objects
  m1 | m2
#> Loading required namespace: leaflet.extras2
```
