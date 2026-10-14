# Delete elements from a map.

Delete elements from a map.

## Usage

``` r
removeMapJunk(map, junk = NULL)
```

## Arguments

- map:

  the map from which to remove elements.

- junk:

  a charcter vector of elements to remove. If NULL (the default),
  nothing is removed and the map is returned as is. See Details for a
  list of currently supported elements.

## Details

Currently supports removal of

- "zoomControl"

- "layersControl"

- "homeButton"

- "scaleBar"

- "drawToolbar"

- "easyButton"

This is mainly useful when taking a static screenshot of a map.

## Examples

``` r
if (interactive()) {
  library(mapview)

  map = mapview(franconia)

  removeMapJunk(map, "zoomControl")
}
```
