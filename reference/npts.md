# count the number of points/vertices/nodes of sf objects

count the number of points/vertices/nodes of sf objects

## Usage

``` r
npts(x, by_feature = FALSE)
```

## Arguments

- x:

  an sf/sfc object

- by_feature:

  count total number of vertices (FALSE) of for each feature (TRUE).

## Note

currently only works for \*POINTS, \*LINES and \*POLYGONS (not
GEOMETRYCOLLECTION).

## Examples

``` r
npts(franconia)
#> [1] 5858
npts(franconia, by_feature = TRUE)
#>  [1]  27  61  47  43 288 331 238 146 232 159 164 153 125  53  39  36 109  33 341
#> [20] 244  98 220 220 232 174  40  33  52 191 212 208 202 153 186 240 245 283
npts(sf::st_geometry(franconia[1, ])) # first polygon
#> [1] 27

npts(breweries) # is the same as
#> [1] 224
nrow(breweries)
#> [1] 224
```
