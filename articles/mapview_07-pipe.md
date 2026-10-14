# 7. ceci constitue la fin du pipe

**mapview** can be used in piped expressions using `%>%`, most likely at
the very end of it…

Here are just a few examples of piped workflows:

## `plot`ting feature unions

``` r

library(mapview)
library(poorman)
library(leaflet)
library(sf)
```

``` r

franconia %>%
  st_union %>%
  plot
```

![](mapview_07-pipe_files/figure-html/pipe1a-3-1.png)

  

## `mapview`ing feature unions

``` r

franconia %>%
  st_union %>%
  mapview
```

  

## union by district

  

## districts by area

  

## counties by area

``` r

franconia %>%
  mutate(area = as.numeric(st_area(.))) %>%
  mapview(zcol = "area", legend = TRUE)
```

  

## breweries colored by district

``` r

breweries %>%
  st_intersection(franconia) %>%
  mapview(zcol = "district")
```

  

## number of breweries by county

``` r

franconia %>%
  mutate(count = lengths(st_contains(., breweries))) %>%
  mapview(zcol = "count")
```

  

## brewery density by county

``` r

franconia %>%
  mutate(count = lengths(st_contains(., breweries)),
         density = count / as.numeric(st_area(.))) %>%
  mapview(zcol = "density")
```

  
