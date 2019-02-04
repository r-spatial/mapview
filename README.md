# mapview #

[![cran checks](https://cranchecks.info/badges/worst/mapview)](https://cran.r-project.org/web/checks/check_results_mapview.html)
[![Travis-CI Build Status](https://travis-ci.org/r-spatial/mapview.svg?branch=develop)](https://travis-ci.org/r-spatial/mapview)
[![monthly](http://cranlogs.r-pkg.org/badges/mapview)](https://www.rpackages.io/package/mapview) 
[![total](http://cranlogs.r-pkg.org/badges/grand-total/mapview)](https://www.rpackages.io/package/mapview)
[![CRAN](http://www.r-pkg.org/badges/version/mapview?color=009999)](https://cran.r-project.org/package=mapview)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-spatial/mapview/develop.svg)](https://codecov.io/github/r-spatial/mapview?branch=develop)

### Interactive viewing of spatial data in R ###

<a href="https://github.com/tim-salabim/mvl"><img align="right" src="https://github.com/tim-salabim/mvl/blob/cstriestohelp/imagery/animated/box_anim.gif?raw=true" /></a>

**mapview** is an R package created to help researchers during their spatial data analysis workflow. It provides functions to very quickly and conveniently create interactive visualisations of spatial data. It was created to fill the gap of quick (not presentation grade) interactive plotting to examine and visually investigate both aspects of spatial data, the geometries and their attributes.

The main user relevant functions are:

* `mapview` - view potentially multiple spatial objects on a set of background maps
* `viewExtent` - view extent / bounding box of spatial objects
* `viewRGB` - view RGB true- or false-color images of raster objects
* `latticeView` & `sync` - view small multiples of several potentially synchronised maps
* `slideView` - view two overlaid raster objects with a horizontal slider to hide/reveal one of them
* `cubeView` - 3D view of raster data cubes (e.g. space-time cube) with keyboard control over position of slices
* `plainview` - view large rasters on a plain background (i.e. no background map)
* `mapshot` - easily save maps (including leaflet maps) as `html` or `png` (or other image formats)

Objects of the following spatial classes are supported:

* [sf](https://cran.r-project.org/package=sf)
* [raster](https://cran.r-project.org/package=raster) (Layer, Stack, Brick and SpatialPixels* / SpatialGridDataFrame)
* [sp](https://cran.r-project.org/package=sp) (Points, Polygons, Lines and their DataFrame version)
* [satellite](https://cran.r-project.org/package=satellite)


In addition, **mapview** provides some extra leaflet compatible functionality:

* `addHomeButton` - add zoom-to-layer buttons 
* `addLogo` - add a logo (or any image really) to a map
* `addFeatures` - add features to a map (sf or sp). This is features type agnostic, meaning the same function can be used for points, lines and polygons
* `addMouseCoordinates` - add mouse cursor postion information when moving over the map
* `addLargeFeatures` - add large data to a map that would otherwise make the leaflet map irresponsive
* `addExtent` - add a polygon denoting the extent/bbox of a spatial object to a map
* `popupTable`, `popupGraph` & `popupImage` - popups of tables, grpahs and images for use with the leaflet `add*` functions


## Installation ##

For CRAN release version of **mapview** use


```S
install.packages("mapview")
```


To install the development version install the [devtools](https://cran.r-project.org/package=devtools) package.

```S
devtools::install_github("r-spatial/mapview@develop")
```


## Usage ##

The most basic call

```S
mapview(breweries)
```

will produce a web map visualisation of the breweries data with the following components:

![](https://raw.githubusercontent.com/r-spatial/mapview/develop/inst/extdata/basic_small.png)

## Contact ##

Please file bug reports and feature requests at https://github.com/r-spatial/mapview/issues

In case of Pull Requests, please make sure to submit them to the `develop` branch of this repository.
