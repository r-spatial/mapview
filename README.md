## mapview [![Travis-CI Build Status](https://travis-ci.org/environmentalinformatics-marburg/mapview.svg?branch=develop)](https://travis-ci.org/environmentalinformatics-marburg/mapview)

Interactive viewing of spatial objects in R

[![CRAN](http://www.r-pkg.org/badges/version/mapview?color=009999)](http://cran.r-project.org/package=mapview)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

![monthly](http://cranlogs.r-pkg.org/badges/mapview) ![total](http://cranlogs.r-pkg.org/badges/grand-total/mapview)

====

The online manual for **mapview** can be found at

<a href="http://environmentalinformatics-marburg.github.io/mapview/introduction.html" target="_blank">http://environmentalinformatics-marburg.github.io/mapview/introduction.html</a>

====

### Installation

For CRAN release version of **mapview** use


```S
install.packages("mapview")
```


To install the development version install the [devtools](http://cran.r-project.org/package=devtools) package.

```S
devtools::install_github("environmentalinformatics-marburg/mapview", ref = "develop")
```

====

### Introduction

**mapview** is an R package created to help researchers during their spatial data analysis workflow. It provides functions to very quickly and conveniently create interactive visualisations of spatial data.

**mapview** was created o fill the gap of quick (not presentation grade) interactive plotting to examine and visually investigate spatial data. So far, one had to either:

* (sp)plot the data in R and then toggle back and forth between the static plots (I use RStudio) or
* save the data to the disk and then open in QGIS or similar to interactively examine the results.

The main workhorse function is `mapView()` and is currently defined for:

* [raster](https://cran.r-project.org/package=raster) *objects (Layer, Stack, Brick) and SpatialPixelsDataFrame
* [sp](https://cran.r-project.org/package=sp) *objects (Points, Polygons, Lines and their DataFrame version)
* [satellite](https://cran.r-project.org/package=satellite) *objects

**mapview** is in large parts based on RStudios **leaflet** package, though in order to aid general spatial analysis workflow, we have added a few special functions.

====

### General design

For small to moderately sized spatial objects, a call to `mapView()` will return an object of class `mapview`. This class has 2 slots:

* @object - a list of the objects that are displayed on the map. This means that this slot will contain the re-projected (and in the case of Raster objects possibly re-sampled) objects which enables tracing of the modifications that took place.
* @map - the **leaflet** map. This is an S3 class object (see **leaflet** package documentation for details on the specifics).

**NOTE:** For big spatial objects, only the map widget is returned. The distinction between small/moderate and big is made with respect to the number of features. The thresholds are 20k features for points and 30k features for polygons and lines. These values can be adjusted by setting the respective [options](http://environmentalinformatics-marburg.github.io/mapview/options/options.html).

By default `mapView()` provides five base layers between which one can toggle:

* CartoDB.Positron (the default active layer)
* OpenStreetMap
* Esri.WorldImagery
* Thunderforest.Landscape
* OpenTopoMap

A preview of possible leaflet layers to use with **mapview** can be found [here](http://leaflet-extras.github.io/leaflet-providers/preview/)

Furthermore, a mouse coordinates strip is displayed at the top of the map giving information about the current mouse position and zoom level. Additionally, if the installed **leaflet** version permits, a scale bar is provided and labels are shown when hovering over a feature.

Depending on the object and/or argument settings one or several layers are created (each with or without it’s own legend) that can also be toggled. Note that in order to render properly, all layers need to be re-projected to leaflet’s underlying web mercator projection

`+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs`

which in the case of large Raster* objects or Spatial objects with lots of features can be time consuming.

====

### Under development

See

<a href="http://gisma.github.io/projView/projView1_0_9.html" target="_blank">projView</a> 

for a brief introduction how to use projected maps with mapview

====

### Contact

Please file bug reports and feature requests at https://github.com/environmentalinformatics-marburg/mapview/issues
