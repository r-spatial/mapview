## mapview 2.6.0

new features:

  * mapview has gained argument viewer.suppress to enable rendering in the browser.
  * popupTable has gained argument feature.id to choose whether to show or hide 'Feature ID' column. #182

bugfixes:

  * isFALSE (caused errors on R < 3.5) is now handled properly.
  * mapshot does not unintentionally delete url anymore if saving to png with same name.
  * mapshot now properly deletes temporary url when saving to png (or png and html).
  * internal function mapview:::nrings now calculates number of polygon rings correctly. #181

## mapview 2.5.0

new features:

  * addMouseCoordinates now displays basic info by default. Detailed info can be accessed by holding Ctrl keyboard button which also copies lon, lat and zoom info to the clipboard when Ctrl + clicking on the map. #113
  * npts has gained argument by_feature to count the number of vertices for each feature individually. #155
  * canvas now auto-detected based on feature complexity.
  * layer.name is now cut at 50 characters which used to be especially problematic for dplyr chains inside a mapview call. Fixes #159
  * added '+' method for RHS class NULL. https://twitter.com/TimSalabim3/status/1012213914262794240
  * popupImage has gained argument embed to specify whether local images should be embeded as base64 in the popup html. #168
  * stars methods re-enabled since now on CRAN.
  * added viewRGB method for stars images.

bugfixes:

  * leaflet::addScalebar would prevent Raster* method from rendering correctly for multiple layers.
  * cex was not respected when burst = TRUE. #149
  * popupTable no longer causes a stack overflow with large point data. #154
  * popupGraph no longer relies on png files on disk but embeds graphs via base64. #156 
  * in mapshot, remove temporary url files only if remove_url = TRUE. #157
  * removeMouseCoordinates should work properly now (also exported). #145
  * home button for single point now zooms to maximum zoom 18.
  
miscellaneous:

  * dropped gdalUtils from Imports. gdalinfo and gdal_translate now used via sf.
  * addMapPane has been migrated to leaflet.
  * legend now TRUE by default as all legends are now linked to layers.
  * better error messages in some cases (e.g. missing zcol, empty objects).
  * package now depends on leaflet >= 1.0.0
  * highlight now also accepts FALSE in addition to NULL to stop highlighting.


## mapview 2.4.0

new features:

  * addImageQuery has gained argument prefix to modify the layerId prefix.
  * mapview methods for raster data have gained arguments label, query.type, query.digits, query.position and query.prefix to modify raster value query settings. https://twitter.com/pierreroudier/status/958476875344392193
  * popupTable now right aligns values.
  * Thanks to Pierre Roudier quantile strectching in viewRGB can now be turned off by simply setting to NULL. #127
  * mapshot has gained argument remove_controls to remove map junk when saving to image file format.
  * mapview method for data.frame has gained argument crs to enable rendering on a basemap #138
  * updated to work with leaflet 2.0.0 #129 (incl. deprecation of previously used large methods)
  * new function addMapPane to enable control over layer order.
  * mapview has gained argument pane as it now uses addMapPane to ensure points overlay lines overlay polygons.
  * mapview has gained argument canvas to enable canvas rendering.
  
bugfixes:

  * viewRGB failed because of missing method argument. #125
  * combineExtent didn't check properly for crs and failed for raster images.


## mapview 2.3.0

new features:

  * popupTable has gained argument row.numbers to disable row numbers. #109
  * new function addStarsImage to visualise stars images.
  * new function addImageQuery to provide raster/stars value query on mouseover/click.
  * mapview has gained new argument `method` to specify the method used for resampling of raster data. #123
  
bugfixes:

  * raster method with native.crs = TRUE did throw an error on windows. #111
  * SpatialPixelsDataFrame and SpatialGridDataFrame failed because of missing map argument.
  * example for multiple popupImages was broken. #118
  * SpatialPixelsDataFrame failed when `zcol` was supplied. #124
  
misc:

  * added more details in description field as per request of Uwe Ligges.
  * moved leaflet from Depends to Imports.
  

## mapview 2.2.0

new features:

  * if supported by the installed leaflet version, legends are now automatically linked to layers.
  * simple features with XYZ, XYM or XYZM geometries are now supported.
  * added function addExtent to add rectangles showing bbox/extent/outline of sf/sp/raster objects.
  * more concise internal recursive method dispatch.
  * if multiple layers are plotted (via "+") initial zoom is now on global extent and a "Zoom full" button is added to the map at the bottom left of the map to re-zoom to this global extent.
  * we now have mapview method for class 'data.frame' which enables interactive scatter plots.
  * we now have mapview method for class 'numeric' which enables interactive plots of a numeric variable.
  * updated slideView to accommodate more than one incident in a flexdashboard #95
  * if supplied data has only one attribute/field column mapview will now colour the plot automatically according to that attribute/field.
  * new mapview method for class 'bbox' -> mapview(st_bbox(x)) is equivalent to viewExtent(x)
  * plainview now provides mouse coordinates

enhancements:

  * added testthat suite of functions.
  * increased performance when using "+"-method (about 10x faster now).

bugfixes:

  * manifold bug fixes - addressing issues with list layers, alpha channel and many more.

## mapview 2.1.4

new features: 

  * popupImage now accepts more than one file name (list or vector) in argument img.
  
bugfixes:

  * na.alpha caused list/burst methods to fail.
  * legend did not work when zcol only had one unique value. This was actually a delibarate choice as for a single map it doesn't really make sense to have a legend for only one color. However, for latticeView/sync this does actually make sense, hence re-enabled.

## mapview 2.1.0

new features: 

  * plainview/cubeview now respects/has gained argument na.color.
  * mapview now supports st_GEOMETRY with truely mixed feature types (e.g. LINESTRING and POLYGON) - fixes #85
  * addFeatures: one function to add them all. Type agnostic version of leaflet::add* functions for simple features objects.
  * mapview (for vector data only) has gained argument na.alpha to control opacity of missing values.

bugfixes:

  * deleted obsolete data.table import.
  * fixed #79: colnames of popupTables are now converted to utf-8.
  * fixed #78: respect explicit setting of 'layer.name' argument.
  * included NEWS file as R-help doesn't render NEWS.md
  * highlight now respects alpha/alpha.regions = 0

## mapview 2.0.1

new features:

  * addMouseCoordinates has gained argument 'style' to specify whether to show 'basic' (lat, lon, zoom) or 'detailed' (x, y, epsg, proj4, lat, lon, zoom) information. Factory-fresh default is 'detailed'.
  * addLogo has gained argument 'alpha' to set the opacity of the image.
  * Someone draws quickest...
  * added new method for list of objects so that we can do mapview(list(x, y, z)) which is great for computational outputs such as lapply.
  * slideView has gained arguments 'label1' and 'label2' to supply slider names for the respective images, img1 and img2.
  * new popup layout (making more use of the space available).
  * added new function addLargeFeatures to render large datasets of up to ~100k features which is used automatically. To lower/elevate the threshold use maxpoints = ... (See ?mapview for details).
  * mapview methods for all basic sf classes (XY/sfg, sfc, sf)
  * added support for sf to "+"
  * we can now render features/objects with arbitrary CRS (without map background) by setting 'native.crs = TRUE'.
  * mapview will now decide which default base map to use based on average luminence of rendered colors.
  * mapview now provides subtle highlighting of polygons (changing opacity slightly) and lines (changing thickness).
  * plainView, slideView and cubeView have gained argument legend. default is TRUE. Legends only available for non-RGB methods!
  * new data sets:
  
      - 'franconia' (administrative district boundaries of Franconia)
      - 'breweries' (extended version of the 'breweries91' data)
      - 'trails' (selected hiking trails in franconia to connect the breweries)
      
  * data sets 'breweries91', 'gadmCHE' and 'atlStorms2005' have been deleted and moved to leaflet.

bugfixes:

  * sync, addMouseCoordinates and addLogo did not work anymore. Now fixed thanks to @timelyportfolio

changes:

  * MAJOR internal change: All vector data are now processed as sf objects internally. This also means that objects returned in slot `@object` will be of class sf (regardless of input class).
  * polygons and points now have a darkish gray line frame (unless add*LargeFeatures is used - where the overhead of passing two sets of colors would be too high).
  * updated examples (in line with new data).
  * github repository moved from https://github.com/environmentalinformatics-marburg/mapview to https://github.com/r-spatial/mapview

## mapview 1.2.0

new features:

  * garnishMap: function to add multiple decoration elements, such as leaflet::addLayersControl or addHomeButton to a map (mainly for internal use).
  * addHomeButton: add a zoom-to-layer button to a map.
  * addLogo: add an image to a map.
  * plainview now shows CRS and dimension info.

bugfixes:

  * na.color was not respected for Raster* and SpatialPiXelsDF.
  * removed lat and lon entries in popupTable for polygons as we now have mousecoordinates.
  * for raster objects the legend did not respect the intervals specified by 'at'.
  * mapview working again for objects with no projection (NA).
  * mapview for SPoints* with only one point did through an error #36.

## mapview 1.1.0

new features:

  * addMouseCoordinates: add cursor position information to mapview or leaflet map. (thanks to Kent Russell).
  * if available from leaflet version, a scalebar is added to the map.
  * latticeView: view mapview or leaflet maps as small multiples and sync some, all or none (thanks to Kent Russell).
  * sync: synchronise two or more leaflet maps (thanks to Kenton Russell).
  * mapshot: to save maps as html page or static image or both.
  * knitr integration (i.e. no need to call the @map slot anymore to render in knitr).
  * cubeView: view raster bricks or stacks hovmoeller style, use keys up & down, left & right, page up & page down to navigate through y, x, z dimensions, respectively.
  * labels: if zcol is set, mouseover will now show the repesctive values of zcol, if zcol is not set moseover shows feature ID. Only available if suitable leaflet package version is installed.
  * new popup functions popupTable, popupGraph and popupImage.
  * functions to turn coordinates into spatial lines or spatial polygons.
  * mapview objects now work natively on shiny applications (i.e. renderMapview and mapviewOutput now available).
  * "zcol = Var" in combination with burst = TRUE now plots one layer for each unique value of the variable supplied to zcol.

changes:

  * spplot method has been removed.
  * colors: viridis based colors now default if viridisLite package is available.
  * basemaps: new default basemap is "CartoDB.Positron" as colors of features are better visible on the grey background.
  * layer names now include the name of the object they originate from (e.g. "meuse lead" instead of "lead").

bugfixes:

  * if attribute was of class "character" mapview did through an error if passed to zcol.
  * user provided layer names were not respected when zcol was set. See also note on changes in default layer names.


## mapview 1.0.0

* Initial release
