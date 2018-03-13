### addMouseCoordinates ######################################################
##############################################################################
#' Add mouse coordinate information at top of map.
#'
#' @description
#' This function adds a box displaying the current cursor location
#' (latitude, longitude and zoom level) at the top of a rendered
#' mapview or leaflet map. In case of mapview, this is automatically added.
#' NOTE: The information will only render once a mouse movement has happened
#' on the map.
#'
#' @param map a mapview or leaflet object.
#' @param style whether to show 'detailed' or 'basic' mouse position info.
#' See Details for an explanation.
#' @param epsg the epsg string to be shown.
#' @param proj4string the proj4string to be shown.
#' @param native.crs logical. whether to use the native crs in the coordinates box.
#'
#' @details
#' If style is set to "detailed", the following information will be displayed:
#' \itemize{
#'   \item x: x-position of the mouse cursor in projected coordinates
#'   \item y: y-position of the mouse cursor in projected coordinates
#'   \item epsg: the epsg code of the coordinate reference system of the map
#'   \item proj4: the proj4 definition of the coordinate reference system of the map
#'   \item lat: latitude position of the mouse cursor
#'   \item lon: longitude position of the mouse cursor
#'   \item zoom: the current zoom level
#' }
#'
#' If style is set to "basic", only 'lat', 'lon' and 'zoom' are shown.
#'
#' @examples
#' library(leaflet)
#'
#' leaflet() %>% addProviderTiles("OpenStreetMap") # without mouse position info
#' leaflet() %>%
#'   addProviderTiles("OpenStreetMap") %>%
#'   addMouseCoordinates(style = "basic") # with basic mouse position info
#' leaflet() %>%
#'   addProviderTiles("OpenStreetMap") %>%
#'   addMouseCoordinates() # with detailed mouse position info
#'
#'
#' @export addMouseCoordinates
#' @name addMouseCoordinates
#' @rdname addMouseCoordinates
#' @aliases addMouseCoordinates

addMouseCoordinates <- function(map, style = c("detailed", "basic"),
                                epsg = NULL, proj4string = NULL,
                                native.crs = FALSE) {

  style <- style[1]

  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

  if (style == "detailed" && !native.crs) {
    txt_detailed <- paste0("
                           ' x: ' + L.CRS.EPSG3857.project(e.latlng).x.toFixed(0) +
                           ' | y: ' + L.CRS.EPSG3857.project(e.latlng).y.toFixed(0) +
                           ' | epsg: 3857 ' +
                           ' | proj4: +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs ' +
                           ' | lon: ' + (e.latlng.lng).toFixed(5) +
                           ' | lat: ' + (e.latlng.lat).toFixed(5) +
                           ' | zoom: ' + map.getZoom() + ' '")
  } else {
    txt_detailed <- paste0("
                           ' x: ' + (e.latlng.lng).toFixed(5) +
                           ' | y: ' + (e.latlng.lat).toFixed(5) +
                           ' | epsg: ", epsg, " ' +
                           ' | proj4: ", proj4string, " ' +
                           ' | zoom: ' + map.getZoom() + ' '")
  }

  txt_basic <- paste0("
                      ' lon: ' + (e.latlng.lng).toFixed(5) +
                      ' | lat: ' + (e.latlng.lat).toFixed(5) +
                      ' | zoom: ' + map.getZoom() + ' '")

  txt <- switch(style,
                detailed = txt_detailed,
                basic = txt_basic)

  map <- htmlwidgets::onRender(
    map,
    paste0(
      "
      function(el, x, data) {

      // get the leaflet map
      var map = this; //HTMLWidgets.find('#' + el.id);

      // we need a new div element because we have to handle
      // the mouseover output separately
      // debugger;
      function addElement () {
      // generate new div Element
      var newDiv = $(document.createElement('div'));
      // append at end of leaflet htmlwidget container
      $(el).append(newDiv);
      //provide ID and style
      newDiv.addClass('lnlt');
      newDiv.css({
      'position': 'relative',
      'bottomleft':  '0px',
      'background-color': 'rgba(255, 255, 255, 0.7)',
      'box-shadow': '0 0 2px #bbb',
      'background-clip': 'padding-box',
      'margin': '0',
      'padding-left': '5px',
      'color': '#333',
      'font': '9px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',
      });
      return newDiv;
      }

      // check for already existing lnlt class to not duplicate
      var lnlt = $(el).find('.lnlt');

      if(!lnlt.length) {
      lnlt = addElement();
      //$(el).keypress(function (e) {
      //  if (e.which == 32 || event.keyCode == 32) {
      //    alert('space key is pressed');
      //  }
      //});
      // grab the special div we generated in the beginning
      // and put the mousmove output there
      map.on('mousemove', function (e) {
      lnlt.text(", txt, ");
      });
      };
      }
      "
  )
)
  map
}


removeMouseCoordinates = function(map) {
  if (inherits(map, "mapview")) map = mapview2leaflet(map)

  rc = map$jsHooks$render
  rc_lnlt = lapply(rc, grep, pattern = "lnlt")
  for (i in seq_along(map$jsHooks$render)) {
    map$jsHooks$render[[i]][rc_lnlt[[i]]] = NULL
  }

  return(map)
}

##############################################################################


### addHomeButton ############################################################
##############################################################################
#' Add a home button / zoom-to-layer button to a map.
#'
#' @description
#' This function adds a button to the map that enables zooming to a
#' provided \code{\link{extent}} / \code{\link{bbox}}.
#'
#' @param map a mapview or leaflet object.
#' @param ext the \code{\link{extent}} / \code{\link{bbox}} to zoom to.
#' @param layer.name the name of the layer to be zoomed to (or any character
#' string)
#' @param position the position of the button (one of 'topleft', 'topright',
#' 'bottomleft', 'bottomright'). Defaults to 'bottomright'.
#' @param add logical. Whether to add the button to the map (mainly for internal use).
#'
#' @examples
#' library(leaflet)
#' library(raster)
#'
#' m <- leaflet() %>%
#'   addProviderTiles("OpenStreetMap") %>%
#'   addCircleMarkers(data = breweries) %>%
#'   addHomeButton(extent(breweries), "breweries")
#' m
#'
#' ## remove the button
#' removeHomeButton(m)
#'
#'
#' @export addHomeButton
#' @name addHomeButton
#' @rdname addHomeButton
#' @aliases addHomeButton
addHomeButton <- function(map, ext, layer.name = "layer",
                          position = 'bottomright', add = TRUE) {
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

  # drop names in case extent of sf object
  ext@xmin = unname(ext@xmin)
  ext@xmax = unname(ext@xmax)
  ext@ymin = unname(ext@ymin)
  ext@ymax = unname(ext@ymax)

  hb <- try(getCallEntryFromMap(map, "addHomeButton"), silent = TRUE)
  if (!inherits(hb, "try-error") & length(hb) == 1) {
    ext_coords <- unlist(map$x$calls[[hb]][["args"]][1:4])
    ext_map <- raster::extent(ext_coords[1],
                              ext_coords[3],
                              ext_coords[2],
                              ext_coords[4])
    if (identical(ext, ext_map)) add = FALSE
  }

  if (add) {
    if (class(extent) == "matrix") ext <- raster::extent(ext)
    label <- paste("Zoom to", layer.name)

    txt <- paste('<strong>', layer.name, '</strong>')

    map$dependencies <- c(map$dependencies, leafletHomeButtonDependencies())
    leaflet::invokeMethod(map, leaflet::getMapData(map), 'addHomeButton',
                          ext@xmin, ext@ymin, ext@xmax, ext@ymax, label, txt,
                          position)
  }

  else map

}


#' Use removeHomeButton to remove home button
#'
#' @describeIn addHomeButton remove a homeButton from a map
#' @aliases removeHomeButton
#' @export removeHomeButton
removeHomeButton <- function(map) {
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))
  leaflet::invokeMethod(map, NULL, 'removeHomeButton')
}


addZoomFullButton = function(map, lst, position = "bottomleft") {
  bb = combineExtent(lst, sf = FALSE)
  names(bb) = NULL
  label = "Zoom to full extent"
  txt = "<strong>Zoom full</strong>"

  leaflet::invokeMethod(map, leaflet::getMapData(map), 'addHomeButton',
                        bb[1], bb[2], bb[3], bb[4], label, txt,
                        position)

}


leafletHomeButtonDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "HomeButton",
      '0.0.1',
      system.file("htmlwidgets/lib/HomeButton", package = "mapview"),
      script = c("home-button.js", 'easy-button-src.min.js'),
      stylesheet = 'home-button.css'
    ))
}

##############################################################################


### addLogo ##################################################################
##############################################################################
#' add a local or remote image (png, jpg, gif, bmp, ...) to a leaflet map
#'
#' @description
#' This function adds an image to a map. Both local and remote (web) image
#' sources are supported. Position on the map is completely controllable.
#'
#' @param map a mapview or leaflet object.
#' @param img the image to be added to the map.
#' @param alpha opacity of the added image.
#' @param src character specifying the source location ("local" for images from
#' the disk, "remote" for web image sources).
#' @param url an optional URL to be opened when clicking on the image
#' (e.g. company's homepage).
#' @param position one of "topleft", "topright", "bottomleft", "bottomright".
#' @param offset.x the offset in x direction from the chosen position (in pixels).
#' @param offset.y the offset in y direction from the chosen position (in pixels).
#' @param width width of the rendered image in pixels.
#' @param height height of the rendered image in pixels.
#'
#' @examples
#' library(leaflet)
#' ## default position is topleft next to zoom control
#'
#' img <- "https://www.r-project.org/logo/Rlogo.svg"
#' leaflet() %>% addTiles() %>% addLogo(img, url = "https://www.r-project.org/logo/")
#'
#' ## with local image
#' library(png)
#'
#' img <- system.file("img", "Rlogo.png", package="png")
#' leaflet() %>% addTiles() %>% addLogo(img, src = "local", alpha = 0.3)
#'
#' ## dancing banana gif :-)
#' m <- mapview(breweries91)
#'
#' addLogo(m, "https://jeroenooms.github.io/images/banana.gif",
#'         position = "bottomleft",
#'         offset.x = 5,
#'         offset.y = 40,
#'         width = 100,
#'         height = 100)
#'
#'
#' @export addLogo
#' @name addLogo
#' @rdname addLogo
#' @aliases addLogo

## courtesy of
## http://gis.stackexchange.com/questions/203265/add-logo-to-a-map-using-leaflet-mapbox
## http://jsfiddle.net/3v7hd2vx/76/

addLogo <- function(map,
                    img,
                    alpha = 1,
                    src = c("remote", "local"),
                    url,
                    position = c("topleft", "topright",
                                 "bottomleft", "bottomright"),
                    offset.x = 50,
                    offset.y = 13,
                    width = 60,
                    height = 60) {
  # check for duplication?
  #  not sure of a good way to do this
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

  if (!missing(url)) url <- paste0('"', url, '"')

  position <- position[1]
  src <- src[1]


  div_topleft <- paste0("newDiv.css({
                        'position': 'absolute',
                        'top': '", offset.y, "px',
                        'left': '", offset.x, "px',
                        'background-color': 'transparent',
                        'border': '0px solid black',
                        'width': '", width, "px',
                        'height': '", height, "px',
});")

  div_topright <- paste0("newDiv.css({
                         'position': 'absolute',
                         'top': '", offset.y, "px',
                         'right': '", offset.x, "px',
                         'background-color': 'transparent',
                         'border': '0px solid black',
                         'width': '", width, "px',
                         'height': '", height, "px',
                         });")

  div_bottomleft <- paste0("newDiv.css({
                           'position': 'absolute',
                           'bottom': '", offset.y, "px',
                           'left': '", offset.x, "px',
                           'background-color': 'transparent',
                           'border': '0px solid black',
                           'width': '", width, "px',
                           'height': '", height, "px',
                           });")

  div_bottomright <- paste0("newDiv.css({
                            'position': 'absolute',
                            'bottom': '", offset.y, "px',
                            'right': '", offset.x, "px',
                            'background-color': 'transparent',
                            'border': '0px solid black',
                            'width': '", width, "px',
                            'height': '", height, "px',
                            });")

  div <- switch(position,
                topleft = div_topleft,
                topright = div_topright,
                bottomleft = div_bottomleft,
                bottomright = div_bottomright)

  div_funk <- paste0("function(el, x, data) {
                     // we need a new div element because we have to handle
                     // the mouseover output seperately
                     // debugger;
                     function addElement () {
                     // generate new div Element
                     var newDiv = $(document.createElement('div'));
                     // append at end of leaflet htmlwidget container
                     $(el).append(newDiv);
                     //provide ID and style
                     newDiv.addClass('logo');\n",
                     div,
                     "return newDiv;
                     }")

  div_add <- paste0("// check for already existing logo class to not duplicate
                    var logo = $(el).find('.logo');
                    if(!logo.length) {
                    logo = addElement();")

  # if (missing(url)) {
  #   div_html <- paste0("logo.html('<img src=", img,
  #                      ", width=", width, "height=", height, "></a>');
  #                      var map = HTMLWidgets.find('#' + el.id).getMap();
  #                      };
  #                      }")
  # } else {
  #   div_html <- paste0("logo.html('<a href=", url, "><img src=", img,
  #                      ", width=", width, "height=", height, "></a>');
  #                      var map = HTMLWidgets.find('#' + el.id).getMap();
  #                      };
  #                      }")
  # }

  div_html <- switch(src,
                     remote = remoteImage(img, alpha, url, width, height),
                     local = localImage(img, alpha, url, width, height))

  render_stuff <- paste0(div_funk, div_add, div_html)

  map <- htmlwidgets::onRender(map, render_stuff)

  return(map)
                    }


### local image
localImage <- function(img, alpha, url, width, height) {
  nm <- basename(img)
  drs <- file.path(tempdir(), "graphs")
  if (!dir.exists(drs)) dir.create(drs)
  fls <- file.path(drs, nm)
  invisible(file.copy(img, file.path(drs, nm)))
  rel_path <- paste0('"', file.path("..", basename(drs), basename(img)), '"')

  style <- paste0(', style="opacity:',
                  alpha,
                  ';filter:alpha(opacity=',
                  alpha * 100, ');"')

  if (missing(url)) {
    div_html <- paste0("logo.html('<img src=", rel_path,
                       ", width=", width, ", height=", height, style,
                       ", ></a>');
                       var map = HTMLWidgets.find('#' + el.id).getMap();
  };
}")
  } else {
    div_html <- paste0("logo.html('<a href=", url, "><img src=", rel_path,
                       ", width=", width, ", height=", height, style,
                       "></a>');
                       var map = HTMLWidgets.find('#' + el.id).getMap();
};
}")
  }

  return(div_html)
}

### remote image
remoteImage <- function(img, alpha, url, width, height) {

  img <- paste0('"', img, '"')

  style <- paste0(', style="opacity:',
                  alpha,
                  ';filter:alpha(opacity=',
                  alpha * 100, ');"')

  if (missing(url)) {
    div_html <- paste0("logo.html('<img src=", img,
                       ", width=", width, ", height=", height, style,
                       "></a>');
                       var map = HTMLWidgets.find('#' + el.id).getMap();
                       };
                       }")
  } else {
    div_html <- paste0("logo.html('<a href=", url, "><img src=", img,
                       ", width=", width, ", height=", height, style,
                       "></a>');
                       var map = HTMLWidgets.find('#' + el.id).getMap();
                       };
                       }")
  }

  return(div_html)
}

##############################################################################


### addFeatures ##############################################################
##############################################################################
#' Type agnositc version of \code{leaflet::add*} functions.
#'
#' @description
#' Add simple features geometries from \code{\link[sf]{sf}}
#'
#' @param map A \code{leaflet} or \code{mapview} map.
#' @param data A \code{sf} object to be added to the \code{map}.
#' @param ... Further arguments passed to the respective \code{leaflet::add*}
#' functions. See \code{\link{addCircleMarkers}}, \code{\link{addPolylines}}
#' and \code{\link{addPolygons}}.
#'
#' @return
#' A leaflet \code{map} object.
#'
#' @examples
#' library(leaflet)
#'
#' leaflet() %>% addProviderTiles("OpenStreetMap") %>% addCircleMarkers(data = breweries)
#' leaflet() %>% addProviderTiles("OpenStreetMap") %>% addFeatures(data = breweries)
#'
#' leaflet() %>% addProviderTiles("OpenStreetMap") %>% addPolylines(data = atlStorms2005)
#' leaflet() %>% addProviderTiles("OpenStreetMap") %>% addFeatures(atlStorms2005)
#'
#' leaflet() %>% addProviderTiles("OpenStreetMap") %>% addPolygons(data = franconia)
#' leaflet() %>% addProviderTiles("OpenStreetMap") %>% addFeatures(franconia)
#'
#' @export addFeatures
#' @name addFeatures
#' @rdname addFeatures
addFeatures <- function(map,
                        data,
                        ...) {

  if (inherits(data, "Spatial")) data = sf::st_as_sf(data)

  switch(getSFClass(sf::st_geometry(data)),
         sfc_POINT           = addPointFeatures(map, data, ...),
         sfc_MULTIPOINT      = addPointFeatures(map, data, ...),
         sfc_LINESTRING      = addLineFeatures(map, data, ...),
         sfc_MULTILINESTRING = addLineFeatures(map, data, ...),
         sfc_POLYGON         = addPolygonFeatures(map, data, ...),
         sfc_MULTIPOLYGON    = addPolygonFeatures(map, data, ...),
         sfc_GEOMETRY        = addGeometry(map, data, ...),
         POINT               = addPointFeatures(map, data, ...),
         MULTIPOINT          = addPointFeatures(map, data, ...),
         LINESTRING          = addLineFeatures(map, data, ...),
         MULTILINESTRING     = addLineFeatures(map, data, ...),
         POLYGON             = addPolygonFeatures(map, data, ...),
         MULTIPOLYGON        = addPolygonFeatures(map, data, ...),
         GEOMETRY            = addGeometry(map, data, ...))

}




### these functions call the appropriate leaflet::add* functions
### depending on geometry type. Additional parameters can be passed via ...

mw = 800

### Point Features
addPointFeatures <- function(map,
                             data,
                             ...) {
  garnishMap(map, leaflet::addCircleMarkers,
             data = sf::st_zm(sf::st_cast(data, "POINT")),
             popupOptions = popupOptions(maxWidth = mw,
                                         closeOnClick = TRUE),
             ...)
}

### Line Features
addLineFeatures <- function(map,
                            data,
                            ...) {
  garnishMap(map, leaflet::addPolylines,
             data = sf::st_zm(data),
             popupOptions = popupOptions(maxWidth = mw,
                                         closeOnClick = TRUE),
             ...)
}

### PolygonFeatures
addPolygonFeatures <- function(map,
                               data,
                               ...) {
  garnishMap(map, leaflet::addPolygons,
             data = sf::st_zm(data),
             popupOptions = popupOptions(maxWidth = mw,
                                         closeOnClick = TRUE),
             ...)
}

### GeometryCollections
addGeometry = function(map,
                       data,
                       ...) {
  ls = list(...)
  if (!is.null(ls$label))
    label = split(ls$label, f = as.character(sf::st_dimension(data)))
  if (!is.null(ls$popup))
    popup = split(ls$popup, f = as.character(sf::st_dimension(data)))
  lst = split(data, f = as.character(sf::st_dimension(data)))
  for (i in 1:length(lst)) {
    ls$map = map
    ls$data = sf::st_cast(lst[[i]])
    if (!is.null(ls$label)) ls$label = label[[i]]
    if (!is.null(ls$popup)) ls$popup = popup[[i]]
    map = do.call(addFeatures, ls)
    # addFeatures(map,
    #                 data = sf::st_cast(lst[[i]]),
    #                 group = ls$group,
    #                 radius = ls$radius,
    #                 weight = ls$weight,
    #                 opacity = ls$opacity,
    #                 fillOpacity = ls$fillOpacity,
    #                 color = ls$color,
    #                 fillColor = ls$fillColor,
    #                 popup = ls$popup[[i]],
    #                 label = ls$label[[i]])
  }
  return(map)
}

##############################################################################


### addImageQuery ############################################################
##############################################################################
#' Add image query functionality to leaflet/mapview map.
#'
#' @details
#' This function enables Raster* objects added to leaflet/mapview maps to be
#' queried. Standard query is on 'mousmove', but can be changed to 'click'.
#' Note that for this to work, the \code{layerId} needs to be the same as the
#' one that was set in \code{\link[leaflet]{addRasterImage}}. Currently only works for
#' numeric values (i.e. numeric/integer and factor values are supported).
#'
#' @param map the map with the RasterLayer to be queried.
#' @param x the RasterLayer that is to be queried.
#' @param group the group of the RasterLayer to be queried.
#' @param layerId the layerId of the RasterLayer to be queried. Needs to be the
#'   same a supplied in \code{\link[leaflet]{addRasterImage}}.
#' @param project whether to project the RasterLayer to conform with leaflets
#'   expected crs. Defaults to \code{TRUE} and things are likely to go haywire
#'   if set to \code{FALSE}.
#' @param type whether query should occur on 'mousemove' or 'click'. Defaults
#'   to 'mousemove'.
#' @param digits the number of digits to be shown in the display field.
#' @param position where to place the display field. Default is 'topright'.
#' @param prefix a character string to be shown as prefix for the layerId.
#' @param ... currently not used.
#'
#' @return
#' A leaflet map object.
#'
#' @examples
#' library(leaflet)
#' library(mapview)
#'
#' leaflet() %>%
#'   addProviderTiles("OpenStreetMap") %>%
#'   addRasterImage(poppendorf[[1]], project = TRUE, group = "poppendorf",
#'                  layerId = "poppendorf") %>%
#'   addImageQuery(poppendorf[[1]], project = TRUE,
#'                 layerId = "poppendorf") %>%
#'   addLayersControl(overlayGroups = "poppendorf")
#'
#'
#' @export addImageQuery
#' @name addImageQuery
#' @rdname addImageQuery
addImageQuery = function(map,
                         x,
                         group = NULL,
                         layerId = NULL,
                         project = TRUE,
                         type = c("mousemove", "click"),
                         digits,
                         position = 'topright',
                         prefix = 'Layer',
                         ...) {

  if (inherits(map, "mapview")) map = mapview2leaflet(map)

  type = match.arg(type)
  if (missing(digits)) digits = "null"
  if (is.null(group)) group = "stars"
  if (is.null(layerId)) layerId = group

  jsgroup <- gsub(".", "", make.names(group), fixed = TRUE)

  tmp <- makepathStars(as.character(jsgroup))
  pathDatFn <- tmp[[2]][1]
  starspathDatFn <- tmp[[3]][1]
  datFn <- tmp[[4]][1]

  if (project) {
    if (inherits(x, "stars")) projected <- st_transform(x, crs = 4326)
    if (inherits(x, "Raster")) projected <- raster::projectRaster(
      x,
      raster::projectExtent(x, crs = sp::CRS(llcrs)),
      method = "ngb"
    )
  } else {
    projected <- x
  }

  pre <- paste0('var data = data || {}; data["', layerId, '"] = ')
  writeLines(pre, pathDatFn)
  cat('[', image2Array(projected), '];',
      file = pathDatFn, sep = "", append = TRUE)

  ## check for existing layerpicker control
  ctrlid = getCallEntryFromMap(map, "addControl")
  imctrl = unlist(sapply(ctrlid, function(i) {
    "imageValues" %in% map$x$calls[[i]]$args
  }))
  ctrlid = ctrlid[imctrl]

  if (length(ctrlid) == 0) {
    # must add empty character instead of NULL for html with addControl
    map = addControl(map, html = "", layerId = 'imageValues', position = position)
  }

  map$dependencies <- c(map$dependencies,
                        starsDataDependency(jFn = pathDatFn,
                                            counter = 1,
                                            group = jsgroup))
  map$dependencies = c(map$dependencies,
                       list(htmltools::htmlDependency(
                         version = "0.0.1",
                         name = "joda",
                         src = system.file("htmlwidgets/lib/joda",
                                           package = "mapview"),
                         script = "joda.js")
                       ))

  map = htmlwidgets::onRender(
    map,
    htmlwidgets::JS(
      paste0(
        'function(el, x, data) {
        var map = this;
        map.on("', type, '", function (e) {
          rasterPicker.pick(e, x, ', digits, ', "', prefix, ' ");
        });
      }'
      )
    )
  )

  return(map)
}

##############################################################################



### addStaticLabels ##########################################################
##############################################################################
#' Add static labels to \code{leaflet} or \code{mapview} objects
#'
#' @description
#' Being a wrapper around \code{\link[leaflet]{addLabelOnlyMarkers}}, this
#' function provides a smart-and-easy solution to add custom text labels to an
#' existing \code{leaflet} or \code{mapview} map object.
#'
#' @param map A \code{leaflet} or \code{mapview} object.
#' @param data A \code{sf} or \code{Spatial*} object used for label placement,
#' defaults to the locations of the first dataset in 'map'.
#' @param label The labels to be placed at the positions indicated by 'data' as
#' \code{character}, or any vector that can be coerced to this type.
#' @param ... Additional arguments passed to
#' \code{\link[leaflet]{addLabelOnlyMarkers}}.
#'
#' @return
#' A labelled \strong{mapview} object.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link[leaflet]{addLabelOnlyMarkers}}.
#'
#' @examples
#' \dontrun{
#' ## leaflet label display options
#' library(leaflet)
#'
#' lopt = labelOptions(noHide = TRUE
#'                     , direction = 'top'
#'                     , textOnly = TRUE)
#'
#' ## point labels
#' m1 = mapview(breweries)
#' l1 = addStaticLabels(m1
#'                      , label = breweries$number.of.types
#'                      , labelOptions = lopt)
#'
#' ## polygon centroid labels
#' m2 = mapview(franconia)
#' l2 = addStaticLabels(m2
#'                      , label = franconia$NAME_ASCI
#'                      , labelOptions = lopt)
#'
#' ## custom labels
#' m3 = m1 + m2
#' l3 = addStaticLabels(m3
#'                      , data = franconia
#'                      , label = franconia$NAME_ASCI
#'                      , labelOptions = lopt)
#' }
#'
#' @export addStaticLabels
#' @name addStaticLabels
addStaticLabels = function(
  map
  , data
  , label
  , group = NULL
  , layerId = NULL
  , ...
) {

  if (inherits(map, "mapview") & missing(data)) {
    data = map@object[[1]]
    if (is.null(group)) {
      group = mapview:::getLayerNamesFromMap(map@map)[1]
    } else {
      group = NULL
    }
  }

  dots = list(...)
  min_opts = list(noHide = TRUE,
                  direction = "top",
                  textOnly = TRUE)

  dots = append(dots, min_opts)

  if (inherits(map, "mapview")) map = mapview2leaflet(map)

  ## 'Raster*' locations not supported so far -> error
  if (inherits(data, "Raster")) {
    stop(paste("'Raster*' input is not supported, yet."
               , "Please refer to ?addLabels for compatible input formats.\n"),
         call. = FALSE)
  }

  ## if input is 'Spatial*', convert to 'sf'
  if (inherits(data, "Spatial")) {
    data = sf::st_as_sf(data)
  }

  if (missing(label)) {
    sf_col = attr(data, "sf_column")
    if (inherits(data, "sf")) {
      if (ncol(data) == 2) {
        colnm = setdiff(colnames(data), sf_col)
        label = data[[colnm]]
      } else {
        label = seq(nrow(data))
      }
    } else {
      label = seq(length(data))
    }
  }

  if (getGeometryType(data) == "ln") {
    crds = as.data.frame(sf::st_coordinates(data))
    crds_lst = split(crds, crds[[ncol(crds)]])
    mat = do.call(rbind, lapply(seq(crds_lst), function(i) {
      crds_lst[[i]][sapply(crds_lst, nrow)[i], c("X", "Y")]
    }))
  } else {
    mat = sf::st_coordinates(suppressWarnings(sf::st_centroid(data)))
  }

  ## add labels to map
  map = garnishMap(leaflet::addLabelOnlyMarkers,
                   map = map,
                   lng = mat[, 1],
                   lat = mat[, 2],
                   label = as.character(label),
                   group = group,
                   layerId = layerId,
                   labelOptions = dots)
  # map = leaflet::addLabelOnlyMarkers(map,
  #                                    lng = mat[, 1],
  #                                    lat = mat[, 2],
  #                                    label = as.character(label),
  #                                    ...)

  return(map)
}


##############################################################################
