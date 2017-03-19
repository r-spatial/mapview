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
#' \dontrun{
#' leaflet() %>% addTiles() # without mouse position info
#' leaflet() %>% addTiles() %>% addMouseCoordinates(style = "basic") # with basic mouse position info
#' mapview(easter.egg = TRUE) # detailed mouse position info by default
#' }
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

    // grab the special div we generated in the beginning
    // and put the mousmove output there
    map.on('mousemove', function (e) {
      lnlt.text(", txt, ");
    })
  };
}
"
  )
)
  map
}

#
