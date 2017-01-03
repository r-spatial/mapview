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
#'
#' @examples
#' \dontrun{
#' leaflet() %>% addTiles() # without mouse position info
#' leaflet() %>% addTiles() %>% addMouseCoordinates() # with mouse position info
#' mapview(easter.egg = TRUE) # mouse position info by default
#' }
#'
#'
#' @export addMouseCoordinates
#' @name addMouseCoordinates
#' @rdname addMouseCoordinates
#' @aliases addMouseCoordinates

addMouseCoordinates <- function(map) {
  # check for duplication?
  #  not sure of a good way to do this

  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

  map <- htmlwidgets::onRender(
    map,
"
function(el, x, data) {
  // we need a new div element because we have to handle
  // the mouseover output separately
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
      'color': '#333',
      'font': '9px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',
    });
    return newDiv;
  }

  // check for already existing lnlt class to not duplicate
  var lnlt = $(el).find('.lnlt');
  if(!lnlt.length) {
    lnlt = addElement();
    // get the leaflet map
    var map = HTMLWidgets.find('#' + el.id).getMap();

    // grab the special div we generated in the beginning
    // and put the mousmove output there
    map.on('mousemove', function (e) {
      lnlt.text(' Latitude: ' + (e.latlng.lat).toFixed(5) +
      ' | Longitude: ' + (e.latlng.lng).toFixed(5) +
      ' | Zoom: ' + map.getZoom() + ' '
      );
    })
  };
}
"
  )
  map
}

