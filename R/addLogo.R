## courtesy of
## http://gis.stackexchange.com/questions/203265/add-logo-to-a-map-using-leaflet-mapbox
## http://jsfiddle.net/3v7hd2vx/76/

addLogo <- function(map,
                    img,
                    url,
                    position = c("topleft", "topright",
                                 "bottomleft", "bottomright"),
                    offset.x = 50,
                    offset.y = 13,
                    width = 60,
                    height = 60) {
  # check for duplication?
  #  not sure of a good way to do this

  img <- paste0('"', img, '"')
  if (!missing(url)) url <- paste0('"', url, '"')

  position <- position[1]

  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

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
                 debugger;
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

  if (missing(url)) {
    div_html <- paste0("logo.html('<img src=", img,
                       ", width=", width, "height=", height, "></a>');
                       var map = HTMLWidgets.find('#' + el.id);
                       };
                       }")
  } else {
    div_html <- paste0("logo.html('<a href=", url, "><img src=", img,
                       ", width=", width, "height=", height, "></a>');
                       var map = HTMLWidgets.find('#' + el.id);
                       };
                       }")
  }

  render_stuff <- paste0(div_funk, div_add, div_html)

  map <- htmlwidgets::onRender(map, render_stuff)

  return(map)
}
