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
#' \dontrun{
#' ## default position is topleft next to zoom control
#' library(mapview)
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
#' library(magick)
#'
#' m <- mapview(breweries91)
#'
#' addLogo(m, "https://jeroenooms.github.io/images/banana.gif",
#'         position = "bottomleft",
#'         offset.x = 5,
#'         offset.y = 40,
#'         width = 100,
#'         height = 100)
#'
#' }
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
