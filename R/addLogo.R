library(mapview)
library(raster)

m <- leaflet() %>% addTiles()

img <- '"https://www.r-project.org/logo/Rlogo.svg"'
url <- '"https://www.r-project.org/logo/", target="_blank"'

m <- mapview(breweries91)

m1 <- htmlwidgets::onRender(
  m@map,
  paste0("
  function(el, x, data) {
  // we need a new div element because we have to handle
  // the mouseover output seperately
  debugger;
  function addElement () {
  // generate new div Element
  var newDiv = $(document.createElement('div'));
  // append at end of leaflet htmlwidget container
  $(el).append(newDiv);
  //provide ID and style
  newDiv.addClass('logo');
  newDiv.css({
  'position': 'absolute',
  'top': '13px',
  'left': '50px',
  'background-color': 'transparent',
  'border': '0px solid black',
  'width': '10px',
  'height': '10px',
  });

  return newDiv;
  }

  // check for already existing logo class to not duplicate
  var logo = $(el).find('.logo');
  if(!logo.length) {
  logo = addElement();

  // grab the special div we generated in the beginning
  // and put the mousmove output there
  logo.html('<a href=", url, "><img src=", img,
         ", width=50px, height=50px></a>');

  // get the leaflet map
  var map = HTMLWidgets.find('#' + el.id);

  };
  }
  "
  ))

m1
