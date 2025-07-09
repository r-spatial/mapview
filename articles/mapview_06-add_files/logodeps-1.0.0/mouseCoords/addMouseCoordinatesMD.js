function addMouseCoordinatesMD(map_id) {

  // get the leaflet map
  //var map = document.querySelector('.mapdeckmap');
  var map = window[map_id+'map']._map.getMap();
  console.log(map);
  // we need a new div element because we have to handle
  // the mouseover output separately
  function addElement () {
    // generate new div Element
    var newDiv = document.createElement('div');
    // append at end of leaflet htmlwidget container
    document.getElementById(map_id).append(newDiv);
    //provide ID and style
    newDiv.id = 'lnlt';
    newDiv.style.cssText = 'position: relative; background: rgba(255, 255, 255, 0.7); box-shadow: 0 0 2px #bbb; background-clip: paddg-box; margin: 0; padding-left: 5px; color: #333; font: 9px/1.5 Helvetica Neue, Arial, Helvetica, sans-serif; z-index: 700; height: 10px;';
    return newDiv;
  }

  // check for already existing lnlt class to not duplicate
  var lnlt = document.getElementById('.lnlt');

  if(lnlt === null) lnlt = addElement();

  map.on('mousemove', function(e) {
    document.getElementById('.lnlt').innerText =
    // e.point is the x, y coordinates of the mousemove event relative
    // to the top-left corner of the map
    JSON.stringify(e.point) +
    // '<br />' +
    // e.lngLat is the longitude, latitude geographical position of the event
    JSON.stringify(e.lngLat.wrap());
  });
}


