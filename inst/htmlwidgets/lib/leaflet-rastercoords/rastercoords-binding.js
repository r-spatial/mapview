LeafletWidget.methods.rastercoords = function (width, height, maxzoom, urlTemplate) {

  var map = this;

  var img = [
    width,  // original width of image
    height  // original height of image
  ];

  var rc = new L.RasterCoords(map, img);

  map.setMaxZoom(maxzoom);
  map.setView(rc.unproject([img[0], img[1]]), 2);

  L.tileLayer(urlTemplate, {
    noWrap: true
  }).addTo(map);
};
