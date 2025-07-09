LeafletWidget.methods.addMBTiles = function(file,
                                         layerId,
                                         group
                                         ) {

  var map = this;
  debugger;
  var data_fl = document.getElementById(layerId + '-1-attachment');
  data_fl = data_fl.href;

  var mb = L.tileLayer.mbTiles(data_fl);

  map.layerManager.addLayer(mb, null, layerId, group);
  //map.fitBounds(layer.getBounds());
  return map;
};