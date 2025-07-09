LeafletWidget.methods.addGeoJSONLayerSelector = function(layers,
                                                         layerId,
                                                         group,
                                                         position,
                                                         options) {

  var map = this;

  window.styleOpts = window.styleOpts || {};
  window.styleOpts[layerId] = options;

  updateLayerStyle = updateLayerStyler(map, layerId, styleOpts, group);
  let lyrscntrl = document.getElementsByClassName("leaflet-control-layers-overlays")[0]

  let innerhtml = '<label><strong>' +
    layerId +
    ': </strong></label><select name="' +
    layerId +
    '" id="layerSelector-' +
    layerId +
    '" onchange = "updateLayerStyle(this.name)" >';
  let txt = '<option> ---choose layer--- </option>';
  innerhtml = innerhtml + txt;
  for(var i = 0; i < layers.length; i++) {
    txt = '<option>' + layers[i] + '</option>';
    innerhtml = innerhtml + txt;
  }
  innerhtml = innerhtml + '</select>'
/*
  var selectr = L.control({position: position});
  selectr.onAdd = function (map) {
      var div = L.DomUtil.create('div', 'layerSelector');
      div.innerHTML = innerhtml;
      div.firstChild.onmousedown = div.firstChild.ondblclick = L.DomEvent.stopPropagation;
      return div;
  };
  selectr.addTo(map);
*/
  var mydiv = L.DomUtil.create('div', 'layerSelector');
  mydiv.innerHTML = innerhtml;
  mydiv.firstChild.onmousedown = mydiv.firstChild.ondblclick = L.DomEvent.stopPropagation;

  lyrscntrl.append(mydiv);

};

updateLayerStyler = function(map, layerId, options, group) {

  layerFunc = function(layerId) {

  var layer = map.layerManager.getLayer("geojson", layerId);

  //if (layer.getLayers()[0].feature.geometry.type === "Point") {
  if (layer.getLayers()[0] instanceof L.Marker) {
    var fg = L.featureGroup();
    map.eachLayer((layer)=>{
     if(layer instanceof L.Marker){ // || layer instanceof L.CircleMarker){
      fg.addLayer(layer);
     }
    });
    console.log(fg.toGeoJSON());

    lyr = L.geoJSON(fg.toGeoJSON(), {
      pointToLayer: function (feature, latlng) {
          return L.circleMarker(latlng, {});
      }
    });

    map.layerManager.removeLayer("geojson", layerId);
    map.layerManager.addLayer(lyr, "geojson", layerId, group); // FIX THIS (group)!
    layer = map.layerManager.getLayer("geojson", layerId);
  }

    var sel = document.getElementById("layerSelector-" + layerId);
    var colname = sel.options[sel.selectedIndex].text;
    console.log(layerId);

    var vals = [];
    layer_keys = Object.keys(layer._layers);
    for (var i = 0; i < layer_keys.length; i++) {
      vals[i] = layer._layers[layer_keys[i]].feature.properties[colname]
    }

    let colorFun = colFunc(vals, options[layerId][colname]);

    layer.eachLayer(function(layer) {
      console.log(layer.feature.properties[colname]);
      if (colname === "---choose layer---") {
        if (layer.feature.geometry.type === "Point") {
          layer.setStyle(layer.defaultOptions.__proto__);
        } else {
          layer.setStyle(layer.defaultOptions.style(layer.feature));
        }
      } else {
        layer.setStyle(
          {
            fillColor: colorFun(layer.feature.properties[colname]),
            fillOpacity: 0.9,
            color: colorFun(layer.feature.properties[colname]),
            opacity: 0.9
          }
        )
      }
    });
  };

  return layerFunc;

};

colFunc = function(values, options) {

  let col;

  if (options === undefined) {
    options = {};
    options.palette = null;
    options.breaks = null;
    options.domain = null;
    options.nacol = null;
  }

  if (typeof(values[0]) === 'number' | typeof(values[0]) === 'boolean') {
    // domain
    if (options.domain === null) {
      mn = Math.min(...values);
      mx = Math.max(...values);
      col = chroma.scale("YlOrRd").domain([mn, mx]);
    } else {
      col = chroma.scale("YlOrRd").domain(options.domain);
    }
  } else if (typeof(values[0]) === 'string' | typeof(values[0]) === 'object') {
    //var arr = ["c", "a", "b", "b"];
    let unique = [...new Set(values)];
    clrs = chroma.scale("Set1").colors(unique.length);
    var clrArr = Object.fromEntries(unique.map((key, index)=> [key, clrs[index]]));
    col = function(val) {
      return clrArr[val];
    }
  }
  return col;
}