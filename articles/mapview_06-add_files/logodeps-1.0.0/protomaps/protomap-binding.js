LeafletWidget.methods.addPMPolygons = function(
  url
  , layerId
  , group
  , style
  , pane
  , attribution
) {

  var map = this;

  let paint_rules = [{
    dataLayer: style.layer,
    symbolizer: new protomaps.PolygonSymbolizer({
      fill: style.fillColor,
      do_stroke: style.do_stroke,
      width: style.width,
      color: style.color
    })
  }];

  var layers = layers || {};

  layers[layerId] = protomaps.leafletLayer({
    url: url,
    // url: data_fl,
    paint_rules: paint_rules,
    label_rules: [],
    pane: pane,
    attribution: attribution
  });

  // debugger;

  map.layerManager.addLayer(layers[layerId], null, layerId, group);
  if (map.hasLayer(layers[layerId])) {
    map.on("click", ev => {
      for (let result of layers[layerId].queryFeatures(ev.latlng.lng,ev.latlng.lat)) {
        if (result[1][0] !== undefined) {
          var popup = L.popup()
          .setLatLng(ev.latlng)
          .setContent(json2table(result[1][0].feature.props))
          .openOn(map);
        }
      }
    });
  }
  return map;
};


LeafletWidget.methods.addPMPoints = function(
  url
  , layerId
  , group
  , style
  , pane
  , attribution
) {

  var map = this;

  let paint_rules = [{
    dataLayer: style.layer,
    symbolizer: new protomaps.CircleSymbolizer({
      fill: style.fillColor,
      stroke: style.stroke,
      width: style.width,
      radius: style.radius
    })
  }];

  var layers = layers || {};

  layers[layerId] = protomaps.leafletLayer({
    url: url,
    // url: data_fl,
    paint_rules: paint_rules,
    label_rules: [],
    pane: pane,
    attribution: attribution
  });

  // debugger;

  map.layerManager.addLayer(layers[layerId], null, layerId, group);
  if (map.hasLayer(layers[layerId])) {
    map.on("click", ev => {
      for (let result of layers[layerId].queryFeatures(ev.latlng.lng,ev.latlng.lat)) {
        if (result[1][0] !== undefined) {
          var popup = L.popup()
          .setLatLng(ev.latlng)
          .setContent(json2table(result[1][0].feature.props))
          .openOn(map);
        }
      }
    });
  }
  return map;
};


LeafletWidget.methods.addPMPolylines = function(
  url
  , layerId
  , group
  , style
  , pane
  , attribution
) {

  var map = this;

  let paint_rules = [{
    dataLayer: style.layer,
    symbolizer: new protomaps.LineSymbolizer({
      color: style.color,
      dash: style.dash,
      width: style.width,
      opacity: style.opacity
    })
  }];

  var layers = layers || {};

  layers[layerId] = protomaps.leafletLayer({
    url: url,
    // url: data_fl,
    paint_rules: paint_rules,
    label_rules: [],
    pane: pane,
    attribution: attribution
  });

  // debugger;

  map.layerManager.addLayer(layers[layerId], null, layerId, group);
  if (map.hasLayer(layers[layerId])) {
    map.on("click", ev => {
      for (let result of layers[layerId].queryFeatures(ev.latlng.lng,ev.latlng.lat)) {
        if (result[1][0] !== undefined) {
          var popup = L.popup()
          .setLatLng(ev.latlng)
          .setContent(json2table(result[1][0].feature.props))
          .openOn(map);
        }
      }
    });
  }
  return map;
};