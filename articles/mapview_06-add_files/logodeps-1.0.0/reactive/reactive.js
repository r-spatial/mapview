LeafletWidget.methods.addReactiveLayer = function(x,
                                                  bindTo,
                                                  by,
                                                  on,
                                                  group,
                                                  layerId,
                                                  options,
                                                  style,
                                                  updateStyle,
                                                  popup) {

    var map = this;
    let out;
    if (on === "click") {
      out = "contextmenu";
    }
    if (on === "mouseover") {
      out = "mouseout";
    }

    var pop = [];

    if (typeof(popup) === "string") {
      //pop = new Array(x.features.length);
      for (var i = 0; i <= x.features.length; i++) {
        pop.push(popup);
      }
    }

    if (typeof(popup) === "object") {
      if (popup === null) {
        pop = null;
      } else if (popup.length == x.features.length) {
        pop = popup;
      }
    }

    var bindto_layer = map.layerManager._byGroup[bindTo];

    if (bindto_layer === undefined) {
        return;
    }

    var bindto_layer_key = Object.keys(bindto_layer);

    var bind_layer = L.geoJSON(x, {
      pointToLayer: function (feature, latlng) {
          return L.circleMarker(latlng, options);
      },
      style: style,
      interactive: true
    });

    if (pop !== null) {
      bind_layer.bindPopup(pop);
    }

    var okeys = Object.keys(bind_layer._layers);
    var nkeys = [...okeys];
    nkeys.forEach( (key, i, self) => self[i] = bind_layer._layers[key].feature.properties[by] );

    bindto_layer[bindto_layer_key]
    .on(on, function(e) {
      if (e.originalEvent.ctrlKey) {
        // console.log(e.layer.feature.properties[by]);
        var cur_by = e.layer.feature.properties[by];
        var ids = getAllIndexes(nkeys, cur_by);

        e.target.eachLayer(function (layer) {
          if(layer.feature.properties[[by]] == cur_by) {
            layer.setStyle(updateStyle);
          }
        });

        ids.forEach(function(i) {
          if (!map.hasLayer(bind_layer._layers[okeys[i]])) {
            if (bind_layer._popup === undefined) {
              map.addLayer(bind_layer._layers[okeys[i]]);
            } else {
            map.addLayer(bind_layer._layers[okeys[i]].bindPopup(bind_layer._popup._content[[i]]));
            }
          }
        });
      }
    })
    .on(out, function (e) {
      if (e.originalEvent.ctrlKey) {
        // console.log(e.layer.feature.properties[by]);
        var cur_by = e.layer.feature.properties[by];
        var ids = getAllIndexes(nkeys, cur_by);

        ids.forEach(function(i) {
          if (map.hasLayer(bind_layer._layers[okeys[i]])) {
            map.removeLayer(bind_layer._layers[okeys[i]]);
          }
        });

        e.layer.setStyle(e.layer.defaultOptions.style(e.layer.feature));

        //e.target.eachLayer(function (layer) {
          //layer.setStyle(layer.defaultOptions.style(layer.feature));
        //});

      }
    });

    bind_layer
    .on(out, function (e) {
      if (e.originalEvent.ctrlKey) {
        // console.log(e.layer.feature.properties[by]);
        var cur_by = e.layer.feature.properties[by];
        var ids = getAllIndexes(nkeys, cur_by);
        var bindto_layer = map.layerManager._byGroup[bindTo];
        var bindto_current = bindto_layer[Object.keys(bindto_layer)];
        var okeys_current = Object.keys(bindto_current._layers);
        var nkeys_current = [...okeys_current];
        nkeys_current.forEach( (key, i, self) => self[i] = bindto_current._layers[key].feature.properties[by] );
        var ids_current = getAllIndexes(nkeys_current, cur_by);
//debugger;
        ids.forEach(function(i) {
          if (map.hasLayer(bind_layer._layers[okeys[i]])) {
            map.removeLayer(bind_layer._layers[okeys[i]]);
          }
        });

        ids_current.forEach(function(i) {
          if (nkeys_current[i] == cur_by) {
            bindto_current._layers[okeys_current[i]].setStyle(bindto_current._layers[okeys_current[i]].defaultOptions.style(bindto_current._layers[okeys_current[i]].feature));
          }
        });
      }
    });
};


function getAllIndexes(arr, val) {
    var indexes = [], i;
    for(i = 0; i < arr.length; i++)
        if (arr[i] === val)
            indexes.push(i);
    return indexes;
}
