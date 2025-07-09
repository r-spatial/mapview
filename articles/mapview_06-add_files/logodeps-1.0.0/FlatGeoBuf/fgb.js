function mouseHandler(mapId, layerId, group, eventName, extraInfo) {
  return function(e) {
    if (!HTMLWidgets.shinyMode) return;

    let latLng = e.target.getLatLng ? e.target.getLatLng() : e.latlng;
    if (latLng) {
      // retrieve only lat, lon values to remove prototype
      //   and extra parameters added by 3rd party modules
      // these objects are for json serialization, not javascript
      let latLngVal = L.latLng(latLng); // make sure it has consistent shape
      latLng = {lat: latLngVal.lat, lng: latLngVal.lng};
    }
    let eventInfo = $.extend(
      {
        id: (e.layer.feature.properties[layerId]?.toString() ?? layerId.toString()),
        ".nonce": Math.random()  // force reactivity
      },
      group !== null ? {group: group} : null,
      latLng,
      extraInfo
    );

    Shiny.onInputChange(mapId + "_" + eventName, eventInfo);
  };
}

LeafletWidget.methods.addFlatGeoBuf = function (layerId,
                                                group,
                                                url,
                                                popup,
                                                label,
                                                style,
                                                options,
                                                className,
                                                scale,
                                                scaleFields,
                                                highlightOptions,
                                                labelOptions) {

  const map = this;
  let gl = false;
  let pane;

  if (options === null || options.pane === undefined) {
    pane = 'overlayPane';
  } else {
    pane = options.pane;
  }

  let data_fl = document.getElementById(group + '-1-attachment');

  if (data_fl === null) {
    data_fl = url;
  } else {
    data_fl = data_fl.href;
  }

  let popUp;
  let colnames = [];

  function handleHeaderMeta(headerMeta) {
    headerMeta.columns.forEach(function(col) {
      colnames.push(col.name);
    });
  }

  function handleResponse(response) {
    // use fgb JavaScript API to iterate stream into results (features as geojson)
    // NOTE: would be more efficient with a special purpose Leaflet deserializer
    let it = flatgeobuf.deserialize(response.body, undefined, handleHeaderMeta);
    let cntr = 0;

    // handle result
    function handleResult(result) {
        if (!result.done) {
          if (gl) {
            map.layerManager.addLayer(
              L.glify.shapes({
                map: map,
                data: result.value,
                className: group
              }).glLayer, null, null, group);
            it.next().then(handleResult);
          } else {

            if (popup) {
              pop = makePopup(popup, className);
            } else {
              pop = null;
            }

            if (scaleFields === null & result.value.properties !== undefined) {
              let vls = Object.values(style);
              scaleFields = [];
              vls.forEach(function(name) {
                //if (name in colnames) {
                if (colnames.includes(name)) {
                  scaleFields.push(true);
                } else {
                  scaleFields.push(false);
                }
              });
            }

            lyr = L.geoJSON(result.value, Object.assign({
                pointToLayer: function (feature, latlng) {
                  return L.circleMarker(latlng, options);
                },
                style: function(feature) {
                  return updateStyle(style, feature, scale, scaleFields);
                },
                onEachFeature: pop,
                pane: pane
              },
              options)
            );

            if (label) {
              if (Object.keys(result.value.properties).includes(label)) {
                lyr.bindTooltip(function (layer) {
                  return layer.feature.properties[label].toString();
                }, labelOptions);
              } else if (typeof(label) === Object || (typeof(label) === 'object' && label.length > 1)) {
                let lb = label[cntr];
                lyr.bindTooltip(function (layer) {
                  return(lb);
                }, labelOptions);
              } else if (typeof(label) === "function") {
                lyr.bindTooltip(label, labelOptions);
              } else {
                lyr.bindTooltip(function (layer) {
                  return(label);
                }, labelOptions);
              }
            }

            if (highlightOptions && typeof highlightOptions === 'object' && Object.keys(highlightOptions).length > 0) {
              lyr.on({
                // highlight on hover
                'mouseover': function(e) {
                    const layer = e.target;
                    layer.setStyle(highlightOptions);
                    if (highlightOptions.bringToFront) {
                      layer.bringToFront();
                    }
                },
                // remove highlight when hover stops
                'mouseout': function(e) {
                    const layer = e.layer;
                    let oldstyle = updateStyleFromProperties(structuredClone(style), layer.feature.properties);
                    layer.setStyle(oldstyle);
                    if (highlightOptions.sendToBack) {
                      layer.bringToBack();
                    }
                }
              })
            }

            lyr.on("click", mouseHandler(map.id, layerId, group, "shape_click"));
            lyr.on("mouseover", mouseHandler(map.id, layerId, group, "shape_mouseover"));
            lyr.on("mouseout", mouseHandler(map.id, layerId, group, "shape_mouseout"));
            map.layerManager.addLayer(lyr, null, null, group);

            it.next().then(handleResult);
          }
        }

        cntr += 1;
    }
    it.next().then(handleResult);
  }

  fetch(data_fl) //, {mode: 'no-cors'})
    .then(handleResponse);

};

function makePopup(popup, className) {
  if (popup === true) {
    pop = function(feature, layer) {
      popUp = json2table(feature.properties, className);
      layer.bindPopup(popUp, { maxWidth: 2000 });
    };
  } else if (typeof(popup) === "string") {
    pop = function(feature, layer) {
      if (feature.properties !== undefined && popup in feature.properties) {
        popup = popup.split();
        popUp = json2table(
          pick(feature.properties, popup),
          className
        );
      } else {
        popUp = popup;
      }
      layer.bindPopup(popUp, { maxWidth: 2000 });
    };
  } else if (typeof(popup) === "object") {
    pop = function(feature, layer) {
      if (feature.properties.mvFeatureId !== undefined) {
        let idx = feature.properties.mvFeatureId;
        layer.bindPopup(popup[idx - 1], { maxWidth: 2000 });
      }
      if (feature.properties.mvFeatureId === undefined) {
        console.log("cannot bind popup to layer without id! Please file an issue at https://github.com/r-spatial/leafem/issues");
        layer.bindPopup("");
      }
    };
  } else if (typeof(popup) === "function") {
    pop = popup
  } else {
    pop = function(feature, layer) {
      popUp = json2table(
        pick(feature.properties, popup),
        className
      );
      layer.bindPopup(popUp, { maxWidth: 2000 });
    };
  }
  return pop;
}

function json2table(json, cls) {
  let cols = Object.keys(json);
  let vals = Object.values(json);
  let tab = "";
  for (let i = 0; i < cols.length; i++) {
    tab += "<tr><th>" + cols[i] + "&emsp;</th>" +
    "<td align='right'>" + vals[i] + "&emsp;</td></tr>";
  }
  return "<table class=" + cls + ">" + tab + "</table>";
}


/**
 * from https://gomakethings.com/how-to-create-a-new-object-with-only-a-subject-of-properties-using-vanilla-js/
 *
 *
 * Create a new object composed of properties picked from another object
 * (c) 2018 Chris Ferdinandi, MIT License, https://gomakethings.com
 * @param  {Object} obj   The object to pick properties from
 * @param  {Array}  props An array of properties to use
 * @return {Object}       The new object
 */
function pick(obj, props) {
	'use strict';
	// Make sure object and properties are provided
	if (!obj || !props) return;
	// Create new object
	let picked = {};
	// Loop through props and push to new object
	props.forEach(function(prop) {
		picked[prop] = obj[prop];
	});
	return picked;
}


function updateStyle(style_obj, feature, scale, scaleValues) {
  let cols = Object.keys(style_obj);
  let vals = Object.values(style_obj);

  let out = {};

  for (let i = 0; i < cols.length; i++) {
    if (vals[i] === null) {
      out[cols[i]] = feature.properties[cols[i]];
    } else {
      if (scaleValues !== undefined & scaleValues !== null) {
        //if (Object.keys(feature.properties).includes(vals[i])) {
        if (scaleValues[i] === true) {
          vals[i] = rescale(
            feature.properties[vals[i]]
            , scale[cols[i]].to[0]
            , scale[cols[i]].to[1]
            , scale[cols[i]].from[0]
            , scale[cols[i]].from[1]
          );
        }
      }
      out[cols[i]] = vals[i];
    }
  }

  return out;
}
function updateStyleFromProperties(style, props) {
  const keysToUpdate = ['stroke', 'color', 'weight', 'opacity',
                        'fill', 'fillColor', 'fillOpacity', 'dashArray'];
  // Create a shallow copy of style to avoid mutating the original object
  const updatedStyle = { ...style };
  keysToUpdate.forEach(key => {
    if (updatedStyle[key] === null && props[key]) {
      updatedStyle[key] = props[key];
    }
  });
  return updatedStyle
}


function rescale(value, to_min, to_max, from_min, from_max) {
  if (value === undefined) {
    value = from_min;
  }
  return (value - from_min) / (from_max - from_min) * (to_max - to_min) + to_min;
}




LeafletWidget.methods.addFlatGeoBufFiltered = function (layerId,
                                                        group,
                                                        url,
                                                        popup,
                                                        label,
                                                        style,
                                                        options,
                                                        className,
                                                        scale,
                                                        scaleFields,
                                                        minZoom,
                                                        maxZoom,
                                                        highlightOptions,
                                                        labelOptions) {

  const map = this;
  let gl = false;
  let pane;

  if (options === null || options.pane === undefined) {
    pane = 'overlayPane';
  } else {
    pane = options.pane;
  }

  let data_fl = document.getElementById(group + '-1-attachment');

  if (data_fl === null) {
    data_fl = url;
  } else {
    data_fl = data_fl.href;
  }

  let popUp;
  let colnames = [];

  function handleHeaderMeta(headerMeta) {
    //const header = document.getElementById('header')
    //const formatter = new JSONFormatter(headerMeta, 10)
    //header.appendChild(formatter.render())
    headerMeta.columns.forEach(function(col) {
      colnames.push(col.name);
    });
  }

  // convert the rect into the format flatgeobuf expects
  function fgBoundingBox() {
      const bounds = map.getBounds();
      return {
          minX: bounds.getWest(),
          maxX: bounds.getEast(),
          minY: bounds.getSouth(),
          maxY: bounds.getNorth(),
      };
  }

  // Initialize previousResults/nextResults if not already initialized
  var previousResults = previousResults || {};
  previousResults[group] = L.layerGroup();
  map.layerManager.addLayer(previousResults[group], null, layerId, group);

  // Async Update Function
  async function updateResults() {

    // remove the old results
    map.layerManager.removeLayer(previousResults[group], layerId);
    previousResults[group].remove()

    // add next results
    var nextResults = nextResults || {};
    nextResults[group] = L.layerGroup();
    map.layerManager.addLayer(nextResults[group], null, layerId, group);

    // Replace previous with next
    previousResults[group] = nextResults[group];

    // Use flatgeobuf JavaScript API to iterate features as geojson.
    // Because we specify a bounding box, flatgeobuf will only fetch the relevant subset of data,
    // rather than the entire file.
    if (map.getZoom() >= minZoom & map.getZoom() <= maxZoom & map.hasLayer(previousResults[group])) {

      let iter = flatgeobuf.deserialize(data_fl, fgBoundingBox(), handleHeaderMeta);

      for await (let feature of iter) {

        if (popup) {
          pop = makePopup(popup, className);
        } else {
          pop = null;
        }

        if (scaleFields === null & feature.properties !== undefined) {
          let vls = Object.values(style);
          scaleFields = [];
          vls.forEach(function(name) {
            //if (name in colnames) {
            if (colnames.includes(name)) {
              scaleFields.push(true);
            } else {
              scaleFields.push(false);
            }
          });
        }

        lyr = L.geoJSON(feature, {
          pointToLayer: function (feature, latlng) {
            return L.circleMarker(latlng, options);
          },
          style: function(feature) {
            return updateStyle(style, feature, scale, scaleFields);
          },
          onEachFeature: pop,
          pane: pane
        });

        if (label) {
          if (Object.keys(feature.properties).includes(label)) {
            lyr.bindTooltip(function (layer) {
              return layer.feature.properties[label].toString();
            }, labelOptions);
          } else if (typeof(label) === "function") {
            lyr.bindTooltip(label, labelOptions);
          } else {
            lyr.bindTooltip(function (layer) {
              return(label);
            }, labelOptions);
          }
        }

        if (highlightOptions && typeof highlightOptions === 'object' && Object.keys(highlightOptions).length > 0) {
          lyr.on({
            // highlight on hover
            'mouseover': function(e) {
                const layer = e.target;
                layer.setStyle(highlightOptions);
                if (highlightOptions.bringToFront) {
                  layer.bringToFront();
                }
            },
            // remove highlight when hover stops
            'mouseout': function(e) {
                const layer = e.target;
                layer.setStyle(style);
                if (highlightOptions.sendToBack) {
                  layer.bringToBack();
                }
            }
          })
        }

        lyr.on("click", mouseHandler(map.id, layerId, group, "shape_click"));
        lyr.on("mouseover", mouseHandler(map.id, layerId, group, "shape_mouseover"));
        lyr.on("mouseout", mouseHandler(map.id, layerId, group, "shape_mouseout"));
        lyr.addTo(nextResults[group]);
      }
    }
  }

  // show results based on the initial map
  updateResults();

  // update the results whenever the map moves or the layer is added again
  map.on("moveend", function(s) {
    updateResults();
  });
  map.on('layeradd', function(event) {
    if (event.layer == previousResults[group]) {
      updateResults();
    }
  });

};
