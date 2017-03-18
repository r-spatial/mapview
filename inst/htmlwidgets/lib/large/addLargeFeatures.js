LeafletWidget.methods.addLargeFeatures = function(x) {

    //#########################################################

    var map = this;
    // var data = eval(x.group);

    addCanvas();

    // define the first layer of the list to be the default one
    //var defaultLayer = L.tileLayer.provider(x.layer[0]).addTo(map);
    //var baseLayers = {};
    //for (var i = 0; i < x.layer.length;  i++) {
    //baseLayers[x.layer[i] ] = L.tileLayer.provider(x.layer[i]);
    //}

    // define a dummy layer for the geojson data
    //var myLayer = L.geoJson(undefined,{style:style,onEachFeature:onEachFeature}).addTo(map);

    if (eval(x.group).features[0].geometry.type == "Point") {
        var myLayer = L.geoJson(undefined, {
                pointToLayer: pointToLayer,
                style: style,
                onEachFeature: onEachFeature
            })
            .addTo(map);
    } else {
        var myLayer = L.geoJson(undefined, {
                style: style,
                onEachFeature: onEachFeature
            })
            .addTo(map);
    }
    var southWest = L.latLng([x.ymin, x.xmin]),
        northEast = L.latLng([x.ymax, x.xmax]),
        bounds = L.latLngBounds(southWest, northEast);
    map.fitBounds(bounds);
    var lzoom = map.getZoom();
    // create a pseudo layer for applying fitBounds
    //var mincorner = L.marker([x.ymin, x.xmin]);
    //var maxcorner = L.marker([x.ymax, x.xmax]);
    //var group = new L.featureGroup([maxcorner, mincorner]);
    //map.fitBounds(group.getBounds());

    //legend.addToMap( pal = x.color, values = x.values.name, opacity = legend.opacity)

    // var data = x[2];
    // var loc = HTMLWidgets.getAttachmentUrl('data', 'jsondata');
    // var data = $.parseJSON(HTMLWidgets.getAttachmentUrl('data', 'jsondata'));
    // check if an array of colors (palette) or a single color is provided
    if (x.color.length <= 7) {
        if (x.color[1].substring(0, 1) != "#") {
            var col = x.color;
        }
    } else {
        var col = eval(x.group).features[0].properties.color;
    }
    // second (first is on R side) coarse spatial zoom adaption
    // if there is a

    //var zoom = lzoom;


    var color = col;
    var opacity = x.opacity;
    //var globalAlpha = x.alpharegions;
    var canvasAlpha = x.canvasOpacity;
    var lnWidth = x.weight;
    var rad = x.cex;
    var tileOptions = {
        maxZoom: 21,  // max zoom to preserve detail on
        maxPoints: 10, // stop slicing each tile below this number of points
        tolerance: 1, // simplification tolerance (higher means simpler)
        extent: 4096, // tile extent (both width and height)
        buffer: 128, // tile buffer on each sidey
        debug: 0, // logging level (0 to disable, 1 or 2)
        indexMaxZoom: 0, // max zoom in the initial tile index
        indexMaxPoints: 1000000, // max number of points per tile in the index
    };
    // construct the rtree object
    var rt = RTree();

    // make leaflet circleobjects
    function pointToLayer(feature, latlng) {
        return L.circleMarker(latlng);
    }

    // The onEachFeature function provides functionality when oneEchfeature is activated
    function onEachFeature(feature, layer) {
      Object.size = function(obj) {
        var size = 0, key;
        for (key in obj) {
          if (obj.hasOwnProperty(key)) size++;
        }
      return size;
      };

        var len = Object.size(feature.properties)-1;
        var i = 1;
        var content = '';
        // does this feature have a property named popupContent?
        if (feature.properties) {
            for (var key in feature.properties) {
              if (key !== "color") {
                if (i === 1) {
                  content += "<tr class='coord'><td>" + "<b>" + key + "<b>" + "</td><td>" + feature.properties[key] + "</td></tr>";
                } else if (!isEven(i)) {
                  content += "<tr><td>" + "<b>" + key + "<b>" + "</td><td>" + feature.properties[key] + "</td></tr>";
                } else if (isEven(i)) {
                  content += "<tr class='alt'><td>" + "<b>" + key + "<b>" + "</td><td>" + feature.properties[key] + "</td></tr>";
                }
                i = i + 1;
              }
            }
            var popupContent = x.html + content + "</table></body></html>";
            //console.log(popupContent);
            layer.bindPopup(popupContent);
        }
    }

    // The styles of the geojson layer
    // it generates specific styles for points lines and polygons
    function style(feature) {

        // polygon style
        if (feature.geometry.type == "MultiPolygon" || feature.geometry.type == "Polygon") {
            return {
                fillColor: feature.properties.color,
                color: feature.properties.color,
                weight: x.weight,
                opacity: x.opacity,
                fillOpacity: x.alpharegions
            };
        }

        // line style
        if (feature.geometry.type == "MultiLineString" || feature.geometry.type == "LineString") {
            return {
                color: feature.properties.color,
                weight: x.weight,
                opacity: x.opacity,
            };

        }
        // point style
        if (feature.geometry.type == "MultiPoint" || feature.geometry.type == "Point") {
            return {
                radius: x.cex,
                fillColor: feature.properties.color,
                color: feature.properties.color,
                weight: x.weight,
                opacity: opacity,
                fillOpacity: x.alpharegions
            };
        }
    }


    /*//  function to filter the layer, if not delete the function here and on the var myLayer
    	function filter(feature, layer) {
    	    if (theFilter != "none") {
    	        layerFilter = feature.height;
    	        if (layerFilter == theFilter) {
    	            return true;
    	        } else {
    	            return false;
    	        }
    	    } else {
    	        return true;
    	    }
    	}rt.bbox([
                [bounds.getSouthWest()
                    .lng, bounds.getSouthWest()
                    .lat
                ],
                [bounds.getNorthEast()
                    .lng, bounds.getNorthEast()
                    .lat
                ]
            ])
    */
function countProperties(obj) {
    var count = 0;

    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            ++count;
    }

    return count;
}
    // function for retrieving the correct box according to the rtree object
    var BoxSelect = L.Map.BoxZoom.extend({
        _onMouseUp: function(e) {
            this._pane.removeChild(this._box);
            this._container.style.cursor = '';
            L.DomUtil.enableTextSelection();
            L.DomEvent
                .off(document, 'mousemove', this._onMouseMove)
                .off(document, 'mouseup', this._onMouseUp);
            var map = this._map,
                layerPoint = map.mouseEventToLayerPoint(e);
            if (this._startLayerPoint.equals(layerPoint)) {
                return;
            }
            var bounds = new L.LatLngBounds(
                map.layerPointToLatLng(this._startLayerPoint),
                map.layerPointToLatLng(layerPoint));
            map.fire("boxselectend", {
                boxSelectBounds: [
                    [bounds.getSouthWest()
                        .lng, bounds.getSouthWest()
                        .lat
                    ],
                    [bounds.getNorthEast()
                        .lng, bounds.getNorthEast()
                        .lat
                    ]
                ]
            });
        }
    });

    //construct boxselect object
    var boxSelect = new BoxSelect(map);
    //add it
    boxSelect.enable();

    map.on("boxselected", function(e) {
      var noF = rt.bbox(e.boxSelectBounds).length;
        // Define here number of features as tipping point for rtree
        if (noF <= x.maxFeatures) {
            //if (layerType == "vectortiles") {
                map.removeLayer(canvasTiles);
                layerType = "geojson";
            //}
            myLayer.clearLayers();
            rt.bbox([
                [bounds.getSouthWest()
                    .lng, bounds.getSouthWest()
                    .lat
                ],
                [bounds.getNorthEast()
                    .lng, bounds.getNorthEast()
                    .lat
                ]
            ]);
            myLayer.addData(rt.bbox(e.boxSelectBounds));
        } else {
            myLayer.clearLayers();
            canvasTiles.addTo(map);
            layerType = "vectortiles";
        }
    });

    // recursive call of layerswitch
    function showLayer() {
      if (map.hasLayer(staticLayer) || map.hasLayer(myLayer)) {
       // get number of features in current bounds
       var bounds = map.getBounds();
       var noF = rt.bbox([
                [bounds.getSouthWest()
                    .lng, bounds.getSouthWest()
                    .lat
                ],
                [bounds.getNorthEast()
                    .lng, bounds.getNorthEast()
                    .lat
                ]
            ]).length;

        if (noF <= x.maxFeatures) {
            //if (layerType == "vectortiles") {
            map.removeLayer(canvasTiles);
            layerType = "geojson";
            //}
            //layerType.clearLayers();
            myLayer.clearLayers();
            var bounds = map.getBounds();
            myLayer.addData(rt.bbox([
                [bounds.getSouthWest()
                    .lng, bounds.getSouthWest()
                    .lat
                ],
                [bounds.getNorthEast()
                    .lng, bounds.getNorthEast()
                    .lat
                ]
            ])).addTo(map);
        } else {
            myLayer.clearLayers();
            canvasTiles.addTo(map);
            layerType = "vectortiles";
        }
      }
    }
    map.on("moveend", function(e) {
        showLayer();
    });
    map.on("load", function(e) {
        showLayer();
    });
    // Add to the r-tree
    rt.geoJSON(eval(x.group));

    // Add to the GeoJson Vector Tiles
    var tileIndex = geojsonvt(eval(x.group), tileOptions);
    var canvasTiles = L.tileLayer.canvas();


    // Draw the canvas tiles
    canvasTiles.drawTile = function(canvas, tilePoint, zoom) {
        var ctx = canvas.getContext('2d');
        extent = 4096;
        padding = 0;
        totalExtent = extent * (1 + padding * 2);
        height = canvas.height = canvas.width = 256;
        ratio = height / totalExtent;
        pad = extent * padding * ratio;

        var x = tilePoint.x;
        var y = tilePoint.y;
        var z = zoom;
        var tile = tileIndex.getTile(z, x, y);
        if (typeof tile != "undefined") {
            var features = tile.features;
            var color = features[0].tags.color;

            // color to the lines
            // create gradients
            //var grdp = ctx.createRadialGradient(75,50,5,90,60,100);
            //var grd = ctx.createLinearGradient(0, 0, 170, 0);
            // define opacity
            ctx.globalAlpha = canvasAlpha;
            // apply gradient to colors
            //grd.addColorStop(0, color[0]);
            //grd.addColorStop(1, color[color.length-1]);
            //define line width
            ctx.lineWidth = lnWidth;
            // define line color
            ctx.strokeStyle = color;
            //define fill color
            ctx.fillStyle = color;

            for (var i = 0; i < features.length; i++) {
                var feature = features[i],
                    typeChanged = type !== feature.type,
                    type = feature.type;

                ctx.beginPath();
                // points
                if (type === 1) {
                    /*ctx.globalAlpha=0.7 ;
                    ctx.lineWidth = 0.0;
                    ctx.strokeStyle = "black";

                    */

                    for (var j = 0; j < feature.geometry.length; j++) {
                        var ring = feature.geometry[j];
                        ctx.arc(ring[0] * ratio + pad,
                                ring[1] * ratio + pad,
                                rad - 1 / zoom * 10,
                                0,
                                2 * Math.PI);
                    }
                    ctx.fillStyle = feature.tags.color;
                    ctx.strokeStyle = feature.tags.color;
                    ctx.fill();
                }
                //
                // lines
                /* if (feature.tags.ELEV != "") {
                  if (feature.tags.ELEV == "1000") {
                    ctx.strokeStyle = grd;
                  } else if (feature.tags.ELEV == "2000") {
                    ctx.strokeStyle = "#00FF00";
                  } else {
                    ctx.globalAlpha=0.3 ;
                    ctx.strokeStyle = "brown";
                  }
                } else {
	                   ctx.strokeStyle = "black";
	              }
             */
                for (var j = 0; j < feature.geometry.length; j++) {
                    var ring = feature.geometry[j];
                    ctx.strokeStyle = feature.tags.color;

                    for (var k = 0; k < ring.length; k++) {
                        var p = ring[k];

                        if (k) ctx.lineTo(p[0] * ratio + pad, p[1] * ratio + pad);
                        else ctx.moveTo(p[0] * ratio + pad, p[1] * ratio + pad);
                    }
                }
                // polygons
                if (type === 3) {
                    ctx.fillStyle = feature.tags.color;
                    ctx.strokeStyle = feature.tags.color;
                    ctx.fill();

                }
                ctx.stroke();


            }
        }
    };

    // create overlay Layers variables
    //var overlayLayers = {};
    //overlayLayers[x.layername] = myLayer;
    //overlayLayers[x.layername + "_static"] = (canvasTiles).addTo(map);

    staticLayer = (canvasTiles).addTo(map);

    map.layerManager.addLayer(myLayer, null, null, x.layername);
    map.layerManager.addLayer(staticLayer, null, null, x.layername);

    //overlayLayers.addTo(tst)

    // ADD LAYER CONTRLS
    //var layerControl = L.control.layers(null, overlayLayers, {collapsed: true}).addTo(tst);
    //map.setView([x.centerLat[0], x.centerLon[0], x.zoom[0]]);

    showLayer();

};

// this function is actually not needed I think!!
function addCanvas() {
    var newMeta = document.createElement("meta");
    document.head.insertBefore(newMeta, null);
    newMeta.name = "viewport";
    newMeta.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no";
}


function isEven(n) {
    return n % 2 == 0;
}

/*
,



resize: function(el, width, height, instance) {
}
});

//  we need to create a new meta tag


// we need a new div element because we have to handle
// the mouseover output seperatly
    function addElement () {
      // generate new div Element
      var newDiv = document.createElement("div");
      // insert to DOM
      document.body.insertBefore(newDiv, null);
      //provide ID and style
      newDiv.id = 'lnlt';
      newDiv.style.cssText = 'position: relative;' +
                              'bottomleft:  0px;' +
                              'background-color: rgba(255, 255, 255, 0.7);' +
                              'box-shadow: 0 0 2px #bbb; ' +
                              'background-clip: padding-box;' +
                              'margin:0; color: #333;' +
                              'font: 9px/1.5 "Helvetica Neue", Arial, Helvetica, sans-serif;' +
                              '></div>;';
      }

*/
