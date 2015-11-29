// fast webGL based leaflet map widget for a lot of points

HTMLWidgets.widget({

  name: 'bView',

  type: 'output',

  initialize: function(el, width, height) {
    // we need some kind of "own" div  in this widget container
    // so we generate it
    addElement ();


    // initialize the leaflet map rendered staticly at the "el" object
    // hard-coding center/zoom here for a non-empty initial view, since there
    // is no way for htmlwidgets to pass initial params to initialize()
    var map = new L.map(el, {
      center: [47, 10],
      zoom: 7
    });


    // we even could add more leaflet stuff here ;)

    // The map is rendered staticly => so there would be no output binding
    // for the further handling to aoboid it we generate the binding to el this.getId(el)
    if (typeof this.getId === 'undefined') return map;
    map.id = this.getId(el);

    // Store the map on the element so we could find it later by ID
    $(el).data("leaflet-map", map);

    //return the initial mapsetup to the renderValue function
    return map;
  },

  renderValue: function(el, x, map) {
      return this.doRenderValue(el, x, map);
    },

  doRenderValue: function(el, x, map) {
//#########################################################

addCanvas();

   // we add some base layers using the plugin L.tileLayer.provider
    var defaultLayer = L.tileLayer.provider(x[1][0]).addTo(map);
    var layerOne = L.tileLayer.provider(x[1][1]);
    var layerTwo = L.tileLayer.provider(x[1][2]);
		var baseLayers = {
			"OpenStreetMap" : defaultLayer,
			"Esri WorldImagery": layerOne,
			"Thunderforest Landscape" : layerTwo,
		};

    var myLayer = L.geoJson(undefined).addTo(map);
		var overlays = {
			"Overlay": myLayer
		};

    // adding all together and the layer control
		var layerControl = L.control.layers(baseLayers, overlays, {collapsed: true}).addTo(map);
		map.setView([x[4], x[5]], x[6]);
    //var data = x[2];
//    var loc = HTMLWidgets.getAttachmentUrl('data', 'jsondata');
    //var data = $.parseJSON(HTMLWidgets.getAttachmentUrl('data', 'jsondata'));
    var baseZ = x[6];
    var maxZ = x[6] ;

        var tileOptions = {
	    baseZoom: baseZ,           // max zoom to preserve detail on
	    maxZoom: maxZ,            // zoom to slice down to on first pass
	    maxPoints: 100,         // stop slicing each tile below this number of points
            tolerance: 1,           // simplification tolerance (higher means simpler)
            extent: 4096,           // tile extent (both width and height)
            buffer: 64,   	    // tile buffer on each side
            debug: 0,     	    // logging level (0 to disable, 1 or 2)
	    indexMaxZoom: 0,        // max zoom in the initial tile index
            indexMaxPoints: 100000, // max number of points per tile in the index
        };

	var rt = RTree();
	var bd;

	//var myLayer = L.geoJson(undefined, { style: style, onEachFeature: onEachFeature/*, filter: filter*/}).addTo(map);
	// The onEachFeature
	function onEachFeature(feature, layer) {
	        layer.bindPopup('Altitude ' + feature.properties.ELEV );
	    }
	// The styles of the layer
	function style(feature) {
	        if (feature.properties.ELEV != "" && feature.properties.ELEV != "<Null>" && feature.properties.ELEV != null) {
	            if (feature.properties.ELEV == "1000") {
	                return {
	                    color: "green",
	                    weight: 4,
	                    opacity: 0.9
	                }
	            } else if (feature.properties.ELEV == "2000") {
	                return {
	                    color: "blue",
	                    weight: 4,
	                    opacity: 0.9
	                }
	            } else {
	                return {
	                    color: "red",
	                    weight: 2,
	                    opacity: 0.9
	                }
	            }
	        } else {
	            return {
	                color: "black",
	                weight: 1,
	                opacity: 0.9
	            }
	        }
	    }
	    // If you want to filter the layer, if not delete the function here and on the var myLayer
//	function filter(feature, layer) {
//	    if (theFilter != "none") {
//	        layerFilter = feature.height;
//	        if (layerFilter == theFilter) {
//	            return true;
//	        } else {
//	            return false;
//	        }
//	    } else {
//	        return true;
//	    }
//	}

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
	                [bounds.getSouthWest().lng, bounds.getSouthWest().lat],
	                [bounds.getNorthEast().lng, bounds.getNorthEast().lat]
	            ]
	        });
	    }
	});
	var boxSelect = new BoxSelect(map); //new box select

	boxSelect.enable(); //add it

	map.on("boxselected", function(e) {
	    // Define here the zoom level of change
	    if (map.getZoom() > maxZ) {
	        if (layerType == "vectortiles") {
	            map.removeLayer(canvasTiles);
	            layerType = "geojson";
	        }
	        myLayer.clearLayers();
	        myLayer.addData(rt.bbox(e.boxSelectBounds));
	    } else {
	        myLayer.clearLayers();
	        canvasTiles.addTo(map);
	        layerType = "vectortiles";
	    }
	});

	function showLayer() {
	    if (map.getZoom() > maxZ) {
	        if (layerType == "vectortiles") {
	            map.removeLayer(canvasTiles);
	            layerType = "geojson";
	        }
	        //layerType.clearLayers();
                myLayer.clearLayers();
	        var bounds = map.getBounds();
	        myLayer.addData(rt.bbox([
	            [bounds.getSouthWest().lng, bounds.getSouthWest().lat],
	            [bounds.getNorthEast().lng, bounds.getNorthEast().lat]
	        ]));
	    } else {
	        myLayer.clearLayers();
	        canvasTiles.addTo(map);
	        layerType = "vectortiles";
	    }
	}
	map.on("moveend", function(e) {
	    showLayer();
	});

	//L.Util.ajax('100.geojson'
	//L.Util.ajax('75.json', function(data) {

	 // Add to the r-tree
	 rt.geoJSON(data);
	 // Add to the GeoJson Vector Tiles
	 var tileIndex = geojsonvt(data,tileOptions);

	// The canvas tile layer for low zoom level
	 var canvasTiles = L.tileLayer.canvas().addTo(map);





        var overlayLayers = {"Overlay":(canvasTiles).addTo(map)};


        var layerControl = L.control.layers(baseLayers, overlayLayers, {collapsed: true}).addTo(map);

	    // Draw the canvas tiles

	    canvasTiles.drawTile = function(canvas, tilePoint, zoom) {
	        var ctx = canvas.getContext('2d');
	        extent = 4096;
	        padding = 0;
	        totalExtent = 4096 * (1 + padding * 2);
	        height = canvas.height = canvas.width = 256;
	        ratio = height / totalExtent;
	        pad = 4096 * padding * ratio;

	        var x = tilePoint.x;
	        var y = tilePoint.y;
	        var z = zoom;
	        var tile = tileIndex.getTile(z, x, y);
	        if (typeof tile != "undefined") {
	            var features = tile.features;
               // color to the lines
		var grd = ctx.createLinearGradient(0, 0, 170, 0);
		grd.addColorStop(0, "green");
		grd.addColorStop(1, "brown");
    ctx.lineWidth = 0.61;

	            for (var i = 0; i < features.length; i++) {
	                var feature = features[i],
	                    typeChanged = type !== feature.type,
	                    type = feature.type;

	                if (feature.tags.ELEV != "") {
	                    if (feature.tags.ELEV == "1000") {
	                        ctx.strokeStyle = grd;
	                    } else if (feature.tags.ELEV == "2000") {
	                        ctx.strokeStyle = "#00FF00";
	                    } else {
	                        ctx.strokeStyle = grd;
	                    }
	                } else {
	                    ctx.strokeStyle = "#magenta";
	                }
	                ctx.lineWidth = 0.61;

	                ctx.beginPath();

	                for (var j = 0; j < feature.geometry.length; j++) {
	                    var ring = feature.geometry[j];

	                    for (var k = 0; k < ring.length; k++) {
	                        var p = ring[k];
	                        if (k) ctx.lineTo(p[0] * ratio + pad, p[1] * ratio + pad);
	                        else ctx.moveTo(p[0] * ratio + pad, p[1] * ratio + pad);
	                    }
	                }

	                if (type === 3) ctx.fill('evenodd');
	                ctx.stroke();
	            }
	        }

	    };

		showLayer();
//	});


//###########################################################


},


resize: function(el, width, height, instance) {
}
});

//  we need to create a new meta tag
    function addCanvas() {
      var newMeta = document.createElement("meta");
      document.head.insertBefore(newMeta,null);
      newMeta.name = "viewport";
      newMeta.content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no";
    }

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

