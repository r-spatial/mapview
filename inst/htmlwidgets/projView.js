// arbitrary projected local tiles as leaflet map

HTMLWidgets.widget({

  name: 'projView',

  type: 'output',

  initialize: function(el, width, height) {
  // initialize the leaflet map staticly at the "el" object
  // hard-coding center/zoom here for a non-empty initial view, since there
  // is no way for htmlwidgets to pass initial params to initialize()
  //Nevetheless we need this functionality so wie intruse Code via the <head> JS declarations

  // we create a "no-htmlwidget"" div in the widget container
  addElement();
    //var southWest = L.latLng(-90, -180),
    //northEast = L.latLng(90, 180),
    //bounds = L.latLngBounds(southWest, northEast);
    var map = new L.map(el, {
      center: mapCenter,
      crs : crs,
      continuousWorld: true,
      worldCopyJump: true
    });

  // create map object
//  var map = new L.map(el, {
//	 crs : crs,
//	 	continuousWorld: true,
//    worldCopyJump: true
//  });


  //you can add more (static) leaflet stuff here ;-)

  // the above 'map' is rendered staticly
  // => for further handling we generate the binding to
  // the el object via this.getId(el)
  if (typeof this.getId === 'undefined') return map;
  map.id = this.getId(el);

  // Store the map on the element 'leaflet-map' so we could find it later by ID
  $(el).data("leaflet-map", map);

  // finally return the container with the initial mapsetup to the renderValue function
  return map;
  // --------------------- end of initialisation
  },

  renderValue: function(el, x, map) {
      return this.doRenderValue(el, x, map);
    },

  doRenderValue: function(el, x, map) {
    //create a pseudo layer for applying fitBounds
    //var mincorner = L.marker([x.ext.ymin, x.ext.xmin]);
    //var maxcorner = L.marker([x.ext.ymax, x.ext.xmax]);
    //var group = new L.featureGroup([maxcorner, mincorner]);
    //map.fitBounds(group.getBounds());
    if (!x.internalList) {var ly = new loadLayers();}
    else { var baseLayers = {};
           var overlayLayers = {};}

   // check if an array of colors (palette) or a single color is provided
   if (x.color.length <= 7 ) {
       if (x.color[1].substring(0,1) != "#" ) {
            var col =  x.color;
       }
    }
    else
    {
        var col =  x.color[x.color.length-1];
    }
    var cex = x.cex
    var color = col;
    var opacity = x.opacity;
    var lnWidth = x.weight;
  // style for polygons
   var polyStyle = {
     "color": col,
     "weight": x.weight,
     "opacity": x.opacity
   };
   // -------------------- end base styles
	  function onEachFeature(feature, layer) {
      var i = 1;
      var content = '';
    // does this feature have a property named popupContent?
    if (feature.properties) {
        for (var key in feature.properties) {
          if (isEven(i)) {
            content += "<tr><td> " +  key + " </td><td>" + feature.properties[key] +" </td></tr>";
          } else {
            content += "<tr class='alt'><td> " +  key + " </td><td>" + feature.properties[key] +" </td></tr>";
          }
          i = i + 1;
        };
        var popupContent = x.html + content + "</table></body></html>";
        //console.log(popupContent);
        layer.bindPopup(popupContent);
    }
  }
	// The styles of the layer
	function style(feature) {
	        if (feature.properties.ELEV != "" && feature.properties.ELEV != "<Null>" && feature.properties.ELEV != null) {
	           /* if (feature.properties.ELEV == "1000") {
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
	        */
	        } else {
	            return {
	                color: color,
	                weight: lnWidth,
	                opacity: opacity
	            }
	        }
	    }
var geojsonMarkerOptions = {
    radius: cex,
    fillColor: color,
    color: color,
    weight: lnWidth,
    opacity: 1,
    fillOpacity: opacity
};

   // define projection params for geojson
   proj4.defs(urnEPSG,x.t_srs);


   // create geojsonlayer
   var polyLayer = L.Proj.geoJson(jsondata,{ pointToLayer: function (feature, latlng) {
        return L.circleMarker(latlng, geojsonMarkerOptions);
    },style:style,onEachFeature:onEachFeature});

  // create geojsonlayer
   var frameLayer = L.Proj.geoJson(framedata,{ pointToLayer: function (feature, latlng) {
        return L.circleMarker(latlng, geojsonMarkerOptions);
    },style:style,onEachFeature:onEachFeature});


    if (!x.internalList) {ly.overlayLayers[x.overlayLayer] = polyLayer;
    ly.overlayLayers["frame"] = frameLayer;

    }

///////////////////////////////////////////////////////////////77
if (x.internalList) {
   var ibaseLayers = {};
   var ioverlayLayers = {};
   // add vector (geojson) layer to the overlay mapset
   ioverlayLayers[x.overlayLayer] = polyLayer;
    var defaultLayer = L.tileLayer(x.layer[0],
                                  {tileSize: x.tilesize,
                                   subdomains: "abc",
                                   noWrap: true,
                                   continuousWorld:
                                   true,minZoom: 0,
                                   maxZoom: x.zoom,
                                     attribution: x.attribution}).addTo(map);


   ibaseLayers[x.layername[0]] = defaultLayer;
   var ioverlayLayers = {};
   ioverlayLayers[x.overlayLayer] = polyLayer;
   for (var i = 1; i < x.layer.length;  i++) {
          ioverlayLayers[x.layername[i] ] = L.tileLayer(x.layer[i],
                                                  {tileSize: x.tilesize,
                                                  subdomains: "abc",
                                                  noWrap: true,
                                                  continuousWorld: true,
                                                  minZoom: 0,
                                                  maxZoom: x.zoom,
                                                  attribution: x.attribution});
       }
   L.control.layers(ibaseLayers, ioverlayLayers).addTo(map);
 }      /////////////////////////////////////////////77




   // add the layer control
   if (!x.internalList) {map.addLayer(ly.defaultLayer);
     L.control.layers(ly.baseLayers, ly.overlayLayers).addTo(map);}

   // center the map
   map.setView([mapCenter[0],mapCenter[1]], initialZoom);
// map.fitWorld();
  // ad the lat lon mousover to the element created in the init
  lnlt = document.getElementById('lnlt');
  map.on('mousemove', function (e) {
        lnlt.textContent =
                " Latitude: " + (e.latlng.lat).toFixed(5)
                + " | Longitude: " + (e.latlng.lng).toFixed(5)
                + " | Zoom: " + map.getZoom() + " ";
  });

// -------------------- end dynamic part
},


resize: function(el, width, height, instance) {
}
});
// ------------   end widget

// we need a new div element because we have to handle
// the mouseover output seperatly
function addElement () {
  // generate new div ElementdefaultLayer
  var newDiv = document.createElement("div");
  // insert to DOM
  document.body.insertBefore(newDiv, null);
      //provide ID and style
      newDiv.id = 'lnlt';
      newDiv.style.cssText = 'position: relative; bottomleft:  0px; background-color: rgba(255, 255, 255, 0.7);box-shadow: 0 0 2px #bbb; background-clip: padding-box; margin:0; color: #333; font: 9px/1.5 "Helvetica Neue", Arial, Helvetica, sans-serif; ></div>;';
}


function isEven(n) {
   return n % 2 == 0;
}

