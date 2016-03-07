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

  // create map object
  var map = new L.map(el, {
	 crs : crs
  });

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
  // style for polygons
   var polyStyle = {
     "color": col,
     "weight": x.weight,
     "opacity": x.opacity
   };
   // -------------------- end base styles



   // define projection params for geojson
   proj4.defs(x.t_epsg,x.t_srs);

   // create geojsonlayer
   var polyLayer = L.Proj.geoJson(jsondata,{
    style: polyStyle
   });

   // add vector (geojson) layer to the overlay mapset
   overlayLayers["poly"] = polyLayer;


   // add the layer control
   L.control.layers(baseLayers, overlayLayers).addTo(map);

   // center the map
   map.setView([mapCenter[0],mapCenter[1]], initialZoom);

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

