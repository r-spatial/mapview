// fast webGL based leaflet map widget for a lot of points

HTMLWidgets.widget({

  name: 'visEarthPole',

  type: 'output',

  initialize: function(el, width, height) {
  // we need a not htmlwidget div in the widget container
  addElement ();
    var epsg3031Code = "urn:ogc:def:crs:EPSG::3031";
    var epsg3031String = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs";


    var tilewh_1 = 512;
    var origin = math.sqrt(x.ulc[0] ^ 2 + x.ulc[1] ^ 2) / 2;

  // initialize the leaflet map staticly at the "el" object
  // hard-coding center/zoom here for a non-empty initial view, since there
  // is no way for htmlwidgets to pass initial params to initialize()
  // so we set maxbounds to the world and center somewhat at 0 Lat 0 Lon
  var crs = new L.Proj.CRS(currentProjCode,currentProj4String,
  {
   resolutions: x.scale,
   origin: [-1*origin/x.zoom, origin/x.zoom]
  })

// create map object
    var map = new L.map(el, {
     crs : crs
    });

    var tileOptions = "'Map Reference'  ";
    var layerName ="SRTM"

//var tileLayerString ="'"+ x.layer[0] +"',"+ tileOptions;

    var defaultLayer = L.tileLayer("https://map1.vis.earthdata.nasa.gov/wmts-antarctic/MODIS_Terra_CorrectedReflectance_TrueColor/default/2015-02-22/EPSG3031_250m/{z}/{y}/{x}.jpg",
    {                           		continuousWorld: true,
                                    noWrap: true,
                                    minZoom: 0,
                                    maxZoom: 5, //crs.options.resolutions.length,
                                  attribution: tileOptions
    });

            //var defaultLayer = L.tileLayer(x.layer[1]).addTo(map);
        //var baseLayers = {};
        //for (var i = 1; i < x.layer.length;  i++) {
        //baseLayers[x.layer[i] ] = L.tileLayer(x.layer[i]);
        //}

       //overlayMaps = {};
       //overlayMaps[layerName[1]] = localLayer;

        //map.addLayer(localLayer);
        //L.control.layers(baseLayer, overlayLayer).addTo(map2);

    var defaultLayer2 = L.tileLayer("http://localhost:4321/{z}/{x}/{y}.png",
    {                               continuousWorld: true,
                                    noWrap: true,
                                    minZoom: 0,
                                    maxZoom: 5, //crs.options.resolutions.length,
                                    attribution: tileOptions
    })

    baseMaps = {};
    // baseMaps["MODIS_Terra_CorrectedReflectance"] = defaultLayer;
    baseMaps["Etopo"] = defaultLayer2;

    // GeoJSON layer (UTM15)
     proj4.defs(currentProjCode,currentProj4String);

    var geojson = {
                  'type': 'Feature',
                  'geometry': {
                    'type': 'Point',
                    'coordinates': [0, 0],
                    },
                  'properties': {
                    'name': 'south pole'
                    },
                  'crs': {
                    'type': 'name',
                      'properties': {
                          'name': "urn:ogc:def:crs:EPSG::3031"
                       }
                    }
    };

    var pointLayer = L.Proj.geoJson(geojson, {
        'pointToLayer': function(feature, latlng) {
        return L.marker(latlng).bindPopup(feature.properties.name);
    }
    });

       overlayMaps = {};
       overlayMaps["NASA"] = defaultLayer;
       overlayMaps["The Pole"] = pointLayer;

L.control.layers(baseMaps, overlayMaps).addTo(map);

map.setView([-90.,0.0], 0);
    // we could add more (static) leaflet stuff here ;-)

    // The map is rendered staticly => so there would be no output binding
    // for the further handling we generate the binding to el this.getId(el)
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

  lnlt = document.getElementById('lnlt');
  map.on('mousemove', function (e) {
        lnlt.textContent =
                " Latitude: " + (e.latlng.lat).toFixed(5)
                + " | Longitude: " + (e.latlng.lng).toFixed(5)
                + " | Zoom: " + map.getZoom() + " ";
  });


},


resize: function(el, width, height, instance) {
}
});

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