// fast webGL based leaflet map widget for a lot of points

HTMLWidgets.widget({

  name: 'fpmap',

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


   // we add some base layers using the plugin L.tileLayer.provider
    var defaultLayer = L.tileLayer.provider(x[1][0]).addTo(map);
    var layerOne = L.tileLayer.provider(x[1][1]);
    var layerTwo = L.tileLayer.provider(x[1][2]);
    var hillshade =  L.tileLayer.wms("http://129.206.228.72/cached/hillshade", {
      layers: 'europe_wms:hs_srtm_europa',
      format: 'image/png',
      opacity: 0.45,
      transparent: true,
      attribution: 'Hillshade layer by GIScience http://www.osm-wms.de',
      crs: L.CRS.EPSG900913});

		var baseLayers = {
			"OpenStreetMap" : defaultLayer,
			"Esri WorldImagery": layerOne,
			"Thunderforest Landscape" : layerTwo,
		};
		var overlays = {

			"Hillshade": hillshade
		};

    // adding all together and the layer control
		var layerControl = L.control.layers(baseLayers, overlays, {collapsed: true}).addTo(map);
		map.setView([x[4], x[5]], x[6]);


  // get the file locations from the shaders and the static external file
  var vertexshader = HTMLWidgets.getAttachmentUrl('vertex-shader', 'vertex-shader');
  var fragmentshader = HTMLWidgets.getAttachmentUrl('fragment-shader', 'fragment-shader');
  var color = x[0];


  // no it is getting tricky after wget-ing all files the text contet of it
  // is passed to the  L.Glify extension of leaflet that handles the webGL shading process
  // big thanks for this to Robert's version of the web gl renderer and his plugin for
  // leaflet https://robertleeplummerjr.github.io/Leaflet.glifyand the popups
  // no it is getting tricky after wget-ing all files the text contet of it
  // is passed to the  L.Glify extension of leaflet that handles the webGL shading process
  // big thanks for this to Robert's version of the web gl renderer and his plugin for
  // leaflet https://robertleeplummerjr.github.io/Leaflet.glifyand the popups

  if (x[2] === 'undefined') {
    var data = HTMLWidgets.getAttachmentUrl('data', 'jsondata');
     wget([fragmentshader, vertexshader, data],function(fragmentshader, vertexshader, data) {
                    L.glify({
                        map: map,
                        vertexShader: vertexshader,
                        fragmentShader: fragmentshader,
                        clickPoint: function (point) {
                        //set up a standalone popup (use a popup as a layer)
                        L.popup()
                          .setLatLng(point)
                          .setContent("<table><tr><td>Longitude</td><td>" + point.lng + "</td></tr><tr><td>Latitude</td><td>" + point.lat + "</td></tr><tr><td>" + x[3][0] + "</td><td>" + point.v1 + "</td></tr><tr><td>" + x[3][1] + "</td><td>" + point.v2 + "</td></tr><tr><td>" + x[3][2] + "</td><td>" + point.v3 + "</td></tr><tr><td>" + x[3][3] + "</td><td>" + point.v4 + "</td></tr><tr><td>" + x[3][4] + "</td><td>" + point.v5 + "</td></tr></table>")

                              .openOn(map);
                              console.log(point);
                        },
                        data: JSON.parse(data),
                        color: color
                    });
  })



  // grab the special div we generated in the beginning
  // and put the mousmove output there
  lnlt = document.getElementById('lnlt');
  map.on('mousemove', function (e) {
        lnlt.textContent =
                " Latitude: " + (e.latlng.lat).toFixed(5)
                + " | Longitude: " + (e.latlng.lng).toFixed(5)
                + " | Zoom: " + map.getZoom() + " ";
  });
  } else
  {
    var data = x[2];
     wget([fragmentshader, vertexshader],function(fragmentshader, vertexshader) {
                    L.glify({
                        map: map,
                        vertexShader: vertexshader,
                        fragmentShader: fragmentshader,
                        clickPoint: function (point) {
                        //set up a standalone popup (use a popup as a layer)
                        L.popup()
                          .setLatLng(point)
                          .setContent("<table><tr><td>Longitude</td><td>" + point.lng + "</td></tr><tr><td>Latitude</td><td>" + point.lat + "</td></tr><tr><td>" + x[3][0] + "</td><td>" + point.v1 + "</td></tr><tr><td>" + x[3][1] + "</td><td>" + point.v2 + "</td></tr><tr><td>" + x[3][2] + "</td><td>" + point.v3 + "</td></tr><tr><td>" + x[3][3] + "</td><td>" + point.v4 + "</td></tr><tr><td>" + x[3][4] + "</td><td>" + point.v5 + "</td></tr></table>")

                              .openOn(map);
                              console.log(point);
                        },
                        data: JSON.parse(data),
                        color: color
                    });
  })
  }



  // grab the special div we generated in the beginning
  // and put the mousmove output there
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

  // get the files and returns them as text stream
  function wget(urls, fn) {
        var results = [],
            lookup = {},
            complete = 0,
            total = urls.length;

        urls.forEach(function(url) {
            var i = lookup[url] = results.length,
                request = new XMLHttpRequest();
            results.push(null);
            request.open('GET', url, true);
            request.onload = function () {
                if (request.status < 200 && request.status > 400) return;
                results[i] = request.responseText;
                complete++;
                if (complete === total) fn.apply(null, results);
            };
            request.send();
        });
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
      newDiv.style.cssText = 'position: relative; left:  0px; color:black;font-size: x-small;" background: rgba(255,255,255,0.4);></div>;';
}