// fast webGL based leaflet map widget for a lot of points

HTMLWidgets.widget({

  name: 'fpView',

  type: 'output',

  initialize: function(el, width, height) {

    // we need a not htmlwidget div in the widget container
    addElement ();

    // initialize the leaflet map staticly at the "el" object
    // hard-coding center/zoom here for a non-empty initial view, since there
    // is no way for htmlwidgets to pass initial params to initialize()
    // so we set maxbounds to the world and center somewhat at 0 Lat 0 Lon
    var southWest = L.latLng(-90, -180),
    northEast = L.latLng(90, 180),
    bounds = L.latLngBounds(southWest, northEast);
    var map = new L.map(el, {
      center: [0, 0],
      maxBounds: bounds
    });

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

   // we define the first layer of the list to be the default one
    var defaultLayer = L.tileLayer.provider(x.layer[0]).addTo(map);
    var baseLayers = {};
    for (var i = 0; i < x.layer.length;  i++) {
      baseLayers[x.layer[i] ] = L.tileLayer.provider(x.layer[i]);
      }

    //focus on the bounds of the input data extent using a virtual marker layer
    var mincorner = L.marker([x.ymin, x.xmin]);
    var maxcorner = L.marker([x.ymax, x.xmax]);
    var group = new L.featureGroup([maxcorner, mincorner]);
    map.fitBounds(group.getBounds());

    // get the file locations from the shaders and the static external file
    var vertexshader = HTMLWidgets.getAttachmentUrl('vertex-shader', 'vertex-shader');
    var fragmentshader = HTMLWidgets.getAttachmentUrl('fragment-shader', 'fragment-shader');
    var color = x.color;

  // after reading the shader files data, popuptemplates and shaders are passed to the
  // L.Glify leaflet extension that handles the webGL shading process
  // big thanks for this to Robert Plummers version of the web gl renderer and his plugin for
  // leaflet https://robertleeplummerjr.github.io/Leaflet.glify
  if (x.data === 'undefined') {
    var data = HTMLWidgets.getAttachmentUrl('data');
     wget([fragmentshader, vertexshader, data],function(fragmentshader, vertexshader, data) {
            L.glify({
                    map: map,
                    vertexShader: vertexshader,
                    fragmentShader: fragmentshader,
                    clickPoint: function (point) {
                    //set up a standalone popup (use a popup as a layer)
                    contentToHtml = x.popTemplate;
                    for (var i = 0; i < x.cHelp.length;  i++) {
                      if (i == 0) {
                          contentToHtml += x.cHelp[i] +  point.lng + "</td></tr>"                           }
                      if (i == 1) {
                          contentToHtml += x.cHelp[i] +  point.lat + "</td></tr>"                           }
                      if (i > 1)  {
                          contentToHtml += x.cHelp[i] +  point.a[i-2] + "</td></tr>" ;
                          }
                      }
                      contentToHtml += "</table></body></html>";
                      L.popup()
                              .setLatLng(point)
                              .setContent(contentToHtml)
                              .openOn(map);
                              //console.log(point);
                        },
                      data: JSON.parse(data),
                      color: color,
                      baseLayers: baseLayers

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
    var data = x.data;
  }

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
      newDiv.style.cssText = 'position: relative; bottomleft:  0px; background-color: rgba(255, 255, 255, 0.7);box-shadow: 0 0 2px #bbb; background-clip: padding-box; margin:0; color: #333; font: 9px/1.5 "Helvetica Neue", Arial, Helvetica, sans-serif; ></div>;';
}