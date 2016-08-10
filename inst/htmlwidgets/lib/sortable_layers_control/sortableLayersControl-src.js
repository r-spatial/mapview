////////////////////////////////////////////////////////////////////////////////////
//taken from this jsfiddle: http://jsfiddle.net/nathansnider/7r763xaq/
//setting up the map//
////////////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////////////
//displaying the data on the map//
/////////////////////////////////////////////////////////////////////////////////////////////

//Create a pane for each of the layers.
//When created, panes have a zIndex of 400, and rendering order is determined by the order
//in which they are defined here (panes defined later are on top of those defined earlier).

/////////////////////////////////////////////////////////////////////////////////////////////
//controls//
/////////////////////////////////////////////////////////////////////////////////////////////

/*
//add layer control/legend
var overlayMaps = {
  '<div class="dotLegend bdotColor"></div> The Blue Dots': bdotLayer,
  '<div class="dotLegend gdotColor"></div> The Green Dots': gdotLayer,
  '<div class="dotLegend rdotColor"></div> The Red Dots': rdotLayer,
  '<div class="iLegend iColor"></div> Image Overlay': imageLayer
};
layerbox = L.control.layers(null, overlayMaps, {
  collapsed: false
}).addTo(map);
*/

(function(inner_html){

//create sortable control
L.ControlSort = L.Control.extend({
  options: {
    position: 'topleft',
  },
  onAdd: function() {
    var sortableDiv = L.DomUtil.create('div', 'sortableContainer', this._container);
    sortableDiv.innerHTML = inner_html;
    return sortableDiv;
  }
});

L.controlSort = function(options) {
  return new L.ControlSort(options);
};

L.controlSort().addTo(this);

layerList = document.getElementById('layer_list');

var layArr = getIds(layerList);
sortLayers(layArr);

Sortable.create(layerList, {
  animation: 150,
  onUpdate: function(evt) {
    layArr = getIds(layerList);
    sortLayers(layArr);
  }
});

function sortLayers(layArr) {
  for (var i = 0; i < layArr.length; i++) {
  	map._getMapPanePos(layArr[i]).style.zIndex = 400 - i;
  }
}

function getIds(inputList) {
  var outputArr = [];
  for (var i = 0; i < inputList.children.length; i++) {
    outputArr[i] = inputList.children[i].id;
  }
  return outputArr;
}

L.DomEvent.disableClickPropagation(layerList);

$( "input" ).change(function( event ) {
    layerClicked = eval(event.target.value);
    if (map.hasLayer(layerClicked)) {
       map.removeLayer(layerClicked);
    }
    else{
       map.addLayer(layerClicked);
    } ;
 });

})();
////////////////////////////////////////////////////////////////////////////////////////////
//synthetic GeoJSON functions//
////////////////////////////////////////////////////////////////////////////////////////////
