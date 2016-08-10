LeafletWidget.methods.sortableLayersControl = function (inner_html) {

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

//var map = this;

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
    var p = map._createPane(layArr[i]);
  	p.style.zIndex = 400 - i;
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
    }
 });


};
