/* global LeafletWidget, $, L */
LeafletWidget.methods.addImageQuery = function(layerId, bounds, type, digits, prefix) {
  (function(){
    var map = this;

    // Create correct bounding box.
    var boundsarr = [
      [bounds[3],bounds[0]],
      [bounds[1],bounds[2]]
      ];

    // Remove previous script tags with data
    var head = document.getElementsByTagName('head')[0],
        scripts = head.getElementsByTagName('script'),
        torm = [];
    Array.prototype.slice.call(scripts).forEach(function(value, i) {
      if (value.src.includes(layerId)) {
        torm.push(i);
      }
    });
    torm.reverse().forEach(function(e) {
      head.removeChild(scripts[e]);
      torm.shift();
    });

    // Add Mouseevent Handler
    map.on(type, function(e) {
      var visible = true;
      if (!(map.layerManager.getVisibleGroups().includes(layerId))) {
        visible = false;
      }
      rasterPicker.pick(e, layerId, boundsarr, digits, prefix, visible);
    });
  }).call(this);
};

