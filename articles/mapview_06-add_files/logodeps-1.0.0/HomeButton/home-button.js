LeafletWidget.methods.addHomeButton = function (xmin, ymin, xmax, ymax, useext,
                                                group, label, icon, position) {

  if (this.easyButton) {
		this.easyButton.removeFrom(this);
	}

  var bx = [];
  if (useext) {
    bx = [[ymin, xmin], [ymax, xmax]];
  } else {
    bx = this.layerManager._groupContainers[group].getBounds();
  }

  var easyButton = new L.easyButton({
    position: position,
    states: [{
            stateName: label,   // name the state
            icon:      icon,          // and define its properties
            title:     label, // like its title
            onClick: function(btn, map){
                map.fitBounds(bx, {maxZoom: 18});
                btn.state(label);
            }
    }]
  });

  easyButton.addTo(this);

  this.currentEasyButton = easyButton;

};


LeafletWidget.methods.removeHomeButton = function () {
  if (this.currentEasyButton) {
    this.currentEasyButton.removeFrom(this);
    this.currentEasyButton = null;
  }
};
