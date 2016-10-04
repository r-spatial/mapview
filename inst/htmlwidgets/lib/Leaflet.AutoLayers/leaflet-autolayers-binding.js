LeafletWidget.methods.addAutoLayersControl = function (baselayers,
                                                       overlays) {

  if (this.control) {
		this.control.removeFrom(this);
	}


 var config = {
        overlays: overlays, //custom overlays group that are static
        baseLayers: baselayers, //custom baselayers group that are static
        selectedBasemap: baselayers, //selected basemap when it loads
        selectedOverlays: [overlays] //which overlays should be on by default
        };

 var control = L.control.autolayers(config);

 control.addTo(this);

  this.currentControl = control;

};


LeafletWidget.methods.removeAutoLayersControl = function () {
  if (this.currentControl) {
    this.currentControl.removeFrom(this);
    this.currentControl = null;
  }
};
