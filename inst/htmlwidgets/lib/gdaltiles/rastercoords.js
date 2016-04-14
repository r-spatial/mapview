/*
 * leaflet plugin for plain image map projection
 * @copyright 2015 commenthol
 * @license MIT
 */

/* globals L */
/*
 * L.RasterCoords
 * @param {L.map} map - the map used
 * @param {Array} imgsize - [ width, height ] image dimensions
 * @param {Number} [tilesize] - tilesize in pixels. Default=256
 */
L.RasterCoords = function (map, imgsize, tilesize) {
	this.map = map;
	this.width = imgsize[0];
	this.height = imgsize[1];
	this.tilesize = tilesize || 256;
	this.zoom = this.zoomLevel();
};

L.RasterCoords.prototype = {
	/*
	 * calculate accurate zoom level for the given image size
	 */
	zoomLevel: function() {
		return Math.ceil(
					Math.log(
						Math.max(this.width, this.height)/
						this.tilesize
					)/Math.log(2)
				);
	},
	/*
	 * unproject `coords` to the raster coordinates used by the raster image projection
	 * @param {Array} coords - [ x, y ]
	 * @return {L.LatLng} - internal coordinates
	 */
	unproject: function(coords) {
		return this.map.unproject(coords, this.zoom);
	},
	/*
	 * project `coords` back to image coordinates
	 * @param {Array} coords - [ x, y ]
	 * @return {L.LatLng} - image coordinates
	 */
	project: function(coords) {
		return this.map.project(coords, this.zoom);
	},
	/*
	 * sets the max bounds on map
	 */
	setMaxBounds: function() {
		var southWest = this.unproject([0, this.height]);
		var northEast = this.unproject([this.width, 0]);
		this.map.setMaxBounds(new L.LatLngBounds(southWest, northEast));
	}
};
