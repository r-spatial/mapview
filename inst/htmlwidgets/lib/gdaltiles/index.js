/*
 * @copyright 2015 commenthol
 * @license MIT
 */

/* globals L */

function init() {
	var minZoom = 1,
		maxZoom = 8,
		img = [
			30001,  // original width of image
			18001   // original height of image
		];

	// create the map
	var map = L.map('map',{
			minZoom: minZoom,
			maxZoom: maxZoom,
		});

	// assign map and image dimensions
	var rc = new L.RasterCoords(map, img);
	// set the bounds on map
	rc.setMaxBounds();

	// set the view on a marker ...
	map.setView(rc.unproject([1589, 1447]), 5);

	// set marker at the image bound edges
	var layerBounds = L.layerGroup([
		L.marker(rc.unproject([0,0])).bindPopup('[0,0]'),
		L.marker(rc.unproject(img)).bindPopup(JSON.stringify(img))
	]);
	map.addLayer(layerBounds);

var OpenStreetMap_Mapnik = L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
	maxZoom: 19,
	attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
});



	// the tile layer containing the image generated with gdal2tiles --leaflet ...
	L.tileLayer('/home/creu/proj/makeTile/tiles/{z}/{x}/{y}.png', {
		noWrap: true,
		tms:false,
		attribution: 'Map <a href="https://commons.wikimedia.org/wiki/'+
			'File:Karta_%C3%B6ver_Europa,_1672_-_Skoklosters_slott_-_95177.tif">'+
			'Karta över Europa, 1672 - Skoklosters</a> under '+
			'<a href="https://creativecommons.org/publicdomain/zero/1.0/deed.en">CC0</a>',
	}).addTo(map);
	
map.on('mousemove click', function(e) {
    window[e.type].innerHTML = e.containerPoint.toString() + ', ' + e.latlng.toString();
});

}
