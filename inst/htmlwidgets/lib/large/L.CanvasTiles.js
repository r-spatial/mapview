L.CanvasTiles =  L.TileLayer.Canvas.extend({

    initialize: function (userDrawFunc, options,callContext) {
        this._userDrawFunc = userDrawFunc;
        this._callContext = callContext;
        L.setOptions(this, options);

        var self = this;
        this.drawTile = function (tileCanvas, tilePoint, zoom) {

            this._draw(tileCanvas, tilePoint, zoom);
            if (self.options.debug) {
                self._drawDebugInfo(tileCanvas, tilePoint, zoom);
            }

        };
        return this;
    },

    drawing: function (userDrawFunc) {
        this._userDrawFunc = userDrawFunc;
        return this;
    },

    params: function (options) {
        L.setOptions(this, options);
        return this;
    },

    addTo: function (map) {
        map.addLayer(this);
        return this;
    },

    _drawDebugInfo: function (tileCanvas, tilePoint, zoom) {

        var max = this.options.tileSize;
        var g = tileCanvas.getContext('2d');
        g.globalCompositeOperation = 'destination-over';
        g.strokeStyle = '#FFFFFF';
        g.fillStyle = '#FFFFFF';
        g.strokeRect(0, 0, max, max);
        g.font = "12px Arial";
        g.fillRect(0, 0, 5, 5);
        g.fillRect(0, max - 5, 5, 5);
        g.fillRect(max - 5, 0, 5, 5);
        g.fillRect(max - 5, max - 5, 5, 5);
        g.fillRect(max / 2 - 5, max / 2 - 5, 10, 10);
        g.strokeText(tilePoint.x + ' ' + tilePoint.y + ' ' + zoom, max / 2 - 30, max / 2 - 10);

    },

    /**
     * Transforms coordinates to tile space
     */
    tilePoint: function (map, coords,tilePoint, tileSize) {
        // start coords to tile 'space'
        var s = tilePoint.multiplyBy(tileSize);

        // actual coords to tile 'space'
        var p = map.project(new L.LatLng(coords[0], coords[1]));

        // point to draw
        var x = Math.round(p.x - s.x);
        var y = Math.round(p.y - s.y);
        return {x: x,
            y: y};
    },
    /**
     * Creates a query for the quadtree from bounds
     */
    _boundsToQuery: function (bounds) {
        if (bounds.getSouthWest() == undefined) { return { x: 0, y: 0, width: 0.1, height: 0.1 }; }  // for empty data sets
        return {
            x: bounds.getSouthWest().lng,
            y: bounds.getSouthWest().lat,
            width: bounds.getNorthEast().lng - bounds.getSouthWest().lng,
            height: bounds.getNorthEast().lat - bounds.getSouthWest().lat
        };
    },

    _draw: function (tileCanvas, tilePoint, zoom) {

        var tileSize = this.options.tileSize;

        var nwPoint = tilePoint.multiplyBy(tileSize);
        var sePoint = nwPoint.add(new L.Point(tileSize, tileSize));


        // padding to draw points that overlap with this tile but their center is in other tile
        var pad = new L.Point(this.options.padding, this.options.padding);

        nwPoint = nwPoint.subtract(pad);
        sePoint = sePoint.add(pad);

        var bounds = new L.LatLngBounds(this._map.unproject(sePoint), this._map.unproject(nwPoint));
        var zoomScale  = 1 / ((40075016.68 / tileSize) / Math.pow(2, zoom));
        // console.time('process');

        if (this._userDrawFunc) {
            this._userDrawFunc.call(
                this._callContext,
                this,
                {
                canvas: tileCanvas,
                tilePoint: tilePoint,
                bounds: bounds,
                size: tileSize,
                zoomScale: zoomScale,
                zoom: zoom,
                options: this.options,
                }
             );
        }


        // console.timeEnd('process');


    },


});

L.canvasTiles = function (userDrawFunc, options, callContext) {
    return new L.CanvasTiles(userDrawFunc, options, callContext);
};

