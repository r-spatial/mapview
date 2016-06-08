/*
 * Extends L.Map to synchronize the interaction on one map to one or more other maps.
 */

(function () {
    var NO_ANIMATION = {
        animate: false,
        reset: true
    };

    L.Map = L.Map.extend({
        sync: function (map, options) {
            this._initSync();
            options = L.extend({
                noInitialSync: false,
                syncCursor: false,
                syncCursorMarkerOptions: {
                    radius: 10,
                    fillOpacity: 0.3,
                    color: '#da291c',
                    fillColor: '#fff'
                }
            }, options);

            // prevent double-syncing the map:
            if (this._syncMaps.indexOf(map) === -1) {
                this._syncMaps.push(map);
            }

            if (!options.noInitialSync) {
                map.setView(this.getCenter(), this.getZoom(), NO_ANIMATION);
            }
            if (options.syncCursor) {
                map.cursor = L.circleMarker([0, 0], options.syncCursorMarkerOptions).addTo(map);

                this._cursors.push(map.cursor);

                this.on('mousemove', this._cursorSyncMove, this);
                this.on('mouseout', this._cursorSyncOut, this);
            }
            return this;
        },

        _cursorSyncMove: function (e) {
            this._cursors.forEach(function (cursor) {
                cursor.setLatLng(e.latlng);
            });
        },

        _cursorSyncOut: function (e) {
            this._cursors.forEach(function (cursor) {
                // TODO: hide cursor in stead of moving to 0, 0
                cursor.setLatLng([0, 0]);
            });
        },


        // unsync maps from each other
        unsync: function (map) {
            var self = this;

            if (this._syncMaps) {
                this._syncMaps.forEach(function (synced, id) {
                    if (map === synced) {
                        self._syncMaps.splice(id, 1);
                        if (map.cursor) {
                            map.cursor.removeFrom(map);
                        }
                    }
                });
            }
            this.off('mousemove', this._cursorSyncMove, this);
            this.off('mouseout', this._cursorSyncOut, this);

            return this;
        },

        // Checks if the maps is synced with anything
        isSynced: function () {
            return (this.hasOwnProperty('_syncMaps') && Object.keys(this._syncMaps).length > 0);
        },

        // overload methods on originalMap to replay interactions on _syncMaps;
        _initSync: function () {
            if (this._syncMaps) {
                return;
            }
            var originalMap = this;

            this._syncMaps = [];
            this._cursors = [];

            L.extend(originalMap, {
                setView: function (center, zoom, options, sync) {
                    if (!sync) {
                        originalMap._syncMaps.forEach(function (toSync) {
                            toSync.setView(center, zoom, options, true);
                        });
                    }
                    return L.Map.prototype.setView.call(this, center, zoom, options);
                },

                panBy: function (offset, options, sync) {
                    if (!sync) {
                        originalMap._syncMaps.forEach(function (toSync) {
                            toSync.panBy(offset, options, true);
                        });
                    }
                    return L.Map.prototype.panBy.call(this, offset, options);
                },

                _onResize: function (event, sync) {
                    if (!sync) {
                        originalMap._syncMaps.forEach(function (toSync) {
                            toSync._onResize(event, true);
                        });
                    }
                    return L.Map.prototype._onResize.call(this, event);
                }
            });

            originalMap.on('zoomend', function () {
                originalMap._syncMaps.forEach(function (toSync) {
                    toSync.setView(originalMap.getCenter(), originalMap.getZoom(), NO_ANIMATION);
                });
            }, this);

            originalMap.dragging._draggable._updatePosition = function () {
                L.Draggable.prototype._updatePosition.call(this);
                var self = this;
                originalMap._syncMaps.forEach(function (toSync) {
                    L.DomUtil.setPosition(toSync.dragging._draggable._element, self._newPos);
                    toSync.eachLayer(function (layer) {
                        if (layer._google !== undefined) {
                            layer._google.setCenter(originalMap.getCenter());
                        }
                    });
                    toSync.fire('moveend');
                });
            };
        }
    });
})();
