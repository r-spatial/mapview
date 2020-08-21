// Test joda
var rasterPicker = {};
rasterPicker.old = function(e, x, data) {
    // TODO: How do we know that x contains the right data layer's bounds?
    var relative_y = (x.limits.lat[1] - e.latlng.lat ) / (x.limits.lat[1] - x.limits.lat[0]);
    var relative_x = (e.latlng.lng - x.limits.lng[0] ) / (x.limits.lng[1] - x.limits.lng[0]);
    var pane = document.getElementById('pixelValue');
    var data_name = '(window.data["stars"])';
    var value_data = eval(data_name);
    if (0 <= relative_y && relative_y <= 1 &&
        0 <= relative_x && relative_x <= 1) {
      if (typeof(value_data)==='object') {
        // Hardcoded mapping
      var iy = (value_data[0].length*x.limits.lat[1] - value_data[0].length*e.latlng.lat ) / (x.limits.lat[1] - x.limits.lat[0]);
      var ix = (e.latlng.lng * value_data.length - x.limits.lng[0] * value_data.length) / (x.limits.lng[1] - x.limits.lng[0]);
        pane.innerHTML = 'z = '+value_data[Math.floor(ix)][Math.floor(iy)];
        //+'<br>x,y='+ix+','+iy;
        /*alert('Data='+stars[ix][iy]+' \n\t' +
        'ix,iy='+ix+','+iy+'\n\t' +
        'lng,lat='+relative_x+','+relative_y);*/
      } else {
        // Error handling if stars is not there
        alert('Typeof('+data_name+') is not object, but: '+typeof(value_data)+'\n'+
        'Lat / Lng:' + e.latlng + "\nImage x,y = "+relative_x + ',' + relative_y);
      }
    } else {
      pane.innerHTML = 'z = NA';
    }
};

rasterPicker.pick = function(event, layerId, bounds, digits, prefix, visible) {
  var outputWidget = this.getInfoLegend(layerId);
  if (!visible) {
    $(outputWidget).hide();
    return;
  }

  var rasterLayers = this.getRasterLayers(layerId, bounds);
  var pickedLayerData = {};
  // collect values of clicked raster layers
  var rasterHitInfos = this.getLayerIdHits(rasterLayers, event.latlng);
  // Return if nothing was found.
  if (rasterHitInfos.length === 0) {
    $(outputWidget).hide();
    return;
  }
  for (var rasterHitInfo_key in rasterHitInfos) {
    var rasterHitInfo = rasterHitInfos[rasterHitInfo_key];
    pickedLayerData[rasterHitInfo.layerId] = this.getLayerData(rasterHitInfo, event.latlng /*, event.zoom?*/);
  }
  // render collected hit values
  outputWidget.innerHTML = this.renderInfo(pickedLayerData, digits, prefix);
};

rasterPicker.getInfoLegend = function(layerId) {
  var element = window.document.getElementById("imageValues" + "-" + layerId);
  if (element === null) {
  // LOG ERROR or WARNING?
    console.log("leafem: No control widget found in Leaflet setup. Can't show layer info.");
  }
  return element;
};

rasterPicker.getRasterLayers = function(layerId, bounds) {
  var rasterLayers = [{
        layerId: layerId,
        bounds: bounds
      }];
  // TODO check if layer is hidden?
  return rasterLayers;
};

rasterPicker.getLayerIdHits = function(rasterLayers, latlng) {
  var layerHits = [];
  for (var raster_key in rasterLayers) {
    var raster = rasterLayers[raster_key];
    var upperLeft = raster.bounds[0],
        lowerRight = raster.bounds[1];
    // bounds values are [[upperLeft.lat, upperLeft.lng], [lowerRight.lat, lowerRight.lng]]
    var relative_y = (latlng.lat - upperLeft[0]) / (lowerRight[0] - upperLeft[0]);
    var relative_x = (latlng.lng - upperLeft[1] ) / (lowerRight[1] - upperLeft[1]);
    if (0 <= relative_y && relative_y <= 1 &&
        0 <= relative_x && relative_x <= 1) {
      raster.image_percent_x = relative_x;
      raster.image_percent_y = relative_y;
      layerHits.push(raster);
    }
  }
  return layerHits;
};

/*
 * var data[layerId] =
 */
rasterPicker.getLayerData = function(rasterHitInfo, latlng, zoom) {
  var layerData = {};
  layerData.layerId = rasterHitInfo.layerId;
  layerData.lat = latlng.lat;
  layerData.lng = latlng.lng;
  if (typeof(window.data) === "object") {
    var valueData = window.data[rasterHitInfo.layerId];
    if (valueData === undefined) {
      console.log("leafem: Failed to find data arrays for layerId '"+rasterHitInfo.layerId+"'");
    } else {
      var data_x = valueData.length,
          data_y = valueData[0].length;
      var ix = data_x*rasterHitInfo.image_percent_x,
          iy = data_y*rasterHitInfo.image_percent_y;
      layerData.index = { x: ix, y: iy };
      layerData.value = valueData[Math.floor(ix)][Math.floor(iy)];
    }
  } else {
    console.log("leafem: Failed to find window.data");
    layerData.value = "Lookup failed";
  }
  return layerData;
};

rasterPicker.renderInfo = function(pickedLayerData, digits, prefix) {
  var text = "";
  for (var layer_key in pickedLayerData) {
    var layer = pickedLayerData[layer_key];
    if (layer.value === undefined || layer.value === null) {
      continue;
    } else {
      $(document.getElementById("imageValues" + "-" + layer_key)).show();
    }
    if(digits === "null" || digits === null) {
      text += "<small>"+ prefix+ " <strong>"+ layer.layerId + ": </strong>"+ layer.value+ "</small>";
    } else {
      text += "<small>"+ prefix+ " <strong>"+ layer.layerId + ": </strong>"+ layer.value.toFixed(digits)+ "</small>";
    }
  }
  return text;
};
