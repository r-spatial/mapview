/* global LeafletWidget, L, $ */
LeafletWidget.methods.addLogo = function(img, layerId, options) {
  (function() {
    var map = this;

    // Initialize logos array if not already present
    if (!map.logos) {
      map.logos = [];
    }

    var imgElement = document.createElement('img');
    imgElement.src = img;
    imgElement.class = options.class;
    if (options.width !== undefined) {
      imgElement.width = options.width;
    }
    if (options.height !== undefined) {
      imgElement.height = options.height;
    }

    // Create a new div for the logo
    var logoDiv = L.DomUtil.create('div', options.class);
    logoDiv.id = layerId;
    logoDiv.style.position = 'absolute';
    //logoDiv.style.width = options.width + 'px';
    //logoDiv.style.height = options.height + 'px';
    logoDiv.style.opacity = options.alpha;
    logoDiv.style.background = 'transparent';

    switch (options.position) {
      case 'topleft':
        logoDiv.style.top = options.offsetY + 'px';
        logoDiv.style.left = options.offsetX + 'px';
        break;
      case 'topright':
        logoDiv.style.top = options.offsetY + 'px';
        logoDiv.style.right = options.offsetX + 'px';
        break;
      case 'bottomleft':
        logoDiv.style.bottom = options.offsetY + 'px';
        logoDiv.style.left = options.offsetX + 'px';
        break;
      case 'bottomright':
        logoDiv.style.bottom = options.offsetY + 'px';
        logoDiv.style.right = options.offsetX + 'px';
        break;
    }

    // Create img-tag and append to document
    if (options.url) {
      var linkElement = document.createElement('a');
      linkElement.href = options.url;
      linkElement.target = "_blank";
      linkElement.appendChild(imgElement);
      logoDiv.appendChild(linkElement);
    } else {
      logoDiv.appendChild(imgElement);
    }

    // Remove an existing logo with the same layerId
    if (map.logos[layerId]) {
      map.logos[layerId].remove();
    }

    // Append Logo to Map and add to map.logos
    map.getContainer().appendChild(logoDiv);
    map.logos[layerId] = logoDiv;

  }).call(this);
};

LeafletWidget.methods.updateLogo = function(img, layerId) {
  var map = this;
  if (map.logos[layerId]) {
    var imgElement = $("#" + layerId + " img")[0];
    imgElement.src = img;
  }
}

LeafletWidget.methods.removeLogo = function(layerId) {
  var map = this;
  if (map.logos[layerId]) {
    map.logos[layerId].remove();
  }
}

LeafletWidget.methods.hideLogo = function(layerId) {
  var map = this;
  if (map.logos[layerId]) {
    $(map.logos[layerId]).hide();
  }
}

LeafletWidget.methods.showLogo = function(layerId) {
  var map = this;
  if (map.logos[layerId]) {
    $(map.logos[layerId]).show();
  }
}

