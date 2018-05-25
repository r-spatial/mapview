HTMLWidgets.widget({
  name: 'plainView',
  type: 'output',
  initialize: function(el, width, height) {return {}},

  renderValue: function(el, x, instance) {
    var root = el;
    var filename = document.getElementById("image-1-attachment").href;
    var legend_filename = x.legend ? document.getElementById("image-2-attachment").href : undefined;
    var name = x.imgnm;
    var crs = x.crs;
    var dims = x.dims;
    init(root, filename, name, crs, dims, legend_filename);
  },

  resize: function(el, width, height, instance) {}
});

var rootNode;
var spanFactor;
var image;
var canvas;
var crisp = true;
var scale = 1;
var offsetX = 0;
var offsetY = 0;
var dragging = false;
var dragX = 0;
var dragY = 0;
var speed = 1;

function ca(root, name, text) {
  var e = document.createElement(name);
  root.appendChild(e);
  if(text!==undefined) {
    e.innerHTML = text;
  }
  return e;
}

function init(root, filename, name, crs, dims, legend_filename) {
  rootNode = root;
  var divInfo = ca(root, "div");
  divInfo.id = "divInfo";

  var divInfoCRS = ca(root, "div");
  divInfoCRS.id = "divInfoCRS";

  var divInfoRight = ca(root, "div");
  divInfoRight.id = "divInfoRight";

  var divInfoPos = ca(divInfoRight, "span");
  divInfoPos.id = "divInfoPos";
  var divInfoZoom = ca(divInfoRight, "span");
  divInfoZoom.id = "divInfoZoom";
  var divLegend = ca(root, "div");
  divLegend.id = "divLegend";

  ca(divInfoPos, "span", "Pos: ").className = "position";
  spanPos = ca(divInfoPos, "span", "?");
  spanPos.className = "position";
  spanPos.innerHTML = "???";
  ca(divInfoPos, "span", "&nbsp;&nbsp;");

  ca(divInfoZoom, "span", "Zoom: ").className = "zoom_factor";
  spanFactor = ca(divInfoZoom, "span", "?");
  spanFactor.className = "zoom_factor";

  ca(divInfo, "span", "&nbsp;&nbsp;");
  var spanName = ca(divInfo, "span", "Layer: " + name +
                    "  |  Dimensions (nrow, ncol, ncell): " + dims);
  spanName.className = "image_name";

  ca(divInfoCRS, "span", "&nbsp;&nbsp;");
  var spanCRS = ca(divInfoCRS, "span", "CRS: " + '"' + crs + '"');
  spanCRS.className = "image_crs";

  canvas = ca(root, "canvas");

  image = new Image();
  image.onload = init_image;
	image.src = filename;

	canvas.onmousedown = onmousedown;
	canvas.onmouseup = onmouseup;
	canvas.onmousemove = onmousemove;
	canvas.onwheel = onwheel;
	canvas.onmousewheel =  onmousewheel;

	window.addEventListener("keydown", onkeydown, true);

	if(legend_filename !== undefined) {
  	var legend_image = new Image();
  	legend_image.src = legend_filename;
  	divLegend.appendChild(legend_image);
	}
}

function init_image() {
  var iw = image.width;
  var ih = image.height;
  var cw = rootNode.clientWidth;
  var ch = rootNode.clientHeight;
  var fw = cw/iw;
  var fh = ch/ih;
  scale = fw<fh?fw:fh;
  var sw = iw*scale;
  var sh = ih*scale;
  offsetX = sw<cw?(cw-sw)/scale/2:0;
  offsetY = sh<ch?(ch-sh)/scale/2:0;
  draw();
}

function draw() {
  if(canvas.width != window.innerWidth || canvas.height != window.innerHeight) {
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
  }
	var ctx = canvas.getContext("2d");
	ctx.imageSmoothingEnabled = !crisp;
	ctx.webkitImageSmoothingEnabled = !crisp;
	ctx.imageSmoothingEnabled  = !crisp;
	ctx.setTransform(1, 0, 0, 1, 0, 0);
	ctx.clearRect(0, 0, canvas.width, canvas.height);
	ctx.scale(scale, scale);
	ctx.translate(offsetX, offsetY);
	ctx.drawImage(image,0,0);
	spanFactor.innerHTML = scale.toFixed(2);
}

function onmousedown(e) {
  dragging = true;
  canvas.style.cursor = "move";
  var rect = canvas.getBoundingClientRect();
  dragX = e.clientX - rect.left;
  dragY = e.clientY - rect.top;
}

function onmouseup(e) {
  dragging = false;
  canvas.style.cursor = "default";
}

function onmousemove(e) {
  var rect = canvas.getBoundingClientRect();
  var x = e.clientX - rect.left;
  var y = e.clientY - rect.top;
  var px = x / scale - offsetX;
  var py = y / scale - offsetY;
  spanPos.innerHTML = Math.ceil(px) + ", " + Math.ceil(py);

  if(e.which === 0) {
    dragging = false;
    canvas.style.cursor = "default";
    return;
  }
  if(dragging) {
		offsetX += (x - dragX)*speed / scale;
		offsetY += (y - dragY)*speed / scale;
		dragX = x;
		dragY = y;
		draw();
    canvas.style.cursor = "grabbing";
	} else {
	  canvas.style.cursor = "default";
	}
}

function onzoom(x, y, zoomin) {
  var offX = x/scale;
	var offY = y/scale;

	if(zoomin) {
    scale *= 1.25;
  } else {
    scale /= 1.25;
  }

	var newX = x/scale;
	var newY = y/scale;
	offsetX -= offX - newX;
	offsetY -= offY - newY;

	draw();
}

function onwheel(e) {
  var rect = canvas.getBoundingClientRect();
  var x = e.clientX - rect.left;
  var y = e.clientY - rect.top;
  onzoom(x, y, e.deltaY<0);
}

function onmousewheel(e) {
  var rect = canvas.getBoundingClientRect();
  var x = e.clientX - rect.left;
  var y = e.clientY - rect.top;
  onzoom(x, y, e.wheelDelta>0);
}

function onkeydown(e) {

  if(e.which==109 || e.which==173 || e.which==189) { // minus key
    var cw = rootNode.clientWidth;
    var ch = rootNode.clientHeight;
    onzoom(cw/2, ch/2, false);
  }

  if(e.which==107 || e.which==171 || e.which==187) { // plus key
    var cw = rootNode.clientWidth;
    var ch = rootNode.clientHeight;
    onzoom(cw/2, ch/2, true);
  }

  if(e.which==32) { // space bar
	  crisp = !crisp;
	  draw();
  }

  if(e.which==27) { // escape
    init_image();
  }

  if(e.which==13) { // enter key
    var iw = image.width;
    var ih = image.height;
    var cw = rootNode.clientWidth;
    var ch = rootNode.clientHeight;
    var fw = cw/iw;
    var fh = ch/ih;
    scale = 1;
    var sw = iw*scale;
    var sh = ih*scale;
    offsetX = sw<cw?(cw-sw)/scale/2:0;
    offsetY = sh<ch?(ch-sh)/scale/2:0;
    draw();
  }

  if(e.which==17) { // control key
    speed=speed==1?10:1;
  }

}



