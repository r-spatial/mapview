HTMLWidgets.widget({
  name: 'plainView',
  type: 'output',
  initialize: function(el, width, height) {return {}},

  renderValue: function(el, x, instance) {
    var root = el;
    var filename = document.getElementById("image-1-attachment").href;
    var name = x.imgnm;
    init(root, filename, name);
  },

  resize: function(el, width, height, instance) {}
});

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

function init(root, filename, name) {
  var divInfo = ca(root, "div");
  divInfo.id = "divInfo";
  ca(divInfo, "span", "x").className = "zoom_factor";
  spanFactor = ca(divInfo, "span", "?");
  spanFactor.className = "zoom_factor";
  ca(divInfo, "span", "&nbsp;&nbsp;");
  var spanName = ca(divInfo, "span", name);
  spanName.className = "image_name";
  canvas = ca(root, "canvas");

  image = new Image();
  image.onload = draw;
	image.src = filename;

	canvas.onmousedown = onmousedown;
	canvas.onmouseup = onmouseup;
	canvas.onmousemove = onmousemove;
	canvas.onwheel = onwheel;
	canvas.onmousewheel =  onmousewheel;

	window.addEventListener("keydown", onkeydown, true);
}

function draw() {
  if(canvas.width != window.innerWidth || canvas.height != window.innerHeight) {
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
  }
	var ctx = canvas.getContext("2d");
	ctx.mozImageSmoothingEnabled = !crisp;
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
  if(e.which==0) {
    dragging = false;
    canvas.style.cursor = "default";
    return;
  }
  if(dragging) {
    var rect = canvas.getBoundingClientRect();
    var x = e.clientX - rect.left;
    var y = e.clientY - rect.top;
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
    onzoom(0, 0, false);
  }

  if(e.which==107 || e.which==171 || e.which==187) { // plus key
    onzoom(0, 0, true);
  }

  if(e.which==32) { // space bar
	  crisp = !crisp;
	  draw();
  }

  if(e.which==27 || e.which==13) { // escape or enter key
    offsetX = 0;
    offsetY = 0;
    scale = 1;
    draw();
  }

}



