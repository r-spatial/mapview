var imageBefore = new Image();
var imageAfter = new Image();
var mousePosX = 0;

var offsetX = 0;
var offsetY = 0;

var scale = 1;

var drag = false;
var dragX = 0;
var dragY = 0;
var speed = 1;

var crisp = true;

var rootNode;
var divInfoSlide;
var divDraw;
var divBefore;

var spanLeft;
var spanMid;
var spanRight;

var canvasBefore;
var canvasAfter;

HTMLWidgets.widget({

  name: 'slideView',

  type: 'output',

  initialize: function(el, width, height) {
    return {}
  },

  renderValue: function(el, x, instance) {
    rootNode = el;
    var fldrnm = x.fldrnm

    var filename1 = document.getElementById(fldrnm.concat("-imager-attachment")).href;
    var filename2 = document.getElementById(fldrnm.concat("-imagel-attachment")).href;

    var legendr_filename = x.legend && document.getElementById(fldrnm.concat("-imager-attachment")) !== null ? document.getElementById(fldrnm.concat("-imager-attachment")).href : undefined;
    var legendl_filename = x.legend && document.getElementById(fldrnm.concat("-imager-attachment")) !== null ? document.getElementById(fldrnm.concat("-imagel-attachment")).href : undefined;

    divInfoSlide = document.createElement("div");
    divInfoSlide.id ="divInfoSlide";
    el.appendChild(divInfoSlide);

    spanLeft = document.createElement("span");
    spanLeft.id ="spanLeft";
    spanLeft.className = "slide";
    spanMid = document.createElement("span");
    spanMid.id ="spanMid";
    spanMid.className ="slide";
    spanRight = document.createElement("span");
    spanRight.id ="spanRight";
    spanRight.className = "slide";

    divInfoSlide.appendChild(spanLeft);
    divInfoSlide.appendChild(spanMid);
    divInfoSlide.appendChild(spanRight);

    spanLeft.innerHTML = x.img1+"&nbsp;&nbsp;&nbsp;";
    spanRight.innerHTML = "&nbsp;&nbsp;&nbsp;"+x.img2;

    spanMid.innerHTML = "?";

    if(legendr_filename !== undefined) {
      var divLegendr = document.createElement("div");
      divLegendr.id ="divLegendr";
      el.appendChild(divLegendr);
    	var legendr_image = new Image();
    	legendr_image.src = legendr_filename;
    	divLegendr.appendChild(legendr_image);
	  }

	  if(legendl_filename !== undefined) {
      var divLegendl = document.createElement("div");
      divLegendl.id ="divLegendl";
      el.appendChild(divLegendl);
    	var legendl_image = new Image();
    	legendl_image.src = legendl_filename;
    	divLegendl.appendChild(legendl_image);
	  }

    divDraw = document.createElement("div");
    divDraw.id ="divDraw";
    divDraw.style.cursor = "col-resize";
    el.appendChild(divDraw);

    canvasAfter = document.createElement("canvas");
    canvasAfter.id = "canvasAfter";
    divDraw.appendChild(canvasAfter);

    divBefore = document.createElement("div");
    divBefore.id ="divBefore";
    divDraw.appendChild(divBefore);

    canvasBefore = document.createElement("canvas");
    canvasBefore.id = "canvasBefore";
    divBefore.appendChild(canvasBefore);

    init(filename1, filename2);
  },

  resize: function(el, width, height, instance) {
  }

});

function init(filename1, filename2) {
	divDraw.onmousemove = mousemove;
	divDraw.onmousedown = mousedown;
	divDraw.onmouseenter = mouseenter;
	divDraw.onmouseup = mouseup;
	divDraw.onmouseleave = mouseleave;
	divDraw.onwheel = wheel;
	divDraw.onmousewheel =  mousewheel; // RStudio
	window.addEventListener("keydown", keydown, true);

	imageBefore.onload = init_image;
	imageAfter.onload = draw;
	imageBefore.src = filename1;
	imageAfter.src = filename2;
}

function mousemove(e) {
	if(drag) {
		var x = e.layerX;
		var y = e.layerY;
		offsetX += (x - dragX)*speed / scale;
		offsetY += (y - dragY)*speed / scale;
		dragX = x;
		dragY = y;
		draw();
    divDraw.style.cursor = "grabbing";
	} else {
	  divDraw.style.cursor = "col-resize";
	}
	/*rect (top right bottom left)*/
	divBefore.style.clip = "rect(0px "+e.layerX+"px auto 0px)";

	var w = spanLeft.offsetWidth + spanMid.offsetWidth + spanRight.offsetWidth;
	var m = spanLeft.offsetWidth + (spanMid.offsetWidth/2);
	var nx = e.layerX - m;
	if(nx<0) {
	  nx = 0;
	}
	if(nx+w>window.innerWidth) {
	  nx = window.innerWidth - w;
	}

	divInfoSlide.style.marginLeft = nx + "px";
}

function mousedown(e) {
	if(e.which>0) {
		dragX = e.layerX;
		dragY = e.layerY;
		drag = true;
    divDraw.style.cursor = "move";
	}
}

function mouseenter(e) {
	//mousedown(e);
}

function mouseup(e) {
	drag = false;
	divDraw.style.cursor = "col-resize";
}

function mouseleave(e) {
	drag = false;
}

function keydown(e) {

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

  if(e.which==13) { // enter key
    var iw = imageBefore.width;
    var ih = imageBefore.height;
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

  if(e.which==27) { // escape
    init_image();
  }

/*  if(e.which==13) { // enter key
    offsetX = 0;
    offsetY = 0;
    scale = 1;
    draw();
  }
*/
  if(e.which==17) { // control key
    speed=speed==1?10:1;
  }

}

function mousewheel(e) {  // RStudio
  e.deltaY = -e.wheelDelta;
  wheel(e);
}

function wheel(e) {
	var x = e.layerX;
	var y = e.layerY;
	var offX = x/scale;
	var offY = y/scale;
	if(e.deltaY>0) {
		scale /= 1.25;
	} else if(e.deltaY<0) {
		scale *= 1.25;
	}
	var newX = x/scale;
	var newY = y/scale;
	offsetX -= offX - newX;
	offsetY -= offY - newY;
	draw();
}

function init_image() {
  var iw = imageBefore.width;
  var ih = imageBefore.height;
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
  spanMid.innerHTML = "x"+scale.toFixed(2);
	drawLayer(canvasBefore,imageBefore);
	drawLayer(canvasAfter,imageAfter);
}

function drawLayer(canvas,image) {
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
}
