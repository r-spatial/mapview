var imageBefore = new Image();
var imageAfter = new Image();
var mousePosX = 0;

var offsetX = 0;
var offsetY = 0;

var scale = 1;

var drag = false;
var dragX = 0;
var dragY = 0;

var crisp = true;

var divInfo;
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

    divInfo = document.createElement("div");
    divInfo.id ="divInfo";
    el.appendChild(divInfo);

    spanLeft = document.createElement("span");
    spanLeft.id ="spanLeft";
    spanMid = document.createElement("span");
    spanMid.id ="spanMid";
    spanRight = document.createElement("span");
    spanRight.id ="spanRight";

    divInfo.appendChild(spanLeft);
    divInfo.appendChild(spanMid);
    divInfo.appendChild(spanRight);

    spanLeft.innerHTML = "img1 &nbsp;&nbsp;&nbsp;";
    spanRight.innerHTML = "&nbsp;&nbsp;&nbsp; img2";

    spanMid.innerHTML = "?";

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


    var filename1 = document.getElementById("image-1-attachment").href;
    var filename2 = document.getElementById("image-2-attachment").href;

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

	imageBefore.onload = draw;
	imageAfter.onload = draw;
	imageBefore.src = filename1;
	imageAfter.src = filename2;
}

function mousemove(e) {
	if(drag) {
		var x = e.layerX;
		var y = e.layerY;
		offsetX += (x - dragX) / scale;
		offsetY += (y - dragY) / scale;
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
	var nx = e.layerX - (w / 2);
	if(nx<0) {
	  nx = 0;
	}
	if(nx+w>window.innerWidth) {
	  nx = window.innerWidth - w;
	}

	divInfo.style.marginLeft = nx + "px";
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
    wheel({layerX:0, layerY:0, deltaY:+1})
  }

  if(e.which==107 || e.which==171 || e.which==187) { // plus key
    wheel({layerX:0, layerY:0, deltaY:-1})
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