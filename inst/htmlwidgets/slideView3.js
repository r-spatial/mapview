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
var spanMid;

HTMLWidgets.widget({

  name: 'slideView3',

  type: 'output',

  initialize: function(el, width, height) {
    return {}
  },

  renderValue: function(el, x, instance) {

    divInfo = document.createElement("div");
    divInfo.id ="divInfo";
    el.appendChild(divInfo);

    var spanLeft = document.createElement("span");
    spanLeft.id ="spanLeft";
    spanMid = document.createElement("span");
    spanMid.id ="spanMid";
    var spanRight = document.createElement("span");
    spanRight.id ="spanRight";

    divInfo.appendChild(spanLeft);
    divInfo.appendChild(spanMid);
    divInfo.appendChild(spanRight);

    spanLeft.innerHTML = "img1 &nbsp;&nbsp;&nbsp;";
    spanRight.innerHTML = "&nbsp;&nbsp;&nbsp; img2";

    spanMid.innerHTML = "?";

    var divDraw = document.createElement("div");
    divDraw.id ="divDraw";
    el.appendChild(divDraw);

    var canvasAfter = document.createElement("canvas");
    canvasAfter.id = "canvasAfter";
    divDraw.appendChild(canvasAfter);

    var divBefore = document.createElement("div");
    divBefore.id ="divBefore";
    divDraw.appendChild(divBefore);

    var canvasBefore = document.createElement("canvas");
    canvasBefore.id = "canvasBefore";
    divBefore.appendChild(canvasBefore);


    var filename1 = document.getElementById("image-1-attachment").href;
    var filename2 = document.getElementById("image-2-attachment").href;

    init(filename1, filename2);

    //alert(navigator.userAgent);
    //alert(navigator.product);


  },

  resize: function(el, width, height, instance) {
  }

});

function init(filename1, filename2) {
	var divDraw = document.getElementById("divDraw");
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
	var divBefore = document.getElementById("divBefore");
	if(drag) {
		var x = e.layerX;
		var y = e.layerY;
		offsetX += (x - dragX) / scale;
		offsetY += (y - dragY) / scale;
		dragX = x;
		dragY = y;
		draw();
	}
	/*rect (top, right, bottom, left)*/
	divBefore.style.clip = "rect(0px "+e.layerX+"px auto 0px)";

	var w = 150;
	var nx = e.layerX - (w / 2);
	if(nx<0) {
	  nx=0;
	}
	if(nx+w>window.innerWidth) {
	  nx=window.innerWidth - divInfo.offsetWidth;
	}



	//window.innerWidth
	divInfo.style.marginLeft = nx + "px";

}

function mousedown(e) {
	//if(e.buttons>0 || e.button!=undefined) {
	if(e.which>0) {
		dragX = e.layerX;
		dragY = e.layerY;
		drag = true;
	}
}

function mouseenter(e) {
	//mousedown(e);
}

function mouseup(e) {
	drag = false;
}

function mouseleave(e) {
	drag = false;
}

function keydown(e) {
  t = e;
  console.log(e.which);
  //if(e.key=='+') {
    // wheel({layerX:0, layerY:0, deltaY:1})
  //}

  if(e.which==32) { // space bar
	  crisp = !crisp;
	  draw();
  }

  if(e.which==13) { // enter key
    offsetX = 0;
    offsetY = 0;
    scale = 1;
    draw();
  }

}

function mousewheel(e) {  // RStudio
  e.deltaY = e.wheelDelta;
  wheel(e);
}

function wheel(e) {
	var x = e.layerX;
	var y = e.layerY;
	var offX = x/scale;
	var offY = y/scale;
	if(e.deltaY<0) {
		scale /= 1.25;
	} else if(e.deltaY>0) {
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
	var canvasBefore = document.getElementById("canvasBefore");
	var canvasAfter = document.getElementById("canvasAfter");
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