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

HTMLWidgets.widget({

  name: 'slideView3',

  type: 'output',

  initialize: function(el, width, height) {

    return {
      // TODO: add instance fields as required
    }

  },

  renderValue: function(el, x, instance) {

    var divDraw = document.createElement("divDraw");
    divDraw.id ="divDraw";
    el.appendChild(divDraw);

    var canvasAfter = document.createElement("canvas");
    canvasAfter.id = "canvasAfter";
    divDraw.appendChild(canvasAfter);

    var divBefore = document.createElement("divBefore");
    divBefore.id ="divBefore";
    divDraw.appendChild(divBefore);

    var canvasBefore = document.createElement("canvas");
    canvasBefore.id = "canvasBefore";
    divBefore.appendChild(canvasBefore);


    var filename1 = document.getElementById("test-1-attachment").href;
    var filename2 = document.getElementById("test-2-attachment").href;



    //initD3();
    //updatePhotos(filename1, filename2);


    //el.style.width = "100%";
    //el.style.height = "100%";

    init(filename1, filename2);

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
    //wheel({layerX:0, layerY:0, deltaY:1})
  //}

  if(e.which==32) {
	  crisp = !crisp;
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