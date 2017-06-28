HTMLWidgets.widget({
  name: 'cubeView',
  type: 'output',
  initialize: function(el, width, height) {
    return {};
  },

  renderValue: function(root, json, instance) {
    var legend_filename = json.legend ? document.getElementById("images-legend-attachment").href : undefined;
    init(root, json, legend_filename);
  },

  resize: function(el, width, height, instance) {
  }

});

var hovmoeller;

var X_SIZE;
var Y_SIZE;
var Z_SIZE;
var XY_SIZE;
var XZ_SIZE;
var ZY_SIZE;
var XYZ_SIZE;
var dR;
var dG;
var dB;

var x_pos = 0;
var y_pos = 0;
var z_pos = 0;

var statusX;
var statusY;
var statusZ;

var show_cross_section_lines = true;

function init(root, json, legend_filename) {
	hovmoeller = new Hovmoeller(root, json, legend_filename);
}

function iDiv(a,b) {
	return (a/b>>0);
}

function b64toArray(data) {
  var byteString = atob(data);
	var buffer = new Uint8Array(byteString.length);
	for (var i = 0; i < byteString.length; i++) {
		buffer[i] = byteString.charCodeAt(i);
	}
	return buffer;
}

function flipY(data) {
  var buffer = new Uint8Array(XYZ_SIZE);
  var yMax = Y_SIZE-1;
  for(var z=0;z<Z_SIZE;z++) {
    var zBase = z*XY_SIZE;
    for(var y=0;y<Y_SIZE;y++) {
      var yBase = zBase+y*X_SIZE;
      var yBaseFlip = zBase+(yMax-y)*X_SIZE;
      for(var x=0;x<X_SIZE;x++) {
        buffer[yBase+x] = data[yBaseFlip+x];
        //buffer[yBase+x] = data[yBase+x];
      }
    }
  }
  return buffer;
}

function Hovmoeller(root, json, legend_filename) {
	X_SIZE = json.x_size;
  Y_SIZE = json.y_size;
  Z_SIZE = json.z_size;
  XY_SIZE = X_SIZE*Y_SIZE;
  XZ_SIZE = X_SIZE*Z_SIZE;
  ZY_SIZE = Z_SIZE*Y_SIZE;
  XYZ_SIZE = X_SIZE*Y_SIZE*Z_SIZE;
  x_pos = 0;
  y_pos = 0;
  z_pos = 0;


  if (json.grey !== undefined) {
    dR = dG = dB = flipY(b64toArray(json.grey));
  }

  if (json.red !== undefined) {
    dR = flipY(b64toArray(json.red));
  }

  if (json.green !== undefined) {
    dG = flipY(b64toArray(json.green));
  }

  if (json.blue !== undefined) {
    dB = flipY(b64toArray(json.blue));
  }

	this.scene = new THREE.Scene();
	this.camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 0.1, 1000);

	this.renderer = new THREE.WebGLRenderer({/*antialias: true*/});
	this.renderer.setSize(window.innerWidth, window.innerHeight);

  this.controls = new THREE.TrackballControls(this.camera);

  this.controls.rotateSpeed = 3.0;
	this.controls.zoomSpeed = 1.0;
	this.controls.panSpeed = 1.0;
  this.controls.staticMoving = true;

	var self = this;
	this.controls.addEventListener('change', function mov() {self.render();});

	root.innerHTML = "";
	if(legend_filename !== undefined) {
	  var divLegend = document.createElement("div");
	  divLegend.id = "divLegend";
  	var legend_image = new Image();
  	legend_image.src = legend_filename;
  	divLegend.appendChild(legend_image);
  	var divStatus = document.createElement("div");
  	var labelX = document.createElement("span");
  	var labelY = document.createElement("span");
  	var labelZ = document.createElement("span");
  	labelX.innerHTML = "X&nbsp;";
  	labelY.innerHTML = "&nbsp;Y&nbsp;";
  	labelZ.innerHTML = "&nbsp;Z&nbsp;";
  	statusX = document.createElement("span");
  	statusY = document.createElement("span");
  	statusZ = document.createElement("span");
  	statusX.innerHTML = "x";
  	statusY.innerHTML = "y";
  	statusZ.innerHTML = "z";
  	divStatus.appendChild(labelX);
  	divStatus.appendChild(statusX);
  	divStatus.appendChild(labelY);
  	divStatus.appendChild(statusY);
  	divStatus.appendChild(labelZ);
  	divStatus.appendChild(statusZ);
  	divLegend.appendChild(divStatus);
  	root.appendChild(divLegend);
	}
	root.appendChild(this.renderer.domElement);

	var MAX_SIZE = Math.max(X_SIZE, Y_SIZE, Z_SIZE);
	var X_BOX = X_SIZE/MAX_SIZE;
	var Y_BOX = Y_SIZE/MAX_SIZE;
	var Z_BOX = Z_SIZE/MAX_SIZE;
	var MIN_RATIO = 0.1;
	if(X_BOX<MIN_RATIO) {
	  X_BOX = MIN_RATIO;
	}
	if(Y_BOX<MIN_RATIO) {
	  Y_BOX = MIN_RATIO;
	}
	if(Z_BOX<MIN_RATIO) {
	  Z_BOX = MIN_RATIO;
	}

	var geometry = new THREE.BoxGeometry(X_BOX, Y_BOX,  Z_BOX);

	geometry.faceVertexUvs =
		[[
			[{"x":0,"y":1},{"x":0,"y":0},{"x":1,"y":1}],[{"x":0,"y":0},{"x":1,"y":0},{"x":1,"y":1}], //RIGHT
			[{"x":1,"y":1},{"x":1,"y":0},{"x":0,"y":1}],[{"x":1,"y":0},{"x":0,"y":0},{"x":0,"y":1}], //LEFT x-mirror
			[{"x":0,"y":1},{"x":0,"y":0},{"x":1,"y":1}],[{"x":0,"y":0},{"x":1,"y":0},{"x":1,"y":1}], //UP
			[{"x":0,"y":0},{"x":0,"y":1},{"x":1,"y":0}],[{"x":0,"y":1},{"x":1,"y":1},{"x":1,"y":0}], //DOWN y-mirror
			[{"x":0,"y":1},{"x":0,"y":0},{"x":1,"y":1}],[{"x":0,"y":0},{"x":1,"y":0},{"x":1,"y":1}], //FRONT
			[{"x":1,"y":1},{"x":1,"y":0},{"x":0,"y":1}],[{"x":1,"y":0},{"x":0,"y":0},{"x":0,"y":1}]  //BACK x-mirror
		]];

	this.materialXY = new THREE.MeshBasicMaterial({});
	this.materialXZ = new THREE.MeshBasicMaterial({});
	this.materialZY = new THREE.MeshBasicMaterial({});
	this.materials = [this.materialZY, this.materialZY, this.materialXZ, this.materialXZ , this.materialXY, this.materialXY];

	var format = THREE.RGBAFormat;
	var type = THREE.UnsignedByteType;
	var mapping = THREE.UVMapping;
	var wrapS = THREE.ClampToEdgeWrapping;
	var wrapT = THREE.ClampToEdgeWrapping;
	var magFilter = THREE.NearestFilter;
	var minFilter = THREE.NearestFilter;
	var anisotropy = 0;

	this.materialXY.map = new THREE.DataTexture(new Uint8Array(XY_SIZE*4), X_SIZE, Y_SIZE, format, type, mapping, wrapS, wrapT, magFilter, minFilter, anisotropy);
	this.materialXZ.map = new THREE.DataTexture(new Uint8Array(XZ_SIZE*4), X_SIZE, Z_SIZE, format, type, mapping, wrapS, wrapT, magFilter, minFilter, anisotropy);
	this.materialZY.map = new THREE.DataTexture(new Uint8Array(ZY_SIZE*4), Z_SIZE, Y_SIZE, format, type, mapping, wrapS, wrapT, magFilter, minFilter, anisotropy);


	this.cube = new THREE.Mesh(geometry, new THREE.MeshFaceMaterial(this.materials));
	this.scene.add(this.cube);

	var line_material_x = new THREE.LineBasicMaterial({color:0xff5555, linewidth:3});
	var line_material_y = new THREE.LineBasicMaterial({color:0x55ff55, linewidth:3});
	var line_material_z = new THREE.LineBasicMaterial({color:0x5555ff, linewidth:3});

	var origin_gap = 0.05;
	var origin_x = -X_BOX/2-origin_gap;
	var origin_y = -Y_BOX/2-origin_gap;
	var origin_z = -Z_BOX/2-origin_gap;
	var origin = new THREE.Vector3(origin_x, origin_y, origin_z);
	var line_size = 0.05 + origin_gap;

  var line_geometry_x = new THREE.Geometry();
  line_geometry_x.vertices.push(
	  origin,
	  new THREE.Vector3(origin_x+line_size, origin_y, origin_z)
  );

  var line_geometry_y = new THREE.Geometry();
  line_geometry_y.vertices.push(
	  origin,
	  new THREE.Vector3(origin_x, origin_y+line_size, origin_z)
  );

  var line_geometry_z = new THREE.Geometry();
  line_geometry_z.vertices.push(
	  origin,
	  new THREE.Vector3(origin_x, origin_y, origin_z+line_size)
  );

  var line_x = new THREE.Line(line_geometry_x, line_material_x);
  var line_y = new THREE.Line(line_geometry_y, line_material_y);
  var line_z = new THREE.Line(line_geometry_z, line_material_z);
  this.scene.add(line_x);
  this.scene.add(line_y);
  this.scene.add(line_z);

	this.camera.position.z = 1.5;

	document.body.addEventListener('keydown', this.onKeyDown, false);

	this.updateMaterial();
	this.animate();
}

Hovmoeller.prototype = {

render: function () {
	 this.renderer.render(this.scene, this.camera);
},

animate: function() {
  var self = this;
	function mov() {self.animate();}
	requestAnimationFrame(mov);
	this.controls.update();
},

updateMaterial: function () {
	this.updateMaterialXY();
	this.updateMaterialXZ();
	this.updateMaterialZY();
	statusX.innerHTML = x_pos+1;
  statusY.innerHTML = y_pos+1;
  statusZ.innerHTML = z_pos+1;
},

updateMaterialXY: function () {
	var data = this.materialXY.map.image.data;
	var width = X_SIZE;
	var height = Y_SIZE;
	var base = z_pos*XY_SIZE;
	for(var y=0;y<Y_SIZE;y++) {
		var y_base = y*X_SIZE;
		for(var x=0;x<X_SIZE;x++) {
			var i = y_base+x;
			var tex_b = i*4;
			var b = i+base;
			data[tex_b] = dR[b];
			data[tex_b+1] = dG[b];
			data[tex_b+2] = dB[b];
			data[tex_b+3] = 255;
			if(show_cross_section_lines && (y==y_pos || x==x_pos)) {
				data[tex_b] = ~data[tex_b];
				data[tex_b+1] = ~data[tex_b+1];
				data[tex_b+2] = ~data[tex_b+2];
			}
		}
	}
	this.materialXY.map.needsUpdate = true;
	this.render();
},

updateMaterialXZ: function () {
	var data = this.materialXZ.map.image.data;
	for(var z=0;z<Z_SIZE;z++) {
		var base = z*XY_SIZE+y_pos*X_SIZE;
		var tex_base = z*X_SIZE;
		for(var x=0;x<X_SIZE;x++) {
			var tex_b = (tex_base+x)*4;
			var b = base+x;
			data[tex_b] = dR[b];
			data[tex_b+1] = dG[b];
			data[tex_b+2] = dB[b];
			data[tex_b+3] = 255;
			if(show_cross_section_lines && (z==z_pos || x==x_pos)) {
				data[tex_b] = ~data[tex_b];
				data[tex_b+1] = ~data[tex_b+1];
				data[tex_b+2] = ~data[tex_b+2];
			}
		}
	}
	this.materialXZ.map.needsUpdate = true;
	this.render();
},

updateMaterialZY: function () {
	var data = this.materialZY.map.image.data;
	for(var z=0;z<Z_SIZE;z++) {
		var base = z*XY_SIZE;
		for(var y=0;y<Y_SIZE;y++) {
			var tex_base = y*Z_SIZE;
			var tex_b = (tex_base+z)*4;
			var b = base+y*X_SIZE+x_pos;
			data[tex_b] = dR[b];
			data[tex_b+1] = dG[b];
			data[tex_b+2] = dB[b];
			data[tex_b+3] = 255;
			if(show_cross_section_lines && (z==z_pos || y==y_pos)) {
				data[tex_b] = ~data[tex_b];
				data[tex_b+1] = ~data[tex_b+1];
				data[tex_b+2] = ~data[tex_b+2];
			}
		}
	}
	this.materialZY.map.needsUpdate = true;
	this.render();
},

onKeyDown: function(e) {
	switch (e.keyCode) {
		case 33: // PAGE_UP
      z_pos = z_pos===0?0:z_pos-1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 34: // PAGE_DOWN
      z_pos = z_pos===Z_SIZE-1?Z_SIZE-1:z_pos+1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 39: //RIGHT
			x_pos =  x_pos===X_SIZE-1?X_SIZE-1:x_pos+1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 37: //LEFT
			x_pos = x_pos===0?0:x_pos-1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 38: //UP
			y_pos =  y_pos===Y_SIZE-1?Y_SIZE-1:y_pos+1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 40: //DOWN
			y_pos = y_pos===0?0:y_pos-1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 32: //SPACE
		  show_cross_section_lines = !show_cross_section_lines;
			break;
		default:
			//console.log(e.keyCode);
	}
	hovmoeller.updateMaterial();
}

}
