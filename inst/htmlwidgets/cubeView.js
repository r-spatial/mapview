HTMLWidgets.widget({
  name: 'cubeView',
  type: 'output',
  initialize: function(el, width, height) {
    return {}
  },

  renderValue: function(root, json, instance) {
    init(root, json);
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

var x_pos = iDiv(X_SIZE*2,3);
var y_pos = iDiv(Y_SIZE*2,3);
var z_pos = iDiv(Z_SIZE*2,3);

/*
var X_SIZE = 41;
var Y_SIZE = 97;
var Z_SIZE = 13;
var XY_SIZE = X_SIZE*Y_SIZE;
var XZ_SIZE = X_SIZE*Z_SIZE;
var ZY_SIZE = Z_SIZE*Y_SIZE;
var XYZ_SIZE = X_SIZE*Y_SIZE*Z_SIZE;
var dR = new Uint8Array(XYZ_SIZE);
var dG = new Uint8Array(XYZ_SIZE);
var dB = new Uint8Array(XYZ_SIZE);

var x_pos = iDiv(X_SIZE*2,3);
var y_pos = iDiv(Y_SIZE*2,3);
var z_pos = iDiv(Z_SIZE*2,3);*/

function init(root, json) {
	hovmoeller = new Hovmoeller(root, json);
}

function iDiv(a,b) {
	return (a/b>>0);
}

function b64toArray(data) {
  console.log("base46decode...");
  var byteString = atob(data);
	var buffer = new Uint8Array(byteString.length);
	for (var i = 0; i < byteString.length; i++) {
		buffer[i] = byteString.charCodeAt(i);
	}
	console.log("base46decode done.");
	return buffer;
}

function Hovmoeller(root, json) {

  this.orbit = true;

	/*for(var z=0;z<Z_SIZE;z++) { // example data: full color cube
		var z_base = z * XY_SIZE;
		for(var y=0;y<Y_SIZE;y++) {
			var y_base = z_base + y*X_SIZE;
			for(var x=0;x<X_SIZE;x++) {
				var i = y_base+x;
				dR[i] = (x*255)/X_SIZE;
				dG[i] = (y*255)/Y_SIZE;
				dB[i] = (z*255)/Z_SIZE;
			}
		}
	}*/

	X_SIZE = json.x_size;
  Y_SIZE = json.y_size;
  Z_SIZE = json.z_size;
  XY_SIZE = X_SIZE*Y_SIZE;
  XZ_SIZE = X_SIZE*Z_SIZE;
  ZY_SIZE = Z_SIZE*Y_SIZE;
  XYZ_SIZE = X_SIZE*Y_SIZE*Z_SIZE;
  x_pos = iDiv(X_SIZE*2,3);
  y_pos = iDiv(Y_SIZE*2,3);
  z_pos = iDiv(Z_SIZE*2,3);


  if (json.grey !== undefined) {
    dR = dG = dB = b64toArray(json.grey);
  }

  if (json.red !== undefined) {
    dR = b64toArray(json.red);
  }

  if (json.green !== undefined) {
    dG = b64toArray(json.green);
  }

  if (json.blue !== undefined) {
    dB = b64toArray(json.blue);
  }

  /*dR = new Uint8Array(XYZ_SIZE);
  dG = new Uint8Array(XYZ_SIZE);
  dB = new Uint8Array(XYZ_SIZE);


  if (json.grey !== undefined) {
    var s = b64toArray(json.grey);
    for(var i=0;i<XYZ_SIZE;i++) {
      dR[i] = s[i];
      dG[i] = s[i];
      dB[i] = s[i];
    }
  }

  if (json.red !== undefined) {
    var s = b64toArray(json.red);
    for(var i=0;i<XYZ_SIZE;i++) {
      dR[i] = s[i];
    }
  }

  if (json.green !== undefined) {
    var s = b64toArray(json.green);
    for(var i=0;i<XYZ_SIZE;i++) {
      dG[i] = s[i];
    }
  }

  if (json.blue !== undefined) {
    var s = b64toArray(json.blue);
    for(var i=0;i<XYZ_SIZE;i++) {
      dB[i] = s[i];
    }
  }*/

	this.scene = new THREE.Scene();
	this.camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.1, 1000 );

	this.renderer = new THREE.WebGLRenderer( {/*antialias: true*/} );
	this.renderer.setSize( window.innerWidth, window.innerHeight );
	//console.log(this.renderer.getMaxAnisotropy());

	if(this.orbit) {
	  this.controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);
	  this.controls.enableKeys = false; // prevents using arrow keys in OrbitControls
	} else {
	  this.controls = new THREE.TrackballControls(this.camera); // currently not functioning

	  this.controls.rotateSpeed = 1.0;
		this.controls.zoomSpeed = 1.2;
		this.controls.panSpeed = 0.8;

		this.controls.noZoom = false;
		this.controls.noPan = false;

		this.controls.staticMoving = true;
		this.controls.dynamicDampingFactor = 0.3;

		this.controls.keys = [ 65, 83, 68 ];

		this.controls.addEventListener( 'change', function mov() {hovmoeller.render();} );
	}

	root.innerHTML = "";
	root.appendChild( this.renderer.domElement );

	var MIN_SIZE = Math.min(X_SIZE, Y_SIZE, Z_SIZE);
	var geometry = new THREE.BoxGeometry( X_SIZE/MIN_SIZE, Y_SIZE/MIN_SIZE, Z_SIZE/MIN_SIZE );

	//console.log(JSON.stringify(this.cube.geometry.faceVertexUvs));

	geometry.faceVertexUvs =
		[[
			[{"x":0,"y":1},{"x":0,"y":0},{"x":1,"y":1}],[{"x":0,"y":0},{"x":1,"y":0},{"x":1,"y":1}], //RIGHT
			[{"x":1,"y":1},{"x":1,"y":0},{"x":0,"y":1}],[{"x":1,"y":0},{"x":0,"y":0},{"x":0,"y":1}], //LEFT x-mirror
			[{"x":0,"y":1},{"x":0,"y":0},{"x":1,"y":1}],[{"x":0,"y":0},{"x":1,"y":0},{"x":1,"y":1}], //UP
			[{"x":0,"y":0},{"x":0,"y":1},{"x":1,"y":0}],[{"x":0,"y":1},{"x":1,"y":1},{"x":1,"y":0}], //DOWN y-mirror
			[{"x":0,"y":1},{"x":0,"y":0},{"x":1,"y":1}],[{"x":0,"y":0},{"x":1,"y":0},{"x":1,"y":1}], //FRONT
			[{"x":1,"y":1},{"x":1,"y":0},{"x":0,"y":1}],[{"x":1,"y":0},{"x":0,"y":0},{"x":0,"y":1}]  //BACK x-mirror
		]];

	this.materialXY = new THREE.MeshBasicMaterial( {} );
	this.materialXZ = new THREE.MeshBasicMaterial( {} );
	this.materialZY = new THREE.MeshBasicMaterial( {} );
	this.materials = [this.materialZY, this.materialZY, this.materialXZ, this.materialXZ , this.materialXY, this.materialXY];

	var format = THREE.RGBAFormat;
	var type = THREE.UnsignedByteType;
	var mapping = THREE.UVMapping;
	var wrapS = THREE.ClampToEdgeWrapping;
	var wrapT = THREE.ClampToEdgeWrapping;
	var magFilter = THREE.NearestFilter;
	var minFilter = THREE.NearestFilter;
	var anisotropy = 0;

	this.materialXY.map = new THREE.DataTexture( new Uint8Array(XY_SIZE*4), X_SIZE, Y_SIZE, format, type, mapping, wrapS, wrapT, magFilter, minFilter, anisotropy );
	this.materialXZ.map = new THREE.DataTexture( new Uint8Array(XZ_SIZE*4), X_SIZE, Z_SIZE, format, type, mapping, wrapS, wrapT, magFilter, minFilter, anisotropy );
	this.materialZY.map = new THREE.DataTexture( new Uint8Array(ZY_SIZE*4), Z_SIZE, Y_SIZE, format, type, mapping, wrapS, wrapT, magFilter, minFilter, anisotropy );


	this.cube = new THREE.Mesh( geometry,  new THREE.MeshFaceMaterial(this.materials) );
	this.scene.add( this.cube );

	this.camera.position.z = 15;

	document.body.addEventListener( 'keydown', this.onKeyDown, false );

	//console.log(JSON.stringify(this.cube.geometry.faceVertexUvs));

	this.updateMaterial();
	this.render();
}

Hovmoeller.prototype = {

render: function () {
	if(this.orbit) {
	  this.renderer.render( this.scene, this.camera );
	} else {
	  this.renderer.render( this.scene, this.camera );
	  this.controls.update();
	}
	var self = this;
	function mov() {self.render();}
	requestAnimationFrame( mov );
},

updateMaterial: function () {
	this.updateMaterialXY();
	this.updateMaterialXZ();
	this.updateMaterialZY();
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
			if(y==y_pos || x==x_pos) {
				data[tex_b] = ~data[tex_b];
				data[tex_b+1] = ~data[tex_b+1];
				data[tex_b+2] = ~data[tex_b+2];
			}
		}
	}
	this.materialXY.map.needsUpdate = true;
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
			if(z==z_pos || x==x_pos) {
				data[tex_b] = ~data[tex_b];
				data[tex_b+1] = ~data[tex_b+1];
				data[tex_b+2] = ~data[tex_b+2];
			}
		}
	}
	this.materialXZ.map.needsUpdate = true;
},

updateMaterialZY: function () {
	var data = this.materialZY.map.image.data;
	for(var z=0;z<Z_SIZE;z++) {
		var base = z*XY_SIZE;
		for(var y=0;y<Y_SIZE;y++) {
			var tex_base = y*Z_SIZE;
			var tex_b = (tex_base+z)*4;
			var b = base+y*X_SIZE+x_pos
			data[tex_b] = dR[b];
			data[tex_b+1] = dG[b];
			data[tex_b+2] = dB[b];
			data[tex_b+3] = 255;
			if(z==z_pos || y==y_pos) {
				data[tex_b] = ~data[tex_b];
				data[tex_b+1] = ~data[tex_b+1];
				data[tex_b+2] = ~data[tex_b+2];
			}
		}
	}
	this.materialZY.map.needsUpdate = true;
},

onKeyDown: function(e) {
	switch (e.keyCode) {
		case 33: // PAGE_UP
			z_pos =  z_pos==Z_SIZE-1?Z_SIZE-1:z_pos+1;
			e.preventDefault();
			//e.stopPropagation();
			break;
		case 34: // PAGE_DOWN
			z_pos = z_pos==0?0:z_pos-1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 39: //RIGHT //case 45: // INS
			x_pos =  x_pos==X_SIZE-1?X_SIZE-1:x_pos+1;
			e.preventDefault();
			//e.stopPropagation();
			break;
		case 37: //LEFT //case 46: // DEL
			x_pos = x_pos==0?0:x_pos-1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 38: //UP //case 36: // HOME
			y_pos =  y_pos==Y_SIZE-1?Y_SIZE-1:y_pos+1;
			e.preventDefault();
			//e.stopPropagation();
			break;
		case 40: //DOWN //case 35: // END
			y_pos = y_pos==0?0:y_pos-1;
			e.preventDefault();
			e.stopPropagation();
			break;
		default:
			//console.log(e.keyCode);
	}
	hovmoeller.updateMaterial();
}

}
