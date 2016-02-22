HTMLWidgets.widget({

  name: 'cubeView',

  type: 'output',

  initialize: function(el, width, height) {

    return {}

  },

  renderValue: function(el, x, instance) {

    init(el);

    //el.innerHTML = "text";

  },

  resize: function(el, width, height, instance) {

  }

});




var hovmoeller;

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
var z_pos = iDiv(Z_SIZE*2,3);

function init(root) {
	hovmoeller = new Hovmoeller(root);
}

function iDiv(a,b) {
	return (a/b>>0);
}

function Hovmoeller(root) {

	for(var z=0;z<Z_SIZE;z++) {
		var z_base = z*XY_SIZE;
		for(var y=0;y<Y_SIZE;y++) {
			var y_base = z_base + y*X_SIZE;
			for(var x=0;x<X_SIZE;x++) {
				var i = y_base+x;
				dR[i] = (x*255)/X_SIZE;
				dG[i] = (y*255)/Y_SIZE;
				dB[i] = (z*255)/Z_SIZE;
			}
		}
	}

	this.scene = new THREE.Scene();
	this.camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.1, 1000 );

	this.renderer = new THREE.WebGLRenderer( {/*antialias: true*/} );
	this.renderer.setSize( window.innerWidth, window.innerHeight );
	console.log(this.renderer.getMaxAnisotropy());

	var controls = new THREE.OrbitControls(this.camera, this.renderer.domElement);

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


	//TODO check http://stackoverflow.com/questions/11961288/three-js-cube-with-different-texture-on-each-face-how-to-hide-edges-vertices?rq=1

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

	this.camera.position.z = 7;

	document.body.addEventListener( 'keydown', this.onKeyDown, false );



	console.log(JSON.stringify(this.cube.geometry.faceVertexUvs));

	this.render();
}

Hovmoeller.prototype = {

render: function () {
	var self = this;
	function mov() {self.render();}
	requestAnimationFrame( mov );

	/*this.cube.rotation.x += 0.005;
	this.cube.rotation.y += 0.007;
	this.cube.rotation.z += 0.003;*/
	this.updateMaterial();
	this.renderer.render( this.scene, this.camera );
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
		case 45: // INS
			x_pos =  x_pos==X_SIZE-1?X_SIZE-1:x_pos+1;
			e.preventDefault();
			//e.stopPropagation();
			break;
		case 46: // DEL
			x_pos = x_pos==0?0:x_pos-1;
			e.preventDefault();
			e.stopPropagation();
			break;
		case 36: // HOME
			y_pos =  y_pos==Y_SIZE-1?Y_SIZE-1:y_pos+1;
			e.preventDefault();
			//e.stopPropagation();
			break;
		case 35: // END
			y_pos = y_pos==0?0:y_pos-1;
			e.preventDefault();
			e.stopPropagation();
			break;
		default:
			console.log(e.keyCode);
	}
}

}
