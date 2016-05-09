var renderer;
var scene;
var camera;
var controls;

function init() {
	
	var geometry = new THREE.BoxGeometry( 10, 15, 5);
	var material = new THREE.MeshBasicMaterial({color: 0xffffff});
	var cube = new THREE.Mesh(geometry, material);
	
	camera = new THREE.PerspectiveCamera(60, window.innerWidth / window.innerHeight, 1, 1000);
	camera.position.z = 20;
	
	controls = new THREE.TrackballControls(camera);
	controls.rotateSpeed = 7.0;
	controls.zoomSpeed = 0.1;
	controls.panSpeed = 0; // no panning
	controls.addEventListener('change', render);
	
	scene = new THREE.Scene();
	scene.fog = new THREE.FogExp2(0x000000, 0.05);
	scene.add(cube);
	
	renderer = new THREE.WebGLRenderer({antialias:false});
	renderer.setClearColor(0x000000);
	renderer.setPixelRatio(window.devicePixelRatio);
	renderer.setSize(window.innerWidth, window.innerHeight);
	
	var container = document.getElementById('container');
	container.appendChild(renderer.domElement);
	
	onWindowResize();
	animate();	
}

function render() {
	//console.log("render");
	renderer.render(scene, camera);
}

function animate() {
	requestAnimationFrame(animate);
	controls.update();
}

function onWindowResize() {
	camera.aspect = window.innerWidth / window.innerHeight;
	camera.updateProjectionMatrix();
	renderer.setSize(window.innerWidth, window.innerHeight);
	controls.handleResize();
	render();
}