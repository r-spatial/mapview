uniform mat4 uMatrix;
attribute vec4 aVertex;
attribute float aPointSize;
attribute vec4 aColor;
varying vec4 vColor;

void main() {
    //set the size of the point
    gl_PointSize =  aPointSize;

    //multiply each vertex by a matrix.
    gl_Position = uMatrix * aVertex;

    //pass the color to the fragment shader
    vColor = aColor;
}
