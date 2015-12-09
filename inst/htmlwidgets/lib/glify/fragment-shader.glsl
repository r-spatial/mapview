precision mediump float;
varying vec4 vColor;
void main() {
    float border = 0.0;
    float radius = 0.5;
    vec4 color0 = vec4(0.0, 0.0, 0.0, 0.0); //background
    vec4 color1 = vec4(vColor[0], vColor[1], vColor[2], 1); //foreground

    vec2 m = gl_PointCoord.xy - vec2(0.5, 0.5);
    float dist = radius - sqrt(m.x * m.x + m.y * m.y);

    float t = 0.0;
    if (dist > border) {
        t = 1.0;
    } else if (dist > 0.0) {
        t = dist / border;
    }

    //works for overlapping circles if blending is enabled
    gl_FragColor = mix(color0, color1, t);
}
