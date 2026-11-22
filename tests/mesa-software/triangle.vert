#version 450
void main() {
    const vec2 vertices[3] = vec2[3](vec2(-1, -1), vec2(1, -1), vec2(-1, 1));
    gl_Position = vec4(vertices[gl_VertexIndex], 0, 1);
}
