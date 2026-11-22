#version 450
void main() {
    const int indices[6] = int[6](0,1,2,0,2,3);
    const vec2 positions[4] = vec2[4](vec2(-1,-1),vec2(1,-1),vec2(1,1),vec2(-1,1));
    gl_Position=vec4(positions[indices[gl_VertexIndex]],0,1);
}
