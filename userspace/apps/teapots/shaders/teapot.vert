#version 450
#extension GL_GOOGLE_include_directive : require
// Teapot gallery vertex stage. SHADING selects the pot's model
// (Teapot_Scene.Shading); only Gouraud lights here, per vertex.
layout(constant_id = 0) const int SHADING = 0;
const int GOURAUD = 1;

layout(push_constant) uniform Pot {
    mat4 mvp;
    vec4 mv0, mv1, mv2;   // rows of the model-view matrix
    vec4 params;          // x: seconds
} pot;

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec3 in_normal;
layout(location = 2) in vec2 in_uv;
layout(location = 3) in vec3 in_bary;

layout(location = 0) out vec3 view_position;
layout(location = 1) out vec3 view_normal;
layout(location = 2) out vec2 uv;
layout(location = 3) out vec3 bary;
layout(location = 4) out vec3 gouraud;
layout(location = 5) out vec3 object_position;

#include "lighting.glsl"

void main() {
    vec4 p = vec4(in_position, 1.0);
    vec4 n = vec4(in_normal, 0.0);
    view_position = vec3(dot(pot.mv0, p), dot(pot.mv1, p), dot(pot.mv2, p));
    view_normal = vec3(dot(pot.mv0, n), dot(pot.mv1, n), dot(pot.mv2, n));
    uv = in_uv;
    bary = in_bary;
    object_position = in_position;
    gouraud = vec3(0.0);
    if (SHADING == GOURAUD) {
        vec3 v = normalize(-view_position);
        vec3 normal = facing(normalize(view_normal), v);
        gouraud = phong(normal, v, GOURAUD_COLOR, PHONG_SHININESS);
    }
    gl_Position = pot.mvp * p;
}
