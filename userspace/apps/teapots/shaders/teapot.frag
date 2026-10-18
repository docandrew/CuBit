#version 450
#extension GL_GOOGLE_include_directive : require
// Teapot gallery fragment stage: one shading model per pot, chosen by the
// SHADING specialization constant (values of Teapot_Scene.Shading).
layout(constant_id = 0) const int SHADING = 0;
const int FLAT = 0;
const int GOURAUD = 1;
const int PHONG = 2;
const int BLINN_PHONG = 3;
const int TOON = 4;
const int WIREFRAME = 5;
const int NORMALS = 6;
const int ENVIRONMENT = 7;
const int PROCEDURAL = 8;

layout(push_constant) uniform Pot {
    mat4 mvp;
    vec4 mv0, mv1, mv2;
    vec4 params;          // x: seconds
} pot;

layout(location = 0) in vec3 view_position;
layout(location = 1) in vec3 view_normal;
layout(location = 2) in vec2 uv;
layout(location = 3) in vec3 bary;
layout(location = 4) in vec3 gouraud;
layout(location = 5) in vec3 object_position;

layout(location = 0) out vec4 color;

#include "lighting.glsl"

const vec3 FLAT_COLOR = vec3(0.30, 0.62, 0.86);
const vec3 PHONG_COLOR = vec3(0.80, 0.16, 0.20);
const vec3 BLINN_COLOR = vec3(0.92, 0.72, 0.22);
const vec3 TOON_COLOR = vec3(0.36, 0.78, 0.42);
const vec3 WIRE_FILL = vec3(0.55, 0.58, 0.70);
const vec3 WIRE_LINE = vec3(0.45, 0.95, 1.00);
const float TOON_BANDS = 4.0;
const float TOON_OUTLINE = 0.28;
const float WIRE_WIDTH = 0.9;
const float WIRE_OPACITY = 0.85;
const float CHECKS = 8.0;

// Procedural sky and ground seen in a reflection (view space, Y up).
vec3 environment(vec3 r) {
    vec3 horizon = vec3(0.85, 0.80, 0.72);
    vec3 zenith = vec3(0.18, 0.36, 0.72);
    vec3 ground = vec3(0.22, 0.18, 0.15);
    if (r.y >= 0.0) {
        vec3 sky = mix(horizon, zenith, pow(r.y, 0.6));
        float sun = pow(max(dot(r, LIGHT_DIRECTION), 0.0), 220.0);
        return sky + vec3(1.0, 0.9, 0.7) * sun * 2.0;
    }
    vec2 tile = r.xz / max(-r.y, 0.05);
    float check = mod(floor(tile.x) + floor(tile.y), 2.0);
    return mix(horizon * 0.6, ground * (0.7 + 0.3 * check), min(-r.y * 4.0, 1.0));
}

void main() {
    vec3 v = normalize(-view_position);
    vec3 n = facing(normalize(view_normal), v);
    vec3 result;
    if (SHADING == FLAT) {
        vec3 face = normalize(cross(dFdx(view_position), dFdy(view_position)));
        result = phong(facing(face, v), v, FLAT_COLOR, PHONG_SHININESS);
    } else if (SHADING == GOURAUD) {
        result = gouraud;
    } else if (SHADING == PHONG) {
        result = phong(n, v, PHONG_COLOR, PHONG_SHININESS);
    } else if (SHADING == BLINN_PHONG) {
        result = blinn(n, v, BLINN_COLOR, BLINN_SHININESS);
    } else if (SHADING == TOON) {
        float diffuse = max(dot(n, LIGHT_DIRECTION), 0.0);
        float band = floor(diffuse * TOON_BANDS) / (TOON_BANDS - 1.0);
        vec3 h = normalize(LIGHT_DIRECTION + v);
        float highlight = step(0.96, dot(n, h));
        result = TOON_COLOR * (0.35 + 0.65 * band) + vec3(highlight * 0.6);
        if (dot(n, v) < TOON_OUTLINE) result = vec3(0.04);
    } else if (SHADING == WIREFRAME) {
        // Shaded teapot with the tessellation's quad edges over it. Each
        // cell's diagonal runs between corners 0 and 2 (Teapot_Mesh), so
        // bary.y = 0 is the diagonal and is left out.
        vec3 shaded = blinn(n, v, WIRE_FILL, BLINN_SHININESS);
        vec2 quad = vec2(bary.x, bary.z);
        vec2 width = fwidth(quad) * WIRE_WIDTH;
        vec2 edge = smoothstep(vec2(0.0), width, quad);
        float line = (1.0 - min(edge.x, edge.y)) * WIRE_OPACITY;
        result = mix(shaded, WIRE_LINE, line);
    } else if (SHADING == NORMALS) {
        result = n * 0.5 + 0.5;
    } else if (SHADING == ENVIRONMENT) {
        vec3 r = reflect(-v, n);
        float fresnel = 0.35 + 0.65 * pow(1.0 - max(dot(n, v), 0.0), 3.0);
        result = environment(r) * mix(0.75, 1.0, fresnel);
    } else {
        // Procedural: animated checks over the patch parameters, lit.
        vec2 grid = uv * CHECKS + vec2(pot.params.x * 0.25, 0.0);
        vec2 aa = fwidth(grid);
        vec2 s = smoothstep(-aa, aa, fract(grid) - 0.5) * 2.0 - 1.0;
        float check = 0.5 - 0.5 * s.x * s.y;
        vec3 base = mix(vec3(0.95, 0.93, 0.88), vec3(0.55, 0.20, 0.60), check);
        result = blinn(n, v, base, BLINN_SHININESS);
    }
    color = vec4(result, 1.0);
}
