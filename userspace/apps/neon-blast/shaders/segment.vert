#version 450
#extension GL_GOOGLE_include_directive : require
// SPDX-License-Identifier: GPL-3.0-or-later
// Neon Blast: every glowing segment (3D lines, HUD strokes) and particle,
// one instance each, expanded to a screen-space quad with room for its glow.
#include "scene.glsl"
layout(location = 0) in vec4 a;      // A (xyz), width in 1080p pixels
layout(location = 1) in vec4 b;      // B (xyz), kind: 0 line, 1 stroke, 2 point
layout(location = 2) in vec4 tint;   // colour, point radius (world)
layout(location = 0) out vec2 local;       // pixel position along/across
layout(location = 1) out flat vec4 shape;  // segment length, half width, glow reach, 0
layout(location = 2) out flat vec3 hue;

const float GLOW_PIXELS = 14.0;   // glow reach at 1080 high

void main() {
    float scale = g.view.z;
    int kind = int(b.w + 0.5);
    vec2 p0, p1;
    float half_width;
    bool visible = true;
    if (kind == 1) {
        p0 = a.xy * g.view.y;
        p1 = b.xy * g.view.y;
        half_width = max(0.5, 0.5 * a.w * scale);
    } else {
        vec3 s0 = project(a.xyz);
        vec3 s1 = kind == 2 ? s0 : project(b.xyz);
        visible = s0.z > 0.5 && s1.z > 0.5;
        p0 = s0.xy;
        p1 = s1.xy;
        half_width = kind == 2 ? max(0.6, tint.w * g.cam_pos.w / max(s0.z, 0.5))
                               : max(0.5, 0.5 * a.w * scale);
    }
    float reach = GLOW_PIXELS * scale + half_width * 2.0;
    vec2 axis = p1 - p0;
    float len = length(axis);
    vec2 dir = len > 1e-3 ? axis / len : vec2(1.0, 0.0);
    vec2 side = vec2(-dir.y, dir.x);
    // Corners: 0..3 as a triangle strip.
    float along = (gl_VertexIndex & 1) == 0 ? -reach : len + reach;
    float across = (gl_VertexIndex & 2) == 0 ? -reach : reach;
    vec2 corner = p0 + dir * along + side * across;
    gl_Position = visible ? to_clip(corner) : vec4(2.0, 2.0, 2.0, 1.0);
    local = vec2(along, across);
    shape = vec4(len, half_width, reach, 0.0);
    hue = tint.rgb;
}
