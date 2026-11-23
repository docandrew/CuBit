#version 450
#extension GL_GOOGLE_include_directive : require
// SPDX-License-Identifier: GPL-3.0-or-later
// Neon Blast post pass. MODE 0 multiplies (vignette, scanlines and the
// breakdown tint); MODE 1 adds the flashes.
#include "scene.glsl"
layout(constant_id = 0) const int MODE = 0;
layout(location = 0) out vec4 color;

void main() {
    vec2 uv = gl_FragCoord.xy / g.view.xy;
    if (MODE == 0) {
        vec2 v = (uv - 0.5) * 2.0;
        float shade = 1.0 - 0.35 * 0.5 * dot(v, v);
        if (g.mood.w > 0.5 && int(gl_FragCoord.y) % 2 == 1) shade *= 0.9;
        float u = g.mood.z;
        color = vec4(shade * vec3(1.0 - 0.2 * u, 1.0 - 0.35 * u, 1.0 - 0.05 * u), 1.0);
    } else {
        color = vec4(g.wash.rgb, 0.0);
    }
}
