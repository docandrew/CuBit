#version 450
#extension GL_GOOGLE_include_directive : require
// SPDX-License-Identifier: GPL-3.0-or-later
// Neon Blast: a segment's core and glow, added to what is below.
#include "scene.glsl"
layout(location = 0) in vec2 local;
layout(location = 1) in flat vec4 shape;
layout(location = 2) in flat vec3 hue;
layout(location = 0) out vec4 color;

void main() {
    float along = clamp(local.x, 0.0, shape.x);
    float d = length(vec2(local.x - along, local.y));   // pixels from the segment
    float core = clamp(shape.y + 0.5 - d, 0.0, 1.0);
    float glow_scale = 6.0 * g.view.z;
    float glow = 0.45 * g.mood.y * exp(-d / glow_scale) * clamp(1.0 - d / shape.z, 0.0, 1.0);
    // Overbright cores whiten, as hot neon does.
    float extra = dot(max(hue * core - 1.0, 0.0), vec3(1.0 / 3.0));
    vec3 c = hue * (core + glow) + extra;
    color = vec4(c, 0.0);
}
