// Neon Blast: the frame's globals, shared by every stage. Layout of
// Neon_GPU's Globals record (std140: vec4 rows only).
// SPDX-License-Identifier: GPL-3.0-or-later
const int LIGHT_CEILING = 32;    // Neon_Scene.Light_Ceiling
const int RIPPLE_CEILING = 16;   // Neon_Scene.Ripple_Ceiling

layout(std140, set = 0, binding = 0) uniform Globals {
    vec4 view;       // width, height, scale (height / 1080), song seconds
    vec4 cam_pos;    // camera position, focal (pixels)
    vec4 cam_up;     // up axis, centre x (pixels, shake included)
    vec4 cam_fwd;    // forward axis, centre y
    vec4 bend;       // bend start z, bend, grid scroll, kick
    vec4 mood;       // sun swell, glow, underwater, scanlines (0 or 1)
    vec4 wash;       // additive flash colour, hurt
    vec4 counts;     // lights, ripples, sun radius (fraction of height), 0
    vec4 lights[2 * LIGHT_CEILING];    // (pos, radius), (colour, 0)
    vec4 ripples[2 * RIPPLE_CEILING];  // (centre xz, radius, width), (colour, strength)
    vec4 look[12];   // the stage's colours (Neon_Style.Look), then grid cell, sun bands, ridge
} g;

// Project a world point to pixels; w = camera depth.
vec3 project(vec3 p) {
    vec3 d = p - g.cam_pos.xyz;
    float yc = dot(d, g.cam_up.xyz);
    float zc = dot(d, g.cam_fwd.xyz);
    float z = max(zc, 0.05);
    return vec3(g.cam_up.w + g.cam_pos.w * d.x / z, g.cam_fwd.w - g.cam_pos.w * yc / z, zc);
}

// Pixels to Vulkan clip space (y down already).
vec4 to_clip(vec2 px) {
    return vec4(px / g.view.xy * 2.0 - 1.0, 0.0, 1.0);
}
