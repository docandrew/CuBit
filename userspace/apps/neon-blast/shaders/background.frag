#version 450
#extension GL_GOOGLE_include_directive : require
// SPDX-License-Identifier: GPL-3.0-or-later
// Neon Blast background: the sky, the banded sun, the mountains and the
// bent, lit floor grid, per pixel. The software renderer (neon_soft.adb)
// draws the same scene with the same constants, a row at a time.
#include "scene.glsl"
layout(location = 0) out vec4 color;

// The stage's look (Neon_Style.Look, Neon_GPU's look rows).
#define SKY_TOP g.look[0].rgb
#define SKY_MID g.look[1].rgb
#define SKY_LOW g.look[2].rgb
#define HORIZON g.look[3].rgb
#define FLOOR_BASE g.look[4].rgb
#define GRID_PINK g.look[5].rgb
#define GRID_VIOLET g.look[6].rgb
#define SUN_TOP g.look[7].rgb
#define SUN_BOTTOM g.look[8].rgb
#define MOUNTAIN_FILL g.look[9].rgb
#define MOUNTAIN_EDGE g.look[10].rgb
#define GRID_CELL g.look[11].x
#define SUN_BANDS g.look[11].y
#define RIDGE_HEIGHT g.look[11].z
const vec3 WHITE = vec3(1.0);
const float HALF_WIDTH = 16.0;    // Neon_Sim.Half_Width
const float HALF_DEPTH = 9.0;     // Neon_Sim.Half_Depth
const float TURN = 6.2831853;

// Where the ray through screen row y meets the floor: (hit, z, t).
vec3 row_floor(float y) {
    float v = (g.cam_fwd.w - y) / g.cam_pos.w;
    float dy = g.cam_fwd.y + v * g.cam_up.y;
    float dz = g.cam_fwd.z + v * g.cam_up.z;
    if (dy >= 0.0) return vec3(0.0);
    float t = -g.cam_pos.y / dy;
    float z = g.cam_pos.z + dz * t;
    if (z <= g.bend.x) return vec3(1.0, z, t);
    float off = g.cam_pos.z - g.bend.x;
    float a = g.bend.y * dz * dz;
    float b = 2.0 * g.bend.y * dz * off + dy;
    float c = g.bend.y * off * off + g.cam_pos.y;
    float disc = b * b - 4.0 * a * c;
    if (a <= 0.0 || disc < 0.0) return vec3(0.0);
    t = (-b - sqrt(disc)) / (2.0 * a);
    z = g.cam_pos.z + dz * t;
    return (t > 0.0 && z > g.bend.x) ? vec3(1.0, z, t) : vec3(0.0);
}

float ridge(float u) {
    float side = smoothstep(0.1, 0.33, abs(u - 0.5));
    float shape = 0.55 + 0.3 * sin(TURN * (u * 1.3 + 0.05)) + 0.18 * sin(TURN * (u * 3.1 + 0.2))
                + 0.12 * abs(sin(TURN * u * 7.3)) + 0.05 * abs(sin(TURN * u * 19.0));
    return 0.07 * side * max(0.0, shape);
}

// A grid line's brightness at distance d (cells) with p cells per pixel.
float line_profile(float d, float p) {
    float px = d / max(p, 1e-5);
    float core = clamp(1.3 - 0.85 * px, 0.0, 1.0);
    float fade = clamp(1.0 - px * 0.16, 0.0, 1.0);
    float near = core + 0.3 * fade * fade;
    float average = min(1.0, 2.2 * p + 0.25);
    return mix(near, average, clamp((p - 0.12) * 4.0, 0.0, 1.0));
}

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
}

// The horizon row: binary search like the software path.
float horizon_row() {
    float lo = 0.0, hi = g.view.y;
    for (int i = 0; i < 16; i++) {
        float mid = floor((lo + hi) * 0.5);
        if (row_floor(mid + 0.5).x > 0.5) hi = mid; else lo = mid + 1.0;
    }
    return lo;
}

void main() {
    vec2 px = gl_FragCoord.xy;
    float H = g.view.y;
    float horizon = horizon_row();
    vec3 c;
    if (px.y < horizon) {
        float up = clamp(px.y / max(horizon, 1.0), 0.0, 1.0);
        c = up < 0.55 ? mix(SKY_TOP, SKY_MID, up / 0.55)
          : up < 0.85 ? mix(SKY_MID, SKY_LOW, (up - 0.55) / 0.3)
          : mix(SKY_LOW, HORIZON, (up - 0.85) / 0.15);
        float above = horizon - px.y;
        c += 0.45 / (1.0 + pow(above / (0.05 * H), 2.0)) * HORIZON;
        float sun_r = g.counts.z * H * (1.0 + 0.05 * g.mood.x);
        vec2 sun = vec2(g.view.x * 0.5, horizon - 0.3 * sun_r);
        float ridge_y = horizon - H * RIDGE_HEIGHT * ridge(px.x / g.view.x);
        if (px.y >= ridge_y) {
            float below = px.y - ridge_y;
            c = mix(MOUNTAIN_FILL, c, 0.25) + clamp(1.6 - below, 0.0, 1.0) * 1.2 * MOUNTAIN_EDGE;
        } else {
            if (up < 0.6) {
                float h = hash(floor(px));
                if (h > 0.9985) c += (0.4 + 0.4 * sin(TURN * (g.view.w * 0.5 + h * 37.0))) * (1.0 - up / 0.6) * WHITE;
            }
            float dy = px.y - sun.y;
            float d = length(px - sun);
            bool cut = false;
            if (dy > -0.15 * sun_r) {
                float depth = (dy + 0.15 * sun_r) / sun_r;
                cut = fract(depth * SUN_BANDS - g.view.w * 0.35) < 0.1 + 0.55 * depth;
            }
            if (d < sun_r) {
                // Anti-aliased disc edge and stripe edges.
                float edge = clamp(sun_r - d, 0.0, 1.0);
                vec3 disc = mix(SUN_TOP, SUN_BOTTOM, clamp((dy + sun_r) / (2.0 * sun_r), 0.0, 1.0))
                          + 0.25 * g.mood.x * WHITE;
                if (!cut) c = mix(c, disc, edge);
            } else {
                c += 0.55 / (1.0 + pow((d - sun_r) / (0.18 * sun_r), 2.0)) * (1.0 + 0.4 * g.mood.x) * SUN_BOTTOM;
            }
        }
    } else {
        vec3 hit = row_floor(px.y);
        float z = hit.x > 0.5 ? hit.y : 200.0;
        float t = hit.x > 0.5 ? hit.z : 1000.0;
        float pixel_world = t / g.cam_pos.w;
        float wx = g.cam_pos.x + (px.x - g.cam_up.w) * pixel_world;
        vec3 next = row_floor(px.y - 1.0);
        float row_cells = max(abs((next.x > 0.5 ? next.y : 1e6) - z), 1e-4) / GRID_CELL;
        float gz = (z + g.bend.z) / GRID_CELL;
        float gx = wx / GRID_CELL;
        float across = line_profile(abs(gz - round(gz)), row_cells);
        float along = line_profile(abs(gx - round(gx)), pixel_world / GRID_CELL);
        float grid = min(1.6, along + across);
        bool inside = abs(z) <= HALF_DEPTH && abs(wx) <= HALF_WIDTH;
        float gain = (0.55 + 0.75 * g.bend.w) * (inside ? 1.0 : 0.5);
        vec3 hue = inside ? GRID_PINK : GRID_VIOLET;
        vec3 lit = vec3(0.0);
        int lights = int(g.counts.x);
        for (int i = 0; i < lights; i++) {
            vec4 lp = g.lights[2 * i];
            vec2 dd = vec2(wx, z) - lp.xz;
            float q = (dot(dd, dd) + 0.3 * lp.y * lp.y) / (lp.w * lp.w);
            if (q < 1.0) lit += (1.0 - q) * (1.0 - q) * g.lights[2 * i + 1].rgb;
        }
        int ripples = int(g.counts.y);
        for (int i = 0; i < ripples; i++) {
            vec4 r = g.ripples[2 * i];
            float ring = clamp(1.0 - abs(length(vec2(wx, z) - r.xy) - r.z) / r.w, 0.0, 1.0);
            lit += 1.6 * g.ripples[2 * i + 1].w * ring * ring * g.ripples[2 * i + 1].rgb;
        }
        c = FLOOR_BASE + grid * (gain * hue + 1.3 * lit) + 0.22 * lit;
        float fog = smoothstep(HALF_DEPTH + 2.0, 80.0, z);
        c = mix(c, mix(SKY_LOW, HORIZON, 0.55), 0.85 * fog);
    }
    color = vec4(c, 1.0);
}
