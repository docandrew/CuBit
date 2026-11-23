#version 450
// SPDX-License-Identifier: GPL-3.0-or-later
// Neon Blast: one triangle covering the target (background and post).
void main() {
    vec2 corner = vec2((gl_VertexIndex << 1) & 2, gl_VertexIndex & 2);
    gl_Position = vec4(corner * 2.0 - 1.0, 0.0, 1.0);
}
