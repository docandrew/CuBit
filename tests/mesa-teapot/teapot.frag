#version 450
layout(location=0) in vec3 world_normal;
layout(location=0) out vec4 color;
void main() {
    vec3 n = normalize(world_normal);
    float light = 0.18 + 0.82 * max(dot(n, normalize(vec3(-0.4, -0.6, 1.0))), 0.0);
    color = vec4(vec3(0.85, 0.28, 0.08) * light, 1.0);
}
