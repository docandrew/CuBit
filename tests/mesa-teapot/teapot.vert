#version 450
layout(location=0) in vec3 position;
layout(location=1) in vec3 normal;
// Model must be a rigid transform: no nonuniform scaling.
layout(push_constant) uniform Transform { mat4 mvp; mat4 model; } transform;
layout(location=0) out vec3 world_normal;
void main() {
    gl_Position = transform.mvp * vec4(position, 1.0);
    world_normal = mat3(transform.model) * normal;
}
