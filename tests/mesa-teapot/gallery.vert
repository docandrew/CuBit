#version 450
layout(location=0) in vec3 position;
layout(location=1) in vec3 normal;
layout(push_constant) uniform Animation { vec4 frame; } animation;
layout(location=0) out vec3 world_normal;
layout(location=1) flat out int material;
void main() {
    int id = gl_InstanceIndex;
    float angle = animation.frame.x * (0.45 + 0.065 * float(id % 5)) + float(id) * 0.37;
    float c = cos(angle), s = sin(angle);
    mat3 rotation = mat3(c,s,0, -s,c,0, 0,0,1);
    vec3 p = rotation * (position - vec3(0,0,1.5));
    vec3 view = vec3(p.x, -0.5*p.y-0.8660254*p.z, 0.8660254*p.y-0.5*p.z);
    vec2 center = vec2(-0.8 + 0.4*float(id%5), -0.75 + 0.5*float(id/5));
    gl_Position = vec4(center + view.xy*vec2(0.052,0.065), 0.5+view.z*0.06, 1);
    world_normal = rotation * normal;
    material = id;
}
