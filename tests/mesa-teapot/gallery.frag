#version 450
layout(location=0) in vec3 world_normal;
layout(location=1) flat in int material;
layout(location=0) out vec4 color;
void main() {
    const vec3 palette[5] = vec3[5](vec3(0.95,0.16,0.07),vec3(0.1,0.62,0.95),
        vec3(0.16,0.8,0.3),vec3(0.95,0.62,0.08),vec3(0.65,0.22,0.9));
    vec3 base = palette[material%5];
    vec3 n = normalize(world_normal);
    vec3 light = normalize(vec3(-0.4,-0.6,1));
    vec3 eye = normalize(vec3(0,-0.866,0.5));
    float diffuse = max(dot(n,light),0);
    float specular = max(dot(n,normalize(light+eye)),0);
    int style = material/5;
    vec3 lit;
    if(style==0) lit=base*(0.16+0.84*diffuse); // matte
    else if(style==1) lit=base*(0.12+0.65*diffuse)+vec3(0.9)*pow(specular,64); // gloss
    else if(style==2) lit=base*(0.08+0.45*diffuse+1.2*pow(specular,20)); // metallic tint
    else lit=base*(0.2+0.8*floor(diffuse*4)/4); // quantized/toon
    color=vec4(clamp(lit,0,1),1);
}
