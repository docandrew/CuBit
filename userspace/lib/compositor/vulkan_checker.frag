#version 450
// ABI shared with vulkan_checker.c. No descriptors or image sampling.
layout(push_constant) uniform Checker {
    ivec4 area;
    ivec2 origin;
    uvec2 scale;
    uvec4 output_info;
    vec4 tint;
} draw;
layout(location=0) out vec4 color;
void main() {
    ivec2 p=ivec2(gl_FragCoord.xy), q;
    int width=int(draw.output_info.x), height=int(draw.output_info.y);
    uint rotation=draw.output_info.z;
    if(rotation==0u) q=p;
    else if(rotation==1u) q=ivec2(p.y,width-1-p.x);
    else if(rotation==2u) q=ivec2(width-1-p.x,height-1-p.y);
    else q=ivec2(height-1-p.y,p.x);
    int n=int(draw.scale.x), d=int(draw.scale.y);
    ivec2 lo=max(draw.area.xy,draw.origin+q*d/n);
    ivec2 hi=min(draw.area.zw-ivec2(1),draw.origin+((q+ivec2(1))*d-ivec2(1))/n);
    if(any(greaterThan(lo,hi))) discard;
    if(all(equal(lo,hi)) && ((lo.x+lo.y)&1)!=0) discard;
    color=draw.tint;
}
