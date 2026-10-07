#version 450
layout(set=0,binding=0) uniform sampler2D source_image;
layout(push_constant) uniform Draw {
    uvec2 u0;
    uvec2 v0;
    uvec2 ud;
    uvec2 vd;
    ivec4 steps;
    vec4 tint;
    uint mask;
    layout(offset=68) uint preview_width;
    layout(offset=72) uint preview_height;
    layout(offset=80) uvec4 region;
} draw;
layout(location=0) out vec4 color;
// Exact 64-bit integer arithmetic using Vulkan 1.0 core 32-bit operations.
// No shaderInt64 feature and no floating interpolation of texture coordinates.
uvec2 twice(uvec2 x) { return uvec2(x.x<<1,(x.y<<1)|(x.x>>31)); }
uvec2 add(uvec2 a,uvec2 b) { uint low=a.x+b.x; return uvec2(low,a.y+b.y+uint(low<a.x)); }
uvec2 signed_word(int x) { return uvec2(uint(x),x<0?0xffffffffu:0u); }
bool le(uvec2 a,uvec2 b) { return a.y<b.y||(a.y==b.y&&a.x<=b.x); }
uvec2 product(uvec2 a,uint b) {
    uint high,low; umulExtended(a.x,b,high,low);
    return uvec2(low,high+a.y*b);
}
float approximate(uvec2 a) { return float(a.y)*4294967296.0+float(a.x); }
uint texel(uvec2 base,ivec2 steps,uvec2 denominator,uint size,ivec2 centre) {
    uvec2 numerator=add(add(twice(base),signed_word(steps.x*centre.x)),signed_word(steps.y*centre.y));
    denominator=twice(denominator);
    if((numerator.y&0x80000000u)!=0u)return 0u;
    if(le(denominator,numerator))return size-1u;
    numerator=product(numerator,size);
#ifdef CUBIT_VULKAN_TEST_FORCE_DIVIDE
    uint q=size-1u;
#else
    uint q=min(uint(approximate(numerator)/approximate(denominator)),size-1u);
#endif
    // Certify the estimated quotient using exact comparisons. Near a floating
    // boundary one correction suffices; the bounded integer fallback keeps
    // correctness independent of that approximation's quality.
    if(!le(product(denominator,q),numerator)&&q>0u)--q;
    else if(q+1u<size&&le(product(denominator,q+1u),numerator))++q;
    if(le(product(denominator,q),numerator)&&!le(product(denominator,q+1u),numerator))return q;
    q=0u;
    for(int bit=23;bit>=0;--bit) {
        uint candidate=q|(1u<<bit);
        if(candidate<size&&le(product(denominator,candidate),numerator))q=candidate;
    }
    return q;
}
// Endpoint-aligned 8-bit bilinear wallpaper sampling. Certified quotient keeps
// exact CPU rounding without requiring shaderInt64 or changing nearest draws.
uvec2 negate_pair(uvec2 value) { return add(~value,uvec2(1u,0u)); }
uint backdrop_quotient(uvec2 numerator,uvec2 denominator,uint limit) {
#ifdef CUBIT_VULKAN_TEST_FORCE_DIVIDE
    uint q=limit;
#else
    uint q=min(uint(approximate(numerator)/approximate(denominator)),limit);
#endif
    if(q>0u&&!le(product(denominator,q),numerator))--q;
    else if(q<limit&&le(product(denominator,q+1u),numerator))++q;
    if(le(product(denominator,q),numerator)&&
       (q==limit||!le(product(denominator,q+1u),numerator)))return q;
    q=0u;
    for(int bit=23;bit>=0;--bit) {
        uint candidate=q|(1u<<bit);
        if(candidate<=limit&&le(product(denominator,candidate),numerator))q=candidate;
    }
    return q;
}
bool backdrop_axis(int pixel,uvec2 origin,uvec2 extent,uint size,out uvec3 sample_axis) {
    uvec2 local=add(signed_word(pixel),negate_pair(origin));
    if((local.y&0x80000000u)!=0u||le(extent,local))return false;
    uint coordinate=0u;
    if(extent.x!=1u||extent.y!=0u) {
        uvec2 divisor=add(extent,uvec2(0xffffffffu,0xffffffffu));
        coordinate=backdrop_quotient(product(local,(size-1u)*256u),divisor,(size-1u)*256u);
    }
    uint first=coordinate/256u;
    sample_axis=uvec3(first,min(first+1u,size-1u),coordinate%256u);
    return true;
}
uvec3 backdrop_pixel(ivec2 pixel) {
    return uvec3(round(texelFetch(source_image,pixel,0).rgb*255.0));
}
uvec3 backdrop_blend(uvec3 a,uvec3 b,uint fraction) {
    return (a*(256u-fraction)+b*fraction+128u)/256u;
}
vec4 backdrop_color(uvec2 size) {
    uvec3 x,y;
    if(!backdrop_axis(int(gl_FragCoord.x),draw.u0,draw.ud,size.x,x)||
       !backdrop_axis(int(gl_FragCoord.y),draw.v0,draw.vd,size.y,y))discard;
    uvec3 top=backdrop_blend(backdrop_pixel(ivec2(x.x,y.x)),backdrop_pixel(ivec2(x.y,y.x)),x.z);
    uvec3 bottom=backdrop_blend(backdrop_pixel(ivec2(x.x,y.y)),backdrop_pixel(ivec2(x.y,y.y)),x.z);
    return vec4(vec3(backdrop_blend(top,bottom,y.z))/255.0,1.0);
}

bool preview_axis(uint centre,uint logical_size,int origin,uint extent,uint size,out uvec3 axis) {
    uint point=min(centre>128u?centre-128u:0u,(logical_size-1u)*256u);
    uvec2 local=add(uvec2(point,0),negate_pair(product(signed_word(origin),256u)));
    if((local.y&0x80000000u)!=0u||le(product(uvec2(extent,0),256u),local))return false;
    uint coordinate=0u;
    if(extent!=1u) {
        uvec2 end_point=product(uvec2(extent-1u,0),256u);
        if(le(end_point,local))local=end_point;
        coordinate=backdrop_quotient(product(local,size-1u),uvec2(extent-1u,0),(size-1u)*256u);
    }
    uint first=coordinate/256u;
    axis=uvec3(first,min(first+1u,size-1u),coordinate%256u);
    return true;
}
vec4 preview_color(uvec2 size) {
    ivec2 centre=2*ivec2(gl_FragCoord.xy)+ivec2(1);
    uint cx=texel(draw.u0,draw.steps.xy,draw.ud,draw.preview_width*256u,centre);
    uint cy=texel(draw.v0,draw.steps.zw,draw.vd,draw.preview_height*256u,centre);
    uvec3 x,y;
    if(!preview_axis(cx,draw.preview_width,int(draw.region.x),draw.region.z,size.x,x)||
       !preview_axis(cy,draw.preview_height,int(draw.region.y),draw.region.w,size.y,y))discard;
    uvec3 top=backdrop_blend(backdrop_pixel(ivec2(x.x,y.x)),backdrop_pixel(ivec2(x.y,y.x)),x.z);
    uvec3 bottom=backdrop_blend(backdrop_pixel(ivec2(x.x,y.y)),backdrop_pixel(ivec2(x.y,y.y)),x.z);
    return vec4(vec3(backdrop_blend(top,bottom,y.z))/255.0,1.0);
}
void main() {
    uvec2 size=uvec2(textureSize(source_image,0));
    if(draw.mask==2u) { color=backdrop_color(size); return; }
    if(draw.mask==4u) { color=preview_color(size); return; }
    uvec2 origin=uvec2(0);
    if(draw.region.z!=0u) {
        origin=draw.region.xy;
        if(any(greaterThanEqual(origin,size))||any(greaterThan(draw.region.zw,size-origin)))discard;
        size=draw.region.zw;
    }
    ivec2 centre=2*ivec2(gl_FragCoord.xy)+ivec2(1);
    ivec2 pixel=ivec2(texel(draw.u0,draw.steps.xy,draw.ud,size.x,centre),
                      texel(draw.v0,draw.steps.zw,draw.vd,size.y,centre));
    vec4 sample_color=texelFetch(source_image,pixel+ivec2(origin),0);
    color=draw.mask!=0u?sample_color.r*draw.tint:sample_color;
}
