// Shared lighting for the teapot gallery (view space, light fixed to the
// camera rig so every pot is lit the same way).
const vec3 LIGHT_DIRECTION = normalize(vec3(-0.45, 0.7, 0.55));
const vec3 LIGHT_COLOR = vec3(1.0, 0.97, 0.92);
const vec3 AMBIENT = vec3(0.12, 0.13, 0.16);
const float PHONG_SHININESS = 24.0;
const float BLINN_SHININESS = 72.0;
const float SPECULAR_STRENGTH = 0.55;
const vec3 GOURAUD_COLOR = vec3(0.85, 0.42, 0.20);

// Two-sided: the teapot is open at the lid seam, so face the viewer.
vec3 facing(vec3 n, vec3 v) { return dot(n, v) < 0.0 ? -n : n; }

vec3 phong(vec3 n, vec3 v, vec3 base, float shininess) {
    float diffuse = max(dot(n, LIGHT_DIRECTION), 0.0);
    vec3 r = reflect(-LIGHT_DIRECTION, n);
    float specular = diffuse > 0.0 ? pow(max(dot(r, v), 0.0), shininess) : 0.0;
    return AMBIENT * base + LIGHT_COLOR * (base * diffuse + SPECULAR_STRENGTH * specular);
}

vec3 blinn(vec3 n, vec3 v, vec3 base, float shininess) {
    float diffuse = max(dot(n, LIGHT_DIRECTION), 0.0);
    vec3 h = normalize(LIGHT_DIRECTION + v);
    float specular = diffuse > 0.0 ? pow(max(dot(n, h), 0.0), shininess) : 0.0;
    return AMBIENT * base + LIGHT_COLOR * (base * diffuse + SPECULAR_STRENGTH * specular);
}
