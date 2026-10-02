"""Independent ray/box oracle for the final native GL cube frame (frame8)."""
import math
import sys
from PIL import Image


def check(image):
    image = image.convert("RGB")
    pixels = image.load()
    origin = next(((x, y) for y in range(image.height) for x in range(image.width)
                   if pixels[x, y] == (255, 0, 0)), None)
    if origin is None:
        raise ValueError("missing origin marker")
    x0, y0 = origin
    if x0 + 512 > image.width or y0 + 384 > image.height:
        raise ValueError("cube window clipped")
    a, b = math.radians(25), math.radians(115)
    sa, ca, sb, cb = math.sin(a), math.cos(a), math.sin(b), math.cos(b)
    # Inverse of T(0,0,-4) Rx(25) Ry(115), for an orthographic camera ray.
    direction = (ca * sb, -sa, -ca * cb)
    colors = ((255,0,0), (0,255,0), (0,0,255), (255,255,0), (255,0,255), (0,255,255))
    checked = 0
    faces = [0] * 6
    # Per-face U/V axes, derived from the object-space face orientation.
    uv_axes = ((1,2), (2,1), (2,0), (0,2), (0,1), (1,0))
    samples = [[0] * 16 for _ in range(6)]
    for y in range(384):
        wy = 1.2 - (y + .5) * 2.4 / 384
        for x in range(512):
            if x < 8 and y < 8:
                expected = (255, 0, 0)
            else:
                wx = (x + .5) * 3.2 / 512 - 1.6
                local = (cb*wx + sa*sb*wy - ca*sb*4,
                         ca*wy + sa*4, sb*wx - sa*cb*wy + ca*cb*4)
                entries, exits = [], []
                for axis in range(3):
                    low = (-.6 - local[axis]) / direction[axis]
                    high = (.6 - local[axis]) / direction[axis]
                    entries.append((min(low, high), 2*axis + int(direction[axis] < 0)))
                    exits.append(max(low, high))
                entries.sort()
                enter, face = entries[-1]
                leave = min(exits)
                # Exclude narrowly bounded silhouette/face edges: Mesa uses
                # float transforms and raster edge rules, this oracle uses doubles.
                if abs(leave - enter) < .015:
                    continue
                if leave >= enter and 1 <= enter <= 10:
                    if enter - entries[-2][0] < .015:
                        continue
                    hit = tuple(local[i] + enter * direction[i] for i in range(3))
                    uv = tuple((hit[i] + .6) / 1.2 for i in uv_axes[face])
                    # Exclude only a narrow numerical band around texel edges.
                    if any(abs(t * 4 - round(t * 4)) < .0001 for t in uv):
                        continue
                    tx, ty = (max(0, min(3, math.floor(t * 4))) for t in uv)
                    level = 64 + 32 * tx + 8 * ty
                    expected = tuple(level if channel else 0 for channel in colors[face])
                    faces[face] += 1
                    samples[face][ty * 4 + tx] += 1
                else:
                    expected = (32, 32, 32)
            if pixels[x0+x, y0+y] != expected:
                raise ValueError(f"cube pixel {x},{y}: {pixels[x0+x,y0+y]} != {expected}")
            checked += 1
    if checked < 190000 or sum(count > 1000 for count in faces) != 3:
        raise ValueError(f"insufficient coverage: {checked}, faces={faces}")
    if any(min(samples[face]) < 100 for face in range(6) if faces[face]):
        raise ValueError(f"insufficient texel coverage: {samples}")
    return checked, faces


if __name__ == "__main__":
    try:
        with Image.open(sys.argv[1]) as image:
            checked, faces = check(image)
    except (OSError, ValueError, IndexError) as error:
        sys.exit(f"Mesa cube: FAIL {error}")
    print(f"Mesa cube: PASS {checked} geometric pixels; face samples={faces}")
