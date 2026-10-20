//! Shared outline rasterization for both glyph ABIs: light (vertical-only)
//! hinting, subpixel x offsets or (low density) vertical stems fitted to the
//! pixel grid, and the bounded coverage grid.
use ab_glyph_rasterizer::{Point, Rasterizer, point};
use alloc::vec::Vec;
use ttf_parser::{Face, GlyphId, OutlineBuilder};

/// Stem darkening (UI-026): how far the outline is emboldened before it is
/// hinted and rasterized, in font units: the total growth of a vertical stem
/// (`x`) and of a horizontal one (`y`), half on each side. FreeType's
/// auto-hinter does the same (FT_Outline_EmboldenXY) when stem darkening is
/// on; see `Darken::measure` for the amount.
#[derive(Clone, Copy, Default)]
pub(crate) struct Darken {
    pub(crate) x: f32,
    pub(crate) y: f32,
}

/// FreeType's default darkening curve (autofitter/CFF "darkening-parameters"
/// {500,400, 1000,275, 1667,275, 2333,0}): stem width in pixels to darkening
/// in pixels, piecewise linear, nothing past 2.333 px stems.
const DARKEN_CURVE: [(f32, f32); 4] = [(0.500, 0.400), (1.000, 0.275), (1.667, 0.275), (2.333, 0.0)];

fn darken_pixels(stem: f32) -> f32 {
    if stem <= DARKEN_CURVE[0].0 {
        return DARKEN_CURVE[0].1;
    }
    for pair in DARKEN_CURVE.windows(2) {
        let ((x0, y0), (x1, y1)) = (pair[0], pair[1]);
        if stem <= x1 {
            return y0 + (stem - x0) * (y1 - y0) / (x1 - x0);
        }
    }
    0.0
}

/// 1 / cos(half the sharpest turn that is shifted): sqrt(2 / (1 - 0.9375)).
const EMBOLDEN_REACH: f32 = 5.66;

/// Glyphs that give the face's standard stems: the left stem of a capital H
/// (its two smallest distinct point x coordinates) and the height of a
/// hyphen's box.
const VERTICAL_STEM: char = 'H';
const HORIZONTAL_STEM: char = '-';

/// The two smallest distinct x coordinates of an outline's points.
struct LeftEdges(f32, f32);
impl LeftEdges {
    fn take(&mut self, x: f32) {
        if x < self.0 {
            self.1 = self.0;
            self.0 = x;
        } else if x > self.0 && x < self.1 {
            self.1 = x;
        }
    }
}
impl OutlineBuilder for LeftEdges {
    fn move_to(&mut self, x: f32, _: f32) { self.take(x) }
    fn line_to(&mut self, x: f32, _: f32) { self.take(x) }
    fn quad_to(&mut self, x1: f32, _: f32, x: f32, _: f32) { self.take(x1); self.take(x) }
    fn curve_to(&mut self, x1: f32, _: f32, x2: f32, _: f32, x: f32, _: f32) {
        self.take(x1); self.take(x2); self.take(x)
    }
    fn close(&mut self) {}
}

impl Darken {
    pub(crate) const NONE: Self = Self { x: 0.0, y: 0.0 };

    /// Darkening for this face at `ratio` pixels per font unit, scaled by
    /// `gain` (1.0: FreeType's amount).
    /// The face's standard vertical stem in font units: the left stem of a
    /// capital H (its two smallest distinct point x coordinates).
    pub(crate) fn stem(font: &Face) -> Option<f32> {
        let mut edges = LeftEdges(f32::MAX, f32::MAX);
        font.outline_glyph(font.glyph_index(VERTICAL_STEM)?, &mut edges)?;
        (edges.1 < f32::MAX).then(|| edges.1 - edges.0)
    }

    pub(crate) fn measure(font: &Face, ratio: f32, gain: f32) -> Self {
        let vertical = || Self::stem(font);
        let horizontal = || -> Option<f32> {
            let b = font.glyph_bounding_box(font.glyph_index(HORIZONTAL_STEM)?)?;
            Some(f32::from(b.y_max - b.y_min))
        };
        let (Some(sx), Some(sy)) = (vertical(), horizontal()) else {
            return Self::NONE;
        };
        Self {
            x: gain * darken_pixels(sx * ratio) / ratio,
            y: gain * darken_pixels(sy * ratio) / ratio,
        }
    }

    fn active(&self) -> bool {
        self.x > 0.0 || self.y > 0.0
    }
}

/// One glyph outline in font units, kept so it can be emboldened as a whole:
/// every point (on-curve and control) in contour order, each with how it was
/// reached.
#[derive(Clone, Copy)]
enum Step {
    Move,
    Line,
    QuadControl,
    QuadEnd,
    CubicControl,
    CubicEnd,
}
#[derive(Default)]
pub(crate) struct Recorded {
    points: Vec<(f32, f32)>,
    steps: Vec<Step>,
    /// One past each contour's last point.
    ends: Vec<usize>,
}
impl Recorded {
    fn push(&mut self, step: Step, x: f32, y: f32) {
        self.points.push((x, y));
        self.steps.push(step);
    }
    pub(crate) fn close_contour(&mut self) {
        if self.ends.last().copied().unwrap_or(0) != self.points.len() {
            self.ends.push(self.points.len());
        }
    }
}
impl OutlineBuilder for Recorded {
    fn move_to(&mut self, x: f32, y: f32) {
        self.close_contour();
        self.push(Step::Move, x, y);
    }
    fn line_to(&mut self, x: f32, y: f32) {
        self.push(Step::Line, x, y);
    }
    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        self.push(Step::QuadControl, x1, y1);
        self.push(Step::QuadEnd, x, y);
    }
    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        self.push(Step::CubicControl, x1, y1);
        self.push(Step::CubicControl, x2, y2);
        self.push(Step::CubicEnd, x, y);
    }
    fn close(&mut self) {
        self.close_contour();
    }
}

impl Recorded {
    /// FreeType's FT_Outline_EmboldenXY, centred: each point moves along the
    /// bisector of its neighbouring edges, outward, so a stem grows by
    /// `darken.x` (vertical) or `darken.y` (horizontal) in total, half on each
    /// side; sharp turns and short segments are limited as FreeType limits
    /// them. Its glyph box and advance stay where they were.
    fn embolden(&mut self, darken: Darken) {
        let (sx, sy) = (darken.x / 2.0, darken.y / 2.0);
        // Orientation from the signed area: TrueType outlines (filled
        // clockwise in y-up font units) have negative area.
        let mut area = 0.0;
        let mut first = 0;
        for &end in &self.ends {
            for i in first..end {
                let (a, b) = (self.points[i], self.points[if i + 1 < end { i + 1 } else { first }]);
                area += a.0 * b.1 - b.0 * a.1;
            }
            first = end;
        }
        if area == 0.0 {
            return;
        }
        let truetype = area < 0.0;
        let original = self.points.clone();
        let mut first = 0;
        for &end in &self.ends {
            let n = end - first;
            for k in 0..n {
                let at = |m: usize| original[first + (m % n)];
                let current = at(k);
                // Nearest distinct neighbours on each side.
                let mut prev = None;
                for back in 1..n {
                    let p = at(k + n - back);
                    if p != current {
                        prev = Some(p);
                        break;
                    }
                }
                let mut next = None;
                for ahead in 1..n {
                    let p = at(k + ahead);
                    if p != current {
                        next = Some(p);
                        break;
                    }
                }
                let (Some(p), Some(q)) = (prev, next) else { continue };
                let unit = |dx: f32, dy: f32| -> (f32, f32, f32) {
                    let l = libm::sqrtf(dx * dx + dy * dy);
                    (dx / l, dy / l, l)
                };
                let (ix, iy, l_in) = unit(current.0 - p.0, current.1 - p.1);
                let (ox, oy, l_out) = unit(q.0 - current.0, q.1 - current.1);
                let d = ix * ox + iy * oy;
                // Shift only if the turn is less than about 160 degrees.
                if d <= -0.9375 {
                    continue;
                }
                let d = d + 1.0;
                let (mut shx, mut shy) = (iy + oy, ix + ox);
                if truetype {
                    shx = -shx;
                } else {
                    shy = -shy;
                }
                let mut q_sin = ox * iy - oy * ix;
                if truetype {
                    q_sin = -q_sin;
                }
                let l = l_in.min(l_out);
                shx = if sx * q_sin <= l * d { shx * sx / d } else { shx * l / q_sin };
                shy = if sy * q_sin <= l * d { shy * sy / d } else { shy * l / q_sin };
                self.points[first + k] = (current.0 + shx, current.1 + shy);
            }
            first = end;
        }
    }

    fn replay(&self, into: &mut impl OutlineBuilder) {
        let mut first = 0;
        for &end in &self.ends {
            let mut i = first;
            while i < end {
                let (x, y) = self.points[i];
                match self.steps[i] {
                    Step::Move => into.move_to(x, y),
                    Step::Line => into.line_to(x, y),
                    Step::QuadControl if i + 1 < end => {
                        let (ex, ey) = self.points[i + 1];
                        into.quad_to(x, y, ex, ey);
                        i += 1;
                    }
                    Step::CubicControl if i + 2 < end => {
                        let ((x2, y2), (ex, ey)) = (self.points[i + 1], self.points[i + 2]);
                        into.curve_to(x, y, x2, y2, ex, ey);
                        i += 2;
                    }
                    _ => into.line_to(x, y),
                }
                i += 1;
            }
            into.close();
            first = end;
        }
    }
}

/// Blue zones snapped by light hinting: (flat glyph, overshoot glyph, edge).
/// Each zone's flat edge is rounded to whole pixels; its overshoot is kept only
/// when at least half a pixel, as FreeType's auto-hinter does at small sizes.
#[derive(Clone, Copy)]
enum Edge {
    Bottom,
    Top,
}
const ZONES: [(char, char, Edge); 4] = [
    ('x', 'o', Edge::Bottom), // baseline, round bottoms below it
    ('x', 'o', Edge::Top),    // x-height, round tops above it
    ('H', 'O', Edge::Top),    // cap height
    ('l', 'l', Edge::Top),    // lowercase ascender (flat)
];
const ZONE_POINTS: usize = 2 * ZONES.len();
/// Overshoots below this many pixels are flattened onto their zone edge.
const OVERSHOOT_KEEP: f32 = 0.5;
/// FreeType's auto-hinter first fits the x-height to whole pixels and scales
/// every vertical metric by the same factor: it rounds up from this fraction
/// of a pixel (40/64 added before truncating), down below it.
const X_HEIGHT_ROUND_UP: f32 = 24.0 / 64.0;

/// Piecewise-linear vertical map from font units to pixels (y up). Outside
/// the zones it continues at the plain scale, so no point moves horizontally
/// and nothing outside the zones is distorted.
#[derive(Clone, Copy)]
pub(crate) struct Zones {
    from: [f32; ZONE_POINTS],
    to: [f32; ZONE_POINTS],
    count: usize,
    ratio: f32,
}

impl Zones {
    pub(crate) fn unhinted(ratio: f32) -> Self {
        Self { from: [0.0; ZONE_POINTS], to: [0.0; ZONE_POINTS], count: 0, ratio }
    }

    pub(crate) fn measure(font: &Face, ratio: f32) -> Self {
        let edge = |c: char, e: Edge| -> Option<f32> {
            let b = font.glyph_bounding_box(font.glyph_index(c)?)?;
            Some(f32::from(match e {
                Edge::Bottom => b.y_min,
                Edge::Top => b.y_max,
            }))
        };
        // The vertical scale that puts the flat x-height on a whole pixel.
        let ratio = match edge(ZONES[1].0, ZONES[1].2) {
            Some(x_height) if x_height > 0.0 => {
                let scaled = x_height * ratio;
                let fitted = libm::floorf(scaled + 1.0 - X_HEIGHT_ROUND_UP);
                if fitted > 0.0 { ratio * fitted / scaled } else { ratio }
            }
            _ => ratio,
        };
        let mut zones = Self::unhinted(ratio);
        let mut pairs = [(0.0f32, 0.0f32); ZONE_POINTS];
        for (i, &(flat, over, e)) in ZONES.iter().enumerate() {
            let (Some(f), Some(o)) = (edge(flat, e), edge(over, e)) else {
                return zones; // unexpected face: leave it unhinted
            };
            let snapped = libm::roundf(f * ratio);
            let delta = (o - f) * ratio;
            let kept = if libm::fabsf(delta) < OVERSHOOT_KEEP { 0.0 } else { libm::roundf(delta) };
            pairs[2 * i] = (f, snapped);
            pairs[2 * i + 1] = (o, snapped + kept);
        }
        // Sort by font unit; keep the map strictly increasing in its input and
        // non-decreasing in its output.
        for i in 1..ZONE_POINTS {
            let mut j = i;
            while j > 0 && pairs[j - 1].0 > pairs[j].0 {
                pairs.swap(j - 1, j);
                j -= 1;
            }
        }
        for &(f, t) in &pairs {
            if zones.count > 0 && f <= zones.from[zones.count - 1] {
                let last = zones.count - 1;
                zones.to[last] = zones.to[last].max(t);
                continue;
            }
            let t = if zones.count > 0 { t.max(zones.to[zones.count - 1]) } else { t };
            zones.from[zones.count] = f;
            zones.to[zones.count] = t;
            zones.count += 1;
        }
        zones
    }

    /// Pixels above the baseline for a y coordinate in font units.
    pub(crate) fn map(&self, y: f32) -> f32 {
        let n = self.count;
        if n == 0 {
            return y * self.ratio;
        }
        if y <= self.from[0] {
            return self.to[0] + (y - self.from[0]) * self.ratio;
        }
        if y >= self.from[n - 1] {
            return self.to[n - 1] + (y - self.from[n - 1]) * self.ratio;
        }
        let mut i = 0;
        while i + 2 < n && y >= self.from[i + 1] {
            i += 1;
        }
        let span = self.from[i + 1] - self.from[i];
        self.to[i] + (y - self.from[i]) * (self.to[i + 1] - self.to[i]) / span
    }
}

/// Grid fitting (UI-026, low density): a piecewise-linear horizontal map
/// from font units to pixels that puts both edges of every vertical stem on
/// whole pixels of the raster, each stem a whole number of pixels wide (at
/// least one). Between and outside the stems it is linear, so a glyph moves
/// and stretches by under a pixel and its advance is untouched. At low
/// density a stem is about one pixel wide (IBM Plex Sans at 13 px: 1.14 px):
/// on a quarter-pixel phase or a fractional edge it spreads over two pixels
/// at half contrast, the blur grid fitting removes. Used only for rasters
/// placed on whole pixels (one phase).
#[derive(Clone)]
pub(crate) struct Columns {
    from: Vec<f32>,
    to: Vec<f32>,
    ratio: f32,
}

/// One straight or round vertical edge of an outline, in font units.
#[derive(Clone, Copy)]
struct Side {
    x: f32,
    low: f32,
    high: f32,
    /// The outline runs upward along it (y up).
    up: bool,
}

/// Points this close in x (font units) count as one vertical line.
const VERTICAL_TOLERANCE: f32 = 1.0;
/// An edge shorter than this many pixels is not a stem's side (a serif
/// tip, a bar's end).
const EDGE_MIN_PIXELS: f32 = 1.0;
/// A pair wider than this many standard stems is a glyph's outline, not a
/// stem (the face's standard stem: the left stem of a capital H).
const STEM_MAX_STEMS: f32 = 2.5;

impl Columns {
    pub(crate) fn plain(ratio: f32) -> Self {
        Self { from: Vec::new(), to: Vec::new(), ratio }
    }

    /// The vertical edges of an outline: its straight vertical segments and
    /// the vertical tangents of its curves (a round glyph's extremes: an
    /// on-curve point with a control point straight above or below it),
    /// joined when they lie on one x and run the same way (the two halves
    /// of an 'o''s side, the pieces of an 'e''s bowl on two contours).
    fn edges(rec: &Recorded, min_length: f32) -> Vec<Side> {
        let on_curve = |step: Step| matches!(step, Step::Move | Step::Line | Step::QuadEnd | Step::CubicEnd);
        let control = |step: Step| matches!(step, Step::QuadControl | Step::CubicControl);
        let mut pieces: Vec<Side> = Vec::new();
        let mut piece = |a: (f32, f32), b: (f32, f32)| {
            if libm::fabsf(b.0 - a.0) <= VERTICAL_TOLERANCE && b.1 != a.1 {
                pieces.push(Side { x: (a.0 + b.0) / 2.0, low: a.1.min(b.1), high: a.1.max(b.1), up: b.1 > a.1 });
            }
        };
        let mut first = 0;
        for &end in &rec.ends {
            let n = end - first;
            for k in 0..n {
                let at = |m: usize| first + (m % n);
                let (i, next, prev) = (at(k), at(k + 1), at(k + n - 1));
                if !on_curve(rec.steps[i]) {
                    continue;
                }
                let (p, q, o) = (rec.points[i], rec.points[next], rec.points[prev]);
                // A straight segment: into a Line point, or the contour's
                // closing segment back to its Move.
                if matches!(rec.steps[next], Step::Line | Step::Move) {
                    piece(p, q);
                }
                if control(rec.steps[prev]) {
                    piece(o, p);
                }
                if control(rec.steps[next]) {
                    piece(p, q);
                }
            }
            first = end;
        }
        pieces.sort_by(|a, b| a.x.total_cmp(&b.x));
        let mut joined: Vec<Side> = Vec::new();
        for item in pieces {
            if let Some(same) = joined
                .iter_mut()
                .find(|j| j.up == item.up && libm::fabsf(j.x - item.x) <= VERTICAL_TOLERANCE)
            {
                same.low = same.low.min(item.low);
                same.high = same.high.max(item.high);
            } else {
                joined.push(item);
            }
        }
        joined.retain(|e| e.high - e.low >= min_length);
        joined
    }

    /// Fit `rec` (glyph outline, font units) at `ratio` pixels per unit with
    /// its pen `shift` pixels right of the raster's left edge. `stem` is the
    /// face's standard stem in font units (`Darken::stem`).
    pub(crate) fn fit(rec: &Recorded, ratio: f32, shift: f32, stem: f32) -> Self {
        let mut result = Self::plain(ratio);
        if ratio <= 0.0 {
            return result;
        }
        // Signed area: TrueType outer contours run clockwise (y up, negative
        // area); there a stem's left edge runs upward.
        let mut area = 0.0;
        let mut first = 0;
        for &end in &rec.ends {
            for i in first..end {
                let (a, b) = (rec.points[i], rec.points[if i + 1 < end { i + 1 } else { first }]);
                area += a.0 * b.1 - b.0 * a.1;
            }
            first = end;
        }
        if area == 0.0 {
            return result;
        }
        let left_runs_up = area < 0.0;
        let edges = Self::edges(rec, EDGE_MIN_PIXELS / ratio);
        let widest = if stem > 0.0 { STEM_MAX_STEMS * stem } else { f32::MAX };
        // Every (left, right) pair that could be a stem, narrowest first;
        // each edge joins at most one stem.
        let mut pairs: Vec<(f32, usize, usize)> = Vec::new();
        for (l, a) in edges.iter().enumerate() {
            if a.up != left_runs_up {
                continue;
            }
            for (r, b) in edges.iter().enumerate() {
                let overlap = a.high.min(b.high) - a.low.max(b.low);
                if b.up == left_runs_up || b.x <= a.x || overlap <= 0.0 || b.x - a.x > widest {
                    continue;
                }
                pairs.push((b.x - a.x, l, r));
            }
        }
        pairs.sort_by(|x, y| x.0.total_cmp(&y.0));
        let mut used = alloc::vec![false; edges.len()];
        let mut points: Vec<(f32, f32)> = Vec::new();
        for &(_, l, r) in &pairs {
            if used[l] || used[r] {
                continue;
            }
            used[l] = true;
            used[r] = true;
            let (left, right) = (edges[l].x * ratio + shift, edges[r].x * ratio + shift);
            let width = libm::roundf(right - left).max(1.0);
            let start = libm::floorf((left + right - width) / 2.0 + 0.5);
            points.push((edges[l].x, start - shift));
            points.push((edges[r].x, start + width - shift));
        }
        points.sort_by(|x, y| x.0.total_cmp(&y.0));
        // Keep the map strictly increasing in font units and increasing in
        // pixels; a point that would fold the glyph is dropped.
        for (f, t) in points {
            if let (Some(&lf), Some(&lt)) = (result.from.last(), result.to.last()) {
                if f <= lf || t <= lt {
                    continue;
                }
            }
            result.from.push(f);
            result.to.push(t);
        }
        result
    }

    /// Pixels right of the pen for an x coordinate in font units.
    pub(crate) fn map(&self, x: f32) -> f32 {
        let n = self.from.len();
        if n == 0 {
            return x * self.ratio;
        }
        if x <= self.from[0] {
            return self.to[0] + (x - self.from[0]) * self.ratio;
        }
        if x >= self.from[n - 1] {
            return self.to[n - 1] + (x - self.from[n - 1]) * self.ratio;
        }
        let mut i = 0;
        while i + 2 < n && x >= self.from[i + 1] {
            i += 1;
        }
        self.to[i] + (x - self.from[i]) * (self.to[i + 1] - self.to[i]) / (self.from[i + 1] - self.from[i])
    }
}

// Stream contours into the coverage rasterizer instead of first allocating a
// Vec containing every curve. Only the bounded coverage grid needs scratch.
pub(crate) struct Contours {
    raster: Rasterizer,
    columns: Columns,
    shift: f32,
    left: f32,
    zones: Zones,
    top: f32,
    first: Point,
    current: Point,
}
impl Contours {
    // Pixel position first, then the grid origin: the same expressions that
    // chose `left` and `top`, so the extreme points land at or right of 0.
    // (Folding the origin into the shift first can put an edge that lies
    // exactly on a pixel boundary a rounding error left of the grid, where
    // its coverage is lost.)
    fn transform(&self, x: f32, y: f32) -> Point {
        point((self.columns.map(x) + self.shift) - self.left, -self.zones.map(y) - self.top)
    }
}
impl OutlineBuilder for Contours {
    fn move_to(&mut self, x: f32, y: f32) {
        self.current = self.transform(x, y);
        self.first = self.current;
    }
    fn line_to(&mut self, x: f32, y: f32) {
        let end = self.transform(x, y);
        self.raster.draw_line(self.current, end);
        self.current = end;
    }
    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        let control = self.transform(x1, y1);
        let end = self.transform(x, y);
        self.raster.draw_quad(self.current, control, end);
        self.current = end;
    }
    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        let control1 = self.transform(x1, y1);
        let control2 = self.transform(x2, y2);
        let end = self.transform(x, y);
        self.raster.draw_cubic(self.current, control1, control2, end);
        self.current = end;
    }
    fn close(&mut self) {
        self.raster.draw_line(self.current, self.first);
        self.current = self.first;
    }
}

/// A glyph outline already streamed into its coverage grid. Grid cell (0, 0)
/// lies at (`left`, `top`) relative to the pen origin on the baseline.
pub(crate) struct Prepared {
    contours: Contours,
    pub(crate) left: i32,
    pub(crate) top: i32,
}

pub(crate) enum Outline {
    Empty,
    Ready(Prepared),
}

/// Prepare glyph `id` with the pen at x offset `shift` pixels (subpixel phase
/// and origin margin), hinted through `zones`, its stems fitted to the
/// pixel grid when `grid` (then not emboldened: a stem fitted to whole
/// pixels needs no darkening). Rejects (None) grids larger than
/// `max_width` x `max_height`, before allocating.
#[allow(clippy::too_many_arguments)]
pub(crate) fn prepare(
    font: &Face,
    id: GlyphId,
    ratio: f32,
    shift: f32,
    zones: Zones,
    darken: Darken,
    grid: bool,
    max_width: u32,
    max_height: u32,
) -> Option<Outline> {
    let Some(bounds) = font.glyph_bounding_box(id) else {
        return Some(Outline::Empty);
    };
    let darken = if grid { Darken::NONE } else { darken };
    let mut recorded = Recorded::default();
    let columns = if grid {
        font.outline_glyph(id, &mut recorded)?;
        recorded.close_contour();
        Columns::fit(&recorded, ratio, shift, Darken::stem(font).unwrap_or(0.0))
    } else {
        Columns::plain(ratio)
    };
    // Emboldening moves a point at most EMBOLDEN_REACH times its per-side
    // strength along an axis (at the sharpest turn it still shifts).
    let (gx, gy) = (EMBOLDEN_REACH * darken.x / 2.0, EMBOLDEN_REACH * darken.y / 2.0);
    let left = libm::floorf(columns.map(f32::from(bounds.x_min) - gx) + shift);
    let right = libm::ceilf(columns.map(f32::from(bounds.x_max) + gx) + shift);
    let top = libm::floorf(-zones.map(f32::from(bounds.y_max) + gy));
    let bottom = libm::ceilf(-zones.map(f32::from(bounds.y_min) - gy));
    let (width, height) = (right - left, bottom - top);
    // Bundled faces only. Reject unexpected font metrics before allocating,
    // judged on the outline's own box (the emboldening margin is bounded by
    // the darkening curve: a few pixels at most); retain the full bounds so
    // overhangs do not corrupt coverage.
    let plain_width = columns.map(f32::from(bounds.x_max)) - columns.map(f32::from(bounds.x_min));
    let plain_height = zones.map(f32::from(bounds.y_max)) - zones.map(f32::from(bounds.y_min));
    let margin = 2.0 * libm::ceilf(EMBOLDEN_REACH * DARKEN_CURVE[0].1 / 2.0) + 2.0;
    if !(0.0..=max_width as f32).contains(&plain_width)
        || !(0.0..=max_height as f32).contains(&plain_height)
        || width > max_width as f32 + margin
        || height > max_height as f32 + margin
    {
        return None;
    }
    let mut contours = Contours {
        raster: Rasterizer::new(width as usize, height as usize),
        columns,
        shift,
        left,
        zones,
        top,
        first: point(0.0, 0.0),
        current: point(0.0, 0.0),
    };
    if grid {
        recorded.replay(&mut contours);
    } else if darken.active() {
        let mut recorded = Recorded::default();
        font.outline_glyph(id, &mut recorded)?;
        recorded.close_contour();
        recorded.embolden(darken);
        recorded.replay(&mut contours);
    } else {
        font.outline_glyph(id, &mut contours)?;
    }
    Some(Outline::Ready(Prepared { contours, left: left as i32, top: top as i32 }))
}

impl Prepared {
    /// Visit every grid pixel as (x, y) relative to the pen origin on the
    /// baseline, with coverage rounded to 0..=255.
    pub(crate) fn each(self, mut visit: impl FnMut(i32, i32, u8)) {
        let (left, top) = (self.left, self.top);
        self.contours.raster.for_each_pixel_2d(|x, y, coverage| {
            visit(x as i32 + left, y as i32 + top, (coverage.clamp(0.0, 1.0) * 255.0 + 0.5) as u8);
        });
    }
}
