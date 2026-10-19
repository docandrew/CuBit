//! Shared userspace TrueType rasterizer. Only bundled font bytes are accepted.
#![cfg_attr(target_os = "none", no_std)]
#![deny(unsafe_op_in_unsafe_fn)]

extern crate alloc;

mod density;
mod outline;
pub use density::{Metrics as RasterMetrics, Request as RasterRequest, cubit_font_raster_mask};

use alloc::boxed::Box;
use core::{
    cell::UnsafeCell,
    ptr,
    sync::atomic::{AtomicPtr, AtomicU8, Ordering},
};
use outline::{Darken, Outline, Zones};
use ttf_parser::{Face, GlyphId, Tag, gpos::PairAdjustment, gpos::PositioningSubtable};

const SANS: &[u8] = include_bytes!(env!("IBM_PLEX_SANS_FONT"));
const MONO: &[u8] = include_bytes!(env!("IBM_PLEX_MONO_FONT"));
const SEMIBOLD: &[u8] = include_bytes!(env!("IBM_PLEX_SANS_SEMIBOLD_FONT"));
/// Face numbers of the C/Ada ABI, in `CuBit.Fonts.Face` order.
pub const FACE_SANS: u32 = 0;
pub const FACE_MONO: u32 = 1;
pub const FACE_SANS_SEMIBOLD: u32 = 2;
pub const WIDTH: usize = 32;
pub const HEIGHT: usize = 36;
const FACES: usize = 3;
const SIZES: usize = 2;
const FIRST_CODE: u32 = 32;
const LAST_CODE: u32 = 126;
const CHARACTERS: usize = (LAST_CODE - FIRST_CODE + 1) as usize;
const FALLBACK_CODE: u32 = b'?' as u32;
/// Logical em and line box of the normal raster size, in pixels.
pub const BASE_EM: f32 = 13.0;
pub const BASE_LINE: u32 = 17;
/// Pixels of every glyph raster left of its pen origin, so ink before the
/// origin (left side bearings below zero, kerned overlaps) is kept.
pub const ORIGIN: u32 = 2;
/// Horizontal subpixel phases per glyph: pen positions in quarter pixels.
pub const PHASES: u32 = 4;
/// Fixed-point layout unit: 1/64 pixel.
pub const PEN_UNIT: f32 = 64.0;
/// How glyphs are shaped before rasterizing: light (vertical-only) hinting
/// and stem darkening. Tests turn both off to compare with ab_glyph.
#[derive(Clone, Copy)]
pub(crate) struct Style {
    pub(crate) hinted: bool,
    pub(crate) darkened: bool,
}
pub(crate) const STYLE: Style = Style { hinted: true, darkened: true };
#[cfg(test)]
pub(crate) const PLAIN: Style = Style { hinted: false, darkened: false };
/// Stem darkening relative to FreeType's default curve. 1.0 measured best
/// against the FreeType reference (tests/ui-fonts/quality; 1.5 and 2.0 are
/// heavier than FreeType at every density).
pub(crate) const DARKEN_GAIN: f32 = 1.0;
/// Whether horizontal stems are darkened too. Off: growing them moves the
/// blue-zone edges (baseline, x-height) off the pixel grid, and the row
/// profile against FreeType gets worse (5.7% -> 10% at 13 px).
pub(crate) const DARKEN_Y: bool = false;

pub(crate) fn darken(font: &Face, ratio: f32, style: Style) -> Darken {
    if !style.darkened {
        return Darken::NONE;
    }
    let d = Darken::measure(font, ratio, DARKEN_GAIN);
    if DARKEN_Y { d } else { Darken { y: 0.0, ..d } }
}

fn face_bytes(face: usize) -> &'static [u8] {
    match face as u32 {
        FACE_SANS => SANS,
        FACE_MONO => MONO,
        _ => SEMIBOLD,
    }
}

fn printable(code: u32) -> u32 {
    if (FIRST_CODE..=LAST_CODE).contains(&code) { code } else { FALLBACK_CODE }
}

fn round_positive_metric(value: f32) -> u32 {
    (value + 0.5) as u32
}

pub(crate) fn zones(font: &Face, ratio: f32, hinted: bool) -> Zones {
    if hinted { Zones::measure(font, ratio) } else { Zones::unhinted(ratio) }
}

/// Stable C/Ada ABI. Coverage is grayscale, not LCD subpixel color. The pen
/// origin is column `origin` on baseline row round(ascender); the cell keeps
/// all ink, also left of the origin and past the advance. Rows/columns
/// outside the ink box are zero.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Cell {
    /// Unhinted advance in 1/64 pixel.
    pub advance: u32,
    pub height: u32,
    pub origin: u32,
    /// Ink box: the only columns/rows that may hold coverage (width 0: none).
    pub ink_left: u32,
    pub ink_top: u32,
    pub ink_width: u32,
    pub ink_height: u32,
    pub reserved: u32,
    pub alpha: [[u8; WIDTH]; HEIGHT],
}
impl Cell {
    const EMPTY: Self = Self {
        advance: 0,
        height: 0,
        origin: ORIGIN,
        ink_left: 0,
        ink_top: 0,
        ink_width: 0,
        ink_height: 0,
        reserved: 0,
        alpha: [[0; WIDTH]; HEIGHT],
    };
}

fn rasterize(face: usize, size: usize, code: u32, phase: u32, style: Style) -> Option<Cell> {
    let font = Face::parse(face_bytes(face), 0).ok()?;
    let scale = if size == 0 { 1.0 } else { 2.0 };
    let ratio = BASE_EM * scale / f32::from(font.units_per_em());
    let id = font.glyph_index(char::from_u32(code)?)?;
    let advance = libm::roundf(f32::from(font.glyph_hor_advance(id)?) * ratio * PEN_UNIT) as u32;
    let mut cell = Cell {
        advance: advance.max(1),
        height: BASE_LINE * scale as u32,
        ..Cell::EMPTY
    };
    let baseline = round_positive_metric(f32::from(font.ascender()) * ratio) as i32;
    let shift = ORIGIN as f32 + phase as f32 / PHASES as f32;
    let zones = zones(&font, ratio, style.hinted);
    if let Outline::Ready(prepared) =
        outline::prepare(&font, id, ratio, shift, zones, darken(&font, ratio, style), WIDTH as u32, HEIGHT as u32)?
    {
        let (mut x0, mut y0, mut x1, mut y1) = (WIDTH as i32, HEIGHT as i32, -1, -1);
        prepared.each(|x, y, coverage| {
            let y = y + baseline;
            if coverage != 0 && x >= 0 && y >= 0 && x < WIDTH as i32 && y < cell.height as i32 {
                cell.alpha[y as usize][x as usize] = coverage;
                (x0, y0, x1, y1) = (x0.min(x), y0.min(y), x1.max(x), y1.max(y));
            }
        });
        if x1 >= x0 {
            cell.ink_left = x0 as u32;
            cell.ink_top = y0 as u32;
            cell.ink_width = (x1 - x0 + 1) as u32;
            cell.ink_height = (y1 - y0 + 1) as u32;
        }
    }
    Some(cell)
}

// Published cells: only the thread winning BUILDING writes; a release
// publication makes the heap cell permanently immutable and never evicted.
const EMPTY: u8 = 0;
const BUILDING: u8 = 1;
const READY: u8 = 2;
const FAILED: u8 = 3;
struct Entry {
    state: AtomicU8,
    cell: AtomicPtr<Cell>,
}
impl Entry {
    const fn new() -> Self {
        Self { state: AtomicU8::new(EMPTY), cell: AtomicPtr::new(ptr::null_mut()) }
    }
}
// Three faces, two native raster sizes, four phases, printable ASCII: a bounded
// table of pointers; a cell (1,184 bytes) is allocated the first time it is
// drawn, so a process pays only for the glyphs and phases it uses.
static CACHE: [Entry; FACES * SIZES * PHASES as usize * CHARACTERS] =
    [const { Entry::new() }; FACES * SIZES * PHASES as usize * CHARACTERS];

/// Returns a permanent, read-only cell, or null for unsupported face/size/phase.
/// Unknown characters intentionally map to '?'; UTF-8 shaping is separate.
#[unsafe(no_mangle)]
pub extern "C" fn cubit_font_glyph(face: u32, size: u32, code: u32, phase: u32) -> *const Cell {
    if face as usize >= FACES || size as usize >= SIZES || phase >= PHASES {
        return ptr::null();
    }
    let code = printable(code);
    let index = ((face as usize * SIZES + size as usize) * PHASES as usize + phase as usize) * CHARACTERS
        + (code - FIRST_CODE) as usize;
    let entry = &CACHE[index];
    loop {
        match entry.state.load(Ordering::Acquire) {
            READY => return entry.cell.load(Ordering::Relaxed),
            FAILED => return ptr::null(),
            EMPTY => {
                if entry
                    .state
                    .compare_exchange(EMPTY, BUILDING, Ordering::Acquire, Ordering::Relaxed)
                    .is_ok()
                {
                    match rasterize(face as usize, size as usize, code, phase, STYLE) {
                        Some(cell) => {
                            entry.cell.store(Box::into_raw(Box::new(cell)), Ordering::Relaxed);
                            entry.state.store(READY, Ordering::Release);
                            return entry.cell.load(Ordering::Relaxed);
                        }
                        None => {
                            entry.state.store(FAILED, Ordering::Release);
                            return ptr::null();
                        }
                    }
                }
            }
            _ => core::hint::spin_loop(),
        }
    }
}

/// Per-face layout metrics at the normal (logical) size, in 1/64 pixel:
/// unhinted advances and GPOS pair kerning, indexed by code - 32.
#[repr(C)]
pub struct Layout {
    pub advance: [u32; CHARACTERS],
    pub kerning: [[i16; CHARACTERS]; CHARACTERS],
}
struct LayoutEntry {
    state: AtomicU8,
    layout: UnsafeCell<Layout>,
}
// Same publication protocol as Entry; static because a table (18 KiB) is
// larger than the native small-object allocation ceiling.
unsafe impl Sync for LayoutEntry {}
static LAYOUTS: [LayoutEntry; FACES] = [const {
    LayoutEntry {
        state: AtomicU8::new(EMPTY),
        layout: UnsafeCell::new(Layout { advance: [0; CHARACTERS], kerning: [[0; CHARACTERS]; CHARACTERS] }),
    }
}; FACES];

const KERN_FEATURE: Tag = Tag::from_bytes(b"kern");
const SCRIPTS: [Tag; 2] = [Tag::from_bytes(b"latn"), Tag::from_bytes(b"DFLT")];
const LOOKUPS_MAX: usize = 16;

/// Lookups of the default language's `kern` feature, latin script first.
fn kern_lookups(font: &Face, lookups: &mut [u16; LOOKUPS_MAX]) -> usize {
    let Some(gpos) = font.tables().gpos else { return 0 };
    let mut count = 0;
    for tag in SCRIPTS {
        let Some(language) = gpos.scripts.find(tag).and_then(|s| s.default_language) else {
            continue;
        };
        for index in language.feature_indices {
            let Some(feature) = gpos.features.get(index) else { continue };
            if feature.tag != KERN_FEATURE {
                continue;
            }
            for lookup in feature.lookup_indices {
                if count < LOOKUPS_MAX && !lookups[..count].contains(&lookup) {
                    lookups[count] = lookup;
                    count += 1;
                }
            }
        }
        if count > 0 {
            break;
        }
    }
    count
}

/// Pair kerning in font units into `kerning`: in each lookup, the first pair
/// subtable that matches a pair decides it; lookups add, as a shaper applies
/// them in turn. Coverage and classes are resolved once per subtable.
fn build_kerning(font: &Face, lookups: &[u16], ids: &[GlyphId; CHARACTERS], kerning: &mut [[i16; CHARACTERS]; CHARACTERS]) {
    let Some(gpos) = font.tables().gpos else { return };
    for &index in lookups {
        let Some(lookup) = gpos.lookups.get(index) else { continue };
        // Pairs this lookup has already decided: one bit per right glyph.
        let mut decided = [0u128; CHARACTERS];
        for subtable in lookup.subtables.into_iter::<PositioningSubtable>() {
            let PositioningSubtable::Pair(pair) = subtable else { continue };
            let mut second = [0u16; CHARACTERS];
            if let PairAdjustment::Format2 { classes, .. } = &pair {
                for (r, id) in ids.iter().enumerate() {
                    second[r] = classes.1.get(*id);
                }
            }
            for (l, &left) in ids.iter().enumerate() {
                let Some(covered) = pair.coverage().get(left) else { continue };
                let set = match &pair {
                    PairAdjustment::Format1 { sets, .. } => sets.get(covered),
                    PairAdjustment::Format2 { .. } => None,
                };
                for (r, &right) in ids.iter().enumerate() {
                    if decided[l] & (1u128 << r) != 0 {
                        continue;
                    }
                    let found = match &pair {
                        PairAdjustment::Format1 { .. } => set.as_ref().and_then(|s| s.get(right)),
                        PairAdjustment::Format2 { classes, matrix, .. } => {
                            matrix.get((classes.0.get(left), second[r]))
                        }
                    };
                    if let Some((first, _)) = found {
                        kerning[l][r] = kerning[l][r].saturating_add(first.x_advance);
                        decided[l] |= 1u128 << r;
                    }
                }
            }
        }
    }
}

fn build_layout(face: usize, layout: &mut Layout) -> Option<()> {
    let font = Face::parse(face_bytes(face), 0).ok()?;
    let ratio = BASE_EM / f32::from(font.units_per_em());
    let scaled = |units: f32| libm::roundf(units * ratio * PEN_UNIT);
    let mut ids = [GlyphId(0); CHARACTERS];
    for (i, id) in ids.iter_mut().enumerate() {
        *id = font.glyph_index(char::from_u32(FIRST_CODE + i as u32)?)?;
        layout.advance[i] = (scaled(f32::from(font.glyph_hor_advance(*id)?)) as u32).max(1);
    }
    let mut lookups = [0u16; LOOKUPS_MAX];
    let count = kern_lookups(&font, &mut lookups);
    build_kerning(&font, &lookups[..count], &ids, &mut layout.kerning);
    for value in layout.kerning.iter_mut().flatten() {
        *value = scaled(f32::from(*value)) as i16;
    }
    Some(())
}

/// Returns the face's permanent layout table, or null for an unknown face.
#[unsafe(no_mangle)]
pub extern "C" fn cubit_font_layout(face: u32) -> *const Layout {
    let Some(entry) = LAYOUTS.get(face as usize) else { return ptr::null() };
    loop {
        match entry.state.load(Ordering::Acquire) {
            READY => return entry.layout.get().cast_const(),
            FAILED => return ptr::null(),
            EMPTY => {
                if entry
                    .state
                    .compare_exchange(EMPTY, BUILDING, Ordering::Acquire, Ordering::Relaxed)
                    .is_ok()
                {
                    // SAFETY: this thread exclusively owns the unpublished table.
                    let built = build_layout(face as usize, unsafe { &mut *entry.layout.get() });
                    entry.state.store(if built.is_some() { READY } else { FAILED }, Ordering::Release);
                }
            }
            _ => core::hint::spin_loop(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ab_glyph::{Font, FontRef, Glyph, PxScale};

    fn ascii(c: char) -> usize {
        c as usize - FIRST_CODE as usize
    }

    #[test]
    fn unhinted_outlines_match_reference_renderer_at_every_phase() {
        for face in 0..FACES {
            let font = FontRef::try_from_slice(face_bytes(face)).unwrap();
            for size in 0..SIZES {
                let ratio = BASE_EM * (size + 1) as f32 / font.units_per_em().unwrap();
                for phase in 0..PHASES {
                    for code in FIRST_CODE..=LAST_CODE {
                        let cell = rasterize(face, size, code, phase, PLAIN).unwrap();
                        let mut expected = [[0u8; WIDTH]; HEIGHT];
                        let glyph = Glyph {
                            id: font.glyph_id(char::from_u32(code).unwrap()),
                            scale: PxScale::from(font.height_unscaled() * ratio),
                            position: ab_glyph::point(
                                ORIGIN as f32 + phase as f32 / PHASES as f32,
                                round_positive_metric(font.ascent_unscaled() * ratio) as f32,
                            ),
                        };
                        if let Some(outline) = font.outline_glyph(glyph) {
                            let bounds = outline.px_bounds();
                            outline.draw(|x, y, coverage| {
                                let x = x as i32 + bounds.min.x as i32;
                                let y = y as i32 + bounds.min.y as i32;
                                if x >= 0 && y >= 0 && x < WIDTH as i32 && y < cell.height as i32 {
                                    expected[y as usize][x as usize] =
                                        (coverage.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
                                }
                            });
                        }
                        for (actual, reference) in cell.alpha.iter().flatten().zip(expected.iter().flatten()) {
                            // Equivalent scale arithmetic can differ at a rounding boundary.
                            assert!(
                                actual.abs_diff(*reference) <= 1,
                                "face={face} size={size} phase={phase} code={code}: {actual} vs {reference}"
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn cells_are_bounded_and_cached() {
        for face in 0..FACES as u32 {
            for size in 0..SIZES as u32 {
                for phase in 0..PHASES {
                    for code in FIRST_CODE..=LAST_CODE {
                        let pointer = cubit_font_glyph(face, size, code, phase);
                        assert!(!pointer.is_null());
                        assert_eq!(pointer, cubit_font_glyph(face, size, code, phase));
                        let cell = unsafe { &*pointer };
                        assert!(cell.advance >= 1 && cell.advance <= WIDTH as u32 * PEN_UNIT as u32);
                        assert!(cell.height <= HEIGHT as u32 && cell.origin == ORIGIN);
                        assert!(cell.ink_left + cell.ink_width <= WIDTH as u32);
                        assert!(cell.ink_top + cell.ink_height <= cell.height);
                        for (y, row) in cell.alpha.iter().enumerate() {
                            for (x, &a) in row.iter().enumerate() {
                                let inside = (x as u32) >= cell.ink_left
                                    && (x as u32) < cell.ink_left + cell.ink_width
                                    && (y as u32) >= cell.ink_top
                                    && (y as u32) < cell.ink_top + cell.ink_height;
                                assert!(inside || a == 0, "ink outside box {face}/{size}/{phase}/{code}");
                            }
                        }
                        if code != b' ' as u32 {
                            assert!(cell.ink_width > 0 && cell.alpha.iter().flatten().any(|&a| a != 0));
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn phases_shift_ink_right_without_changing_its_amount() {
        // Darkening off: a stem it widens past a full pixel saturates, which
        // moves the clamped coverage's centroid by more than the phase does.
        let style = Style { hinted: true, darkened: false };
        for code in [b'l', b'o', b'H', b'm'] {
            let total = |phase| -> u32 {
                let cell = &rasterize(0, 0, code as u32, phase, style).unwrap();
                cell.alpha.iter().flatten().map(|&a| u32::from(a)).sum()
            };
            let centroid = |phase| -> f32 {
                let cell = &rasterize(0, 0, code as u32, phase, style).unwrap();
                let mut sum = 0.0;
                let mut weight = 0.0;
                for row in &cell.alpha {
                    for (x, &a) in row.iter().enumerate() {
                        sum += x as f32 * f32::from(a);
                        weight += f32::from(a);
                    }
                }
                sum / weight
            };
            let base = total(0) as f32;
            for phase in 1..PHASES {
                let moved = centroid(phase) - centroid(0);
                let expected = phase as f32 / PHASES as f32;
                assert!((moved - expected).abs() < 0.08, "{code} phase {phase}: {moved}");
                assert!((total(phase) as f32 - base).abs() / base < 0.03);
            }
        }
    }

    #[test]
    fn light_hinting_snaps_baseline_and_x_height_rows() {
        // A flat-topped, flat-bottomed lowercase glyph: its top and bottom ink
        // rows are solid after hinting (edges on the pixel grid), and the
        // hinting never moves ink sideways.
        for face in 0..FACES {
            let hinted = Style { hinted: true, darkened: false };
            let x = rasterize(face, 0, b'x' as u32, 0, hinted).unwrap();
            let plain = rasterize(face, 0, b'z' as u32, 0, PLAIN).unwrap();
            let z = rasterize(face, 0, b'z' as u32, 0, hinted).unwrap();
            let row_max = |c: &Cell, y: u32| *c.alpha[y as usize].iter().max().unwrap();
            let top = z.ink_top;
            let bottom = z.ink_top + z.ink_height - 1;
            // Thin mono bars cover a little under a pixel once on the grid.
            assert!(row_max(&z, top) >= 230 && row_max(&z, bottom) >= 230, "face {face}");
            assert!(top == 0 || row_max(&z, top - 1) == 0, "nothing above the x-height row");
            assert_eq!(bottom, 12, "baseline row above round(ascender) = 13");
            assert_eq!(x.ink_top, z.ink_top, "x and z share the x-height");
            let column_sum = |c: &Cell| -> [u32; WIDTH] {
                let mut s = [0u32; WIDTH];
                for row in &c.alpha {
                    for (x, &a) in row.iter().enumerate() {
                        s[x] += u32::from(a);
                    }
                }
                s
            };
            let (a, b) = (column_sum(&z), column_sum(&plain));
            let first = |s: &[u32; WIDTH]| s.iter().position(|&v| v > 0).unwrap();
            let last = |s: &[u32; WIDTH]| WIDTH - 1 - s.iter().rev().position(|&v| v > 0).unwrap();
            assert_eq!((first(&a), last(&a)), (first(&b), last(&b)), "no horizontal change");
        }
    }

    #[test]
    fn layout_advances_and_kerning() {
        let sans = unsafe { &*cubit_font_layout(FACE_SANS) };
        let mono = unsafe { &*cubit_font_layout(FACE_MONO) };
        let semibold = unsafe { &*cubit_font_layout(FACE_SANS_SEMIBOLD) };
        assert_eq!(cubit_font_layout(0), cubit_font_layout(0));
        assert!(cubit_font_layout(FACES as u32).is_null());
        let font = Face::parse(SANS, 0).unwrap();
        let ratio = BASE_EM / f32::from(font.units_per_em());
        for code in FIRST_CODE..=LAST_CODE {
            let id = font.glyph_index(char::from_u32(code).unwrap()).unwrap();
            let exact = f32::from(font.glyph_hor_advance(id).unwrap()) * ratio * PEN_UNIT;
            assert!((sans.advance[(code - FIRST_CODE) as usize] as f32 - exact).abs() <= 0.5);
            let cell = unsafe { &*cubit_font_glyph(0, 0, code, 0) };
            assert_eq!(cell.advance, sans.advance[(code - FIRST_CODE) as usize]);
            // Mono: 600/1000 em, the same advance for every character.
            assert_eq!(mono.advance[(code - FIRST_CODE) as usize], mono.advance[0]);
        }
        // Classic kerning pairs pull together; plain pairs stay put.
        for pair in ["AV", "To", "Te", "LT", "Ty", "P."] {
            let mut c = pair.chars();
            let (l, r) = (ascii(c.next().unwrap()), ascii(c.next().unwrap()));
            assert!(sans.kerning[l][r] < 0, "{pair}: {}", sans.kerning[l][r]);
            assert!(semibold.kerning[l][r] < 0, "semibold {pair}: {}", semibold.kerning[l][r]);
        }
        // SemiBold is its own face: wider than Regular, never the same table.
        let total = |t: &Layout| -> u32 { t.advance.iter().sum() };
        assert!(total(semibold) > total(sans));
        for code in FIRST_CODE..=LAST_CODE {
            let cell = unsafe { &*cubit_font_glyph(FACE_SANS_SEMIBOLD, 0, code, 0) };
            assert_eq!(cell.advance, semibold.advance[(code - FIRST_CODE) as usize]);
        }
        assert_eq!(sans.kerning[ascii('n')][ascii('n')], 0);
        assert!(mono.kerning.iter().flatten().all(|&k| k == 0));
        // No adjustment as large as two pixels (the largest is F_, -1.7 px).
        assert!(sans.kerning.iter().flatten().all(|&k| k.abs() < (2.0 * PEN_UNIT) as i16));
    }

    #[test]
    fn invalid_requests() {
        assert!(cubit_font_glyph(FACES as u32, 0, 65, 0).is_null());
        assert!(cubit_font_glyph(0, 2, 65, 0).is_null());
        assert!(cubit_font_glyph(0, 0, 65, PHASES).is_null());
        assert_eq!(cubit_font_glyph(0, 0, u32::MAX, 1), cubit_font_glyph(0, 0, 63, 1));
    }

    #[test]
    fn publication_is_thread_safe() {
        std::thread::scope(|scope| {
            for t in 0..8 {
                scope.spawn(move || {
                    for code in FIRST_CODE..=LAST_CODE {
                        let cell = unsafe { &*cubit_font_glyph(0, 1, code, t % PHASES) };
                        assert!(cell.advance > 0);
                    }
                    assert!(!cubit_font_layout(t % FACES as u32).is_null());
                });
            }
        });
    }
}

#[cfg(target_os = "none")]
#[global_allocator]
static ALLOCATOR: cubit_allocator::BoundedAllocator = cubit_allocator::BoundedAllocator;

#[cfg(target_os = "none")]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo<'_>) -> ! {
    use core::fmt::Write;
    struct Diagnostic;
    impl core::fmt::Write for Diagnostic {
        fn write_str(&mut self, text: &str) -> core::fmt::Result {
            cubit::debug_write(text);
            Ok(())
        }
    }
    let _ = writeln!(Diagnostic, "fonts: fatal Rust runtime error: {info}");
    cubit::exit(1)
}
