//! Shared userspace TrueType rasterizer. Only bundled font bytes, and a
//! program's own embedded face (`cubit_font_register`), are accepted.
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
    sync::atomic::{AtomicU8, AtomicUsize, Ordering},
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
/// The reading faces: up to READING_SLOTS faces a program embeds (or, later,
/// the system font store hands it) and registers itself, one per slot
/// (`cubit_font_register`; the Bible app's Literata, Source Serif 4 and EB
/// Garamond). No bytes are linked into other programs; until a slot is
/// registered its glyphs are null. Each slot is its own face number, so its
/// cells and every density-mask cache stay valid when a reader switches.
pub const FACE_SERIF: u32 = 3;
pub const READING_SLOTS: usize = 3;
/// Glyph cell columns and rows (CuBit.UI_Metrics.Glyph_Cell_Columns/_Rows,
/// the shape of CuBit.Fonts.Coverage): the double-size raster's widest
/// glyph past its origin in whole 8-pixel blocks, and its line plus 2.
pub const WIDTH: usize = 40;
pub const HEIGHT: usize = 44;
const FACES: usize = BUNDLED_FACES + READING_SLOTS;
/// The faces linked into every program (the density masks' and the
/// layout tests' faces).
const BUNDLED_FACES: usize = 3;
/// Raster sizes, in `CuBit.Fonts.Raster_Size` order: the base em, twice it,
/// and the small em (superscripts: verse numbers, footnote marks).
const SIZES: usize = 3;
pub const SIZE_NORMAL: u32 = 0;
pub const SIZE_DOUBLE: u32 = 1;
pub const SIZE_SMALL: u32 = 2;
/// The small em: three quarters of the base em, a whole multiple of it over
/// a denominator up to 16 at every UI density (the density masks' rule).
pub const SMALL_EM_NUMERATOR: u32 = 3;
pub const SMALL_EM_DENOMINATOR: u32 = 4;
const FIRST_CODE: u32 = 32;
const LAST_CODE: u32 = 126;
const CHARACTERS: usize = (LAST_CODE - FIRST_CODE + 1) as usize;
/// The reading face also draws 16#80# .. 16#FF# as Windows-1252: curly
/// quotes, dashes, the ellipsis, the pilcrow and the Latin-1 letters
/// (Bible_Text maps UTF-8 to these).
const FIRST_EXTENDED: u32 = 0x80;
const LAST_EXTENDED: u32 = 0xFF;
pub const EXTENDED: usize = (LAST_EXTENDED - FIRST_EXTENDED + 1) as usize;
const FALLBACK_CODE: u32 = b'?' as u32;
/// Windows-1252 at 16#80# .. 16#9F# (0: unassigned, drawn as '?').
const WINDOWS_1252_HIGH: [u16; 32] = [
    0x20AC, 0, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0,
    0x017D, 0, 0, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, 0x02DC, 0x2122, 0x0161, 0x203A,
    0x0153, 0, 0x017E, 0x0178,
];
/// Logical em and line box of the normal raster size, in pixels: the UI's
/// base em (CuBit.UI_Metrics.Base_Em) and its line, the faces' ascender
/// plus descender at that em, rounded (`base_line_is_the_fonts_line_box`).
/// A density's em is a whole multiple of BASE_EM_PX over its denominator.
pub const BASE_EM_PX: u32 = 16;
pub const BASE_EM: f32 = BASE_EM_PX as f32;
pub const BASE_LINE: u32 = 21;
/// One normal-size raster's logical width (CuBit.UI_Metrics.Glyph_Width_Bound).
pub const BASE_WIDTH_BOUND: u32 = 2 * BASE_EM_PX;
/// Pixels of every glyph raster left of its pen origin, so ink before the
/// origin (left side bearings below zero, kerned overlaps) is kept.
pub const ORIGIN: u32 = 2;
/// Horizontal subpixel phases per glyph: pen positions in quarter pixels.
pub const PHASES: u32 = 4;
/// Fixed-point layout unit: 1/64 pixel.
pub const PEN_UNIT: f32 = 64.0;
/// A registered face's bytes: at most this many (Literata Regular is
/// 320 KiB).
pub const MAXIMUM_REGISTERED_BYTES: usize = 4 * 1024 * 1024;
/// How glyphs are shaped before rasterizing: light (vertical-only) hinting
/// and stem darkening, at every density. Tests turn them off to compare with
/// ab_glyph.
///
/// No horizontal grid fitting (UI-032): snapping UI-size (13 px) stems to whole
/// pixels (and dropping their darkening) left 1x text about 15-25% lighter
/// than FreeType's and unevenly spaced ("jagged/thin" on the laptop); light
/// hinting with darkened outlines on quarter-pixel phases matches FreeType's
/// light auto-hinter (tests/ui-fonts/quality).
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

// The registered reading faces: each slot's bytes' address and length,
// published once (release) and never changed.
static SERIF_BYTES: [AtomicUsize; READING_SLOTS] = [const { AtomicUsize::new(0) }; READING_SLOTS];
static SERIF_LENGTH: [AtomicUsize; READING_SLOTS] = [const { AtomicUsize::new(0) }; READING_SLOTS];

/// A reading face's slot, if `face` is one.
fn slot(face: usize) -> Option<usize> {
    face.checked_sub(BUNDLED_FACES).filter(|&s| s < READING_SLOTS)
}

/// A face's font bytes: bundled, or registered (None until it is).
pub(crate) fn face_bytes(face: usize) -> Option<&'static [u8]> {
    match face as u32 {
        FACE_SANS => Some(SANS),
        FACE_MONO => Some(MONO),
        FACE_SANS_SEMIBOLD => Some(SEMIBOLD),
        _ if slot(face).is_some() => {
            let slot = slot(face)?;
            let address = SERIF_BYTES[slot].load(Ordering::Acquire);
            if address == 0 {
                return None;
            }
            let length = SERIF_LENGTH[slot].load(Ordering::Relaxed);
            // SAFETY: cubit_font_register's caller guarantees the bytes are
            // immutable and live for the process; published once, above.
            Some(unsafe { core::slice::from_raw_parts(address as *const u8, length) })
        }
        _ => None,
    }
}

/// The face draws 16#80# .. 16#FF# (Windows-1252).
pub(crate) fn extended(face: usize) -> bool {
    slot(face).is_some()
}

/// Characters a face draws: printable ASCII, and for the reading face the
/// Windows-1252 upper half.
const fn repertoire(face: usize) -> usize {
    if face >= BUNDLED_FACES { CHARACTERS + EXTENDED } else { CHARACTERS }
}

/// The code a face draws for `code`: itself, or '?'.
pub(crate) fn printable(face: usize, code: u32) -> u32 {
    if (FIRST_CODE..=LAST_CODE).contains(&code)
        || (extended(face) && (FIRST_EXTENDED..=LAST_EXTENDED).contains(&code) && char_of(code).is_some())
    {
        code
    } else {
        FALLBACK_CODE
    }
}

/// The character of a printable code: ASCII, Windows-1252 above it.
pub(crate) fn char_of(code: u32) -> Option<char> {
    match code {
        0x80..=0x9F => match WINDOWS_1252_HIGH[(code - 0x80) as usize] {
            0 => None,
            unicode => char::from_u32(u32::from(unicode)),
        },
        _ => char::from_u32(code),
    }
}

/// A printable code's slot in its face's repertoire.
fn slot_of(code: u32) -> usize {
    if code <= LAST_CODE { (code - FIRST_CODE) as usize } else { CHARACTERS + (code - FIRST_EXTENDED) as usize }
}

fn round_positive_metric(value: f32) -> u32 {
    (value + 0.5) as u32
}

pub(crate) fn zones(font: &Face, ratio: f32, hinted: bool) -> Zones {
    if hinted { Zones::measure(font, ratio) } else { Zones::unhinted(ratio) }
}

/// A raster size's em as a fraction of the base em.
fn size_scale(size: usize) -> f32 {
    match size as u32 {
        SIZE_NORMAL => 1.0,
        SIZE_DOUBLE => 2.0,
        _ => SMALL_EM_NUMERATOR as f32 / SMALL_EM_DENOMINATOR as f32,
    }
}

/// A face's line box at an em of `em` pixels: its baseline row and height.
/// The bundled faces' is their ascender and BASE_LINE (scaled). A
/// registered face's ascender may be far above its ink (Literata's is
/// 1.18 em, for stacked accents it does not need here), so its baseline
/// sits just above the tallest glyph it draws, in the same line box.
pub(crate) fn line_box(face: usize, font: &Face, em: f32) -> (i32, u32) {
    let ratio = em / f32::from(font.units_per_em());
    let height = round_positive_metric(BASE_LINE as f32 * em / BASE_EM);
    let ascender = if face < BUNDLED_FACES { f32::from(font.ascender()) } else { f32::from(tallest(face, font)) };
    (round_positive_metric(ascender * ratio) as i32, height)
}

// Each registered face's tallest glyph (font units), measured once.
static TALLEST: [AtomicUsize; READING_SLOTS] = [const { AtomicUsize::new(0) }; READING_SLOTS];

fn tallest(face: usize, font: &Face) -> i16 {
    let Some(slot) = slot(face) else { return font.ascender() };
    let known = TALLEST[slot].load(Ordering::Relaxed);
    if known != 0 {
        return known as i16;
    }
    let mut top: i16 = font.ascender().min(font.units_per_em() as i16);
    let mut highest: i16 = 1;
    for code in (FIRST_CODE..=LAST_CODE).chain(FIRST_EXTENDED..=LAST_EXTENDED) {
        if printable(face, code) != code {
            continue;
        }
        let bounds = char_of(code).and_then(|c| font.glyph_index(c)).and_then(|id| font.glyph_bounding_box(id));
        if let Some(b) = bounds {
            highest = highest.max(b.y_max);
        }
    }
    top = top.min(highest);
    TALLEST[slot].store(top.max(1) as usize, Ordering::Relaxed);
    top.max(1)
}

/// Stable C/Ada ABI. Coverage is grayscale, not LCD subpixel color. The pen
/// origin is column `origin` on baseline row `baseline`; the cell keeps
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
    /// The baseline's row: text in different faces or sizes lines up by it.
    pub baseline: u32,
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
        baseline: 0,
        alpha: [[0; WIDTH]; HEIGHT],
    };
}

fn rasterize(face: usize, size: usize, code: u32, phase: u32, style: Style) -> Option<Cell> {
    let font = Face::parse(face_bytes(face)?, 0).ok()?;
    let em = BASE_EM * size_scale(size);
    let ratio = em / f32::from(font.units_per_em());
    let id = font.glyph_index(char_of(code)?)?;
    let advance = libm::roundf(f32::from(font.glyph_hor_advance(id)?) * ratio * PEN_UNIT) as u32;
    let (baseline, height) = line_box(face, &font, em);
    let mut cell = Cell { advance: advance.max(1), height, baseline: baseline as u32, ..Cell::EMPTY };
    let shift = ORIGIN as f32 + phase as f32 / PHASES as f32;
    let zones = zones(&font, ratio, style.hinted);
    if let Outline::Ready(prepared) = outline::prepare(
        &font, id, ratio, shift, zones, darken(&font, ratio, style), WIDTH as u32, HEIGHT as u32,
    )?
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
// An entry is one word: EMPTY, BUILDING, FAILED or the cell's address.
const EMPTY: usize = 0;
const BUILDING: usize = 1;
const FAILED: usize = 2;
const READY_STATE: u8 = 2;
struct Entry {
    cell: AtomicUsize,
}
impl Entry {
    const fn new() -> Self {
        Self { cell: AtomicUsize::new(EMPTY) }
    }
}
/// Where a face's cells start in CACHE: per face, sizes x phases x its
/// repertoire, lazily allocated (a cell is 1,184 bytes; a process pays
/// only for the glyphs, sizes and phases it draws).
const fn face_base(face: usize) -> usize {
    let mut base = 0;
    let mut f = 0;
    while f < face {
        base += SIZES * PHASES as usize * repertoire(f);
        f += 1;
    }
    base
}
const CACHE_ENTRIES: usize = face_base(FACES);
static CACHE: [Entry; CACHE_ENTRIES] = [const { Entry::new() }; CACHE_ENTRIES];

/// Returns a permanent, read-only cell, or null for unsupported
/// face/size/phase or an unregistered face. Unknown characters map to '?';
/// UTF-8 shaping is separate.
#[unsafe(no_mangle)]
pub extern "C" fn cubit_font_glyph(face: u32, size: u32, code: u32, phase: u32) -> *const Cell {
    if face as usize >= FACES || size as usize >= SIZES || phase >= PHASES {
        return ptr::null();
    }
    let face = face as usize;
    let code = printable(face, code);
    let index = face_base(face) + (size as usize * PHASES as usize + phase as usize) * repertoire(face) + slot_of(code);
    let entry = &CACHE[index];
    loop {
        match entry.cell.load(Ordering::Acquire) {
            EMPTY => {
                if face_bytes(face).is_none() {
                    return ptr::null(); // not registered yet: never cached
                }
                if entry.cell.compare_exchange(EMPTY, BUILDING, Ordering::Acquire, Ordering::Relaxed).is_ok() {
                    match rasterize(face, size as usize, code, phase, STYLE) {
                        Some(cell) => {
                            let address = Box::into_raw(Box::new(cell)) as usize;
                            entry.cell.store(address, Ordering::Release);
                            return address as *const Cell;
                        }
                        None => {
                            entry.cell.store(FAILED, Ordering::Release);
                            return ptr::null();
                        }
                    }
                }
            }
            BUILDING => core::hint::spin_loop(),
            FAILED => return ptr::null(),
            address => return address as *const Cell,
        }
    }
}

/// Registers a reading face slot (`FACE_SERIF` + 0 .. READING_SLOTS - 1):
/// `length` bytes at `bytes`, a
/// TrueType font the program embeds (ccl-build's pinned `embedded` blob).
/// 0: registered (or already registered with these bytes); 1: refused
/// (another face, a null pointer, an empty or oversized font, or other
/// bytes already registered); 2: not a font this engine draws (it must
/// parse, hold every printable ASCII glyph, and fit the line box).
///
/// # Safety
/// The bytes must stay valid and unchanged for the rest of the process:
/// they are read on every cold glyph, never copied.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_font_register(face: u32, bytes: *const u8, length: usize) -> u32 {
    let Some(slot) = slot(face as usize) else { return 1 };
    if bytes.is_null() || length == 0 || length > MAXIMUM_REGISTERED_BYTES {
        return 1;
    }
    let address = bytes as usize;
    match SERIF_BYTES[slot].load(Ordering::Acquire) {
        0 => {}
        known if known == address && SERIF_LENGTH[slot].load(Ordering::Relaxed) == length => return 0,
        _ => return 1,
    }
    // SAFETY: the caller guarantees `length` readable bytes that live for
    // the process.
    let data: &'static [u8] = unsafe { core::slice::from_raw_parts(bytes, length) };
    let Ok(font) = Face::parse(data, 0) else { return 2 };
    for code in FIRST_CODE..=LAST_CODE {
        if char_of(code).and_then(|c| font.glyph_index(c)).is_none() {
            return 2;
        }
    }
    SERIF_LENGTH[slot].store(length, Ordering::Relaxed);
    if SERIF_BYTES[slot].compare_exchange(0, address, Ordering::Release, Ordering::Acquire).is_err() {
        return if SERIF_BYTES[slot].load(Ordering::Acquire) == address { 0 } else { 1 };
    }
    0
}

/// Per-face layout metrics at the normal (logical) size, in 1/64 pixel:
/// unhinted advances and GPOS pair kerning, indexed by code - 32; then, for
/// the reading face, the advances of 16#80# .. 16#FF# (unkerned; 0 for
/// codes it does not draw, which are drawn as '?').
#[repr(C)]
pub struct Layout {
    pub advance: [u32; CHARACTERS],
    pub kerning: [[i16; CHARACTERS]; CHARACTERS],
    pub extended_advance: [u32; EXTENDED],
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
        state: AtomicU8::new(0),
        layout: UnsafeCell::new(Layout {
            advance: [0; CHARACTERS],
            kerning: [[0; CHARACTERS]; CHARACTERS],
            extended_advance: [0; EXTENDED],
        }),
    }
}; FACES];
const LAYOUT_EMPTY: u8 = 0;
const LAYOUT_BUILDING: u8 = 1;
const LAYOUT_FAILED: u8 = 3;

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
    let font = Face::parse(face_bytes(face)?, 0).ok()?;
    let ratio = BASE_EM / f32::from(font.units_per_em());
    let scaled = |units: f32| libm::roundf(units * ratio * PEN_UNIT);
    let mut ids = [GlyphId(0); CHARACTERS];
    for (i, id) in ids.iter_mut().enumerate() {
        *id = font.glyph_index(char::from_u32(FIRST_CODE + i as u32)?)?;
        layout.advance[i] = (scaled(f32::from(font.glyph_hor_advance(*id)?)) as u32).max(1);
    }
    if extended(face) {
        for (i, advance) in layout.extended_advance.iter_mut().enumerate() {
            let code = FIRST_EXTENDED + i as u32;
            let drawn = if printable(face, code) == code { code } else { FALLBACK_CODE };
            let id = char_of(drawn).and_then(|c| font.glyph_index(c));
            *advance = match id.and_then(|id| font.glyph_hor_advance(id)) {
                Some(units) => (scaled(f32::from(units)) as u32).max(1),
                None => layout.advance[(FALLBACK_CODE - FIRST_CODE) as usize],
            };
        }
    }
    let mut lookups = [0u16; LOOKUPS_MAX];
    let count = kern_lookups(&font, &mut lookups);
    build_kerning(&font, &lookups[..count], &ids, &mut layout.kerning);
    for value in layout.kerning.iter_mut().flatten() {
        *value = scaled(f32::from(*value)) as i16;
    }
    Some(())
}

/// Returns the face's permanent layout table, or null for an unknown face
/// or a reading face not yet registered.
#[unsafe(no_mangle)]
pub extern "C" fn cubit_font_layout(face: u32) -> *const Layout {
    let Some(entry) = LAYOUTS.get(face as usize) else { return ptr::null() };
    loop {
        match entry.state.load(Ordering::Acquire) {
            READY_STATE => return entry.layout.get().cast_const(),
            LAYOUT_FAILED => return ptr::null(),
            LAYOUT_EMPTY => {
                if face_bytes(face as usize).is_none() {
                    return ptr::null(); // not registered yet: never cached
                }
                if entry
                    .state
                    .compare_exchange(LAYOUT_EMPTY, LAYOUT_BUILDING, Ordering::Acquire, Ordering::Relaxed)
                    .is_ok()
                {
                    // SAFETY: this thread exclusively owns the unpublished table.
                    let built = build_layout(face as usize, unsafe { &mut *entry.layout.get() });
                    entry.state.store(if built.is_some() { READY_STATE } else { LAYOUT_FAILED }, Ordering::Release);
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
        for face in 0..BUNDLED_FACES {
            let font = FontRef::try_from_slice(face_bytes(face).unwrap()).unwrap();
            for size in 0..2 {
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

    /// The base line box is the faces' own (ascender + descender + line gap
    /// at BASE_EM, rounded), and every printable glyph of both raster sizes
    /// fits its cell without touching the last column or its line's last
    /// row (a glyph that did would be clipped).
    #[test]
    fn base_line_is_the_fonts_line_box() {
        for face in 0..BUNDLED_FACES {
            let font = Face::parse(face_bytes(face).unwrap(), 0).unwrap();
            let ratio = BASE_EM / f32::from(font.units_per_em());
            let height = f32::from(font.ascender()) - f32::from(font.descender()) + f32::from(font.line_gap());
            assert_eq!(round_positive_metric(height * ratio), BASE_LINE, "face {face}");
            for size in 0..2 {
                for code in FIRST_CODE..=LAST_CODE {
                    let cell = rasterize(face, size, code, PHASES - 1, STYLE).unwrap();
                    assert!((cell.height as usize) < HEIGHT);
                    assert!(cell.ink_left + cell.ink_width < WIDTH as u32, "{face}/{size}/{code} touches the edge");
                    assert!(cell.ink_top + cell.ink_height < cell.height || cell.ink_width == 0);
                }
            }
        }
        assert_eq!(BASE_WIDTH_BOUND, 2 * BASE_EM_PX);
    }

    #[test]
    fn cells_are_bounded_and_cached() {
        for face in 0..BUNDLED_FACES as u32 {
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
        // Glyphs of one contour each: overlapping contours (Plex's H) clamp
        // where they cross, by an amount that depends on the phase.
        for code in [b'l', b'o', b'n', b'm'] {
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
        for face in 0..BUNDLED_FACES {
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
            let ratio = BASE_EM / f32::from(Face::parse(face_bytes(face).unwrap(), 0).unwrap().units_per_em());
            let baseline = round_positive_metric(1025.0 * ratio);
            assert_eq!(bottom, baseline - 1, "baseline row above round(ascender)");
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

    /// UI-032 regression: normal (base em, density 1) cells are darkened like
    /// FreeType's light auto-hinter and keep true subpixel phases. Grid
    /// fitting had snapped their stems to whole pixels without darkening:
    /// no more ink than the undarkened outline, and phases that moved stems
    /// by whole pixels instead of quarters.
    #[test]
    fn normal_cells_are_darkened_on_quarter_pixel_phases() {
        let undarkened = Style { darkened: false, ..STYLE };
        let ink = |c: &Cell| -> f32 { c.alpha.iter().flatten().map(|&a| f32::from(a)).sum() };
        let centroid = |c: &Cell| -> f32 {
            let (mut sum, mut weight) = (0.0, 0.0);
            for row in &c.alpha {
                for (x, &a) in row.iter().enumerate() {
                    sum += x as f32 * f32::from(a);
                    weight += f32::from(a);
                }
            }
            sum / weight
        };
        for face in 0..BUNDLED_FACES {
            for code in "lIHnmhu".chars() {
                let dark = rasterize(face, 0, code as u32, 0, STYLE).unwrap();
                let plain = rasterize(face, 0, code as u32, 0, undarkened).unwrap();
                // Darkening falls as stems widen (FreeType's curve): SemiBold
                // stems at the 16 px em gain about 4%.
                assert!(ink(&dark) > 1.03 * ink(&plain), "face {face} {code}: {} vs {}", ink(&dark), ink(&plain));
                for phase in 1..PHASES {
                    let moved = centroid(&rasterize(face, 0, code as u32, phase, STYLE).unwrap()) - centroid(&dark);
                    let expected = phase as f32 / PHASES as f32;
                    assert!((moved - expected).abs() < 0.15, "face {face} {code} phase {phase}: {moved}");
                }
            }
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
        // No adjustment as large as 0.15 em (the largest is F_, -0.13 em).
        assert!(sans.kerning.iter().flatten().all(|&k| f32::from(k.abs()) < 0.15 * BASE_EM * PEN_UNIT));
    }


    /// The reading face: registered once from a program's own bytes (here
    /// Plex Sans stands in for Literata), drawn at every size with the
    /// Windows-1252 upper half, laid out with its extended advances.
    #[test]
    fn reading_face_registration_and_repertoire() {
        assert!(cubit_font_glyph(FACE_SERIF, 0, b'A' as u32, 0).is_null() || face_bytes(FACE_SERIF as usize).is_some());
        assert_eq!(unsafe { cubit_font_register(FACE_SANS, SANS.as_ptr(), SANS.len()) }, 1, "bundled faces are fixed");
        assert_eq!(unsafe { cubit_font_register(FACE_SERIF, ptr::null(), 10) }, 1);
        let junk: &'static [u8] = &[0u8; 64];
        assert_eq!(unsafe { cubit_font_register(FACE_SERIF, junk.as_ptr(), junk.len()) }, 2, "not a font");
        assert_eq!(unsafe { cubit_font_register(FACE_SERIF, SANS.as_ptr(), SANS.len()) }, 0);
        assert_eq!(unsafe { cubit_font_register(FACE_SERIF, SANS.as_ptr(), SANS.len()) }, 0, "again: same bytes");
        assert_eq!(unsafe { cubit_font_register(FACE_SERIF, MONO.as_ptr(), MONO.len()) }, 1, "other bytes refused");
        // Another slot holds another face, beside the first.
        assert_eq!(unsafe { cubit_font_register(FACE_SERIF + 1, MONO.as_ptr(), MONO.len()) }, 0);
        assert_ne!(cubit_font_glyph(FACE_SERIF, 0, b'a' as u32, 0), cubit_font_glyph(FACE_SERIF + 1, 0, b'a' as u32, 0));
        let (one, two) = unsafe { (&*cubit_font_layout(FACE_SERIF), &*cubit_font_layout(FACE_SERIF + 1)) };
        assert_ne!(one.advance, two.advance, "each slot lays out its own face");
        assert_eq!(unsafe { cubit_font_register(FACE_SERIF + READING_SLOTS as u32, MONO.as_ptr(), MONO.len()) }, 1,
                   "past the last slot");
        for size in 0..SIZES as u32 {
            for code in (FIRST_CODE..=LAST_CODE).chain(FIRST_EXTENDED..=LAST_EXTENDED) {
                let cell = unsafe { &*cubit_font_glyph(FACE_SERIF, size, code, 1) };
                assert!(cell.height <= HEIGHT as u32 && cell.baseline < cell.height);
                assert!(cell.ink_left + cell.ink_width <= WIDTH as u32);
                assert!(cell.ink_top + cell.ink_height <= cell.height);
            }
        }
        // Curly quotes, dashes, the ellipsis and the pilcrow have their own
        // glyphs; an unassigned code is '?'.
        let quote = cubit_font_glyph(FACE_SERIF, 0, 0x93, 0);
        assert_ne!(quote, cubit_font_glyph(FACE_SERIF, 0, b'?' as u32, 0));
        assert_ne!(quote, cubit_font_glyph(FACE_SERIF, 0, b'"' as u32, 0));
        assert_eq!(cubit_font_glyph(FACE_SERIF, 0, 0x81, 0), cubit_font_glyph(FACE_SERIF, 0, b'?' as u32, 0));
        // The bundled faces keep printable ASCII.
        assert_eq!(cubit_font_glyph(FACE_SANS, 0, 0x93, 0), cubit_font_glyph(FACE_SANS, 0, b'?' as u32, 0));
        let layout = unsafe { &*cubit_font_layout(FACE_SERIF) };
        let em_dash = unsafe { &*cubit_font_glyph(FACE_SERIF, 0, 0x97, 0) };
        assert_eq!(layout.extended_advance[0x97 - 0x80], em_dash.advance);
        assert!(layout.extended_advance.iter().all(|&a| a > 0));
        // The small size is three quarters of the normal em.
        let normal = unsafe { &*cubit_font_glyph(FACE_SERIF, SIZE_NORMAL, b'M' as u32, 0) };
        let small = unsafe { &*cubit_font_glyph(FACE_SERIF, SIZE_SMALL, b'M' as u32, 0) };
        assert!((small.advance as f32 - normal.advance as f32 * 0.75).abs() <= 1.0);
        assert!(small.height < normal.height && small.ink_height < normal.ink_height);
    }

    #[test]
    fn invalid_requests() {
        assert!(cubit_font_glyph(FACES as u32, 0, 65, 0).is_null());
        assert!(cubit_font_glyph(0, SIZES as u32, 65, 0).is_null());
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
                    assert!(!cubit_font_layout(t % BUNDLED_FACES as u32).is_null());
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
