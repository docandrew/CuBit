//! Shared userspace TrueType rasterizer. Only bundled font bytes are accepted.
#![cfg_attr(target_os = "none", no_std)]
#![deny(unsafe_op_in_unsafe_fn)]

use ab_glyph_rasterizer::{Point, Rasterizer, point};
use core::{
    cell::UnsafeCell,
    sync::atomic::{AtomicU8, Ordering},
};
use ttf_parser::{Face, OutlineBuilder};

const SANS: &[u8] = include_bytes!(env!("IBM_PLEX_SANS_FONT"));
const MONO: &[u8] = include_bytes!(env!("IBM_PLEX_MONO_FONT"));
pub const WIDTH: usize = 32;
pub const HEIGHT: usize = 36;
const CHARACTERS: usize = 95;

/// Stable C/Ada ABI. Coverage is grayscale, not LCD subpixel color.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Cell {
    pub advance: u32,
    pub height: u32,
    pub alpha: [[u8; WIDTH]; HEIGHT],
}
impl Cell {
    const EMPTY: Self = Self {
        advance: 0,
        height: 0,
        alpha: [[0; WIDTH]; HEIGHT],
    };
}

struct Entry {
    state: AtomicU8,
    cell: UnsafeCell<Cell>,
}
// Only the thread winning 0 -> 1 writes. A release publication of 2 makes
// the cell permanently immutable; returned pointers are never evicted.
unsafe impl Sync for Entry {}
impl Entry {
    const fn new() -> Self {
        Self {
            state: AtomicU8::new(0),
            cell: UnsafeCell::new(Cell::EMPTY),
        }
    }
}
// Two faces, two native raster sizes, printable ASCII. No unbounded cache.
static CACHE: [Entry; 2 * 2 * CHARACTERS] = [const { Entry::new() }; 2 * 2 * CHARACTERS];

// Stream contours into the coverage rasterizer instead of first allocating a
// Vec containing every curve. Only the bounded coverage grid needs scratch.
struct Contours {
    raster: Rasterizer,
    ratio: f32,
    origin: Point,
    first: Point,
    current: Point,
}
impl Contours {
    fn transform(&self, x: f32, y: f32) -> Point {
        point(
            x * self.ratio + self.origin.x,
            -y * self.ratio + self.origin.y,
        )
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
        self.raster
            .draw_cubic(self.current, control1, control2, end);
        self.current = end;
    }
    fn close(&mut self) {
        self.raster.draw_line(self.current, self.first);
        self.current = self.first;
    }
}

fn rasterize(face: usize, size: usize, code: u32) -> Option<Cell> {
    let font = Face::parse(if face == 0 { SANS } else { MONO }, 0).ok()?;
    let em = if size == 0 { 13.0 } else { 26.0 };
    let ratio = em / f32::from(font.units_per_em());
    let id = font.glyph_index(char::from_u32(code)?)?;
    let advance = round_positive_metric(f32::from(font.glyph_hor_advance(id)?) * ratio)
        .clamp(1, WIDTH as u32);
    let mut cell = Cell {
        advance,
        height: if size == 0 { 17 } else { 34 },
        ..Cell::EMPTY
    };
    let baseline = round_positive_metric(f32::from(font.ascender()) * ratio) as f32;
    if let Some(bounds) = font.glyph_bounding_box(id) {
        let left = libm::floorf(f32::from(bounds.x_min) * ratio);
        let top = libm::floorf(-f32::from(bounds.y_max) * ratio);
        let width = (libm::ceilf(f32::from(bounds.x_max) * ratio) - left) as usize;
        let height = (libm::ceilf(-f32::from(bounds.y_min) * ratio) - top) as usize;
        // Bundled faces only. Reject unexpected font metrics before allocating;
        // retain the full outline bounds so overhangs do not corrupt coverage.
        if width > WIDTH + 2 || height > HEIGHT + 2 {
            return None;
        }
        let mut contours = Contours {
            raster: Rasterizer::new(width, height),
            ratio,
            origin: point(-left, -top),
            first: point(0.0, 0.0),
            current: point(0.0, 0.0),
        };
        font.outline_glyph(id, &mut contours)?;
        contours.raster.for_each_pixel_2d(|x, y, coverage| {
            let x = x as i32 + left as i32;
            let y = y as i32 + (top + baseline) as i32;
            if x >= 0 && y >= 0 && x < advance as i32 && y < cell.height as i32 {
                cell.alpha[y as usize][x as usize] = (coverage.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
            }
        });
    }
    Some(cell)
}

fn round_positive_metric(value: f32) -> u32 {
    (value + 0.5) as u32
}

/// Returns a permanent, read-only cell, or null for unsupported face/size.
/// Unknown characters intentionally map to '?'; UTF-8 shaping is separate.
#[unsafe(no_mangle)]
pub extern "C" fn cubit_font_glyph(face: u32, size: u32, code: u32) -> *const Cell {
    if face > 1 || size > 1 {
        return core::ptr::null();
    }
    let code = if (32..=126).contains(&code) {
        code
    } else {
        b'?' as u32
    };
    let entry = &CACHE[(face as usize * 2 + size as usize) * CHARACTERS + (code - 32) as usize];
    loop {
        match entry.state.load(Ordering::Acquire) {
            2 => return entry.cell.get().cast_const(),
            3 => return core::ptr::null(),
            0 => {
                if entry
                    .state
                    .compare_exchange(0, 1, Ordering::Acquire, Ordering::Relaxed)
                    .is_ok()
                {
                    match rasterize(face as usize, size as usize, code) {
                        Some(cell) => {
                            // SAFETY: this thread exclusively owns the unpublished cell.
                            unsafe {
                                entry.cell.get().write(cell);
                            }
                            entry.state.store(2, Ordering::Release);
                            return entry.cell.get().cast_const();
                        }
                        None => {
                            entry.state.store(3, Ordering::Release);
                            return core::ptr::null();
                        }
                    }
                }
            }
            _ => core::hint::spin_loop(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn streamed_outlines_match_reference_renderer() {
        use ab_glyph::{Font, FontRef, Glyph, PxScale};
        for face in 0..2 {
            let font = FontRef::try_from_slice(if face == 0 { SANS } else { MONO }).unwrap();
            for size in 0..2 {
                let ratio = (if size == 0 { 13.0 } else { 26.0 }) / font.units_per_em().unwrap();
                for code in 32..=126 {
                    let cell = rasterize(face, size, code).unwrap();
                    let mut expected = [[0u8; WIDTH]; HEIGHT];
                    let glyph = Glyph {
                        id: font.glyph_id(char::from_u32(code).unwrap()),
                        scale: PxScale::from(font.height_unscaled() * ratio),
                        position: ab_glyph::point(
                            0.0,
                            round_positive_metric(font.ascent_unscaled() * ratio) as f32,
                        ),
                    };
                    if let Some(outline) = font.outline_glyph(glyph) {
                        let bounds = outline.px_bounds();
                        outline.draw(|x, y, coverage| {
                            let x = x as i32 + bounds.min.x as i32;
                            let y = y as i32 + bounds.min.y as i32;
                            if x >= 0 && y >= 0 && x < cell.advance as i32 && y < cell.height as i32
                            {
                                expected[y as usize][x as usize] =
                                    (coverage.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
                            }
                        });
                    }
                    for (actual, reference) in
                        cell.alpha.iter().flatten().zip(expected.iter().flatten())
                    {
                        // Equivalent scale arithmetic can differ at a rounding boundary.
                        assert!(
                            actual.abs_diff(*reference) <= 1,
                            "face={face} size={size} code={code}: {actual} vs {reference}"
                        );
                    }
                }
            }
        }
    }
    #[test]
    fn cells_are_bounded_and_cached() {
        for face in 0..2 {
            for size in 0..2 {
                for code in 32..=126 {
                    let pointer = cubit_font_glyph(face, size, code);
                    assert!(!pointer.is_null());
                    assert_eq!(pointer, cubit_font_glyph(face, size, code));
                    let cell = unsafe { &*pointer };
                    assert!((1..=WIDTH as u32).contains(&cell.advance));
                    assert!(cell.height <= HEIGHT as u32);
                    for row in cell.alpha.iter().skip(cell.height as usize) {
                        assert!(row.iter().all(|&a| a == 0));
                    }
                    for row in &cell.alpha {
                        assert!(row.iter().skip(cell.advance as usize).all(|&a| a == 0));
                    }
                    if code != 32 {
                        assert!(cell.alpha.iter().flatten().any(|&a| a != 0));
                    }
                }
            }
        }
    }
    #[test]
    fn mono_metrics_and_invalid_requests() {
        for code in 32..=126 {
            assert_eq!(unsafe { &*cubit_font_glyph(1, 0, code) }.advance, 8);
        }
        assert!(cubit_font_glyph(2, 0, 65).is_null());
        assert!(cubit_font_glyph(0, 2, 65).is_null());
        assert_eq!(cubit_font_glyph(0, 0, u32::MAX), cubit_font_glyph(0, 0, 63));
    }
    #[test]
    fn publication_is_thread_safe() {
        std::thread::scope(|scope| {
            for _ in 0..8 {
                scope.spawn(|| {
                    for code in 32..=126 {
                        let cell = unsafe { &*cubit_font_glyph(0, 1, code) };
                        assert!(cell.advance > 0);
                    }
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
