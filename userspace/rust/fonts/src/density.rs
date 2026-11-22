//! Audited caller-storage ABI for the SPARK glyph layout policy.
use super::*;
use crate::outline::{self, Outline};

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Request {
    pub face: u32,
    pub code: u32,
    pub em_numerator: u32,
    pub em_denominator: u32,
    pub width: u32,
    pub height: u32,
    pub pitch: u32,
    pub capacity: u32,
    /// Horizontal subpixel phases, 1..=PHASES: the mask holds one raster per
    /// phase, stacked (phase i in rows i*height..), phase i shifted right by
    /// i/phases of a pixel of this density.
    pub phases: u32,
}
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Metrics {
    pub advance: u32,
    pub height: u32,
}
fn layout(r: &Request) -> Option<usize> {
    if r.face as usize >= FACES
        || !(BASE_EM_PX..=BASE_EM_PX * 16).contains(&r.em_numerator)
        || r.em_numerator % BASE_EM_PX != 0
        || !(1..=16).contains(&r.em_denominator)
        || !(1..=PHASES).contains(&r.phases)
    {
        return None;
    }
    let n = r.em_numerator / BASE_EM_PX;
    let d = r.em_denominator;
    let w = (BASE_WIDTH_BOUND * n + d - 1) / d;
    let h = (BASE_LINE * n + d - 1) / d;
    let pitch = ((w + 15) / 16) * 16;
    let bytes = pitch * h * r.phases;
    if r.width != w || r.height != h || r.pitch != pitch || r.capacity < bytes {
        return None;
    }
    Some(bytes as usize)
}
fn render(r: &Request, pixels: &mut [u8]) -> Option<Metrics> {
    render_with(r, pixels, STYLE)
}
/// The pen origin sits ORIGIN logical pixels from the left edge (at the
/// request's density), plus the phase's fraction of a pixel, so ink left of
/// the origin is kept; ink is clipped only to the raster, never to the advance.
fn render_with(r: &Request, pixels: &mut [u8], style: Style) -> Option<Metrics> {
    let bytes = layout(r)?;
    if pixels.len() < bytes {
        return None;
    }
    let face = r.face as usize;
    let font = Face::parse(face_bytes(face)?, 0).ok()?;
    let density = r.em_numerator as f32 / BASE_EM / r.em_denominator as f32;
    let ratio = (r.em_numerator as f32 / r.em_denominator as f32) / f32::from(font.units_per_em());
    let code = printable(face, r.code);
    let id = font.glyph_index(char_of(code)?)?;
    let advance = round_positive_metric(f32::from(font.glyph_hor_advance(id)?) * ratio).max(1);
    if advance > r.width {
        return None;
    }
    let (baseline, _) = line_box(face, &font, r.em_numerator as f32 / r.em_denominator as f32);
    let (zones, darken) = measured(face, &font, r.em_numerator, r.em_denominator, ratio, style);
    let glyph = outline::decode(&font, id, darken)?;
    let raster = (r.pitch * r.height) as usize;
    // Prepare every phase's outline before changing caller pixels. These
    // temporary coverage grids are bounded; no glyph pointer or destination
    // is retained by this API.
    let mut prepared: [Option<outline::Prepared>; PHASES as usize] = [const { None }; PHASES as usize];
    for (phase, slot) in prepared.iter_mut().enumerate().take(r.phases as usize) {
        let shift = ORIGIN as f32 * density + phase as f32 / r.phases as f32;
        *slot = match outline::prepare(&font, id, &glyph, ratio, shift, zones, darken, r.width + 2, r.height + 2)? {
            Outline::Ready(p) => Some(p),
            Outline::Empty => None,
        };
    }
    for row in pixels[..bytes].chunks_mut(r.pitch as usize) {
        row[..r.width as usize].fill(0);
    }
    for (phase, slot) in prepared.into_iter().enumerate() {
        let Some(p) = slot else { continue };
        let first = phase * raster;
        p.each(|x, y, coverage| {
            let y = y + baseline;
            if x >= 0 && y >= 0 && x < r.width as i32 && y < r.height as i32 {
                pixels[first + y as usize * r.pitch as usize + x as usize] = coverage;
            }
        });
    }
    Some(Metrics {
        advance,
        height: r.height,
    })
}
/// A face's hinting zones and darkening at one size: the same for every
/// glyph of it, so measured once per (face, size, style) (each measure
/// reads reference glyphs) and kept. A full table measures without keeping.
#[derive(Clone, Copy)]
struct Measure {
    face: u32,
    em_numerator: u32,
    em_denominator: u32,
    hinted: bool,
    darkened: bool,
    zones: Zones,
    darken: Darken,
}
const MEASURES: usize = 32;
struct MeasureTable {
    busy: core::sync::atomic::AtomicBool,
    items: core::cell::UnsafeCell<[Option<Measure>; MEASURES]>,
}
// Only read or written while `busy` is held (acquire/release below).
unsafe impl Sync for MeasureTable {}
static MEASURED: MeasureTable = MeasureTable {
    busy: core::sync::atomic::AtomicBool::new(false),
    items: core::cell::UnsafeCell::new([None; MEASURES]),
};
fn with_table<T>(f: impl FnOnce(&mut [Option<Measure>; MEASURES]) -> T) -> T {
    use core::sync::atomic::Ordering;
    while MEASURED.busy.compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed).is_err() {
        core::hint::spin_loop();
    }
    // SAFETY: `busy` is held: no other reference to the table exists.
    let result = f(unsafe { &mut *MEASURED.items.get() });
    MEASURED.busy.store(false, Ordering::Release);
    result
}
fn measured(face: usize, font: &Face, em_numerator: u32, em_denominator: u32, ratio: f32, style: Style) -> (Zones, Darken) {
    let same = |m: &Measure| {
        m.face == face as u32 && m.em_numerator == em_numerator && m.em_denominator == em_denominator
            && m.hinted == style.hinted && m.darkened == style.darkened
    };
    if let Some(m) = with_table(|t| t.iter().flatten().find(|m| same(m)).copied()) {
        return (m.zones, m.darken);
    }
    let (zones, darken) = (zones(font, ratio, style.hinted), darken(font, ratio, style));
    with_table(|t| {
        if !t.iter().flatten().any(|m| same(m)) {
            if let Some(free) = t.iter_mut().find(|m| m.is_none()) {
                *free = Some(Measure { face: face as u32, em_numerator, em_denominator, hinted: style.hinted,
                                       darkened: style.darkened, zones, darken });
            }
        }
    });
    (zones, darken)
}
fn separate(a: usize, size_a: usize, b: usize, size_b: usize) -> bool {
    match (a.checked_add(size_a), b.checked_add(size_b)) {
        (Some(end_a), Some(end_b)) => end_a <= b || end_b <= a,
        _ => false,
    }
}
/// 0 = completed; 1 = rejected layout/pointers; 2 = rejected font metrics.
/// Rejection leaves pixels and metrics unchanged; allocation failure may abort.
/// Row padding is untouched. All rendering completes before return.
///
/// # Safety
/// Non-null pointers must refer to accessible allocations of the declared
/// lengths, with exclusive access to pixels/metrics until return. Numeric range
/// and overlap checks cannot establish mapping authority or physical aliases.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_font_raster_mask(
    request: *const Request,
    pixels: *mut u8,
    metrics: *mut Metrics,
) -> u32 {
    if request.is_null()
        || pixels.is_null()
        || metrics.is_null()
        || request.addr() % core::mem::align_of::<Request>() != 0
        || metrics.addr() % core::mem::align_of::<Metrics>() != 0
    {
        return 1;
    }
    let r = unsafe { request.read() };
    let Some(bytes) = layout(&r) else {
        return 1;
    };
    if !separate(
        pixels.addr(),
        bytes,
        metrics.addr(),
        core::mem::size_of::<Metrics>(),
    ) || !separate(
        pixels.addr(),
        bytes,
        request.addr(),
        core::mem::size_of::<Request>(),
    ) || !separate(
        metrics.addr(),
        core::mem::size_of::<Metrics>(),
        request.addr(),
        core::mem::size_of::<Request>(),
    ) {
        return 1;
    }
    let output = unsafe { core::slice::from_raw_parts_mut(pixels, bytes) };
    let Some(result) = render(&r, output) else {
        return 2;
    };
    unsafe {
        metrics.write(result);
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    fn request(face: u32, code: u32, n: u32, d: u32) -> Request {
        let w = (BASE_WIDTH_BOUND * n + d - 1) / d;
        let h = (BASE_LINE * n + d - 1) / d;
        let pitch = (w + 15) / 16 * 16;
        Request {
            face,
            code,
            em_numerator: BASE_EM_PX * n,
            em_denominator: d,
            width: w,
            height: h,
            pitch,
            capacity: pitch * h * PHASES,
            phases: PHASES,
        }
    }
    #[test]
    fn density_masks_match_reference_and_preserve_padding() {
        use ab_glyph::{Font, FontRef, Glyph, PxScale};
        let mut cases = 0;
        for face in 0..BUNDLED_FACES as u32 {
            let font = FontRef::try_from_slice(face_bytes(face as usize).unwrap()).unwrap();
            for n in 1..=16 {
                for d in 1..=16 {
                    for code in 32..=126 {
                        let r = request(face, code, n, d);
                        let mut actual = vec![0xA5; r.capacity as usize + 16];
                        let m = render_with(&r, &mut actual, PLAIN).unwrap();
                        assert!(m.advance >= 1);
                        // ab_glyph (the reference) mis-draws Mono 'T' at a
                        // 6.67 px em (density 5/12 of 16 px): its stem rows
                        // ink both bar ends. Ours is the outline's (stem at
                        // 0.5 em). No UI density is that small.
                        if (face, code, n, d) == (FACE_MONO, b'T' as u32, 5, 12) {
                            cases += 1;
                            continue;
                        }
                        let ratio = (BASE_EM * n as f32 / d as f32) / font.units_per_em().unwrap();
                        let mut expected = vec![0u8; r.capacity as usize];
                        let raster = (r.pitch * r.height) as usize;
                        for phase in 0..PHASES {
                            let glyph = Glyph {
                                id: font.glyph_id(char::from_u32(code).unwrap()),
                                scale: PxScale::from(font.height_unscaled() * ratio),
                                position: ab_glyph::point(
                                    ORIGIN as f32 * n as f32 / d as f32 + phase as f32 / PHASES as f32,
                                    round_positive_metric(font.ascent_unscaled() * ratio) as f32,
                                ),
                            };
                            if let Some(outline) = font.outline_glyph(glyph) {
                                let bounds = outline.px_bounds();
                                outline.draw(|x, y, c| {
                                    let x = x as i32 + bounds.min.x as i32;
                                    let y = y as i32 + bounds.min.y as i32;
                                    if x >= 0 && y >= 0 && x < r.width as i32 && y < r.height as i32 {
                                        expected[phase as usize * raster + y as usize * r.pitch as usize + x as usize] =
                                            (c.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
                                    }
                                });
                            }
                        }
                        for y in 0..(r.height * PHASES) as usize {
                            for x in 0..r.pitch as usize {
                                let i = y * r.pitch as usize + x;
                                if x < r.width as usize {
                                    assert!(
                                        actual[i].abs_diff(expected[i]) <= 1,
                                        "{face}/{n}/{d}/{code} pixel{x},{y}: {} vs {}", actual[i], expected[i]
                                    );
                                } else {
                                    assert_eq!(actual[i], 0xA5);
                                }
                            }
                        }
                        assert!(actual[r.capacity as usize..].iter().all(|v| *v == 0xA5));
                        cases += 1;
                    }
                }
            }
        }
        assert_eq!(cases, 256 * 95 * BUNDLED_FACES);
    }
    #[test]
    fn ffi_rejections_do_not_write_and_unknown_code_maps_to_question() {
        assert_eq!(core::mem::size_of::<Request>(), 36);
        assert_eq!(core::mem::size_of::<Metrics>(), 8);
        for fault in 0..13 {
            let mut r = request(0, 65, 5, 4);
            let mut pixels = vec![0xA5; r.capacity as usize];
            let mut m = Metrics {
                advance: 123,
                height: 456,
            };
            match fault {
                0 => r.face = FACES as u32,
                1 => r.em_numerator = 0,
                2 => r.em_numerator = BASE_EM_PX * 16 + 1,
                3 => r.em_numerator = BASE_EM_PX + 1,
                4 => r.em_denominator = 0,
                5 => r.em_denominator = 17,
                6 => r.width += 1,
                7 => r.height += 1,
                8 => r.pitch += 1,
                9 => r.capacity -= 1,
                10 => r.phases = 0,
                11 => r.phases = PHASES + 1,
                _ => r.width = u32::MAX,
            }
            assert_eq!(
                unsafe { cubit_font_raster_mask(&r, pixels.as_mut_ptr(), &mut m) },
                1
            );
            assert!(pixels.iter().all(|v| *v == 0xA5));
            assert_eq!(
                m,
                Metrics {
                    advance: 123,
                    height: 456
                }
            );
        }
        let r = request(0, 65, 5, 4);
        let mut pixels = vec![0xA5; r.capacity as usize];
        assert_eq!(
            unsafe { cubit_font_raster_mask(&r, pixels.as_mut_ptr(), pixels.as_mut_ptr().cast()) },
            1
        );
        assert!(pixels.iter().all(|v| *v == 0xA5));
        let mut m = Metrics {
            advance: 0,
            height: 0,
        };
        assert_eq!(
            unsafe {
                cubit_font_raster_mask(&r, pixels.as_mut_ptr(), pixels.as_mut_ptr().add(1).cast())
            },
            1
        );
        assert_eq!(
            unsafe {
                cubit_font_raster_mask(pixels.as_ptr().add(1).cast(), pixels.as_mut_ptr(), &mut m)
            },
            1
        );
        assert_eq!(
            unsafe {
                cubit_font_raster_mask(
                    &r,
                    pixels.as_mut_ptr(),
                    (&r as *const Request).cast_mut().cast(),
                )
            },
            1
        );
        assert_eq!(
            unsafe { cubit_font_raster_mask(&r, (&r as *const Request).cast_mut().cast(), &mut m) },
            1
        );
        assert!(pixels.iter().all(|v| *v == 0xA5));
        assert!(!separate(usize::MAX - 1, 4, 0, 1));
        assert_eq!(
            unsafe { cubit_font_raster_mask(&r, pixels.as_mut_ptr(), &mut m) },
            0
        );
        let mut unknown = r;
        unknown.code = u32::MAX;
        let mut question = r;
        question.code = 63;
        let mut a = vec![0; r.capacity as usize];
        let mut b = a.clone();
        assert_eq!(render(&unknown, &mut a), render(&question, &mut b));
        assert_eq!(a, b);
        assert_eq!(
            unsafe { cubit_font_raster_mask(core::ptr::null(), pixels.as_mut_ptr(), &mut m) },
            1
        );
        assert_eq!(
            unsafe { cubit_font_raster_mask(&r, core::ptr::null_mut(), &mut m) },
            1
        );
        assert_eq!(
            unsafe { cubit_font_raster_mask(&r, pixels.as_mut_ptr(), core::ptr::null_mut()) },
            1
        );
    }
}

/// Hosted timing of one miss, step by step (cargo test --release breakdown
/// -- --ignored --nocapture). Linux-hosted: not native, not QEMU.
#[cfg(test)]
mod breakdown {
    use super::*;
    extern crate std;
    use std::{println, time::Instant, vec};

    fn request(face: u32, code: u32, n: u32, d: u32) -> Request {
        let w = (BASE_WIDTH_BOUND * n + d - 1) / d;
        let h = (BASE_LINE * n + d - 1) / d;
        let pitch = (w + 15) / 16 * 16;
        Request { face, code, em_numerator: BASE_EM_PX * n, em_denominator: d, width: w, height: h, pitch,
                  capacity: pitch * h * PHASES, phases: PHASES }
    }

    #[test]
    #[ignore]
    fn miss_breakdown() {
        for (face, n, d) in [(0u32, 1u32, 1u32), (2, 1, 1), (0, 2, 1), (0, 3, 2)] {
            let codes: vec::Vec<u32> = (33..=126).collect();
            let rounds = 20u32;
            let (mut parse, mut zone, mut dark, mut prep, mut each, mut total, mut decode) = (0u128, 0u128, 0u128, 0u128, 0u128, 0u128, 0u128);
            let mut glyphs = 0u128;
            for _ in 0..rounds {
                for &code in &codes {
                    let r = request(face, code, n, d);
                    let mut pixels = vec![0u8; r.capacity as usize];
                    let t0 = Instant::now();
                    render_with(&r, &mut pixels, STYLE).unwrap();
                    total += t0.elapsed().as_nanos();
                    // The same steps, timed one by one.
                    let t = Instant::now();
                    let font = Face::parse(face_bytes(face as usize).unwrap(), 0).unwrap();
                    parse += t.elapsed().as_nanos();
                    let ratio = (r.em_numerator as f32 / r.em_denominator as f32) / f32::from(font.units_per_em());
                    let density = r.em_numerator as f32 / BASE_EM / r.em_denominator as f32;
                    let t = Instant::now();
                    let zones = zones(&font, ratio, STYLE.hinted);
                    zone += t.elapsed().as_nanos();
                    let t = Instant::now();
                    let dk = darken(&font, ratio, STYLE);
                    dark += t.elapsed().as_nanos();
                    let id = font.glyph_index(char_of(printable(face as usize, code)).unwrap()).unwrap();
                    struct Count(u32);
                    impl ttf_parser::OutlineBuilder for Count {
                        fn move_to(&mut self, _: f32, _: f32) { self.0 += 1; }
                        fn line_to(&mut self, _: f32, _: f32) { self.0 += 1; }
                        fn quad_to(&mut self, _: f32, _: f32, _: f32, _: f32) { self.0 += 1; }
                        fn curve_to(&mut self, _: f32, _: f32, _: f32, _: f32, _: f32, _: f32) { self.0 += 1; }
                        fn close(&mut self) {}
                    }
                    let t = Instant::now();
                    let mut c = Count(0);
                    let _ = font.outline_glyph(id, &mut c);
                    decode += t.elapsed().as_nanos() * PHASES as u128;
                    core::hint::black_box(c.0);
                    for phase in 0..PHASES {
                        let shift = ORIGIN as f32 * density + phase as f32 / PHASES as f32;
                        let t = Instant::now();
                        let g = outline::decode(&font, id, dk).unwrap();
                        let o = outline::prepare(&font, id, &g, ratio, shift, zones, dk, r.width + 2, r.height + 2).unwrap();
                        prep += t.elapsed().as_nanos();
                        if let Outline::Ready(p) = o {
                            let t = Instant::now();
                            let mut sum = 0u32;
                            p.each(|_, _, c| sum += c as u32);
                            each += t.elapsed().as_nanos();
                            core::hint::black_box(sum);
                        }
                    }
                    glyphs += 1;
                }
            }
            let us = |v: u128| v as f64 / glyphs as f64 / 1000.0;
            println!("BREAKDOWN face={face} density={n}/{d}: total={:.2}us parse={:.2} zones={:.2} darken={:.2} \
                      prepare(4 phases)={:.2} [of which outline decode x4={:.2}] each(4)={:.2} (per glyph, {} glyphs)",
                     us(total), us(parse), us(zone), us(dark), us(prep), us(decode), us(each), glyphs);
        }
    }
}

/// Every printable glyph of both bundled UI faces at densities 1, 3/2 and
/// 2, all four phases, hashed with its advance and compared with
/// tests/masks.golden (the engine's output when the golden was written):
/// a change to the engine must leave every byte of every mask the same.
/// CUBIT_FONTS_WRITE_GOLDEN=1 rewrites the file.
#[cfg(test)]
mod golden {
    use super::*;
    extern crate std;
    use std::{format, string::String, vec};

    fn request(face: u32, code: u32, n: u32, d: u32) -> Request {
        let w = (BASE_WIDTH_BOUND * n + d - 1) / d;
        let h = (BASE_LINE * n + d - 1) / d;
        let pitch = (w + 15) / 16 * 16;
        Request { face, code, em_numerator: BASE_EM_PX * n, em_denominator: d, width: w, height: h, pitch,
                  capacity: pitch * h * PHASES, phases: PHASES }
    }

    #[test]
    fn masks_match_golden() {
        let mut text = String::new();
        for face in [0u32, 2] {
            for (n, d) in [(1u32, 1u32), (3, 2), (2, 1)] {
                for code in 32..=126u32 {
                    let r = request(face, code, n, d);
                    let mut pixels = vec![0u8; r.capacity as usize];
                    let m = render(&r, &mut pixels).expect("printable glyphs render");
                    let mut h: u64 = 0xCBF2_9CE4_8422_2325;
                    for &b in &pixels {
                        h = (h ^ u64::from(b)).wrapping_mul(0x0000_0100_0000_01B3);
                    }
                    h = (h ^ u64::from(m.advance)).wrapping_mul(0x0000_0100_0000_01B3);
                    text.push_str(&format!("face={face} density={n}/{d} code={code} advance={} hash={h:016x}\n", m.advance));
                }
            }
        }
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/masks.golden");
        if std::env::var_os("CUBIT_FONTS_WRITE_GOLDEN").is_some() {
            std::fs::write(path, &text).unwrap();
        }
        let golden = std::fs::read_to_string(path).expect("tests/masks.golden");
        for (line, (got, want)) in text.lines().zip(golden.lines()).enumerate() {
            assert_eq!(got, want, "mask {line} differs from the golden");
        }
        assert_eq!(text.lines().count(), golden.lines().count());
    }
}
