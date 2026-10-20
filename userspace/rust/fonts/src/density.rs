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
    /// 1: each phase's vertical stems fitted to whole pixels of its raster
    /// (the caller's low-density rule, `Compositor_Glyph_Layout.Grid_Fitted`);
    /// 0: outline positions. Anything else is rejected.
    pub grid: u32,
}
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Metrics {
    pub advance: u32,
    pub height: u32,
}
fn layout(r: &Request) -> Option<usize> {
    if r.face as usize >= FACES
        || !(13..=208).contains(&r.em_numerator)
        || r.em_numerator % 13 != 0
        || !(1..=16).contains(&r.em_denominator)
        || !(1..=PHASES).contains(&r.phases)
        || r.grid > GRID_ON
    {
        return None;
    }
    let n = r.em_numerator / 13;
    let d = r.em_denominator;
    let w = (32 * n + d - 1) / d;
    let h = (17 * n + d - 1) / d;
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
    let font = Face::parse(face_bytes(r.face as usize), 0).ok()?;
    let density = r.em_numerator as f32 / BASE_EM / r.em_denominator as f32;
    let ratio = (r.em_numerator as f32 / r.em_denominator as f32) / f32::from(font.units_per_em());
    let code = printable(r.code);
    let id = font.glyph_index(char::from_u32(code)?)?;
    let advance = round_positive_metric(f32::from(font.glyph_hor_advance(id)?) * ratio).max(1);
    if advance > r.width {
        return None;
    }
    let baseline = round_positive_metric(f32::from(font.ascender()) * ratio) as i32;
    let zones = zones(&font, ratio, style.hinted);
    let darken = darken(&font, ratio, style);
    let grid = style.grid && r.grid == GRID_ON;
    let raster = (r.pitch * r.height) as usize;
    // Prepare every phase's outline before changing caller pixels. These
    // temporary coverage grids are bounded; no glyph pointer or destination
    // is retained by this API.
    let mut prepared: [Option<outline::Prepared>; PHASES as usize] = [const { None }; PHASES as usize];
    for (phase, slot) in prepared.iter_mut().enumerate().take(r.phases as usize) {
        let shift = ORIGIN as f32 * density + phase as f32 / r.phases as f32;
        *slot = match outline::prepare(&font, id, ratio, shift, zones, darken, grid, r.width + 2, r.height + 2)? {
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
        let w = (32 * n + d - 1) / d;
        let h = (17 * n + d - 1) / d;
        let pitch = (w + 15) / 16 * 16;
        Request {
            face,
            code,
            em_numerator: 13 * n,
            em_denominator: d,
            width: w,
            height: h,
            pitch,
            capacity: pitch * h * PHASES,
            phases: PHASES,
            grid: 0,
        }
    }
    #[test]
    fn density_masks_match_reference_and_preserve_padding() {
        use ab_glyph::{Font, FontRef, Glyph, PxScale};
        let mut cases = 0;
        for face in 0..FACES as u32 {
            let font = FontRef::try_from_slice(face_bytes(face as usize)).unwrap();
            for n in 1..=16 {
                for d in 1..=16 {
                    for code in 32..=126 {
                        let r = request(face, code, n, d);
                        let mut actual = vec![0xA5; r.capacity as usize + 16];
                        let m = render_with(&r, &mut actual, PLAIN).unwrap();
                        assert!(m.advance >= 1);
                        let ratio = (13.0 * n as f32 / d as f32) / font.units_per_em().unwrap();
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
        assert_eq!(cases, 256 * 95 * FACES);
    }
    #[test]
    fn grid_masks_put_stems_on_whole_pixels() {
        // 125% is below the grid-fit threshold: the compositor sets grid,
        // and in every phase the stem of an 'l' fills whole columns.
        for face in 0..FACES as u32 {
            let mut r = request(face, b'l' as u32, 5, 4);
            r.grid = GRID_ON;
            let mut grid = vec![0u8; r.capacity as usize];
            render(&r, &mut grid).unwrap();
            let raster = (r.pitch * r.height) as usize;
            for phase in 0..PHASES as usize {
                let column_max = (0..r.width as usize)
                    .map(|x| (0..r.height as usize).map(|y| grid[phase * raster + y * r.pitch as usize + x]).max().unwrap())
                    .max()
                    .unwrap();
                assert!(column_max >= 250, "face {face} phase {phase}: no solid stem column");
            }
            // Without grid the same request gives the outline's (phased) masks.
            r.grid = 0;
            let mut a = vec![0u8; r.capacity as usize];
            let mut b = vec![0u8; r.capacity as usize];
            render(&r, &mut a).unwrap();
            render_with(&r, &mut b, Style { grid: false, ..STYLE }).unwrap();
            assert_eq!(a, b);
        }
    }

    #[test]
    fn ffi_rejections_do_not_write_and_unknown_code_maps_to_question() {
        assert_eq!(core::mem::size_of::<Request>(), 40);
        assert_eq!(core::mem::size_of::<Metrics>(), 8);
        for fault in 0..14 {
            let mut r = request(0, 65, 5, 4);
            let mut pixels = vec![0xA5; r.capacity as usize];
            let mut m = Metrics {
                advance: 123,
                height: 456,
            };
            match fault {
                0 => r.face = FACES as u32,
                1 => r.em_numerator = 0,
                2 => r.em_numerator = 209,
                3 => r.em_numerator = 14,
                4 => r.em_denominator = 0,
                5 => r.em_denominator = 17,
                6 => r.width += 1,
                7 => r.height += 1,
                8 => r.pitch += 1,
                9 => r.capacity -= 1,
                10 => r.phases = 0,
                11 => r.phases = PHASES + 1,
                12 => r.grid = GRID_ON + 1,
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
