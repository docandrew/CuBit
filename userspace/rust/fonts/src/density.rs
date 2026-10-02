//! Audited caller-storage ABI for the SPARK glyph layout policy.
use super::*;

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
}
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Metrics {
    pub advance: u32,
    pub height: u32,
}
fn layout(r: &Request) -> Option<usize> {
    if r.face > 1
        || !(13..=208).contains(&r.em_numerator)
        || r.em_numerator % 13 != 0
        || !(1..=16).contains(&r.em_denominator)
    {
        return None;
    }
    let n = r.em_numerator / 13;
    let d = r.em_denominator;
    let w = (32 * n + d - 1) / d;
    let h = (17 * n + d - 1) / d;
    let pitch = ((w + 15) / 16) * 16;
    let bytes = pitch * h;
    if r.width != w || r.height != h || r.pitch != pitch || r.capacity < bytes {
        return None;
    }
    Some(bytes as usize)
}
fn render(r: &Request, pixels: &mut [u8]) -> Option<Metrics> {
    let bytes = layout(r)?;
    if pixels.len() < bytes {
        return None;
    }
    let font = Face::parse(if r.face == 0 { SANS } else { MONO }, 0).ok()?;
    let ratio = (r.em_numerator as f32 / r.em_denominator as f32) / f32::from(font.units_per_em());
    let code = if (32..=126).contains(&r.code) {
        r.code
    } else {
        63
    };
    let id = font.glyph_index(char::from_u32(code)?)?;
    let advance = round_positive_metric(f32::from(font.glyph_hor_advance(id)?) * ratio).max(1);
    if advance > r.width {
        return None;
    }
    let baseline = round_positive_metric(f32::from(font.ascender()) * ratio) as i32;
    // Prepare the outline before changing caller pixels. This temporary coverage
    // grid is bounded; no glyph pointer or destination is retained by this API.
    let prepared = if let Some(bounds) = font.glyph_bounding_box(id) {
        let left = libm::floorf(f32::from(bounds.x_min) * ratio);
        let top = libm::floorf(-f32::from(bounds.y_max) * ratio);
        let w = libm::ceilf(f32::from(bounds.x_max) * ratio) - left;
        let h = libm::ceilf(-f32::from(bounds.y_min) * ratio) - top;
        if w < 0.0 || h < 0.0 || w > (r.width + 2) as f32 || h > (r.height + 2) as f32 {
            return None;
        }
        let mut contours = Contours {
            raster: Rasterizer::new(w as usize, h as usize),
            ratio,
            origin: point(-left, -top),
            first: point(0.0, 0.0),
            current: point(0.0, 0.0),
        };
        font.outline_glyph(id, &mut contours)?;
        Some((contours, left as i32, top as i32 + baseline))
    } else {
        None
    };
    for row in pixels[..bytes].chunks_mut(r.pitch as usize) {
        row[..r.width as usize].fill(0);
    }
    if let Some((contours, left, top)) = prepared {
        contours.raster.for_each_pixel_2d(|x, y, coverage| {
            let x = x as i32 + left;
            let y = y as i32 + top;
            if x >= 0 && y >= 0 && x < advance as i32 && y < r.height as i32 {
                pixels[y as usize * r.pitch as usize + x as usize] =
                    (coverage.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
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
            capacity: pitch * h,
        }
    }
    #[test]
    fn density_masks_match_reference_and_preserve_padding() {
        use ab_glyph::{Font, FontRef, Glyph, PxScale};
        let mut cases = 0;
        for face in 0..2 {
            let font = FontRef::try_from_slice(if face == 0 { SANS } else { MONO }).unwrap();
            for n in 1..=16 {
                for d in 1..=16 {
                    for code in 32..=126 {
                        let r = request(face, code, n, d);
                        let mut actual = vec![0xA5; r.capacity as usize + 16];
                        let m = render(&r, &mut actual).unwrap();
                        let ratio = (13.0 * n as f32 / d as f32) / font.units_per_em().unwrap();
                        let mut expected = vec![0u8; r.capacity as usize];
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
                            outline.draw(|x, y, c| {
                                let x = x as i32 + bounds.min.x as i32;
                                let y = y as i32 + bounds.min.y as i32;
                                if x >= 0 && y >= 0 && x < m.advance as i32 && y < r.height as i32 {
                                    expected[y as usize * r.pitch as usize + x as usize] =
                                        (c.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
                                }
                            });
                        }
                        for y in 0..r.height as usize {
                            for x in 0..r.pitch as usize {
                                let i = y * r.pitch as usize + x;
                                if x < r.width as usize {
                                    assert!(
                                        actual[i].abs_diff(expected[i]) <= 1,
                                        "{face}/{n}/{d}/{code} pixel{x},{y}"
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
        assert_eq!(cases, 48640);
    }
    #[test]
    fn ffi_rejections_do_not_write_and_unknown_code_maps_to_question() {
        assert_eq!(core::mem::size_of::<Request>(), 32);
        assert_eq!(core::mem::size_of::<Metrics>(), 8);
        for fault in 0..11 {
            let mut r = request(0, 65, 5, 4);
            let mut pixels = vec![0xA5; r.capacity as usize];
            let mut m = Metrics {
                advance: 123,
                height: 456,
            };
            match fault {
                0 => r.face = 2,
                1 => r.em_numerator = 0,
                2 => r.em_numerator = 209,
                3 => r.em_numerator = 14,
                4 => r.em_denominator = 0,
                5 => r.em_denominator = 17,
                6 => r.width += 1,
                7 => r.height += 1,
                8 => r.pitch += 1,
                9 => r.capacity -= 1,
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
