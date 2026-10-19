//! Prints the layout tables (advances, GPOS kerning; 1/64 px at the normal
//! 13 px em) for the Linux-hosted HarfBuzz comparison in tests/ui-fonts.
use cubit_fonts::{FACE_SANS, FACE_SANS_SEMIBOLD, cubit_font_layout};

fn main() {
    for face in FACE_SANS..=FACE_SANS_SEMIBOLD {
        let layout = unsafe { &*cubit_font_layout(face) };
        let advances: Vec<String> = layout.advance.iter().map(|a| a.to_string()).collect();
        println!("advance {face} {}", advances.join(" "));
        for (l, row) in layout.kerning.iter().enumerate() {
            for (r, &k) in row.iter().enumerate() {
                if k != 0 {
                    println!("kern {face} {} {} {k}", l + 32, r + 32);
                }
            }
        }
    }
}
