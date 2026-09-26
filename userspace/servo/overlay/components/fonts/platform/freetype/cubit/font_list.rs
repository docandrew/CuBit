/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! The font list on CuBit: the font files in one directory, described by
//! their own tables (read-fonts). There is no fontconfig; the directory is a
//! capability-scoped path the browser is granted (docs/servo-port.md).

use std::path::{Path, PathBuf};
use std::sync::LazyLock;
use std::{fs, io};

use log::{debug, warn};
use read_fonts::{FileRef, FontRef, TableProvider};
use servo_base::text::{UnicodeBlock, UnicodeBlockMethod};
use style::Atom;
use style::values::computed::font::GenericFontFamily;
use style::values::computed::{
    FontStyle as StyleFontStyle, FontWeight as StyleFontWeight, FontWidth as StyleFontWidth,
};
use unicode_script::Script;

use crate::{
    EmojiPresentationPreference, FallbackFontSelectionOptions, FontIdentifier, FontTemplate,
    FontTemplateDescriptor, LocalFontIdentifier, LowercaseFontFamilyName,
};

/// Where CuBit's font files are.
static CUBIT_FONTS_DIR: &str = "/fonts";

static FONT_LIST: LazyLock<FontList> = LazyLock::new(FontList::new);

struct Font {
    path: String,
    face_index: u16,
    weight: StyleFontWeight,
    style: StyleFontStyle,
    width: StyleFontWidth,
}

struct FontFamily {
    name: String,
    fonts: Vec<Font>,
}

struct FontList {
    families: Vec<FontFamily>,
}

/// CSS generic families and the CuBit families that serve them, in order
/// of preference (the fonts the CuBit desktop ships).
const GENERICS: &[(&str, &[&str])] = &[
    ("sans-serif", &["IBM Plex Sans", "Noto Sans"]),
    ("serif", &["Noto Serif", "IBM Plex Serif", "IBM Plex Sans"]),
    ("monospace", &["IBM Plex Mono", "Noto Sans Mono"]),
];

fn font_files() -> io::Result<Vec<PathBuf>> {
    let mut files = vec![];
    for entry in fs::read_dir(CUBIT_FONTS_DIR)?.flatten() {
        let path = entry.path();
        let is_font = path
            .extension()
            .and_then(|e| e.to_str())
            .is_some_and(|e| matches!(e, "ttf" | "otf" | "ttc"));
        if is_font && entry.file_type().is_ok_and(|t| t.is_file()) {
            files.push(path);
        }
    }
    files.sort();
    Ok(files)
}

fn describe(font: &FontRef, path: &Path, face_index: u16) -> Option<(String, Font)> {
    let names = font.name().ok()?;
    // Prefer the typographic family (name id 16) over the legacy family (1).
    let family = [16u16, 1]
        .iter()
        .find_map(|&id| {
            names
                .name_record()
                .iter()
                .filter(|record| record.name_id().to_u16() == id)
                .find_map(|record| record.string(names.string_data()).ok())
                .map(|s| s.to_string())
        })?;
    let (weight, width, italic) = match font.os2() {
        Ok(os2) => (
            StyleFontWeight::from_float(os2.us_weight_class() as f32),
            match os2.us_width_class() {
                1 => StyleFontWidth::ULTRA_CONDENSED,
                2 => StyleFontWidth::EXTRA_CONDENSED,
                3 => StyleFontWidth::CONDENSED,
                4 => StyleFontWidth::SEMI_CONDENSED,
                6 => StyleFontWidth::SEMI_EXPANDED,
                7 => StyleFontWidth::EXPANDED,
                8 => StyleFontWidth::EXTRA_EXPANDED,
                9 => StyleFontWidth::ULTRA_EXPANDED,
                _ => StyleFontWidth::NORMAL,
            },
            os2.fs_selection().bits() & 1 != 0,
        ),
        Err(_) => (StyleFontWeight::NORMAL, StyleFontWidth::NORMAL, false),
    };
    let italic = italic ||
        font
            .post()
            .is_ok_and(|post| post.italic_angle() != (0_i32).into());
    Some((
        family,
        Font {
            path: path.to_str()?.to_string(),
            face_index,
            weight,
            style: if italic {
                StyleFontStyle::ITALIC
            } else {
                StyleFontStyle::NORMAL
            },
            width,
        },
    ))
}

impl FontList {
    fn new() -> FontList {
        let files = font_files().unwrap_or_else(|e| {
            warn!("No font directory {CUBIT_FONTS_DIR}: {e:?}");
            vec![]
        });
        let mut families: Vec<FontFamily> = vec![];
        for path in files {
            let Ok(bytes) = fs::read(&path) else {
                continue;
            };
            let faces: Vec<(u16, FontRef)> = match FileRef::new(&bytes) {
                Ok(FileRef::Font(font)) => vec![(0, font)],
                Ok(FileRef::Collection(collection)) => collection
                    .iter()
                    .enumerate()
                    .filter_map(|(i, f)| Some((i as u16, f.ok()?)))
                    .collect(),
                Err(_) => continue,
            };
            for (index, face) in faces {
                let Some((name, font)) = describe(&face, &path, index) else {
                    continue;
                };
                debug!("Font {name:?}: {}#{index}", font.path);
                match families
                    .iter_mut()
                    .find(|f| f.name.eq_ignore_ascii_case(&name))
                {
                    Some(family) => family.fonts.push(font),
                    None => families.push(FontFamily {
                        name,
                        fonts: vec![font],
                    }),
                }
            }
        }
        FontList { families }
    }

    fn find_family(&self, name: &str) -> Option<&FontFamily> {
        self.families
            .iter()
            .find(|family| family.name.eq_ignore_ascii_case(name))
    }

    /// The installed family serving a CSS generic family name, if any.
    fn resolve_generic(&self, name: &str) -> Option<&FontFamily> {
        let (_, candidates) = GENERICS
            .iter()
            .find(|(generic, _)| generic.eq_ignore_ascii_case(name))?;
        candidates.iter().find_map(|c| self.find_family(c))
    }
}

// Functions used by SystemFontService

pub(crate) fn for_each_available_family<F>(mut callback: F)
where
    F: FnMut(String),
{
    for family in &FONT_LIST.families {
        callback(family.name.clone());
    }
    for (generic, _) in GENERICS {
        if FONT_LIST.resolve_generic(generic).is_some() {
            callback(generic.to_string());
        }
    }
}

pub(crate) fn for_each_variation<F>(family_name: &str, mut callback: F)
where
    F: FnMut(FontTemplate),
{
    let Some(family) = FONT_LIST
        .find_family(family_name)
        .or_else(|| FONT_LIST.resolve_generic(family_name))
    else {
        return;
    };
    for font in &family.fonts {
        let identifier = LocalFontIdentifier {
            path: Atom::from(font.path.as_str()),
            face_index: font.face_index,
            named_instance_index: 0,
        };
        callback(FontTemplate::new(
            FontIdentifier::Local(identifier),
            FontTemplateDescriptor::new(font.weight, font.width, font.style),
            None,
        ));
    }
}

pub fn fallback_font_families(options: FallbackFontSelectionOptions) -> Vec<&'static str> {
    let mut families = vec![];
    if options.presentation_preference == EmojiPresentationPreference::Emoji {
        families.push("Noto Color Emoji");
    }
    if Script::from(options.character) == Script::Han {
        families.push("Noto Sans CJK SC");
    }
    if let Some(block) = options.character.block() {
        match block {
            UnicodeBlock::Hiragana |
            UnicodeBlock::Katakana |
            UnicodeBlock::KatakanaPhoneticExtensions => families.push("Noto Sans CJK JP"),
            UnicodeBlock::HangulSyllables |
            UnicodeBlock::HangulJamo |
            UnicodeBlock::HangulCompatibilityJamo => families.push("Noto Sans CJK KR"),
            UnicodeBlock::Arabic => families.push("Noto Sans Arabic"),
            UnicodeBlock::Hebrew => families.push("Noto Sans Hebrew"),
            UnicodeBlock::Devanagari => families.push("Noto Sans Devanagari"),
            UnicodeBlock::Thai => families.push("Noto Sans Thai"),
            _ => {},
        }
    }
    families.push("IBM Plex Sans");
    families.push("Noto Sans");
    families.push("Noto Sans Symbols");
    families.push("Noto Sans Symbols 2");
    families
}

pub(crate) fn default_system_generic_font_family(
    generic: GenericFontFamily,
) -> LowercaseFontFamilyName {
    let name = match generic {
        GenericFontFamily::Monospace => "monospace",
        GenericFontFamily::Serif => "serif",
        _ => "sans-serif",
    };
    match FONT_LIST.resolve_generic(name) {
        Some(family) => family.name.as_str().into(),
        None => "IBM Plex Sans".into(),
    }
}
