#!/usr/bin/env python3
"""Patch a Servo checkout for x86_64-unknown-cubit (docs/servo-port.md).

Idempotent, exact, asserted edits; each states why. Run on a checkout:

    patch_servo.py <servo-checkout>
"""
import os, sys

root = sys.argv[1]

def edit(path, old, new, count=None):
    p = os.path.join(root, path)
    s = open(p, encoding="utf-8").read()
    if new in s:
        return
    assert old in s, (path, old[:80])
    if count is not None:
        assert s.count(old) == count, (path, old[:80], s.count(old))
    s = s.replace(old, new)
    open(p, "w", encoding="utf-8").write(s)

# gaol is Servo's multiprocess sandbox for Linux and macOS. On CuBit a
# process's authority is its capabilities, and Servo runs single-process:
# exclude it like the other platforms that do not use it.
GAOL_LIST = 'not(target_os = "ios"), not(target_os = "android")'
for path in ("components/servo/Cargo.toml",):
    edit(path, 'not(target_os = "windows"), not(target_os = "ios"), not(target_os = "android"), not(target_env = "ohos"), not(target_arch = "arm")',
         'not(target_os = "windows"), not(target_os = "ios"), not(target_os = "android"), not(target_os = "cubit"), not(target_env = "ohos"), not(target_arch = "arm")')
edit("components/constellation/Cargo.toml",
     'not(target_os = "windows"), not(target_os = "ios"), not(target_os="android"), not(target_env="ohos"), not(target_arch="arm")',
     'not(target_os = "windows"), not(target_os = "ios"), not(target_os="android"), not(target_os = "cubit"), not(target_env="ohos"), not(target_arch="arm")')
for path in ("components/servo/servo.rs", "components/constellation/sandboxing.rs"):
    p = os.path.join(root, path)
    s = open(p, encoding="utf-8").read()
    if 'not(target_os = "cubit")' not in s:
        s = s.replace('    not(target_os = "android"),\n', '    not(target_os = "android"),\n    not(target_os = "cubit"),\n')
        s = s.replace('        not(target_os = "android"),\n', '        not(target_os = "android"),\n        not(target_os = "cubit"),\n')
        open(p, "w", encoding="utf-8").write(s)
# Fonts: FreeType (bundled) with a CuBit font list, not fontconfig. New
# files live in userspace/servo/overlay, mirroring the Servo tree.
import shutil
overlay = os.path.join(os.path.dirname(os.path.abspath(__file__)), "overlay")
for d, _, files in os.walk(overlay):
    for name in files:
        src = os.path.join(d, name)
        dst = os.path.join(root, os.path.relpath(src, overlay))
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        # Preserve mtimes for identical overlays. Touching font_list.rs on
        # every shell edit otherwise rebuilds most of Servo unnecessarily.
        content = open(src, "rb").read()
        if name == "Cargo.toml" and b"__CUBIT_FONTS__" in content:
            import json
            fonts = os.path.realpath(os.path.join(os.path.dirname(__file__), "../rust/fonts"))
            content = content.replace(b'"__CUBIT_FONTS__"', json.dumps(fonts).encode())
        if not os.path.exists(dst) or content != open(dst, "rb").read():
            with open(dst, "wb") as output:
                output.write(content)

FREETYPE_OSES = 'any(target_os = "linux", target_os = "android", target_os = "freebsd")'
FREETYPE_OSES_CUBIT = 'any(target_os = "linux", target_os = "android", target_os = "freebsd", target_os = "cubit")'
edit("components/shared/fonts/font_identifier.rs", FREETYPE_OSES, FREETYPE_OSES_CUBIT, 1)
edit("components/fonts/platform/mod.rs", FREETYPE_OSES, FREETYPE_OSES_CUBIT, 2)
edit("components/fonts/Cargo.toml",
     "[target.'cfg(any(target_os = \"linux\", target_os = \"android\", target_os = \"freebsd\"))'.dependencies]",
     "[target.'cfg(any(target_os = \"linux\", target_os = \"android\", target_os = \"freebsd\", target_os = \"cubit\"))'.dependencies]", 1)
edit("components/fonts/Cargo.toml",
     "[target.'cfg(any(target_os = \"android\",target_env = \"ohos\"))'.dependencies]\n# Always bundle freetype on android and openharmony",
     "[target.'cfg(any(target_os = \"android\",target_env = \"ohos\", target_os = \"cubit\"))'.dependencies]\n# Always bundle freetype on android, openharmony and CuBit", 1)
edit("components/fonts/platform/freetype/mod.rs",
     "mod library_handle;",
     """#[cfg(target_os = "cubit")]
mod cubit {
    pub mod font_list;
}
#[cfg(target_os = "cubit")]
pub use self::cubit::font_list;

mod library_handle;""", 1)

# Identify as CuBit, honestly (navigator.platform and the user agent); the
# Firefox token keeps the Gecko-compatible content sites serve Servo.
edit("components/script/dom/navigator/navigatorinfo.rs",
     """#[expect(non_snake_case)]
#[cfg(target_os = "macos")]""",
     """#[expect(non_snake_case)]
#[cfg(target_os = "cubit")]
pub(crate) fn Platform() -> DOMString {
    DOMString::from_static("CuBit")
}

#[expect(non_snake_case)]
#[cfg(target_os = "macos")]""", 1)
edit("components/config/prefs.rs",
     """            UserAgentPlatform::Desktop => {
                format!(
                    "Mozilla/5.0 (X11; Linux""",
     """            UserAgentPlatform::Desktop if cfg!(target_os = "cubit") => {
                format!(
                    "Mozilla/5.0 (CuBit; {ARCH}; rv:153.0) Servo/{SERVO_VERSION} Firefox/153.0"
                )
            },
            UserAgentPlatform::Desktop => {
                format!(
                    "Mozilla/5.0 (X11; Linux""", 1)

# The CuBit embedder (overlay/ports/cubitshell) joins the workspace, so it
# shares Servo's lock file and [patch] table.
edit("Cargo.toml", '    "ports/servoshell",\n', '    "ports/servoshell",\n    "ports/cubitshell",\n', 1)

# WebRender over SWGL (cubitshell's software GL): SWGL has no GL_ALWAYS depth
# function, which WebRender's quad clears use; Gecko turns them off for
# software rendering too ("scissored clears work well").
edit("components/paint/painter.rs",
     """        let painter_id = PainterId::next();
        let (mut webrender_renderer, webrender_api_sender) = webrender::create_webrender_instance(""",
     """        let software_gl = webrender_gl.get_string(RENDERER) == "Software WebRender";
        let painter_id = PainterId::next();
        let (mut webrender_renderer, webrender_api_sender) = webrender::create_webrender_instance(""", 1)
edit("components/paint/painter.rs",
     "                shared_font_namespace: Some(painter_id.into()),",
     """                shared_font_namespace: Some(painter_id.into()),
                clear_caches_with_quads: !software_gl,""", 1)

# Client storage without a granted config directory: a CuBit program has
# no ambient /tmp, so use Servo's own in-memory engine (its fallback when a
# disk engine fails) instead of creating a temporary directory.
edit("components/storage/client_storage.rs",
     """        let (generic_sender, generic_receiver) = generic_channel::channel().unwrap();
        let mut temp_dir: Option<tempfile::TempDir> = None;""",
     """        let (generic_sender, generic_receiver) = generic_channel::channel().unwrap();
        #[cfg(target_os = "cubit")]
        if config_dir.is_none() {
            let sender_clone = generic_sender.clone();
            thread::Builder::new()
                .name("ClientStorageThread".to_owned())
                .spawn(move || {
                    let engine = SqliteEngine::memory().unwrap();
                    ClientStorageThread::new(sender_clone, generic_receiver, engine).start();
                })
                .expect("Thread spawning failed");
            return ClientStorageThreadHandle::new(generic_sender);
        }
        let mut temp_dir: Option<tempfile::TempDir> = None;""", 1)

# Cache storage keeps caches in memory; its directory is only created. Do
# not create one on CuBit without a granted config directory.
edit("components/storage/cache_storage.rs",
     """        let (generic_sender, generic_receiver) = generic_channel::channel().unwrap();
        let mut temp_dir: Option<tempfile::TempDir> = None;""",
     """        let (generic_sender, generic_receiver) = generic_channel::channel().unwrap();
        #[cfg(target_os = "cubit")]
        if config_dir.is_none() {
            let sender_clone = generic_sender.clone();
            thread::Builder::new()
                .name("CacheStorageThread".to_owned())
                .spawn(move || {
                    let engine = MemCacheStorageEngine {
                        name_to_cache_map: Default::default(),
                    };
                    CacheStorageThread::new(sender_clone, generic_receiver, engine).start();
                })
                .expect("Thread spawning failed");
            return CacheStorageThreadHandle::new(generic_sender);
        }
        let mut temp_dir: Option<tempfile::TempDir> = None;""", 1)

# TLS trust on CuBit: the system's trust store (today the concatenated-DER
# bundle at @nvme:0/tls/roots.der that tls.svc also reads; later a trust
# store service) is the only source of roots. certificate_path accepts that
# DER bundle as well as PEM, and on CuBit it replaces the built-in list.
edit("components/net/resource_thread.rs",
     """fn load_root_cert_store_from_file(file_path: String) -> io::Result<Vec<CertificateDer<'static>>> {
    let mut pem = BufReader::new(File::open(file_path)?);
""",
     """fn load_root_cert_store_from_file(file_path: String) -> io::Result<Vec<CertificateDer<'static>>> {
    #[cfg(target_os = "cubit")]
    {
        let bytes = std::fs::read(&file_path)?;
        if !bytes.starts_with(b"-----") {
            return Ok(der_bundle(&bytes));
        }
    }
    let mut pem = BufReader::new(File::open(file_path)?);
""", 1)
edit("components/net/resource_thread.rs",
     """/// Returns a tuple of (public, private) senders to the new threads.""",
     """/// Certificates concatenated as DER (each a SEQUENCE with a definite length).
#[cfg(target_os = "cubit")]
fn der_bundle(mut bytes: &[u8]) -> Vec<CertificateDer<'static>> {
    let mut certs = Vec::new();
    while bytes.len() >= 2 && bytes[0] == 0x30 {
        let (header, length) = match bytes[1] {
            n if n < 0x80 => (2, n as usize),
            0x81 if bytes.len() >= 3 => (3, bytes[2] as usize),
            0x82 if bytes.len() >= 4 => (4, (bytes[2] as usize) << 8 | bytes[3] as usize),
            0x83 if bytes.len() >= 5 => (
                5,
                (bytes[2] as usize) << 16 | (bytes[3] as usize) << 8 | bytes[4] as usize,
            ),
            _ => break,
        };
        let Some(cert) = bytes.get(..header + length) else {
            break;
        };
        certs.push(CertificateDer::from(cert.to_vec()));
        bytes = &bytes[header + length..];
    }
    certs
}

/// Returns a tuple of (public, private) senders to the new threads.""", 1)
edit("components/net/connector.rs",
     """            let mut root_store =
                rustls::RootCertStore::from_iter(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());""",
     """            // On CuBit the given roots (the system trust store) are the only ones.
            let mut root_store = if cfg!(target_os = "cubit") &&
                matches!(ca_certficates, CACertificates::Override(_))
            {
                rustls::RootCertStore::empty()
            } else {
                rustls::RootCertStore::from_iter(webpki_roots::TLS_SERVER_ROOTS.iter().cloned())
            };""", 1)

# Repair a prior local build's unsupported internal limit before exact edits.
painter_path = os.path.join(root, "components/paint/painter.rs")
painter_source = open(painter_path, encoding="utf-8").read()
if "max_internal_texture_size: if software_gl { Some(1024) } else { None }" in painter_source:
    edit("components/paint/painter.rs",
         "max_internal_texture_size: if software_gl { Some(1024) } else { None }",
         "max_internal_texture_size: if software_gl { Some(2048) } else { None }", 1)

painter_source = open(painter_path, encoding="utf-8").read()
old_limit = "max_internal_texture_size: if software_gl { Some(2048) } else { None },"
with_image_limit = old_limit + '\n                #[cfg(target_os = "cubit")]\n                image_tiling_threshold: if software_gl { 1024 } else { 4096 },'
if old_limit in painter_source and with_image_limit not in painter_source:
    edit("components/paint/painter.rs", old_limit, with_image_limit, 1)

# SWGL advertises 32768px textures, but CuBit's owned mapping limit is 16MiB.
# Even a 2048x2048 RGBA atlas exceeds that once malloc adds metadata. Bound
# software atlas/target dimensions through WebRender's existing options;
# hardware contexts keep upstream defaults. This is not a whole-engine cap.
edit("components/paint/painter.rs",
     "                clear_caches_with_quads: !software_gl,\n                ..Default::default()",
     """                clear_caches_with_quads: !software_gl,
                #[cfg(target_os = "cubit")]
                max_internal_texture_size: if software_gl { Some(2048) } else { None },
                #[cfg(target_os = "cubit")]
                image_tiling_threshold: if software_gl { 1024 } else { 4096 },
                #[cfg(target_os = "cubit")]
                texture_cache_config: if software_gl {
                    webrender::TextureCacheConfig {
                        color8_linear_texture_size: 1024,
                        color8_glyph_texture_size: 1024,
                        alpha8_glyph_texture_size: 1024,
                        ..webrender::TextureCacheConfig::DEFAULT
                    }
                } else {
                    webrender::TextureCacheConfig::DEFAULT
                },
                ..Default::default()""", 1)

# CuBit intentionally rejects MAP_SHARED file mappings. The upstream font
# reader maps then copies bytes; single-process FontData can own a read Vec
# directly, preserving scoped file access and avoiding that extra copy.
edit("components/shared/fonts/font_identifier.rs",
     """            let file = File::open(Path::new(&*self.path)).ok()?;
            let mmap = unsafe { Mmap::map(&file).ok()? };
            let data = FontData::from_bytes(&mmap);""",
     """            #[cfg(target_os = "cubit")]
            let data = FontData::from_vec(std::fs::read(Path::new(&*self.path)).ok()?);
            #[cfg(not(target_os = "cubit"))]
            let data = {
                let file = File::open(Path::new(&*self.path)).ok()?;
                let mmap = unsafe { Mmap::map(&file).ok()? };
                FontData::from_bytes(&mmap)
            };""", 1)

# FreeType retains an Arc<Mmap> for both the face and table provider. Keep
# that ownership and face-index contract while selecting a private read-only
# mapping on CuBit, whose libc deliberately does not implement MAP_SHARED.
edit("components/fonts/platform/freetype/font.rs",
     """            .and_then(|file| unsafe { Mmap::map(&file) })""",
     """            .and_then(|file| unsafe {
                #[cfg(target_os = "cubit")]
                { memmap2::MmapOptions::new().map_copy_read_only(&file) }
                #[cfg(not(target_os = "cubit"))]
                { Mmap::map(&file) }
            })""", 1)

print("servo patched")
