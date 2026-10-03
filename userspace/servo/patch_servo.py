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

# SWGL 0.70 precompiles gradient shaders without DITHERING. Asking WebRender
# for that variant leaves the SWGL program without an implementation and
# aborts at BindAttribLocation when a page first draws a gradient.
edit("components/paint/painter.rs",
     "                enable_dithering: true,",
     "                enable_dithering: !software_gl,", 1)

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

# Migrate the previous CuBit options before matching the full replacement.
old_shared = "image_tiling_threshold: if software_gl { 1024 } else { 4096 },"
new_shared = old_shared + '\n                #[cfg(target_os = "cubit")]\n                max_shared_surface_size: if software_gl { 1024 } else { 2048 },'
painter_source = open(painter_path, encoding="utf-8").read()
if old_shared in painter_source and new_shared not in painter_source:
    edit("components/paint/painter.rs", old_shared, new_shared, 1)

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
                max_shared_surface_size: if software_gl { 1024 } else { 2048 },
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

tls_script = "components/script/event_loop/script_thread.rs"
if ".filter(|der| der.len() <= 65536)" in open(os.path.join(root, tls_script), encoding="utf-8").read():
    edit(tls_script, ".filter(|der| der.len() <= 65536)", ".take_while(|der| der.len() <= 65536)", 1)

# Penny's main-document inspector reuses the handshake captured by rustls.
# Carry it with Document so history activation restores the right certificate;
# never infer it from a host-wide cache or a separate probe connection.
edit("components/shared/embedder/lib.rs", "pub mod embedder_controls;", """/// TLS of a committed top-level document; certificate chain is leaf first.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct DocumentTlsInfo {
    pub url: String,
    pub protocol: String,
    pub cipher: String,
    pub alpn: String,
    pub certificates: Vec<Vec<u8>>,
}

pub mod embedder_controls;""", 1)
edit("components/shared/embedder/lib.rs",
     "    ChangePageTitle(WebViewId, Option<String>),",
     "    ChangePageTitle(WebViewId, Option<String>),\n    DocumentTls(WebViewId, Option<DocumentTlsInfo>),", 1)
edit("components/script/dom/document/document.rs",
     "\n    last_modified: Option<String>,",
     """
    last_modified: Option<String>,
    #[no_trace]
    #[ignore_malloc_size_of = "bounded TLS certificate snapshot"]
    document_tls: RefCell<Option<embedder_traits::DocumentTlsInfo>>,""", 1)
edit("components/script/dom/document/document.rs", "            last_modified,\n            url: DomRefCell::new(url),",
     "            last_modified,\n            document_tls: RefCell::new(None),\n            url: DomRefCell::new(url),", 1)
edit("components/script/dom/document/document.rs",
     "    pub(crate) fn send_title_to_embedder(&self) {",
     """    pub(crate) fn set_document_tls(&self, info: Option<embedder_traits::DocumentTlsInfo>) {
        *self.document_tls.borrow_mut() = info;
    }

    pub(crate) fn send_title_to_embedder(&self) {""", 1)
edit("components/script/dom/document/document.rs",
     "            self.send_to_embedder(EmbedderMsg::ChangePageTitle(self.webview_id(), title));",
     """            self.send_to_embedder(EmbedderMsg::ChangePageTitle(self.webview_id(), title));
            if self.is_fully_active() {
                self.send_to_embedder(EmbedderMsg::DocumentTls(
                    self.webview_id(), self.document_tls.borrow().clone()));
            }""", 1)
edit("components/script/event_loop/script_thread.rs",
     "        document.set_navigation_start(incomplete.navigation_start);",
     """        document.set_navigation_start(incomplete.navigation_start);
        document.set_document_tls(metadata.tls_security_info.as_ref().map(|tls| {
            embedder_traits::DocumentTlsInfo {
                url: metadata.final_url.to_string(),
                protocol: tls.protocol_version.as_ref().map(|v| format!("{v:?}")).unwrap_or_default(),
                cipher: tls.cipher_suite.as_ref().map(|v| format!("{v:?}")).unwrap_or_default(),
                alpn: tls.alpn_protocol.clone().unwrap_or_default(),
                certificates: tls.certificate_chain_der.iter().take(8)
                    .take_while(|der| der.len() <= 65536).cloned().collect(),
            }
        }));""", 1)
edit("components/servo/servo.rs", "            EmbedderMsg::ChangePageTitle(webview_id, title) => {",
     """            EmbedderMsg::DocumentTls(webview_id, info) => {
                if let Some(webview) = self.get_webview_handle(webview_id) {
                    webview.set_document_tls(info);
                }
            },
            EmbedderMsg::ChangePageTitle(webview_id, title) => {""", 1)
edit("components/servo/webview.rs", "    page_title: Option<String>,",
     "    page_title: Option<String>,\n    document_tls: Option<embedder_traits::DocumentTlsInfo>,", 1)
edit("components/servo/webview.rs", "            page_title: None,",
     "            page_title: None,\n            document_tls: None,", 1)
edit("components/servo/webview.rs", "    pub fn page_title(&self) -> Option<String> {",
     """    pub fn document_tls(&self) -> Option<embedder_traits::DocumentTlsInfo> {
        self.inner().document_tls.clone()
    }

    pub(crate) fn set_document_tls(self, info: Option<embedder_traits::DocumentTlsInfo>) {
        if self.inner().document_tls == info { return; }
        self.inner_mut().document_tls = info;
        self.delegate().notify_document_tls_changed(self);
    }

    pub fn page_title(&self) -> Option<String> {""", 1)
edit("components/servo/webview_delegate.rs",
     "    fn notify_page_title_changed(&self, _webview: WebView, _title: Option<String>) {}",
     """    fn notify_page_title_changed(&self, _webview: WebView, _title: Option<String>) {}
    fn notify_document_tls_changed(&self, _webview: WebView) {}""", 1)

# Clear a previous connection before a new navigation/reload, including a
# reload of the same URL. HeadParsed can then expose the committed document's
# TLS even when unrelated subresources keep the load event outstanding.
edit("components/servo/webview.rs",
     "    pub(crate) fn set_load_status(self, new_value: LoadStatus) {",
     """    pub(crate) fn set_load_status(self, new_value: LoadStatus) {
        if new_value == LoadStatus::Started { self.inner_mut().document_tls = None; }""", 1)
edit("components/servo/webview.rs",
     "    pub fn load_request(&self, url_request: UrlRequest) {",
     """    pub fn load_request(&self, url_request: UrlRequest) {
        self.clone().set_load_status(LoadStatus::Started);""", 1)
edit("components/servo/webview.rs",
     "        self.inner_mut().load_status = LoadStatus::Started;",
     "        self.clone().set_load_status(LoadStatus::Started);", 1)

# CuBit grants a bounded number of live TCP channels. Hyper's default idle
# timeout has no effect without a pool timer; abandoned origins otherwise
# retain channels indefinitely. Keep brief reuse without exhausting the grant.
edit("components/net/connector.rs",
     """    Client::builder(TokioExecutor {})
        .http1_title_case_headers(true)
        .build(InstrumentedConnector::from(connector))""",
     """    let mut builder = Client::builder(TokioExecutor {});
    #[cfg(target_os = "cubit")]
    builder
        .pool_timer(hyper_util::rt::TokioTimer::new())
        .pool_idle_timeout(Duration::from_secs(5))
        .pool_max_idle_per_host(2);
    builder
        .http1_title_case_headers(true)
        .build(InstrumentedConnector::from(connector))""", 1)

# Clearing history must retire unreachable documents through Servo's normal
# asynchronous shutdown path. The resident tab container is not a lifetime
# oracle; script/paint acknowledgments and owned-memory samples remain separate.
with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "history_clear.rs"), encoding="utf-8") as f:
    history_clear = f.read().rstrip("\n")
edit("components/constellation/constellation.rs",
     """    fn handle_clear_session_history(&mut self, webview_id: WebViewId) {
        let Some(webview) = self.webviews.get_mut(&webview_id) else {
            return;
        };
        webview.session_history.future.clear();
        webview.session_history.past.clear();
        self.notify_history_changed(webview_id);
    }""", history_clear, 1)
edit("components/constellation/constellation.rs",
     "    fn handle_pipeline_exited(&mut self, pipeline_id: PipelineId, exit_source: PipelineExitSource) {",
     """    #[cfg(target_os = "cubit")]
    fn cubit_lifetime_trace(&self, event: &str) {
        static ENABLED: std::sync::LazyLock<bool> = std::sync::LazyLock::new(||
            std::path::Path::new("/servo/perf-check").exists());
        if *ENABLED {
            warn!("CUBITSHELL-LIFETIME: event={} pipeline_entries={} contexts={} webviews={}",
                event, self.pipelines.len(), self.browsing_contexts.len(), self.webviews.len());
        }
    }

    fn handle_pipeline_exited(&mut self, pipeline_id: PipelineId, exit_source: PipelineExitSource) {""", 1)
edit("components/constellation/constellation.rs",
     """            pipeline.id,
            exit_source,
        ));""",
     """            pipeline.id,
            exit_source,
        ));
        #[cfg(target_os = "cubit")]
        self.cubit_lifetime_trace("pipeline-exited");""", 1)

# Media error callbacks can synchronously replace the player.
# Do not stop the replacement or clear its load-event delay afterward.
edit('components/script/dom/html/embedded_content/htmlmediaelement.rs',
     """                    this.upcast::<EventTarget>().fire_event(cx, atom!("error"));

                    if let Some(ref player)""",
     """                    this.upcast::<EventTarget>().fire_event(cx, atom!("error"));

                    // The error handler (including its promise microtasks) may load
                    // a new resource. Never stop that replacement player.
                    if generation_id != this.generation_id.get() {
                        return;
                    }

                    if let Some(ref player)""", 1)
edit('components/script/dom/html/embedded_content/htmlmediaelement.rs',
     """                // Step 7. Set the element's delaying-the-load-event flag to false. This stops
                // delaying the load event.
                this.delay_load_event(false, cx);""",
     """                // An error handler may have started another load, whose load-event
                // delay belongs to that new generation.
                if generation_id != this.generation_id.get() {
                    return;
                }

                // Step 7. Set the element's delaying-the-load-event flag to false. This stops
                // delaying the load event.
                this.delay_load_event(false, cx);""", 1)

# Keep acknowledging every retired media instance, until all senders close.
# Upgrade an already-patched shutdown loop before the full idempotent edit.
if "while let Ok(message) = recvr.recv()" in open(os.path.join(root, "components/media/backends/gstreamer/lib.rs")).read():
    edit("components/media/backends/gstreamer/lib.rs", '                while let Ok(message) = recvr.recv() {\n                    match message {\n', '                while let Ok(message) = recvr.recv() {\n                    match message {\n                    BackendMsg::Retire { resources } => drop(resources),\n', 1)
edit('components/media/backends/gstreamer/lib.rs', '        thread::Builder::new()\n            .name("GStreamerBackend ShutdownThread".to_owned())\n            .spawn(move || {\n                match recvr.recv().unwrap() {\n                    BackendMsg::Shutdown {\n                        context,\n                        id,\n                        tx_ack,\n                    } => {\n                        let mut instances_ = instances_.lock().unwrap();\n                        if let Some(vec) = instances_.get_mut(&context) {\n                            vec.retain(|m| m.0 != id);\n                            if vec.is_empty() {\n                                instances_.remove(&context);\n                            }\n                        }\n                        // tell caller we are done removing this instance\n                        let _ = tx_ack.send(());\n                    },\n                };\n            })\n            .unwrap();', '        thread::Builder::new()\n            .name("GStreamerBackend ShutdownThread".to_owned())\n            .spawn(move || {\n                while let Ok(message) = recvr.recv() {\n                    match message {\n                    BackendMsg::Retire { resources } => drop(resources),\n                    BackendMsg::Shutdown {\n                        context,\n                        id,\n                        tx_ack,\n                    } => {\n                        let mut instances_ = instances_.lock().unwrap();\n                        if let Some(vec) = instances_.get_mut(&context) {\n                            vec.retain(|m| m.0 != id);\n                            if vec.is_empty() {\n                                instances_.remove(&context);\n                            }\n                        }\n                        // tell caller we are done removing this instance\n                        let _ = tx_ack.send(());\n                    },\n                    }\n                }\n            })\n            .unwrap();', 1)

# Media ownership: BEGIN
# PlayerInner owns its signal adapter/pipeline; their callbacks must not own
# PlayerInner back. The source bin likewise owns its appsrc callbacks.
edit('components/media/backends/gstreamer/player.rs', 'let inner_clone = inner.clone();', 'let inner_clone = Arc::downgrade(inner);', 4)
edit('components/media/backends/gstreamer/player.rs', '            inner_clone.lock().unwrap().play_state = play_state;', '            let Some(inner_clone) = inner_clone.upgrade() else { return; };\n            inner_clone.lock().unwrap().play_state = play_state;', 1)
edit('components/media/backends/gstreamer/player.rs', '\n            let mut inner = inner_clone.lock().unwrap();', '\n            let Some(inner_clone) = inner_clone.upgrade() else { return; };\n            let mut inner = inner_clone.lock().unwrap();', 2)
edit('components/media/backends/gstreamer/player.rs', '\n                let mut inner = inner_clone.lock().unwrap();', '\n                let Some(inner_clone) = inner_clone.upgrade() else { return None; };\n                let mut inner = inner_clone.lock().unwrap();', 1)
edit('components/media/backends/gstreamer/player.rs', '                        let servosrc_ = servosrc.clone();', '                        let weak_servosrc = servosrc.downgrade();', 1)
edit('components/media/backends/gstreamer/player.rs', '                                .seek_data(move |_, offset| {', '                                .seek_data(move |_, offset| {\n                                    let Some(servosrc_) = weak_servosrc.upgrade() else { return false; };', 1)
edit('components/media/backends/gstreamer/audio_sink.rs', 'impl Drop for GStreamerAudioSink {\n    fn drop(&mut self) {\n        let _ = self.stop();\n    }\n}', 'impl Drop for GStreamerAudioSink {\n    fn drop(&mut self) {\n        // Paused pipelines retain streaming resources; destruction must also\n        // release the device/session held by the output sink.\n        let _ = self.pipeline.set_state(gstreamer::State::Null);\n    }\n}', 1)
# Media ownership: END

# CuBit uses one capability-scoped output shared by media and WebAudio.
edit("components/media/backends/gstreamer/player.rs", '        if let Some(ref audio_renderer) = self.audio_renderer {', '        #[cfg(target_os = "cubit")]\n        if self.audio_renderer.is_none() {\n            let sink = gstreamer::ElementFactory::make("pennyaudiosink")\n                .build()\n                .map_err(|error| PlayerError::Backend(format!("Penny audio output: {error:?}")))?;\n            pipeline.set_property("audio-sink", &sink);\n        }\n\n        if let Some(ref audio_renderer) = self.audio_renderer {', 1)
edit("components/media/backends/gstreamer/audio_sink.rs", 'gstreamer::ElementFactory::make("autoaudiosink")', 'gstreamer::ElementFactory::make(if cfg!(target_os = "cubit") { "pennyaudiosink" } else { "autoaudiosink" })', 1)


# CuBit media may not create temporary download-buffer files.
edit("components/media/backends/gstreamer/player.rs",
     'if !cfg!(any(target_os = "windows", target_os = "android")) &&',
     'if !cfg!(any(target_os = "windows", target_os = "android", target_os = "cubit")) &&', 1)

# Split native software-renderer stalls only when the diagnostic profiler is active.
edit('components/shared/profile/time.rs',
     '    Painting = 0x00,',
     '    Painting = 0x00,\n    PaintUpdate = 0x01,\n    PaintDraw = 0x02,', 1)
edit('components/shared/profile/time.rs',
     '            ProfilerCategory::Painting => "Painting",',
     '            ProfilerCategory::Painting => "Painting",\n            ProfilerCategory::PaintUpdate => "PaintUpdate",\n            ProfilerCategory::PaintDraw => "PaintDraw",', 1)
upstream_paint = """                if let Some(renderer) = self.webrender_renderer.as_mut() {
                    renderer.update();
                }

                // Paint the scene.
                // TODO(gw): Take notice of any errors the renderer returns!
                self.clear_background();
                if let Some(renderer) = self.webrender_renderer.as_mut() {
                    let size = self.rendering_context.size2d().to_i32();
                    renderer.render(size, 0 /* buffer_age */).ok();
                }"""

phase_paint = """                let mut update = || {
                    if let Some(renderer) = self.webrender_renderer.as_mut() {
                        renderer.update();
                    }
                };
                if time_profiler_channel.0.is_some() {
                    time_profile!(ProfilerCategory::PaintUpdate, None,
                        time_profiler_channel.clone(), update);
                } else {
                    update();
                }

                let mut draw = || {
                    self.clear_background();
                    if let Some(renderer) = self.webrender_renderer.as_mut() {
                        let size = self.rendering_context.size2d().to_i32();
                        renderer.render(size, 0 /* buffer_age */).ok();
                    }
                };
                if time_profiler_channel.0.is_some() {
                    time_profile!(ProfilerCategory::PaintDraw, None,
                        time_profiler_channel.clone(), draw);
                } else {
                    draw();
                }"""

stats_paint = """                let mut update = || {
                    if let Some(renderer) = self.webrender_renderer.as_mut() {
                        renderer.update();
                    }
                };
                if time_profiler_channel.0.is_some() {
                    time_profile!(ProfilerCategory::PaintUpdate, None,
                        time_profiler_channel.clone(), update);
                } else {
                    update();
                }

                let mut draw = || {
                    self.clear_background();
                    if let Some(renderer) = self.webrender_renderer.as_mut() {
                        let size = self.rendering_context.size2d().to_i32();
                        let results = renderer.render(size, 0 /* buffer_age */);
                        if time_profiler_channel.0.is_some() {
                            match &results {
                                Ok(result) => {
                                    let stats = &result.stats;
                                    warn!("PENNY-RENDER: draws={} upload_mb={:.3} upload_ms={:.3} scene_ms={:.3} frame_ms={:.3} targets_color={} targets_alpha={} rasterized={}",
                                        stats.total_draw_calls, stats.texture_upload_mb,
                                        stats.resource_upload_time, stats.scene_build_time,
                                        stats.frame_build_time, stats.color_target_count,
                                        stats.alpha_target_count, result.did_rasterize_any_tile);
                                },
                                Err(errors) => warn!("PENNY-RENDER: errors={errors:?}"),
                            }
                        }
                    }
                };
                if time_profiler_channel.0.is_some() {
                    time_profile!(ProfilerCategory::PaintDraw, None,
                        time_profiler_channel.clone(), draw);
                } else {
                    draw();
                }"""

query_paint = """                let mut update = || {
                    if let Some(renderer) = self.webrender_renderer.as_mut() {
                        if time_profiler_channel.0.is_some() {
                            let flag = webrender::DebugFlags::GPU_TIME_QUERIES;
                            let flags = renderer.get_debug_flags();
                            if !flags.contains(flag) {
                                renderer.set_debug_flags(flags | flag);
                            }
                            // Consume completed query batches before WebRender does.
                            // SWGL implements these as CPU monotonic-clock intervals,
                            // not hardware GPU time. Results lag several frames.
                            let (frame, timers, _) = renderer.gpu_profiler.build_samples();
                            for timer in timers {
                                if timer.time_ns >= 1_000_000 {
                                    warn!("PENNY-DRAW-TIMER: frame={frame:?} tag={} ms={:.3}",
                                        timer.tag.label, timer.time_ns as f64 / 1_000_000.0);
                                }
                            }
                        }
                        renderer.update();
                    }
                };
                if time_profiler_channel.0.is_some() {
                    time_profile!(ProfilerCategory::PaintUpdate, None,
                        time_profiler_channel.clone(), update);
                } else {
                    update();
                }

                let mut draw = || {
                    self.clear_background();
                    if let Some(renderer) = self.webrender_renderer.as_mut() {
                        let size = self.rendering_context.size2d().to_i32();
                        let results = renderer.render(size, 0 /* buffer_age */);
                        if time_profiler_channel.0.is_some() {
                            match &results {
                                Ok(result) => {
                                    let stats = &result.stats;
                                    warn!("PENNY-RENDER: draws={} upload_mb={:.3} upload_ms={:.3} scene_ms={:.3} frame_ms={:.3} targets_color={} targets_alpha={} rasterized={}",
                                        stats.total_draw_calls, stats.texture_upload_mb,
                                        stats.resource_upload_time, stats.scene_build_time,
                                        stats.frame_build_time, stats.color_target_count,
                                        stats.alpha_target_count, result.did_rasterize_any_tile);
                                },
                                Err(errors) => warn!("PENNY-RENDER: errors={errors:?}"),
                            }
                        }
                    }
                };
                if time_profiler_channel.0.is_some() {
                    time_profile!(ProfilerCategory::PaintDraw, None,
                        time_profiler_channel.clone(), draw);
                } else {
                    draw();
                }"""

painter_source = open(painter_path, encoding="utf-8").read()
if query_paint not in painter_source:
    if stats_paint in painter_source:
        edit("components/paint/painter.rs", stats_paint, query_paint, 1)
    elif phase_paint in painter_source:
        edit("components/paint/painter.rs", phase_paint, query_paint, 1)
    else:
        edit("components/paint/painter.rs", upstream_paint, query_paint, 1)

# Preserve user mute while a disabled audio track is silenced.
edit('components/media/backends/gstreamer/player.rs', '    muted: Cell<bool>,', '    muted: Cell<bool>,\n    audio_track_enabled: bool,', 1)
edit('components/media/backends/gstreamer/player.rs', '        self.player.set_mute(muted);', '        self.player.set_mute(muted || !self.audio_track_enabled);', 1)
edit('components/media/backends/gstreamer/player.rs', '        self.player.set_audio_track_enabled(enabled);', '        // GstPlay cannot select an empty stream set for audio-only media.\n        // Preserve the requested mute state while silencing a disabled track.\n        self.audio_track_enabled = enabled;\n        self.player.set_mute(self.muted.get() || !enabled);\n        self.player.set_audio_track_enabled(enabled);', 1)
edit('components/media/backends/gstreamer/player.rs', '            muted: Cell::new(DEFAULT_MUTED),', '            muted: Cell::new(DEFAULT_MUTED),\n            audio_track_enabled: true,', 1)

# Apply an HTML element volume set before the backend player was created.
edit('components/script/dom/html/embedded_content/htmlmediaelement.rs', '            if let Err(error) = player_guard.set_mute(self.muted.get()) {\n                warn!("Could not set mute state: {error:?}");\n            }\n', '            if let Err(error) = player_guard.set_mute(self.muted.get()) {\n                warn!("Could not set mute state: {error:?}");\n            }\n\n            // The element may receive volume before its backend player exists.\n            if let Err(error) = player_guard.set_volume(self.volume.get()) {\n                warn!("Could not set initial volume: {error:?}");\n            }\n', 1)

# Keep media state transitions and periodic events working across pause/resume.
edit('components/script/dom/html/embedded_content/htmlmediaelement.rs', '        if self.is_potentially_playing() && !is_playing {', '        let should_play = self.is_potentially_playing();\n        if should_play && !is_playing {', 1)
edit('components/script/dom/html/embedded_content/htmlmediaelement.rs', '        } else if is_playing &&\n            let Some(ref player) = *self.player.borrow() &&', '        } else if !should_play && is_playing &&\n            let Some(ref player) = *self.player.borrow() &&', 1)
edit('components/script/dom/html/embedded_content/htmlmediaelement.rs', '        let Some((current_cues, other_cues)) = self.current_and_other_cues(cx.no_gc()) else {\n            return;\n        };', '        // No text tracks means empty cue lists, not an end to playback updates.\n        let (current_cues, other_cues) = self\n            .current_and_other_cues(cx.no_gc())\n            .unwrap_or_default();', 1)

# Media retirement: BEGIN
# Quiesce GstPlay on the shutdown worker before releasing native references.
edit('components/media/traits/lib.rs', 'pub enum BackendMsg {', 'pub enum BackendMsg {\n    /// Release native resources away from their own callback/streaming threads.\n    Retire {\n        #[ignore_malloc_size_of = "Native resources queued for destruction"]\n        resources: Box<dyn Send>,\n    },', 1)
edit('components/media/backends/ohos/lib.rs', '                    BackendMsg::Shutdown {', '                    BackendMsg::Retire { resources } => drop(resources),\n                    BackendMsg::Shutdown {', 1)
edit('components/media/backends/gstreamer/player.rs', 'use std::ops::Range;', 'use std::ops::Range;\nuse std::mem::ManuallyDrop;', 1)
edit('components/media/backends/gstreamer/player.rs', '    player: gstreamer_play::Play,\n    signal_adapter: gstreamer_play::PlaySignalAdapter,', '    player: ManuallyDrop<gstreamer_play::Play>,\n    signal_adapter: ManuallyDrop<gstreamer_play::PlaySignalAdapter>,\n    retirement: Sender<BackendMsg>,', 1)
edit('components/media/backends/gstreamer/player.rs', 'impl PlayerInner {', "struct RetiredPlayer {\n    player: gstreamer_play::Play,\n    _signal_adapter: gstreamer_play::PlaySignalAdapter,\n}\n\nimpl Drop for RetiredPlayer {\n    fn drop(&mut self) {\n        // Native GstMessages can still reference the player after Rust's last\n        // callback reference is gone. Quiesce it while retaining our strong\n        // reference, so such a message cannot trigger disposal on its thread.\n        // SAFETY: PlayerInner has been exclusively destroyed, so application\n        // code cannot call player methods again. This runs on the backend's\n        // shutdown worker. GstPlay::dispose quits and joins its native thread;\n        // no inner mutex is held while it waits for in-flight callbacks.\n        unsafe { self.player.run_dispose(); }\n    }\n}\n\nimpl Drop for PlayerInner {\n    fn drop(&mut self) {\n        // A weak callback can temporarily hold the final Arc<PlayerInner>.\n        // GstPlay must not finalize on its own thread: dispose would skip join\n        // while the native main loop still uses self during shutdown.\n        // SAFETY: Drop runs once with exclusive access. These two fields are\n        // taken once, and ManuallyDrop prevents a second field destruction.\n        let resources: Box<dyn Send> = unsafe {\n            Box::new(RetiredPlayer {\n                player: ManuallyDrop::take(&mut self.player),\n                _signal_adapter: ManuallyDrop::take(&mut self.signal_adapter),\n            })\n        };\n        if let Err(_retirement) = self.retirement.send(BackendMsg::Retire { resources }) {\n            // Losing the lifetime owner is fatal. Do not release the returned\n            // resources here on a possible native callback thread.\n            std::process::abort();\n        }\n    }\n}\n\nimpl PlayerInner {", 1)
edit('components/media/backends/gstreamer/player.rs', '            player,\n            signal_adapter: signal_adapter.clone(),', '            player: ManuallyDrop::new(player),\n            signal_adapter: ManuallyDrop::new(signal_adapter.clone()),\n            retirement: self.backend_chan.lock().unwrap().clone(),', 1)
edit('components/media/backends/gstreamer/player.rs', 'obj = &self.player', 'obj = &*self.player', 1)
edit('components/media/backends/gstreamer/player.rs', 'obj = &inner.player', 'obj = &*inner.player', 2)
edit('components/media/backends/gstreamer/player.rs', '&inner.lock().unwrap().signal_adapter,', '&*inner.lock().unwrap().signal_adapter,', 1)
# Media retirement: END

# Parent navigation can suspend a proxy before child response processing.
edit('components/script/dom/window/windowproxy.rs', '            // Step 4. Assert: parentDoc is fully active.\n            assert!(parent_proxy.currently_active().is_some());\n            parent_proxy.document_origin_and_internal_ancestor_origin_objects_list()', "            // A parent can be suspended by navigation while this document's\n            // response is still being processed. As with the same-thread\n            // frame-element branch above, unavailable parent state yields no\n            // ancestor-origin snapshot. The lookup already handles None and\n            // failed constellation replies; do not invent a parent origin.\n            parent_proxy.document_origin_and_internal_ancestor_origin_objects_list()", 1)

# Preserve remote-parent origin retrieval and fail closed for unavailable CSP ancestry.
edit('components/shared/constellation/from_script_message.rs', '    /// Get the browsing context id of the browsing context in which pipeline is', "    /// Resolve a browsing context's current document even when its WindowProxy\n    /// belongs to another script thread.\n    GetActivePipeline(BrowsingContextId, GenericSender<Option<PipelineId>>),\n    /// Get the browsing context id of the browsing context in which pipeline is", 1)
edit('components/constellation/tracing.rs', '                Self::GetBrowsingContextInfo(..) => target!("GetBrowsingContextInfo"),', '                Self::GetActivePipeline(..) => target!("GetActivePipeline"),\n                Self::GetBrowsingContextInfo(..) => target!("GetBrowsingContextInfo"),', 1)
edit('components/constellation/constellation.rs', '            ScriptToConstellationMessage::GetDocumentOriginDetails(\n', '            ScriptToConstellationMessage::GetActivePipeline(context_id, response_sender) => {\n                let pipeline = self.browsing_contexts.get(&context_id).map(|context| context.pipeline_id);\n                let _ = response_sender.send(pipeline);\n            },\n            ScriptToConstellationMessage::GetDocumentOriginDetails(\n', 1)
edit('components/script/dom/window/windowproxy.rs', '        let pipeline_id = self.currently_active()?;\n        let (result_sender, result_receiver) = generic_channel::channel().unwrap();', '        let pipeline_id = if let Some(pipeline_id) = self.currently_active() {\n            pipeline_id\n        } else {\n            // Remote proxies deliberately have no local active pipeline. Ask\n            // the constellation to resolve the browsing context, rather than\n            // treating a remote parent as absent.\n            let (sender, receiver) = generic_channel::channel()?;\n            self.global().script_to_constellation_chan().send(\n                ScriptToConstellationMessage::GetActivePipeline(self.browsing_context_id(), sender),\n            ).ok()?;\n            receiver.recv().ok()??\n        };\n        // Never synchronously send an origin query back to this script thread.\n        if let Some(document) = ScriptThread::find_document(pipeline_id) {\n            return Some((document.origin().snapshot(),\n                document.internal_ancestor_origin_objects_list().clone().unwrap_or_default()));\n        }\n        let (result_sender, result_receiver) = generic_channel::channel().unwrap();', 1)
edit('components/script/dom/security/csp.rs', '                    parent_proxy.document_origin_and_internal_ancestor_origin_objects_list()\n                else {\n                    break;\n                };', '                    parent_proxy.document_origin_and_internal_ancestor_origin_objects_list()\n                else {\n                    // A missing parent snapshot must not silently shorten the\n                    // ancestor chain used for frame-ancestors enforcement.\n                    return true;\n                };', 1)

from pointer_cancel import apply as apply_pointer_cancel
apply_pointer_cancel(edit)

from intersection_observer import apply as apply_intersection_observer
apply_intersection_observer(edit)

from profiler_output import apply as apply_profiler_output
apply_profiler_output(edit)

from script_profile import apply as apply_script_profile
apply_script_profile(edit)
