/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! cubitshell: Servo embedded on CuBit (docs/servo-port.md).
//!
//! Pages are rendered by WebRender over SWGL, its software OpenGL, into
//! memory; there is no GPU driver, windowing system or surfman platform
//! backend. It loads one page and reports the frame:
//!
//!     cubitshell [url] [out.ppm]
//!
//! Hosted builds write the frame as a PPM image. On CuBit, given a desktop
//! capability, frames are presented in a desktop window and its input is
//! forwarded to the page; without one (headless tests) it reports and exits.

#[cfg(target_os = "cubit")]
mod bookmarks;
#[cfg(target_os = "cubit")]
mod abort_trace;
#[cfg(target_os = "cubit")]
mod stderr_capture;
#[cfg(target_os = "cubit")]
mod memory_report;
#[cfg(target_os = "cubit")]
mod security;
#[cfg(target_os = "cubit")]
mod cubit_desktop;
#[cfg(target_os = "cubit")]
mod native_check;
#[cfg(target_os = "cubit")]
mod tab_model;

use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::sync::Arc;
use std::time::{Duration, Instant};

use dpi::PhysicalSize;
use gleam::gl::{self, Gl};
use servo::{
    DeviceIntPoint, DeviceIntRect, DeviceIntSize, EventLoopWaker, LoadStatus, RenderingContext,
    RgbaImage, ServoBuilder, WebView, WebViewBuilder, WebViewDelegate,
};
#[cfg(target_os = "cubit")]
use servo::{
    Code, DevicePoint, InputEvent, Key, KeyState, KeyboardEvent, Location, Modifiers,
    MouseButton, MouseButtonAction, MouseButtonEvent, MouseMoveEvent, MouseLeftViewportEvent, NamedKey, WheelDelta,
    WheelEvent, WheelMode,
};
use url::Url;

const DEFAULT_PAGE: &str = "data:text/html,<body style='background:%23fff'>\
<h1 style='color:%23135'>Penny</h1><p>Powered by Servo. Built for CuBit.</p>\
<div style='width:200px;height:100px;background:%23e33'></div></body>";

/// A RenderingContext over SWGL: WebRender draws into SWGL's default
/// framebuffer, which lives in this process's memory.
struct SwglContext {
    swgl: swgl::Context,
    gl: Rc<dyn Gl>,
    size: Cell<PhysicalSize<u32>>,
    #[cfg(target_os = "cubit")]
    window: RefCell<Option<cubit_desktop::Window>>,
}

impl SwglContext {
    fn new(size: PhysicalSize<u32>) -> Self {
        let swgl = swgl::Context::create();
        swgl.make_current();
        // No buffer: SWGL allocates and owns the framebuffer memory.
        swgl.init_default_framebuffer(
            0,
            0,
            size.width as i32,
            size.height as i32,
            0,
            std::ptr::null_mut(),
        );
        SwglContext {
            swgl,
            gl: Rc::new(swgl),
            size: Cell::new(size),
            #[cfg(target_os = "cubit")]
            window: RefCell::new(None),
        }
    }
}

impl Drop for SwglContext {
    fn drop(&mut self) {
        self.swgl.destroy();
    }
}

impl RenderingContext for SwglContext {
    fn read_to_image(&self, rect: DeviceIntRect) -> Option<RgbaImage> {
        self.swgl.make_current();
        let gl = &self.gl;
        gl.bind_framebuffer(gl::FRAMEBUFFER, 0);
        let pixels = gl.read_pixels(
            rect.min.x,
            rect.min.y,
            rect.width(),
            rect.height(),
            gl::RGBA,
            gl::UNSIGNED_BYTE,
        );
        // GL rows run bottom-up.
        let (w, h) = (rect.width() as usize, rect.height() as usize);
        let mut flipped = vec![0u8; w * h * 4];
        for y in 0..h {
            let src = (h - 1 - y) * w * 4;
            flipped[y * w * 4..(y + 1) * w * 4].copy_from_slice(&pixels[src..src + w * 4]);
        }
        RgbaImage::from_raw(w as u32, h as u32, flipped)
    }

    fn size(&self) -> PhysicalSize<u32> {
        self.size.get()
    }

    fn resize(&self, size: PhysicalSize<u32>) {
        if size == self.size.get() {
            return;
        }
        self.swgl.make_current();
        self.size.set(size);
        self.swgl.init_default_framebuffer(
            0,
            0,
            size.width as i32,
            size.height as i32,
            0,
            std::ptr::null_mut(),
        );
    }

    fn present(&self) {
        self.swgl.make_current();
        // Borrow SWGL storage only during the synchronous native frame copy.
        // No temporary full-frame readback or RGBA/BGRA conversion is needed.
        #[cfg(target_os = "cubit")]
        if let Some(window) = self.window.borrow().as_ref() {
            let Some(frame) = window.prepare() else { return; };
            let size = self.size.get();
            let configured = window.viewport();
            if size.width != configured.width || size.height != configured.height {
                return; // Frame guard cancels; resize/repaint runs next turn.
            }
            let (pixels, width, height, stride) = self.swgl.get_color_buffer(0, true);
            if pixels.is_null() || width <= 0 || height <= 0 || stride <= 0 ||
                width as u32 != size.width || height as u32 != size.height ||
                (stride as u64) < u64::from(size.width) * 4 || stride % 4 != 0 {
                return; // The guard cancels the frame lease.
            }
            let length = stride as u64 * height as u64;
            if length > 16 * 1024 * 1024 ||
                (pixels as usize).checked_add(length as usize).is_none() {
                return;
            }
            // SAFETY: SWGL owns this default framebuffer and prepares pending
            // clears above. Its allocation covers stride*height. No SWGL call
            // occurs until this synchronous copy returns; the Ada lease owns
            // separate storage and does not retain the borrowed source pointer.
            unsafe { frame.present_bgra(pixels.cast(), length, size.width,
                                        size.height, stride as u32); }
        }
    }

    fn make_current(&self) -> Result<(), surfman::Error> {
        self.swgl.make_current();
        Ok(())
    }

    fn gleam_gl_api(&self) -> Rc<dyn Gl> {
        self.gl.clone()
    }

    fn glow_gl_api(&self) -> Arc<glow::Context> {
        // Only servoshell's GUI and offscreen contexts use glow; Servo's
        // painter uses gleam.
        unimplemented!("glow over SWGL")
    }
}

#[derive(Clone)]
struct Waker(std::thread::Thread);

impl EventLoopWaker for Waker {
    fn clone_box(&self) -> Box<dyn EventLoopWaker> {
        Box::new(self.clone())
    }

    // Wake the owning event-loop thread when Servo has work.
    fn wake(&self) { self.0.unpark(); }
}

#[derive(Default)]
struct Delegate {
    loaded: Cell<bool>,
    load_started: Cell<Option<Instant>>,
    load_marks: Cell<u32>,
    load_seconds: Cell<u32>,
    awaiting_document: Cell<bool>,
    frames: Cell<u32>,
    state_dirty: Cell<bool>,
    browser_check: bool,
    tls_check: bool,
    perf_check: bool,
    security_dirty: Cell<bool>,
    #[cfg(target_os = "cubit")]
    security_report: RefCell<String>,
    blank_history: Cell<bool>,
}

impl Delegate {
    fn begin_load(&self) {
        self.load_started.set(Some(Instant::now()));
        self.load_marks.set(1);
        self.load_seconds.set(0);
        self.awaiting_document.set(true);
        self.loaded.set(false);
        self.state_dirty.set(true);
    }
}

impl WebViewDelegate for Delegate {
    fn notify_load_status_changed(&self, webview: WebView, status: LoadStatus) {
        // The engine can repeat Started when committing a requested load.
        // Preserve its connection wait; explicit navigation resets below.
        if status == LoadStatus::Started && self.load_started.get().is_none() {
            self.begin_load();
        }
        // A cancelled previous document can finish after a reload was issued.
        // Wait for a start/parse event before accepting its replacement's end.
        if status == LoadStatus::Complete && self.awaiting_document.get() { return; }
        if status != LoadStatus::Complete { self.awaiting_document.set(false); }
        match status {
            LoadStatus::Started => {},
            LoadStatus::HeadParsed => self.load_marks.set(self.load_marks.get() | 2),
            LoadStatus::Complete => self.load_marks.set(self.load_marks.get() | 4),
        }
        if self.perf_check {
            // Some reloads omit Started. Never carry a completed navigation's
            // clock into the next document or invent its missing start time.
            let elapsed = self.load_started.get().map(|start| start.elapsed().as_millis().to_string())
                .unwrap_or_else(|| "unavailable".into());
            let host = webview.url().and_then(|url| url.host_str().map(str::to_owned))
                .unwrap_or_else(|| "local".into());
            // HeadParsed means HTML head parsing, not HTTP header arrival.
            say(&format!("PENNY-LOAD: stage={status:?} elapsed_ms={elapsed} host={host}"));
        }
        if let Some(start) = self.load_started.get() {
            self.load_seconds.set(start.elapsed().as_secs().min(0x00ff_ffff) as u32);
        }
        if status == LoadStatus::Complete { self.load_started.set(None); }
        self.loaded.set(status == LoadStatus::Complete);
        self.security_dirty.set(true);
        self.state_dirty.set(true);
        if self.browser_check && status == LoadStatus::Complete {
            say("CUBITSHELL-BROWSER: loaded");
            if let Some(url) = webview.url().filter(|url| url.host_str() == Some("10.0.2.2") && url.port() == Some(18470)) {
                say(&format!("CUBITSHELL-BROWSER: loaded-path {}", url.path()));
            }
        }
    }

    fn notify_new_frame_ready(&self, _webview: WebView) {
        // Paint once after event dispatch, never reenter a foreign frame lease
        // from a delegate notification. Multiple notifications coalesce.
        self.frames.set(self.frames.get().wrapping_add(1));
        // A frame notification after HTML head parsing, not proof of scanout.
        if self.load_marks.get() & 2 != 0 && self.load_marks.get() & 8 == 0 {
            self.load_marks.set(self.load_marks.get() | 8);
            self.state_dirty.set(true);
        }
    }

    fn notify_url_changed(&self, _webview: WebView, url: Url) {
        self.security_dirty.set(true);
        self.state_dirty.set(true);
        if self.browser_check && url.host_str() == Some("10.0.2.2") && url.port() == Some(18470) {
            say(&format!("CUBITSHELL-BROWSER: url {}", url.path()));
        }
    }

    fn notify_page_title_changed(&self, _webview: WebView, title: Option<String>) {
        self.state_dirty.set(true);
        if self.perf_check {
            if let Some(value) = title.as_ref().filter(|s| s.starts_with("CuBitBrowserPerf") && s.len() < 96 && s.is_ascii()) {
                say(&format!("CUBITSHELL-PERF: {value}"));
            }
        }
        if self.browser_check {
            if let Some(title) = title.filter(|s| s.starts_with("CuBitBrowser") && s.len() < 64 && s.is_ascii()) {
                say(&format!("CUBITSHELL-BROWSER: title {title}"));
            }
        }
    }

    fn notify_document_tls_changed(&self, _webview: WebView) {
        self.security_dirty.set(true);
        self.state_dirty.set(true);
    }

    fn notify_favicon_changed(&self, webview: WebView) {
        #[cfg(target_os = "cubit")]
        if let (Some(url), Some(icon)) = (webview.url(), webview.favicon()) { bookmarks::remember_icon(url.as_str(), &icon); }
        self.state_dirty.set(true);
    }

    fn notify_history_changed(&self, _webview: WebView, entries: Vec<Url>, _current: usize) {
        self.blank_history.set(entries.len() == 1 && entries[0].as_str() == "about:blank");
        self.state_dirty.set(true);
    }

    fn notify_traversal_complete(&self, _webview: WebView, _traversal_id: servo::TraversalId) {
        self.state_dirty.set(true);
        if self.browser_check { say("CUBITSHELL-BROWSER: history traversal complete"); }
    }
}

fn say(message: &str) {
    #[cfg(target_os = "cubit")]
    {
        unsafe extern "C" {
            fn cubit_debug_write(data: *const u8, len: usize);
        }
        let line = format!("{message}\n");
        unsafe { cubit_debug_write(line.as_ptr(), line.len()) };
    }
    #[cfg(not(target_os = "cubit"))]
    eprintln!("{message}");
}

/// The caller chain as raw return addresses (the binary is stripped;
/// symbolize them offline against the unstripped build).
fn return_addresses() -> String {
    use std::ffi::c_void;
    type Callback = extern "C" fn(*mut c_void, *mut c_void) -> i32;
    unsafe extern "C" {
        fn _Unwind_Backtrace(trace: Callback, data: *mut c_void) -> i32;
        fn _Unwind_GetIP(context: *mut c_void) -> usize;
    }
    extern "C" fn frame(context: *mut c_void, data: *mut c_void) -> i32 {
        let out = unsafe { &mut *(data as *mut Vec<usize>) };
        out.push(unsafe { _Unwind_GetIP(context) });
        if out.len() >= 40 { 5 } else { 0 } // _URC_END_OF_STACK stops the walk
    }
    let mut ips: Vec<usize> = Vec::new();
    unsafe { _Unwind_Backtrace(frame, &mut ips as *mut Vec<usize> as *mut c_void) };
    ips.iter().map(|ip| format!("{ip:#x}")).collect::<Vec<_>>().join(" ")
}

/// Servo's warnings and errors, through the diagnostics channel (the level
/// is raised with a /servo/log-level file containing "info" or "debug").
struct DiagnosticsLogger;

impl log::Log for DiagnosticsLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::max_level()
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            say(&format!(
                "servo {} {}: {}",
                record.level(),
                record.target(),
                record.args()
            ));
        }
    }

    fn flush(&self) {}
}

/// A debugging view of a frame over the diagnostics channel: 200x150
/// greyscale, one hex line per row (tests/servo/frame_from_log.py turns the
/// log back into an image).
fn dump_frame(index: usize, image: &RgbaImage) {
    let (w, h) = (200u32, 150u32);
    say(&format!("CUBITSHELL-FRAME {index} {w} {h}"));
    for y in 0..h {
        let mut line = String::with_capacity(w as usize * 2 + 16);
        line.push_str("CUBITSHELL-ROW ");
        for x in 0..w {
            let sx = x * image.width() / w;
            let sy = y * image.height() / h;
            let p = image.get_pixel(sx, sy).0;
            let grey = (p[0] as u32 * 30 + p[1] as u32 * 59 + p[2] as u32 * 11) / 100;
            line.push_str(&format!("{grey:02x}"));
        }
        say(&line);
    }
}

fn write_ppm(path: &str, image: &RgbaImage) -> std::io::Result<()> {
    let mut data = format!("P6\n{} {}\n255\n", image.width(), image.height()).into_bytes();
    for pixel in image.pixels() {
        data.extend_from_slice(&pixel.0[..3]);
    }
    std::fs::write(path, data)
}

#[cfg(target_os = "cubit")]
mod socket_check;

#[cfg(all(target_os = "cubit", feature = "media"))]
mod media_init;

mod stall_probe;

fn spin_servo(servo: &servo::Servo) {
    stall_probe::mark(2);
    #[cfg(all(target_os = "cubit", feature = "media"))]
    media_init::poll();
    stall_probe::mark(3);
    servo.spin_event_loop();
    stall_probe::mark(4);
}

fn main() {
    #[cfg(all(target_os = "cubit", feature = "media"))]
    let _audio = media_init::initialize();
    static LOGGER: DiagnosticsLogger = DiagnosticsLogger;
    let level = match std::fs::read_to_string("/servo/log-level").as_deref().map(str::trim) {
        Ok("debug") => log::LevelFilter::Debug,
        Ok("info") => log::LevelFilter::Info,
        _ => log::LevelFilter::Warn,
    };
    let _ = log::set_logger(&LOGGER).map(|()| log::set_max_level(level));

    // Panics must reach the test log: stderr is a stream no one reads here.
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let thread = std::thread::current();
        say(&format!(
            "CUBITSHELL: panic in thread {}: {info}",
            thread.name().unwrap_or("?")
        ));
        say(&format!("CUBITSHELL: return addresses {}", return_addresses()));
        default_hook(info);
    }));

    // Pages: the arguments (hosted), else /servo/pages on CuBit (one URL
    // per line, inside the program's filesystem scope), else a test page.
    // Hosted: [url] [out.ppm] writes the first page's frame.
    let mut args: Vec<String> = std::env::args().skip(1).collect();
    let output = if args.len() > 1 { args.pop() } else { None };
    let mut pages = args;
    if pages.is_empty() {
        if let Ok(list) = std::fs::read_to_string("/servo/pages") {
            pages = list
                .lines()
                .map(str::trim)
                .filter(|l| !l.is_empty() && !l.starts_with('#'))
                .map(String::from)
                .collect();
        }
    }
    if pages.is_empty() {
        pages.push(DEFAULT_PAGE.to_string());
    }

    #[cfg(target_os = "cubit")]
    if std::path::Path::new("/servo/browser-check").exists() {
        if pages.len() < 3 || !pages[0].starts_with("data:text/html,<body style='background:%23fff'>") {
            say("CUBITSHELL: FAIL stale browser page fixture");
            return;
        }
        match native_check::fonts() {
            Ok(count) => say(&format!("CUBITSHELL-BROWSER: fonts PASS count={count}")),
            Err(error) => {
                say(&format!("CUBITSHELL: FAIL font path/read-dir fixture: {error}"));
                return;
            },
        }
        if !native_check::aligned_gc_allocator() {
            say("CUBITSHELL: FAIL aligned GC allocator");
            return;
        }
        say("CUBITSHELL-BROWSER: aligned allocator PASS cycles=32");
    }

    #[cfg(target_os = "cubit")]
    if std::path::Path::new("/servo/sandbox-check").exists() {
        match native_check::filesystem_scopes() {
            Ok(()) => {
                say("CUBITSHELL-SANDBOX: PASS filesystem scopes");
                let result = cubit_desktop::config_scope_check();
                if result == 0 { say("CUBITSHELL-SANDBOX: PASS config scopes"); }
                else { say(&format!("CUBITSHELL-SANDBOX: FAIL config stage={result}")); }
            },
            Err(error) => say(&format!("CUBITSHELL-SANDBOX: FAIL {error}")),
        }
        return;
    }

    #[cfg(target_os = "cubit")]
    let window = cubit_desktop::Window::open();
    #[cfg(target_os = "cubit")]
    if std::path::Path::new("/servo/perf-check").exists() {
        assert!(native_check::memory_accounting(), "self-memory accounting mapping/release oracle");
        say("CUBITSHELL-MEMORY: PASS charge/release 2097152 bytes");
    }
    #[cfg(target_os = "cubit")]
    if let Ok(address) = std::fs::read_to_string("/servo/socket-check") {
        let started = Instant::now();
        let result = address.trim().parse().map_err(|e| format!("address: {e}"))
            .and_then(socket_check::run);
        match result {
            Ok(()) => say(&format!("CUBITSHELL-SOCKETS: PASS complete ms={} owned_bytes={}", started.elapsed().as_millis(), cubit_desktop::memory_owned())),
            Err(error) => say(&format!("CUBITSHELL-SOCKETS: FAIL {error}")),
        }
        drop(window);
        return;
    }
    #[cfg(target_os = "cubit")]
    if std::path::Path::new("/servo/browser-check").exists() {
        let Some(w) = &window else {
            say("CUBITSHELL: FAIL frame cancel window");
            return;
        };
        for _ in 0..2 {
            let Some(frame) = w.prepare() else {
                say("CUBITSHELL: FAIL frame cancel acquire");
                return;
            };
            // A second acquisition must fail without cancelling the first.
            if w.prepare().is_some() {
                say("CUBITSHELL: FAIL nested frame acquire");
                return;
            }
            drop(frame);
        }
        say("CUBITSHELL-BROWSER: frame cancel PASS cycles=2");
    }
    #[cfg(target_os = "cubit")]
    let size = match &window {
        Some(w) => { let v = w.viewport(); PhysicalSize::new(v.width, v.height) },
        None => PhysicalSize::new(800, 600),
    };
    #[cfg(not(target_os = "cubit"))]
    let size = PhysicalSize::new(800, 600);
    #[cfg(target_os = "cubit")]
    if std::path::Path::new("/servo/browser-check").exists() {
        say(&format!("CUBITSHELL-BROWSER: initial viewport {}x{}", size.width, size.height));
    }
    let context = Rc::new(SwglContext::new(size));
    #[cfg(target_os = "cubit")]
    {
        say(if window.is_some() {
            "CUBITSHELL: desktop window"
        } else {
            "CUBITSHELL: no desktop (headless)"
        });
        *context.window.borrow_mut() = window;
    }
    context.make_current().expect("SWGL context");

    // TLS trust: CuBit's system trust store (a DER bundle, read through the
    // filesystem scope; later a trust store service). Without it, Servo
    // falls back to its built-in Mozilla list. A hosts file in the page
    // directory can redirect names for tests (the certificate is still
    // checked against the name in the URL).
    let mut preferences = servo::Preferences::default();
    preferences.network_use_webpki_roots = true;
    preferences.dom_intersection_observer_enabled = true;
    let mut opts = servo::Opts::default();
    #[cfg(target_os = "cubit")]
    if std::path::Path::new("/servo/profile-check").exists() {
        // Explicit diagnostic fixture only; retain the existing writable scope.
        // Servo flushes this timing report during orderly shutdown.
        opts.time_profiling = Some(servo::OutputOptions::FileName(
            "/Bookmarks/penny-profile.tsv".into()));
        opts.debug.toggle_option(servo::DiagnosticsLoggingOption::ProfileScriptEvents, true);
    }
    if std::path::Path::new("/tls/roots.der").exists() {
        opts.certificate_path = Some("/tls/roots.der".into());
    }
    if std::path::Path::new("/servo/hosts").exists() {
        opts.host_file = Some("/servo/hosts".into());
    }
    let servo = ServoBuilder::default()
        .opts(opts)
        .preferences(preferences)
        .event_loop_waker(Box::new(Waker(std::thread::current())))
        .build();
    let delegate = Rc::new(Delegate {
        browser_check: std::path::Path::new("/servo/browser-check").exists(),
        perf_check: std::path::Path::new("/servo/perf-check").exists(),
        tls_check: std::path::Path::new("/servo/tls-check").exists(),
        ..Default::default()
    });
    let mut webview: Option<WebView> = None;
    let mut all_inked = true;

    for (index, page) in pages.iter().enumerate() {
        let Ok(url) = Url::parse(page) else {
            say(&format!("CUBITSHELL: page {index}: not a URL: {page}"));
            all_inked = false;
            continue;
        };
        delegate.begin_load();
        delegate.frames.set(0);
        match &webview {
            None => {
                webview = Some(
                    WebViewBuilder::new(&servo, context.clone())
                        .delegate(delegate.clone())
                        .url(url)
                        .build(),
                )
            },
            Some(view) => view.load(url),
        }
        let view = webview.as_ref().unwrap();
        #[cfg(target_os = "cubit")]
        synchronize_window(view, &context, &delegate);
        say(&format!("CUBITSHELL: loading page {index}"));

        // A normal desktop browser remains interactive during its first load.
        // The explicit native fixture flag preserves the historical batch
        // page/ink oracle before entering interactive navigation regression.
        #[cfg(target_os = "cubit")]
        if context.window.borrow().is_some() && !std::path::Path::new("/servo/batch-test").exists() {
            run_window(&servo, webview.take().expect("initial webview"), context, &delegate);
            return;
        }

        let start = Instant::now();
        let deadline = Duration::from_secs(60);
        while !(delegate.loaded.get() && delegate.frames.get() > 0) {
            spin_servo(&servo);
            if start.elapsed() > deadline {
                say(&format!("CUBITSHELL: page {index}: no frame before the deadline"));
                break;
            }
            std::thread::sleep(Duration::from_millis(5));
        }
        // Let the page settle: keep turning until no new frame has arrived
        // for half a second (layout after stylesheets, images and fonts),
        // at most five seconds.
        let settle = Instant::now();
        let mut last_frames = delegate.frames.get();
        let mut last_change = Instant::now();
        while settle.elapsed() < Duration::from_secs(5) &&
            last_change.elapsed() < Duration::from_millis(500)
        {
            spin_servo(&servo);
            std::thread::sleep(Duration::from_millis(5));
            if delegate.frames.get() != last_frames {
                last_frames = delegate.frames.get();
                last_change = Instant::now();
            }
        }
        #[cfg(target_os = "cubit")]
        synchronize_window(view, &context, &delegate);
        view.paint();
        context.present();

        let rect = DeviceIntRect::from_origin_and_size(
            DeviceIntPoint::zero(),
            DeviceIntSize::new(size.width as i32, size.height as i32),
        );
        let image = context.read_to_image(rect).expect("a frame");
        let inked = image
            .pixels()
            .filter(|p| p.0[0] < 250 || p.0[1] < 250 || p.0[2] < 250)
            .count();
        say(&format!(
            "CUBITSHELL: page {index} frame {}x{} inked={} frames={} ms={}",
            image.width(),
            image.height(),
            inked,
            delegate.frames.get(),
            start.elapsed().as_millis()
        ));
        if std::path::Path::new("/servo/dump-frames").exists() {
            dump_frame(index, &image);
        }
        if index == 0 {
            if let Some(path) = &output {
                write_ppm(path, &image).expect("write the frame");
            }
        }
        all_inked &= inked > 0 && delegate.loaded.get();
    }
    say(if all_inked {
        "CUBITSHELL: PASS"
    } else {
        "CUBITSHELL: FAIL (a page did not render)"
    });
    let webview = webview.expect("a webview");

    #[cfg(target_os = "cubit")]
    {
        let interactive = context.window.borrow().is_some();
        if interactive { run_window(&servo, webview, context, &delegate); }
    }
}

/// Configure the physical viewport and native chrome from current engine state.
/// Servo's viewport_details divides physical size by hidpi_scale_factor;
/// pointer events use WebViewPoint in device pixels, already mapped by Ada.
#[cfg(target_os = "cubit")]
fn synchronize_window(webview: &WebView, context: &SwglContext, delegate: &Delegate) {
    if let Some(window) = context.window.borrow().as_ref() {
        let v = window.viewport();
        if v.width > 0 && v.height > 0 && v.denominator > 0 {
            let size = PhysicalSize::new(v.width, v.height);
            if size != context.size() {
                webview.resize(size);
                if delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: viewport {}x{}", size.width, size.height)); }
            }
            let mut scale = webview.hidpi_scale_factor();
            scale.0 = v.numerator as f32 / v.denominator as f32;
            webview.set_hidpi_scale_factor(scale);
        }
        if let Some(start) = delegate.load_started.get() {
            let seconds = start.elapsed().as_secs().min(0x00ff_ffff) as u32;
            if delegate.load_seconds.replace(seconds) != seconds {
                delegate.state_dirty.set(true);
            }
        }
        if delegate.state_dirty.replace(false) {
            let mut report = delegate.security_report.borrow_mut();
            let changed = delegate.security_dirty.replace(false) || report.is_empty();
            if changed { *report = security::report(webview, webview.load_status() == LoadStatus::Started); }
            if changed && delegate.tls_check && webview.load_status() != LoadStatus::Started {
                say(&format!("PENNY-TLS: {}\n{}PENNY-TLS-END", webview.url().map_or(String::new(), |u|u.to_string()), report));
            }
            window.security(&report);
            window.state(webview.url().as_ref().map_or("", |url| url.as_str()),
                webview.page_title().as_deref().unwrap_or("Penny"),
                !delegate.loaded.get(), webview.can_go_back(), webview.can_go_forward(),
                delegate.load_marks.get(), delegate.load_seconds.get());
        }
    }
}

#[cfg(target_os = "cubit")]
#[derive(Clone)]
struct Tab { view: WebView, delegate: Rc<Delegate>, parking: u8, pristine: bool, title: String }

#[cfg(target_os = "cubit")]
fn trace_tabs(tabs: &tab_model::Tabs<Tab>, parked: &[(u64, Tab)], blank: &Option<Tab>, enabled: bool) {
    if !enabled { return; }
    let logical = tabs.len();
    let dedicated = tabs.values().filter(|tab| !tab.pristine).count() + parked.len();
    say(&format!("CUBITSHELL-TABS: logical={logical} frontend_views={}",
        dedicated + usize::from(blank.is_some())));
}

#[cfg(target_os = "cubit")]
struct BrowserWindow {
    context: Rc<SwglContext>,
    tabs: tab_model::Tabs<Tab>,
    parked: Vec<(u64, Tab)>,
    // Untouched empty tabs share this document. Navigation detaches the selected
    // tab before changing its URL; no real page ever enters this shared view.
    blank: Option<Tab>,
    pointer: DevicePoint,
    shown_frames: u32,
    painted_size: Option<PhysicalSize<u32>>,
    closed: bool,
    ready_reported: bool,
}

#[cfg(target_os = "cubit")]
fn park_tab(id: u64, mut tab: Tab, parked: &mut Vec<(u64, Tab)>) {
    tab.view.blur(); tab.view.hide();
    if tab.pristine {
        if tab.delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: tab parked {id}")); }
        return;
    }
    // Reserve the single reuse slot before starting a replacement document.
    // Excess views retire after the input loop, outside Servo's borrows; making
    // each of them load a blank page first creates a large close-time spike.
    if parked.iter().any(|(_, entry)| entry.parking != 4) {
        tab.parking = 4;
    } else {
        tab.parking = 1;
        tab.delegate.loaded.set(false);
        tab.delegate.blank_history.set(false);
        tab.view.load(Url::parse("about:blank").unwrap());
    }
    parked.push((id, tab));
}

#[cfg(target_os = "cubit")]
fn park_window(tabs: &mut tab_model::Tabs<Tab>, parked: &mut Vec<(u64, Tab)>) {
    for (id, tab) in tabs.drain() { park_tab(id, tab, parked); }
}

#[cfg(target_os = "cubit")]
impl BrowserWindow {
    fn new(context: Rc<SwglContext>, view: WebView, delegate: Rc<Delegate>) -> Self {
        let mut tabs = tab_model::Tabs::default();
        view.focus();
        assert!(tabs.insert(Tab { view, delegate, parking: 0, pristine: false, title: String::new() }).is_ok());
        Self { context, tabs, parked: Vec::new(), blank: None, pointer: DevicePoint::new(0.0, 0.0),
            shown_frames: u32::MAX, painted_size: None, closed: false, ready_reported: false }
    }

    fn ready_to_retire(&self) -> bool {
        self.closed && self.parked.iter().all(|(_, tab)| tab.parking == 3)
    }

    fn step(&mut self, servo: &servo::Servo) -> (bool, bool) {
        use cubit_desktop::Input;
        let context = &self.context;
        let tabs = &mut self.tabs;
        let blank = &mut self.blank;
        let parked = &mut self.parked;
        let mut active = tabs.active().unwrap_or(0);
        let mut pointer = self.pointer;
        let mut shown_frames = self.shown_frames;
        let mut request_window = false;
        let mut idle = true;
        stall_probe::mark(1);
        if let Some(window) = context.window.borrow_mut().as_mut() { window.begin_input(); }
        // The Ada bridge bounds each batch to 32 polls and time-limits fresh
        // input fetches. Cached events can drain within that cap; individual
        // event handlers and painting may still block.
        loop {
            // Release this borrow before engine calls can deliver callbacks.
            let input = context.window.borrow_mut().as_mut().and_then(|w| w.poll());
            let Some(input) = input else { break; };
            idle = false;
            let webview = &tabs.get(active).expect("active tab").view.clone();
            let delegate = tabs.get(active).unwrap().delegate.clone();
            // Desktop routes input by surface; Servo maintains one keyboard
            // focus across all WebViews. A native title-bar raise has no page
            // event, so restore engine focus before forwarding its next input.
            // Passive motion/configuration must not steal another window's focus.
            if matches!(&input, Input::Key { .. } | Input::Text(_) |
                Input::Button { down: true, .. } | Input::Wheel { .. } |
                Input::Navigate(_) | Input::Back | Input::Forward | Input::Reload)
                && !webview.focused() {
                webview.focus();
            }
            let event = match input {
                Input::NewWindow => { request_window = true; continue; },
                Input::NewTab => {
                    context.swgl.make_current();
                    let tab = if let Some(index) = parked.iter().position(|(_, tab)| tab.parking == 3) {
                        let mut tab = parked.swap_remove(index).1; tab.parking = 0; tab
                    } else {
                        blank.get_or_insert_with(|| {
                            let d = Rc::new(Delegate { browser_check: delegate.browser_check, tls_check: delegate.tls_check, perf_check: delegate.perf_check, ..Default::default() });
                            let view = WebViewBuilder::new(servo, context.clone()).delegate(d.clone())
                                .url(Url::parse("about:blank").unwrap()).build();
                            Tab { view, delegate: d, parking: 0, pristine: true, title: String::new() }
                        }).clone()
                    };
                    let Ok(index) = tabs.insert(tab) else { continue; };
                    webview.blur(); webview.hide(); active = index;
                    let tab = tabs.get(active).unwrap();
                    tab.delegate.state_dirty.set(true);
                    tab.view.show(); tab.view.focus(); shown_frames = u32::MAX;
                    if delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: tab new {index}")); }
                    trace_tabs(tabs, parked, blank, delegate.perf_check);
                    continue;
                },
                Input::SelectTab(_) | Input::CycleTab(_) => {
                    let index = match input {
                        Input::SelectTab(id) if tabs.select(id) => id,
                        Input::CycleTab(backward) => tabs.cycle(backward).unwrap(),
                        _ => continue,
                    };
                    webview.blur(); webview.hide(); active = index;
                    let tab = tabs.get(active).unwrap();
                    tab.view.show(); tab.view.focus(); tab.delegate.state_dirty.set(true);
                    shown_frames = u32::MAX;
                    if delegate.browser_check {
                        say(&format!("CUBITSHELL-BROWSER: tab select {index}"));
                        say(&format!("CUBITSHELL-BROWSER: tab state {index} url={}",
                            tab.view.url().as_ref().map_or("", |url| url.as_str())));
                    }
                    continue;
                },
                Input::CloseTab(index) => {
                    let Some(tab) = tabs.remove(index) else { continue; };
                    if delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: tab close {index}")); }
                    park_tab(index, tab, parked);
                    let Some(next) = tabs.active() else {
                        context.window.borrow_mut().take(); self.closed = true;
                        if delegate.browser_check { say("CUBITSHELL-BROWSER: window closed"); }
                        break;
                    };
                    active = next;
                    let tab = tabs.get(active).unwrap();
                    tab.view.show(); tab.view.focus(); tab.delegate.state_dirty.set(true);
                    shown_frames = u32::MAX;
                    continue;
                },
                Input::Navigate(text) => {
                    let text = text.trim();
                    let candidate = if text.contains("://") || text.starts_with("about:") || text.starts_with("data:") {
                        text.to_owned()
                    } else { format!("https://{text}") };
                    match Url::parse(&candidate) {
                        Ok(url) if matches!(url.scheme(), "http" | "https" | "about" | "data") => {
                            if delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: submitted {url}")); }
                            if tabs.get(active).unwrap().pristine {
                                webview.blur(); webview.hide();
                                let d = Rc::new(Delegate { browser_check: delegate.browser_check, tls_check: delegate.tls_check, perf_check: delegate.perf_check, ..Default::default() });
                                d.begin_load();
                                context.swgl.make_current();
                                let view = WebViewBuilder::new(servo, context.clone()).delegate(d.clone())
                                    .url(url).build();
                                view.show(); view.focus();
                                *tabs.get_mut(active).unwrap() = Tab { view, delegate: d, parking: 0, pristine: false, title: String::new() };
                                shown_frames = u32::MAX;
                            } else {
                                tabs.get(active).unwrap().delegate.begin_load();
                                webview.load(url);
                            }
                            let current = &tabs.get(active).unwrap().delegate;
                            current.loaded.set(false);
                            current.state_dirty.set(true);
                            trace_tabs(tabs, parked, blank, delegate.perf_check);
                            say("CUBITSHELL: navigate");
                        },
                        _ => {
                            if let Some(window) = context.window.borrow().as_ref() { window.navigation_error(); }
                            say("CUBITSHELL: invalid navigation URL");
                        },
                    }
                    continue;
                },
                Input::Back => {
                    if !tabs.get(active).unwrap().pristine { tabs.get(active).unwrap().delegate.begin_load(); webview.go_back(1); }
                    say("CUBITSHELL: back"); continue;
                },
                Input::Forward => {
                    if !tabs.get(active).unwrap().pristine { tabs.get(active).unwrap().delegate.begin_load(); webview.go_forward(1); }
                    say("CUBITSHELL: forward"); continue;
                },
                Input::Reload => {
                    if !tabs.get(active).unwrap().pristine { tabs.get(active).unwrap().delegate.begin_load(); webview.reload(); }
                    say("CUBITSHELL: reload"); continue;
                },
                Input::Configure { settings_opened } => {
                    if settings_opened && delegate.browser_check {
                        say("CUBITSHELL-BROWSER: settings opened");
                    }
                    // Resynchronization can also mean that a button edge was
                    // lost. Reset engine state even if our local mask is empty.
                    webview.notify_input_event(InputEvent::MouseCancel);
                    webview.notify_input_event(InputEvent::MouseLeftViewport(MouseLeftViewportEvent::default()));
                    synchronize_window(webview, context, &delegate);
                    continue;
                },
                Input::Consumed => continue,
                Input::Close => {
                    park_window(tabs, parked);
                    context.window.borrow_mut().take(); self.closed = true;
                        if delegate.browser_check { say("CUBITSHELL-BROWSER: window closed"); }
                        break;
                },
                Input::Leave => InputEvent::MouseLeftViewport(MouseLeftViewportEvent::default()),
                Input::Move { x, y } => {
                    pointer = DevicePoint::new(x as f32, y as f32);
                    InputEvent::MouseMove(MouseMoveEvent::new(pointer.into()))
                },
                Input::Button { down, changed, x, y } => {
                    pointer = DevicePoint::new(x as f32, y as f32);
                    for (mask, button) in [(1, MouseButton::Primary), (2, MouseButton::Secondary), (4, MouseButton::Auxiliary)] {
                        if changed & mask == 0 { continue; }
                        webview.notify_input_event(InputEvent::MouseButton(MouseButtonEvent::new(
                            if down { MouseButtonAction::Down } else { MouseButtonAction::Up },
                            button, DevicePoint::new(x as f32, y as f32).into())));
                    }
                    continue;
                },
                Input::Wheel { delta, x, y } => InputEvent::Wheel(WheelEvent::new(
                    // Match servoshell's discrete-wheel conversion. The
                    // current renderer consumes device-pixel scroll deltas;
                    // merely marking a small value DeltaLine does not scale it.
                    WheelDelta { x: 0.0, y: delta as f64 * 76.0, z: 0.0, mode: WheelMode::DeltaPixel },
                    DevicePoint::new(x as f32, y as f32).into())),
                Input::Text(ch) => {
                    for state in [KeyState::Down, KeyState::Up] {
                        webview.notify_input_event(InputEvent::Keyboard(KeyboardEvent::new_without_event(
                            state, Key::Character(ch.to_string()), Code::Unidentified,
                            Location::Standard, Modifiers::empty(), false, false)));
                    }
                    continue;
                },
                Input::Key { down, scancode, modifiers } => {
                    let key = match scancode {
                        0x3c => NamedKey::F2, 0x01 => NamedKey::Escape, 0x0e => NamedKey::Backspace,
                        0x0f => NamedKey::Tab, 0x1c => NamedKey::Enter,
                        0x47 => NamedKey::Home, 0x48 => NamedKey::ArrowUp,
                        0x49 => NamedKey::PageUp, 0x4b => NamedKey::ArrowLeft,
                        0x4d => NamedKey::ArrowRight, 0x4f => NamedKey::End,
                        0x50 => NamedKey::ArrowDown, 0x51 => NamedKey::PageDown,
                        0x53 => NamedKey::Delete,
                        _ => continue,
                    };
                    let mut mods = Modifiers::empty();
                    if modifiers & 1 != 0 { mods |= Modifiers::SHIFT; }
                    if modifiers & 2 != 0 { mods |= Modifiers::CONTROL; }
                    if modifiers & 4 != 0 { mods |= Modifiers::ALT; }
                    InputEvent::Keyboard(KeyboardEvent::new_without_event(
                        if down { KeyState::Down } else { KeyState::Up },
                        Key::Named(key), Code::Unidentified, Location::Standard, mods, false, false))
                },
            };
            webview.notify_input_event(event);
        }
        spin_servo(&servo);
        // Clear history only after blank has
        // actually loaded, then require its history notification before reuse.
        // This is not a claim that every old pipeline/resource has retired.
        for (index, tab) in parked.iter_mut() {
            if tab.parking == 1 && tab.delegate.loaded.get() &&
                tab.view.url().is_some_and(|url| url.as_str() == "about:blank") {
                tab.delegate.blank_history.set(false);
                tab.view.clear_session_history(); tab.parking = 2;
            } else if tab.parking == 2 && tab.delegate.blank_history.get() {
                tab.parking = 3;
                if tab.delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: tab parked {index}")); }
            }
        }
        // Drop excess views only after the input loop and event dispatch have
        // released their borrows. CloseWebView retires pipelines asynchronously.
        if parked.iter().any(|(_, tab)| tab.parking == 4) {
            context.swgl.make_current();
            parked.retain(|(id, tab)| {
                if tab.parking != 4 { return true; }
                if tab.delegate.perf_check {
                    say(&format!("CUBITSHELL-TABS: retire-request id={id}"));
                }
                false
            });
        }
        if self.closed { return (idle, request_window); }
        stall_probe::mark(5);
        if let Some(window) = context.window.borrow().as_ref() {
            let capacity = window.tab_capacity();
            for id in tabs.projection(capacity.max(1)) {
                let tab = tabs.get_mut(id).unwrap();
                tab.title = tab.view.page_title().unwrap_or_else(|| "New tab".to_owned());
            }
            window.tabs(&tabs.snapshot(capacity, |tab| &tab.title));
        }
        let tab = tabs.get(active).expect("active tab");
        let webview = &tab.view;
        let delegate = &tab.delegate;
        stall_probe::mark(6);
        synchronize_window(webview, context, delegate);
        let frames = delegate.frames.get();
        let pending = context.window.borrow().as_ref().is_some_and(|w| w.pending());
        let render = frames != shown_frames || self.painted_size != Some(context.size());
        if render || pending {
            context.swgl.make_current();
            let started = Instant::now();
            if render {
                stall_probe::mark(7);
                webview.paint();
                shown_frames = frames;
                self.painted_size = Some(context.size());
            }
            let paint_ms = started.elapsed().as_millis();
            // Chrome-only changes reuse SWGL's current page pixels. Resizes,
            // tab switches and new engine frames still render before copying.
            stall_probe::mark(8);
            context.present();
            if delegate.perf_check {
                say(&format!("PENNY-FRAME: render={render} paint_ms={paint_ms} total_ms={}",
                    started.elapsed().as_millis()));
            }
        }
        if !self.ready_reported && delegate.loaded.get() {
            self.ready_reported = true;
            if delegate.browser_check { say("CUBITSHELL-BROWSER: window ready"); }
        }
        self.pointer = pointer;
        self.shown_frames = shown_frames;
        (idle, request_window)
    }
}

#[cfg(target_os = "cubit")]
fn run_window(servo: &servo::Servo, webview: WebView, context: Rc<SwglContext>, delegate: &Rc<Delegate>) {
    let _stall_probe = stall_probe::Probe::start();
    let mut memory_reports = memory_report::Probe::new();
    if delegate.browser_check { eprintln!("PENNY-DIAGNOSTIC: stderr capture ready"); }
    let mut windows = vec![BrowserWindow::new(context, webview, delegate.clone())];
    let measurement_start = Instant::now();
    let mut next_measurement = Duration::ZERO;
    let mut peak_owned = 0;
    loop {
        memory_reports.poll(servo);
        if delegate.perf_check && measurement_start.elapsed() >= next_measurement {
            let owned = cubit_desktop::memory_owned();
            if owned == u64::MAX {
                say("CUBITSHELL-MEMORY: unavailable (kernel lacks self-memory query)");
            } else {
                peak_owned = peak_owned.max(owned);
                say(&format!("CUBITSHELL-MEMORY: ms={} owned_bytes={owned} sampled_peak_bytes={peak_owned} windows={}", measurement_start.elapsed().as_millis(), windows.iter().filter(|w| !w.closed).count()));
            }
            for window in windows.iter().filter(|w| !w.closed) {
                if let Some(native) = window.context.window.borrow().as_ref() {
                    let (id, stats) = native.input_statistics();
                    say(&format!("CUBITSHELL-INPUT: window={id} batch={} disabled={} fetches={} fetched={} delivered={} fallback={} rejected={}",
                        stats.batch_enabled, stats.channel_disabled, stats.successful_fetches,
                        stats.fetched_events, stats.delivered_events, stats.fallback_polls, stats.cache_rejections));
                }
            }
            next_measurement = measurement_start.elapsed() + Duration::from_secs(5);
        }
        let mut idle = true;
        let mut requests = Vec::new();
        for (index, window) in windows.iter_mut().enumerate() {
            let (window_idle, requested) = window.step(servo);
            idle &= window_idle;
            if requested && !window.closed { requests.push(index); }
        }
        if windows.iter().all(|window| window.closed) {
            say("CUBITSHELL: closed");
            return;
        }
        for origin in requests {
            let admitted = windows.iter().filter(|w| !w.ready_to_retire()).count() < cubit_desktop::MAX_WINDOWS;
            let native = if admitted { cubit_desktop::Window::open() } else { None };
            if let Some(native) = native {
                let viewport = native.viewport();
                let context = Rc::new(SwglContext::new(PhysicalSize::new(viewport.width, viewport.height)));
                *context.window.borrow_mut() = Some(native);
                let d = Rc::new(Delegate { browser_check: delegate.browser_check, tls_check: delegate.tls_check, perf_check: delegate.perf_check, ..Default::default() });
                let view = WebViewBuilder::new(servo, context.clone()).delegate(d.clone())
                    .url(Url::parse("about:blank").unwrap()).build();
                windows.push(BrowserWindow::new(context, view, d));
                if delegate.browser_check { say("CUBITSHELL-BROWSER: window opened"); }
            } else if let Some(window) = windows[origin].context.window.borrow().as_ref() {
                window.window_error();
                if delegate.browser_check { say("CUBITSHELL-BROWSER: window limit"); }
            }
        }
        // Finish request handling before removing entries: origin indices still
        // refer to this iteration's window list. Closed windows keep their
        // contexts only while their asynchronous blank/history work is pending.
        windows.retain_mut(|window| {
            if !window.ready_to_retire() { return true; }
            window.context.swgl.make_current();
            window.parked.clear();
            window.blank.take();
            if delegate.perf_check { say("CUBITSHELL-WINDOWS: retire-request"); }
            false
        });
        stall_probe::mark(9);
        if idle { std::thread::park_timeout(Duration::from_millis(1)); }
    }
}
