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
mod cubit_desktop;
#[cfg(target_os = "cubit")]
mod native_check;

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
        // SWGL fallback still requires readback. The native bridge converts
        // directly into its protected frame; no intermediate BGRA image.
        #[cfg(target_os = "cubit")]
        if let Some(window) = self.window.borrow().as_ref() {
            let Some(frame) = window.prepare() else { return; };
            let size = self.size.get();
            let configured = window.viewport();
            if size.width != configured.width || size.height != configured.height {
                return; // Frame guard cancels; resize/repaint runs next turn.
            }
            self.gl.bind_framebuffer(gl::FRAMEBUFFER, 0);
            let pixels = self.gl.read_pixels(0, 0, size.width as i32,
                size.height as i32, gl::RGBA, gl::UNSIGNED_BYTE);
            frame.present(&pixels, size.width, size.height);
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
    frames: Cell<u32>,
    state_dirty: Cell<bool>,
    browser_check: bool,
    blank_history: Cell<bool>,
}

impl WebViewDelegate for Delegate {
    fn notify_load_status_changed(&self, webview: WebView, status: LoadStatus) {
        self.loaded.set(status == LoadStatus::Complete);
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
    }

    fn notify_url_changed(&self, _webview: WebView, url: Url) {
        self.state_dirty.set(true);
        if self.browser_check && url.host_str() == Some("10.0.2.2") && url.port() == Some(18470) {
            say(&format!("CUBITSHELL-BROWSER: url {}", url.path()));
        }
    }

    fn notify_page_title_changed(&self, _webview: WebView, title: Option<String>) {
        self.state_dirty.set(true);
        if self.browser_check {
            if let Some(title) = title.filter(|s| s.starts_with("CuBitBrowser") && s.len() < 64 && s.is_ascii()) {
                say(&format!("CUBITSHELL-BROWSER: title {title}"));
            }
        }
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

fn main() {
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
    let window = cubit_desktop::Window::open();
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
    let mut opts = servo::Opts::default();
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
        delegate.loaded.set(false);
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
            run_window(&servo, view, &context, &delegate);
            return;
        }

        let start = Instant::now();
        let deadline = Duration::from_secs(60);
        while !(delegate.loaded.get() && delegate.frames.get() > 0) {
            servo.spin_event_loop();
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
            servo.spin_event_loop();
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
    if context.window.borrow().is_some() {
        run_window(&servo, &webview, &context, &delegate);
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
        if delegate.state_dirty.replace(false) {
            window.state(webview.url().as_ref().map_or("", |url| url.as_str()),
                webview.page_title().as_deref().unwrap_or("Penny"),
                !delegate.loaded.get(), webview.can_go_back(), webview.can_go_forward());
        }
    }
}

#[cfg(target_os = "cubit")]
struct Tab { view: WebView, delegate: Rc<Delegate>, parking: u8 }

#[cfg(target_os = "cubit")]
struct BrowserWindow {
    context: Rc<SwglContext>,
    tabs: [Option<Tab>; cubit_desktop::MAX_TABS],
    active: usize,
    pointer: DevicePoint,
    shown_frames: u32,
    closed: bool,
    ready_reported: bool,
}

#[cfg(target_os = "cubit")]
fn park_window(tabs: &mut [Option<Tab>; cubit_desktop::MAX_TABS]) {
    for tab in tabs.iter_mut().flatten().filter(|tab| tab.parking == 0) {
        tab.view.blur(); tab.view.hide(); tab.parking = 1;
        tab.delegate.loaded.set(false); tab.delegate.blank_history.set(false);
        tab.view.load(Url::parse("about:blank").unwrap());
    }
}

#[cfg(target_os = "cubit")]
impl BrowserWindow {
    fn new(context: Rc<SwglContext>, view: WebView, delegate: Rc<Delegate>) -> Self {
        let mut tabs = std::array::from_fn(|_| None);
        view.focus();
        tabs[0] = Some(Tab { view, delegate, parking: 0 });
        Self { context, tabs, active: 0, pointer: DevicePoint::new(0.0, 0.0),
            shown_frames: u32::MAX, closed: false, ready_reported: false }
    }

    fn ready_to_reopen(&self) -> bool {
        self.closed && self.tabs.iter().flatten().all(|tab| tab.parking == 3)
    }

    fn reopen(&mut self, window: cubit_desktop::Window) {
        assert!(self.ready_to_reopen());
        *self.context.window.borrow_mut() = Some(window);
        self.active = 0;
        self.pointer = DevicePoint::new(0.0, 0.0);
        self.shown_frames = u32::MAX;
        self.closed = false;
        self.ready_reported = false;
        let tab = self.tabs[0].as_mut().unwrap();
        tab.parking = 0;
        tab.delegate.state_dirty.set(true);
        tab.view.show(); tab.view.focus();
    }

    fn step(&mut self, servo: &servo::Servo) -> (bool, bool) {
        use cubit_desktop::Input;
        let context = &self.context;
        let tabs = &mut self.tabs;
        let mut active = self.active;
        let mut pointer = self.pointer;
        let mut shown_frames = self.shown_frames;
        let mut request_window = false;
        let mut idle = true;
        if let Some(window) = context.window.borrow_mut().as_mut() { window.begin_input(); }
        // The Ada bridge enforces the existing proven Client_Input_Budget.
        // Return to Servo and presentation after each bounded input batch.
        loop {
            // Release this borrow before engine calls can deliver callbacks.
            let input = context.window.borrow_mut().as_mut().and_then(|w| w.poll());
            let Some(input) = input else { break; };
            idle = false;
            let webview = &tabs[active].as_ref().expect("active tab").view.clone();
            let delegate = tabs[active].as_ref().unwrap().delegate.clone();
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
                Input::NewTab(index) => {
                    context.swgl.make_current();
                    webview.blur(); webview.hide();
                    active = index - 1;
                    if tabs[active].is_none() {
                        let d = Rc::new(Delegate { browser_check: delegate.browser_check, ..Default::default() });
                        let view = WebViewBuilder::new(servo, context.clone()).delegate(d.clone())
                            .url(Url::parse("about:blank").unwrap()).build();
                        tabs[active] = Some(Tab { view, delegate: d, parking: 0 });
                    }
                    let tab = tabs[active].as_mut().unwrap();
                    assert!(tab.parking == 0 || tab.parking == 3, "reuse before parking acknowledgment");
                    tab.parking = 0;
                    tab.delegate.state_dirty.set(true);
                    tab.view.show(); tab.view.focus(); shown_frames = u32::MAX;
                    if delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: tab new {index}")); }
                    continue;
                },
                Input::SelectTab(index) => {
                    if tabs[index - 1].as_ref().is_some_and(|tab| tab.parking == 0) {
                        webview.blur(); webview.hide(); active = index - 1;
                        let tab = tabs[active].as_ref().unwrap();
                        tab.view.show(); tab.view.focus(); tab.delegate.state_dirty.set(true);
                        shown_frames = u32::MAX;
                        if delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: tab select {index}")); }
                    }
                    continue;
                },
                Input::CloseTab { index, next } => {
                    let tab = tabs[index - 1].as_mut().expect("close live tab");
                    tab.view.blur(); tab.view.hide(); tab.parking = 1;
                    tab.delegate.loaded.set(false); tab.delegate.blank_history.set(false);
                    tab.view.load(Url::parse("about:blank").unwrap());
                    if delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: tab close {index}")); }
                    if next == 0 {
                        park_window(tabs);
                        context.window.borrow_mut().take(); self.closed = true;
                        if delegate.browser_check { say("CUBITSHELL-BROWSER: window closed"); }
                        break;
                    }
                    active = next - 1;
                    let tab = tabs[active].as_ref().expect("selected tab");
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
                            webview.load(url);
                            delegate.loaded.set(false);
                            delegate.state_dirty.set(true);
                            say("CUBITSHELL: navigate");
                        },
                        _ => {
                            if let Some(window) = context.window.borrow().as_ref() { window.navigation_error(); }
                            say("CUBITSHELL: invalid navigation URL");
                        },
                    }
                    continue;
                },
                Input::Back => { webview.go_back(1); say("CUBITSHELL: back"); continue; },
                Input::Forward => { webview.go_forward(1); say("CUBITSHELL: forward"); continue; },
                Input::Reload => { webview.reload(); say("CUBITSHELL: reload"); continue; },
                Input::Configure { released, settings_opened } => {
                    if settings_opened && delegate.browser_check {
                        say("CUBITSHELL-BROWSER: settings opened");
                    }
                    for (mask, button) in [(1, MouseButton::Primary), (2, MouseButton::Secondary), (4, MouseButton::Auxiliary)] {
                        if released & mask != 0 {
                            webview.notify_input_event(InputEvent::MouseButton(MouseButtonEvent::new(
                                MouseButtonAction::Up, button, pointer.into())));
                        }
                    }
                    webview.notify_input_event(InputEvent::MouseLeftViewport(MouseLeftViewportEvent::default()));
                    synchronize_window(webview, context, &delegate);
                    continue;
                },
                Input::Consumed => continue,
                Input::Close => {
                    park_window(tabs);
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
        servo.spin_event_loop();
        // Retain the resident containers. Clear history only after blank has
        // actually loaded, then require its history notification before reuse.
        // This is not a claim that every old pipeline/resource has retired.
        for (index, entry) in tabs.iter_mut().enumerate() {
            if let Some(tab) = entry {
                if tab.parking == 1 && tab.delegate.loaded.get() &&
                    tab.view.url().is_some_and(|url| url.as_str() == "about:blank") {
                    tab.delegate.blank_history.set(false);
                    tab.view.clear_session_history(); tab.parking = 2;
                } else if tab.parking == 2 && tab.delegate.blank_history.get() {
                    tab.parking = 3;
                    if let Some(window) = context.window.borrow().as_ref() { window.tab_parked(index + 1); }
                    if tab.delegate.browser_check { say(&format!("CUBITSHELL-BROWSER: tab parked {}", index + 1)); }
                }
                if tab.parking == 0 {
                    if let Some(window) = context.window.borrow().as_ref() {
                        window.tab_title(index + 1, tab.view.page_title().as_deref().unwrap_or("New tab"));
                    }
                }
            }
        }
        if self.closed { return (idle, request_window); }
        let tab = tabs[active].as_ref().expect("active tab");
        let webview = &tab.view;
        let delegate = &tab.delegate;
        synchronize_window(webview, context, delegate);
        let frames = delegate.frames.get();
        let pending = context.window.borrow().as_ref().is_some_and(|w| w.pending());
        if frames != shown_frames || pending {
            shown_frames = frames;
            context.swgl.make_current();
            webview.paint();
            context.present();
        }
        if !self.ready_reported && delegate.loaded.get() {
            self.ready_reported = true;
            if delegate.browser_check { say("CUBITSHELL-BROWSER: window ready"); }
        }
        self.active = active;
        self.pointer = pointer;
        self.shown_frames = shown_frames;
        (idle, request_window)
    }
}

#[cfg(target_os = "cubit")]
fn run_window(servo: &servo::Servo, webview: &WebView, context: &Rc<SwglContext>, delegate: &Rc<Delegate>) {
    let mut windows = vec![BrowserWindow::new(context.clone(), webview.clone(), delegate.clone())];
    loop {
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
            let reusable = windows.iter().position(BrowserWindow::ready_to_reopen);
            let admitted = reusable.is_some() || windows.len() < cubit_desktop::MAX_WINDOWS;
            let native = if admitted { cubit_desktop::Window::open() } else { None };
            if let Some(native) = native {
                if let Some(index) = reusable {
                    windows[index].reopen(native);
                } else {
                    let viewport = native.viewport();
                    let context = Rc::new(SwglContext::new(PhysicalSize::new(viewport.width, viewport.height)));
                    *context.window.borrow_mut() = Some(native);
                    let d = Rc::new(Delegate { browser_check: delegate.browser_check, ..Default::default() });
                    let view = WebViewBuilder::new(servo, context.clone()).delegate(d.clone())
                        .url(Url::parse("about:blank").unwrap()).build();
                    windows.push(BrowserWindow::new(context, view, d));
                }
                if delegate.browser_check { say("CUBITSHELL-BROWSER: window opened"); }
            } else if let Some(window) = windows[origin].context.window.borrow().as_ref() {
                window.window_error();
                if delegate.browser_check { say("CUBITSHELL-BROWSER: window limit"); }
            }
        }
        if idle { std::thread::park_timeout(Duration::from_millis(1)); }
    }
}
