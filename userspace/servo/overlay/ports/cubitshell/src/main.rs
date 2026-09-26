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
mod cubit_desktop;

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
    MouseButton, MouseButtonAction, MouseButtonEvent, MouseMoveEvent, NamedKey, WheelDelta,
    WheelEvent, WheelMode,
};
use url::Url;

const DEFAULT_PAGE: &str = "data:text/html,<body style='background:%23fff'>\
<h1 style='color:%23135'>Servo on CuBit</h1>\
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
        // The frame is complete in SWGL's framebuffer. On CuBit, copy it
        // (BGRA, rows top first) into the window's lent buffer.
        #[cfg(target_os = "cubit")]
        if let Some(window) = self.window.borrow().as_ref() {
            let (w, h) = (window.width() as usize, window.height() as usize);
            let gl = &self.gl;
            gl.bind_framebuffer(gl::FRAMEBUFFER, 0);
            // RGBA (what read_to_image uses), bottom-up; the window wants
            // BGRA rows top first.
            let pixels = gl.read_pixels(0, 0, w as i32, h as i32, gl::RGBA, gl::UNSIGNED_BYTE);
            if pixels.len() >= w * h * 4 {
                let mut frame = vec![0u8; w * h * 4];
                for y in 0..h {
                    let src = &pixels[(h - 1 - y) * w * 4..(h - y) * w * 4];
                    let dst = &mut frame[y * w * 4..(y + 1) * w * 4];
                    for (d, p) in dst.chunks_exact_mut(4).zip(src.chunks_exact(4)) {
                        d[0] = p[2];
                        d[1] = p[1];
                        d[2] = p[0];
                        d[3] = 255;
                    }
                }
                window.present(&frame);
            }
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
struct Waker;

impl EventLoopWaker for Waker {
    fn clone_box(&self) -> Box<dyn EventLoopWaker> {
        Box::new(self.clone())
    }

    // The event loop below polls; nothing to wake.
    fn wake(&self) {}
}

#[derive(Default)]
struct Delegate {
    loaded: Cell<bool>,
    frames: Cell<u32>,
}

impl WebViewDelegate for Delegate {
    fn notify_load_status_changed(&self, _webview: WebView, status: LoadStatus) {
        if status == LoadStatus::Complete {
            self.loaded.set(true);
        }
    }

    fn notify_new_frame_ready(&self, webview: WebView) {
        webview.paint();
        self.frames.set(self.frames.get() + 1);
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
    let window = cubit_desktop::Window::open(1024, 700);
    #[cfg(target_os = "cubit")]
    let size = match &window {
        Some(w) => PhysicalSize::new(w.width(), w.height()),
        None => PhysicalSize::new(800, 600),
    };
    #[cfg(not(target_os = "cubit"))]
    let size = PhysicalSize::new(800, 600);
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
        .event_loop_waker(Box::new(Waker))
        .build();
    let delegate = Rc::new(Delegate::default());
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
        say(&format!("CUBITSHELL: loading page {index}"));

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

/// The interactive loop on the CuBit desktop: forward window input to the
/// page and keep Servo's event loop turning.
#[cfg(target_os = "cubit")]
fn run_window(servo: &servo::Servo, webview: &WebView, context: &SwglContext, delegate: &Delegate) {
    use cubit_desktop::Input;
    let mut pointer = DevicePoint::new(0.0, 0.0);
    let mut shown_frames = u32::MAX;
    loop {
        let mut idle = true;
        while let Some(input) = context.window.borrow_mut().as_mut().and_then(|w| w.poll()) {
            idle = false;
            let event = match input {
                Input::Move { x, y } => {
                    pointer = DevicePoint::new(x as f32, y as f32);
                    InputEvent::MouseMove(MouseMoveEvent::new(pointer.into()))
                },
                Input::Button { down, changed } => {
                    let button = if changed & 1 != 0 {
                        MouseButton::Primary
                    } else if changed & 2 != 0 {
                        MouseButton::Secondary
                    } else if changed & 4 != 0 {
                        MouseButton::Auxiliary
                    } else {
                        continue;
                    };
                    let action = if down {
                        MouseButtonAction::Down
                    } else {
                        MouseButtonAction::Up
                    };
                    InputEvent::MouseButton(MouseButtonEvent::new(action, button, pointer.into()))
                },
                Input::Wheel { delta } => InputEvent::Wheel(WheelEvent::new(
                    WheelDelta {
                        x: 0.0,
                        y: delta as f64 * 3.0,
                        z: 0.0,
                        mode: WheelMode::DeltaLine,
                    },
                    pointer.into(),
                )),
                Input::Text(ch) => {
                    for state in [KeyState::Down, KeyState::Up] {
                        webview.notify_input_event(InputEvent::Keyboard(
                            KeyboardEvent::new_without_event(
                                state,
                                Key::Character(ch.to_string()),
                                Code::Unidentified,
                                Location::Standard,
                                Modifiers::empty(),
                                false,
                                false,
                            ),
                        ));
                    }
                    continue;
                },
                Input::Key {
                    down,
                    scancode,
                    modifiers: _,
                } => {
                    let key = match scancode {
                        0x01 => NamedKey::Escape,
                        0x0e => NamedKey::Backspace,
                        0x0f => NamedKey::Tab,
                        0x1c => NamedKey::Enter,
                        0x47 => NamedKey::Home,
                        0x48 => NamedKey::ArrowUp,
                        0x49 => NamedKey::PageUp,
                        0x4b => NamedKey::ArrowLeft,
                        0x4d => NamedKey::ArrowRight,
                        0x4f => NamedKey::End,
                        0x50 => NamedKey::ArrowDown,
                        0x51 => NamedKey::PageDown,
                        0x53 => NamedKey::Delete,
                        _ => continue,
                    };
                    InputEvent::Keyboard(KeyboardEvent::new_without_event(
                        if down { KeyState::Down } else { KeyState::Up },
                        Key::Named(key),
                        Code::Unidentified,
                        Location::Standard,
                        Modifiers::empty(),
                        false,
                        false,
                    ))
                },
            };
            webview.notify_input_event(event);
        }
        servo.spin_event_loop();
        // Show each new frame in the window.
        let frames = delegate.frames.get();
        if frames != shown_frames {
            shown_frames = frames;
            webview.paint();
            context.present();
        }
        if idle {
            std::thread::sleep(Duration::from_millis(5));
        }
    }
}
