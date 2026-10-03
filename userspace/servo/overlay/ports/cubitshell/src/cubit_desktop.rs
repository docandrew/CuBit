/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Serialized bridge to the native Ada browser shell. Ada owns chrome,
//! configuration, damage, protected frame leases and all Desktop IPC. Rust
//! borrows source pixels only for `present`; no grant or destination pointer
//! is exposed here. The engine/rendering context stays on this thread.
use std::sync::{Once, OnceLock};
use std::cell::Cell;

static INIT: Once = Once::new();
static OWNER: OnceLock<std::thread::ThreadId> = OnceLock::new();
thread_local! { static IN_NATIVE: Cell<bool> = const { Cell::new(false) }; }

/// The standalone Ada runtime and window owner are single-threaded. Check
/// every FFI entry, including initialization and RAII cleanup, before entry.
fn native<T>(call: impl FnOnce() -> T) -> T {
    let current = std::thread::current().id();
    assert_eq!(*OWNER.get_or_init(|| current), current, "Servo Ada bridge used off its owner thread");
    IN_NATIVE.with(|active| {
        assert!(!active.replace(true), "Servo Ada bridge reentered");
        struct Leave<'a>(&'a Cell<bool>);
        impl Drop for Leave<'_> { fn drop(&mut self) { self.0.set(false); } }
        let _leave = Leave(active);
        call()
    })
}

#[repr(C)]
#[derive(Clone, Copy, Default, Debug, PartialEq)]
pub struct Viewport {
    pub width: u32,
    pub height: u32,
    pub numerator: u32,
    pub denominator: u32,
}

#[repr(C)]
#[derive(Default)]
struct Event { kind: u64, a: u64, b: u64 }

#[repr(C)]
#[derive(Default)]
pub struct InputStats {
    pub batch_enabled: u64,
    pub channel_disabled: u64,
    pub successful_fetches: u64,
    pub fetched_events: u64,
    pub delivered_events: u64,
    pub fallback_polls: u64,
    pub cache_rejections: u64,
}
const _: () = assert!(std::mem::size_of::<InputStats>() == 56);

unsafe extern "C" {
    fn servo_shell_hostinit();
    fn cubit_servo_stack_check() -> u32;
    fn cubit_servo_memory_owned() -> u64;
    fn cubit_servo_config_scope_check() -> u32;
    fn cubit_servo_open() -> u32;
    fn cubit_servo_select_window(id: u32) -> u32;
    fn cubit_servo_window_error();
    fn cubit_servo_metrics(result: *mut Viewport);
    fn cubit_servo_input_statistics(result: *mut InputStats);
    fn cubit_servo_begin_input();
    fn cubit_servo_poll(result: *mut Event) -> u32;
    fn cubit_servo_location(text: *mut u8, capacity: u32) -> u32;
    fn cubit_servo_security(text: *const u8, length: u32);
    fn cubit_servo_state(url: *const u8, url_len: u32, title: *const u8, title_len: u32, flags: u32);
    fn cubit_servo_navigation_error();
    fn cubit_servo_tab_capacity() -> u32;
    fn cubit_servo_tabs(value: *const crate::tab_model::native::Snapshot) -> u32;
    fn cubit_servo_prepare() -> u32;
    fn cubit_servo_cancel();
    fn cubit_servo_present(bgra: *const u8, len: u64, width: u32, height: u32, stride: u32) -> u32;
    fn cubit_servo_pending() -> u32;
    fn cubit_servo_close();
}

pub fn initialize_native() {
    INIT.call_once(|| native(|| unsafe { servo_shell_hostinit() }));
}

pub fn config_scope_check() -> u32 {
    INIT.call_once(|| native(|| unsafe { servo_shell_hostinit() }));
    native(|| unsafe { cubit_servo_config_scope_check() })
}

pub fn memory_owned() -> u64 { native(|| unsafe { cubit_servo_memory_owned() }) }

pub enum Input {
    Move { x: i32, y: i32 },
    Button { down: bool, changed: u64, x: i32, y: i32 },
    Wheel { delta: i32, x: i32, y: i32 },
    Text(char),
    Key { down: bool, scancode: u8, modifiers: u64 },
    NewWindow, NewTab, SelectTab(u64), CycleTab(bool), CloseTab(u64),
    Navigate(String), Back, Forward, Reload, Configure { settings_opened: bool }, Consumed, Close, Leave,
}

// Rc marker prevents Send/Sync: callbacks and GNAT state are serialized.
pub const MAX_WINDOWS: usize = 4;

pub struct Window {
    id: u32,
    buttons: u64,
    _main_thread: std::marker::PhantomData<std::rc::Rc<()>>,
}

pub struct Frame<'a> {
    _window: &'a Window,
    finished: bool,
}

impl Frame<'_> {
    /// Copy a prepared SWGL framebuffer synchronously into the native lease.
    ///
    /// # Safety
    /// `bgra` must remain allocated for `len` bytes for this call; each pixel
    /// must be initialized BGRA in bottom-up rows with the supplied stride.
    /// It must not alias the native destination. No SWGL operation may mutate
    /// or resize its storage until this call returns. Row padding is not read.
    pub unsafe fn present_bgra(mut self, bgra: *const u8, len: u64,
                              width: u32, height: u32, stride: u32) -> u32 {
        let result = self._window.call(|| unsafe {
            cubit_servo_present(bgra, len, width, height, stride)
        });
        self.finished = true;
        result
    }
}

impl Drop for Frame<'_> {
    fn drop(&mut self) {
        if !self.finished { self._window.call(|| unsafe { cubit_servo_cancel() }); }
    }
}

impl Window {
    fn call<T>(&self, call: impl FnOnce() -> T) -> T {
        native(|| {
            assert_eq!(unsafe { cubit_servo_select_window(self.id) }, 1,
                "invalid window or another window owns a frame lease");
            call()
        })
    }
    pub fn window_error(&self) { self.call(|| unsafe { cubit_servo_window_error() }); }
    pub fn open() -> Option<Self> {
        // Keep the shared font C exports reachable from the Ada archive,
        // while using Servo's one allocator/runtime for their implementation.
        std::hint::black_box(cubit_fonts::cubit_font_glyph as *const ());
        std::hint::black_box(cubit_fonts::cubit_font_raster_mask as *const ());
        INIT.call_once(|| native(|| unsafe { servo_shell_hostinit() }));
        if native(|| unsafe { cubit_servo_stack_check() }) != 1 { return None; }
        let id = native(|| unsafe { cubit_servo_open() });
        (id != 0).then(|| Self {
            id, buttons: 0, _main_thread: std::marker::PhantomData,
        })
    }

    pub fn input_statistics(&self) -> (u32, InputStats) {
        let mut result = InputStats::default();
        self.call(|| unsafe { cubit_servo_input_statistics(&mut result) });
        (self.id, result)
    }

    pub fn viewport(&self) -> Viewport {
        let mut result = Viewport::default();
        self.call(|| unsafe { cubit_servo_metrics(&mut result) });
        result
    }

    /// Acquire before readback, so pending retirement doesn't cause another
    /// full pixel copy. Dropping the guard cancels and preserves repair debt.
    pub fn prepare(&self) -> Option<Frame<'_>> {
        if self.call(|| unsafe { cubit_servo_prepare() }) != 0 {
            Some(Frame { _window: self, finished: false })
        } else {
            None
        }
    }

    pub fn pending(&self) -> bool { self.call(|| unsafe { cubit_servo_pending() != 0 }) }

    pub fn begin_input(&mut self) { self.call(|| unsafe { cubit_servo_begin_input() }) }

    pub fn tab_capacity(&self) -> usize {
        self.call(|| unsafe { cubit_servo_tab_capacity() }) as usize
    }
    pub fn tabs(&self, value: &crate::tab_model::native::Snapshot) {
        assert_eq!(self.call(|| unsafe { cubit_servo_tabs(value) }), 1, "invalid tab projection");
    }
    pub fn navigation_error(&self) { self.call(|| unsafe { cubit_servo_navigation_error() }) }

    pub fn security(&self, text: &str) {
        self.call(|| unsafe { cubit_servo_security(text.as_ptr(), text.len().min(12288) as u32) });
    }

    pub fn state(&self, url: &str, title: &str, loading: bool, back: bool, forward: bool, marks: u32, seconds: u32) {
        fn bounded(s: &str, max: usize) -> &str {
            let mut end = s.len().min(max);
            while !s.is_char_boundary(end) { end -= 1; }
            &s[..end]
        }
        let title = bounded(title, 256);
        // The bridge blanks overlong URLs instead of presenting a truncated
        // address as an editable, apparently complete navigation target.
        self.call(|| unsafe { cubit_servo_state(url.as_ptr(), url.len().min(1025) as u32,
            title.as_ptr(), title.len() as u32,
            u32::from(loading) | u32::from(back) << 1 | u32::from(forward) << 2 |
            (marks & 15) << 3 | seconds.min(0x00ff_ffff) << 8) });
    }

    /// Exactly one Desktop poll. Consumed chrome events are returned too, so
    /// a caller's event budget also bounds work spent inside the native shell.
    pub fn poll(&mut self) -> Option<Input> {
        let mut event = Event::default();
        if self.call(|| unsafe { cubit_servo_poll(&mut event) }) == 0 { return None; }
        let (x, y) = (event.a as u32 as i32, (event.a >> 32) as u32 as i32);
        Some(match event.kind {
            1 | 2 if event.a <= 127 => Input::Key {
                down: event.kind == 1, scancode: event.a as u8, modifiers: event.b,
            },
            3 => Input::Move { x, y },
            4 | 5 => {
                let changed = self.buttons ^ event.b;
                self.buttons = event.b;
                Input::Button { down: event.kind == 4, changed, x, y }
            },
            6 => char::from_u32(event.a as u32).map(Input::Text).unwrap_or(Input::Consumed),
            7 => Input::Wheel { delta: event.b as u32 as i32, x, y },
            16 => {
                let mut text = [0u8; 1024];
                let len = self.call(|| unsafe { cubit_servo_location(text.as_mut_ptr(), text.len() as u32) }) as usize;
                if len > text.len() { return Some(Input::Consumed); }
                Input::Navigate(String::from_utf8_lossy(&text[..len]).into_owned())
            },
            17 => Input::Back,
            18 => Input::Forward,
            19 => Input::Reload,
            20 | 28 => {
                self.buttons = 0;
                Input::Configure { settings_opened: event.kind == 28 }
            },
            21 => Input::Close,
            27 => Input::NewWindow,
            23 => Input::Leave,
            24 => Input::NewTab,
            25 if event.a != 0 => Input::SelectTab(event.a),
            26 if event.a != 0 => Input::CloseTab(event.a),
            29 => Input::CycleTab(event.a != 0),
            _ => Input::Consumed,
        })
    }
}

impl Drop for Window {
    fn drop(&mut self) { self.call(|| unsafe { cubit_servo_close() }) }
}

#[cfg(test)]
mod entry_tests {
    #[test]
    fn rejects_other_threads_and_reentrancy() {
        super::native(|| ());
        assert!(std::thread::spawn(|| super::native(|| ())).join().is_err());
        assert!(std::panic::catch_unwind(|| super::native(|| super::native(|| ()))).is_err());
        // A rejected reentry must not leave the outer guard stuck after unwind.
        super::native(|| ());
    }
}
