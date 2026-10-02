/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Serialized bridge to the native Ada browser shell. Ada owns chrome,
//! configuration, damage, protected frame leases and all Desktop IPC. Rust
//! borrows source pixels only for `present`; no grant or destination pointer
//! is exposed here. The engine/rendering context stays on this thread.
use std::sync::{Once, OnceLock};
use std::cell::Cell;

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

unsafe extern "C" {
    fn servo_shell_hostinit();
    fn cubit_servo_stack_check() -> u32;
    fn cubit_servo_open() -> u32;
    fn cubit_servo_select_window(id: u32) -> u32;
    fn cubit_servo_window_error();
    fn cubit_servo_metrics(result: *mut Viewport);
    fn cubit_servo_begin_input();
    fn cubit_servo_poll(result: *mut Event) -> u32;
    fn cubit_servo_location(text: *mut u8, capacity: u32) -> u32;
    fn cubit_servo_state(url: *const u8, url_len: u32, title: *const u8, title_len: u32, flags: u32);
    fn cubit_servo_navigation_error();
    fn cubit_servo_tab_title(index: u32, title: *const u8, length: u32);
    fn cubit_servo_tab_parked(index: u32);
    fn cubit_servo_prepare() -> u32;
    fn cubit_servo_cancel();
    fn cubit_servo_present(rgba: *const u8, len: u64, width: u32, height: u32) -> u32;
    fn cubit_servo_pending() -> u32;
    fn cubit_servo_close();
}

pub enum Input {
    Move { x: i32, y: i32 },
    Button { down: bool, changed: u64, x: i32, y: i32 },
    Wheel { delta: i32, x: i32, y: i32 },
    Text(char),
    Key { down: bool, scancode: u8, modifiers: u64 },
    NewWindow, NewTab(usize), SelectTab(usize), CloseTab { index: usize, next: usize },
    Navigate(String), Back, Forward, Reload, Configure { released: u64, settings_opened: bool }, Consumed, Close, Leave,
}

// Rc marker prevents Send/Sync: callbacks and GNAT state are serialized.
pub const MAX_TABS: usize = 32;
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
    pub fn present(mut self, rgba: &[u8], width: u32, height: u32) -> u32 {
        let result = self._window.call(|| unsafe { cubit_servo_present(rgba.as_ptr(), rgba.len() as u64, width, height) });
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
        static INIT: Once = Once::new();
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

    pub fn tab_title(&self, index: usize, title: &str) {
        let mut end = title.len().min(64);
        while !title.is_char_boundary(end) { end -= 1; }
        self.call(|| unsafe { cubit_servo_tab_title(index as u32, title.as_ptr(), end as u32) });
    }
    pub fn tab_parked(&self, index: usize) {
        self.call(|| unsafe { cubit_servo_tab_parked(index as u32) });
    }
    pub fn navigation_error(&self) { self.call(|| unsafe { cubit_servo_navigation_error() }) }

    pub fn state(&self, url: &str, title: &str, loading: bool, back: bool, forward: bool) {
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
            u32::from(loading) | u32::from(back) << 1 | u32::from(forward) << 2) });
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
                let released = self.buttons;
                self.buttons = 0;
                Input::Configure { released, settings_opened: event.kind == 28 }
            },
            21 => Input::Close,
            27 => Input::NewWindow,
            23 => Input::Leave,
            24 if (1..=MAX_TABS as u64).contains(&event.a) => Input::NewTab(event.a as usize),
            25 if (1..=MAX_TABS as u64).contains(&event.a) => Input::SelectTab(event.a as usize),
            26 if (1..=MAX_TABS as u64).contains(&event.a) && event.b <= MAX_TABS as u64 =>
                Input::CloseTab { index: event.a as usize, next: event.b as usize },
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
