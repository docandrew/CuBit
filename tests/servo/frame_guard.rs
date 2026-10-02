//! Hosted FFI fault simulation of the actual Rust frame guard, not Ada IPC.
#![allow(dead_code)]
extern crate self as cubit_fonts;
pub fn cubit_font_glyph() {}
pub fn cubit_font_raster_mask() {}
#[path = "../../userspace/servo/overlay/ports/cubitshell/src/cubit_desktop.rs"]
mod desktop;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering::SeqCst};
static OPEN_ALLOWED: AtomicBool = AtomicBool::new(true);
static ACTIVE: AtomicBool = AtomicBool::new(false);
static DEFER: AtomicBool = AtomicBool::new(false);
static CANCELS: AtomicUsize = AtomicUsize::new(0);
static PRESENTS: AtomicUsize = AtomicUsize::new(0);
static CLOSES: AtomicUsize = AtomicUsize::new(0);
#[no_mangle] extern "C" fn servo_shell_hostinit() {}
#[no_mangle] extern "C" fn cubit_servo_stack_check() -> u32 { 1 }
#[no_mangle] extern "C" fn cubit_servo_open() -> u32 { if OPEN_ALLOWED.load(SeqCst) { 1 } else { 0 } }
#[no_mangle] extern "C" fn cubit_servo_select_window(id: u32) -> u32 { assert_eq!(id, 1); 1 }
#[no_mangle] extern "C" fn cubit_servo_prepare() -> u32 {
    if DEFER.load(SeqCst) || ACTIVE.swap(true, SeqCst) { 0 } else { 1 }
}
#[no_mangle] extern "C" fn cubit_servo_cancel() {
    assert!(ACTIVE.swap(false, SeqCst), "cancel without active lease");
    CANCELS.fetch_add(1, SeqCst);
}
#[no_mangle] extern "C" fn cubit_servo_present(_: *const u8, _: u64, _: u32, _: u32) -> u32 {
    assert!(ACTIVE.swap(false, SeqCst));
    PRESENTS.fetch_add(1, SeqCst);
    1
}
#[no_mangle] extern "C" fn cubit_servo_close() {
    assert!(!ACTIVE.load(SeqCst), "close with outstanding lease");
    CLOSES.fetch_add(1, SeqCst);
}
fn main() {
    OPEN_ALLOWED.store(false, SeqCst);
    assert!(desktop::Window::open().is_none());
    assert_eq!(CLOSES.load(SeqCst), 0, "failed open must not close a window");
    OPEN_ALLOWED.store(true, SeqCst);
    let window = desktop::Window::open().unwrap();
    DEFER.store(true, SeqCst);
    assert!(window.prepare().is_none());
    assert_eq!(CANCELS.load(SeqCst), 0);
    DEFER.store(false, SeqCst);
    for _ in 0..1000 {
        let frame = window.prepare().unwrap();
        assert!(window.prepare().is_none());
        assert!(ACTIVE.load(SeqCst));
        drop(frame);
        assert!(!ACTIVE.load(SeqCst));
        let frame = window.prepare().unwrap();
        assert_eq!(frame.present(&[0; 4], 1, 1), 1);
        assert!(!ACTIVE.load(SeqCst));
    }
    assert_eq!(CANCELS.load(SeqCst), 1000);
    assert_eq!(PRESENTS.load(SeqCst), 1000);
    drop(window);
    assert_eq!(CLOSES.load(SeqCst), 1);
    println!("Servo frame guard PASS: 1000 cancel/reacquire/present cycles, deferral and nested rejection");
}
