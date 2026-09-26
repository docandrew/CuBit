/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! A window on the CuBit desktop (desktop.svc), as NetSurf's frontend uses
//! it (userspace/c/netsurf/libnsfb-cubit.c): the browser draws into a
//! buffer it owns and lends to desktop.svc; only input for its own surface
//! comes back. No display or global input authority.

use std::arch::asm;

const SLOT_DESKTOP: u64 = 21;

const SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY: u64 = 41;
const SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY: u64 = 42;
const SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY: u64 = 106;
const SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION: u64 = 108;

const OP_DESKTOP_HELLO: u32 = 0x0800;
const OP_SURFACE_CREATE: u32 = 0x0810;
const OP_SURFACE_PRESENT: u32 = 0x0812;
const OP_SURFACE_ATTACH_BUFFER: u32 = 0x0814;
const OP_INPUT_POLL: u32 = 0x0821;
const OP_WINDOW_SET_LIMITS: u32 = 0x0841;

const SURFACE_FLAG_WINDOW: u64 = 2;
const WINDOW_FLAGS: u64 = 1 | 4 | 16 | 128; // decorated, minimizable, closeable, fixed
const WINDOW_CHROME_W: u32 = 20;
const WINDOW_CHROME_H: u32 = 44;
const PAGE: usize = 4096;
const MAX_SURFACE_BYTES: usize = 4096 * PAGE;
const NO_COMPLETION_TOKEN: u64 = !0;

/// CuBit.Messages.Message (48 bytes).
#[repr(C)]
#[derive(Default)]
struct Message {
    label: u32,
    length: u8,
    flags: u8,
    reserved: u16,
    authority: u64,
    words: [u64; 4],
}

unsafe fn syscall4(n: u64, a: u64, b: u64, c: u64, d: u64) -> u64 {
    let ret: u64;
    unsafe {
        asm!("syscall", inlateout("rax") n => ret, in("rdi") a, in("rsi") b,
             in("rdx") c, in("r10") d, lateout("rcx") _, lateout("r11") _,
             options(nostack));
    }
    ret
}

fn call(label: u32, words: [u64; 4]) -> Option<Message> {
    let mut message = Message {
        label,
        length: 4,
        words,
        ..Default::default()
    };
    let ret = unsafe {
        syscall4(
            SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY,
            SLOT_DESKTOP,
            &mut message as *mut Message as u64,
            0,
            0,
        )
    };
    (ret != u64::MAX).then_some(message)
}

fn pack(low: u32, high: u32) -> u64 {
    low as u64 | (high as u64) << 32
}

/// Input for this window, as desktop.svc reports it.
pub enum Input {
    Move { x: i32, y: i32 },
    /// Buttons now held (bit 0 left, 1 right, 2 middle) and which changed.
    Button { down: bool, changed: u64 },
    Wheel { delta: i32 },
    Text(char),
    Key { down: bool, scancode: u8, modifiers: u64 },
}

pub struct Window {
    surface: u64,
    buffer: *mut u8,
    width: u32,
    height: u32,
    serial: u64,
    buttons: u64,
}

impl Window {
    /// A decorated window with a width x height BGRA buffer, or None
    /// without a desktop capability.
    pub fn open(width: u32, height: u32) -> Option<Window> {
        let bytes = width as usize * height as usize * 4;
        if bytes == 0 || bytes > MAX_SURFACE_BYTES {
            return None;
        }
        let hello = call(OP_DESKTOP_HELLO, [0x0000_0001_0000_0000, 0, 0, 0])?;
        if hello.words[0] == 0 {
            return None;
        }
        let (outer_w, outer_h) = (width + WINDOW_CHROME_W, height + WINDOW_CHROME_H);
        let created = call(
            OP_SURFACE_CREATE,
            [outer_w as u64, outer_h as u64, SURFACE_FLAG_WINDOW, 0],
        )?;
        let surface = created.words[0];
        if surface == 0 {
            return None;
        }
        call(
            OP_WINDOW_SET_LIMITS,
            [surface, pack(outer_w, outer_h), pack(outer_w, outer_h), WINDOW_FLAGS],
        );

        // Process-lifetime storage, page aligned, lent to desktop.svc.
        let pages = bytes.div_ceil(PAGE);
        let layout = std::alloc::Layout::from_size_align(pages * PAGE, PAGE).ok()?;
        let buffer = unsafe { std::alloc::alloc_zeroed(layout) };
        if buffer.is_null() {
            return None;
        }
        let slot = unsafe {
            syscall4(
                SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
                SLOT_DESKTOP,
                buffer as u64,
                pages as u64,
                0,
            )
        };
        if slot == u64::MAX {
            return None;
        }
        let generation =
            unsafe { syscall4(SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, slot, 0, 0, 0) };
        if generation == 0 || generation > u32::MAX as u64 {
            return None;
        }
        let attached = call(
            OP_SURFACE_ATTACH_BUFFER,
            [
                surface,
                slot,
                generation,
                width as u64 | (height as u64) << 16 | ((width * 4) as u64) << 32,
            ],
        )?;
        if attached.label != OP_SURFACE_ATTACH_BUFFER || attached.words[0] != 0 {
            return None;
        }
        Some(Window {
            surface,
            buffer,
            width,
            height,
            serial: 0,
            buttons: 0,
        })
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    /// Copy a frame (BGRA rows, top first) into the buffer and present it.
    pub fn present(&self, bgra: &[u8]) {
        let bytes = self.width as usize * self.height as usize * 4;
        if bgra.len() < bytes {
            return;
        }
        unsafe { std::ptr::copy_nonoverlapping(bgra.as_ptr(), self.buffer, bytes) };
        // Desktop present: submit (no reply), a reserved zero fourth word.
        let tag = OP_SURFACE_PRESENT as u64 | 4 << 32;
        let ret: u64;
        unsafe {
            asm!("syscall",
                 inlateout("rax") SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY => ret,
                 in("rdi") SLOT_DESKTOP, in("rsi") tag, in("rdx") self.surface,
                 in("r10") 0u64, in("r8") pack(self.width, self.height), in("r9") 0u64,
                 in("r12") NO_COMPLETION_TOKEN,
                 lateout("rcx") _, lateout("r11") _, options(nostack));
        }
        let _ = ret;
    }

    /// The next input event for this window, if any.
    pub fn poll(&mut self) -> Option<Input> {
        let reply = call(OP_INPUT_POLL, [self.surface, self.serial, 0, 0])?;
        if reply.label != OP_INPUT_POLL || reply.length != 4 {
            return None;
        }
        self.serial = reply.words[1];
        let (a, b) = (reply.words[2], reply.words[3]);
        match reply.words[0] {
            1 | 2 if a <= 127 => Some(Input::Key {
                down: reply.words[0] == 1,
                scancode: a as u8,
                modifiers: b,
            }),
            3 => Some(Input::Move {
                x: a as u32 as i32,
                y: (a >> 32) as u32 as i32,
            }),
            4 | 5 => {
                let changed = self.buttons ^ b;
                self.buttons = b;
                Some(Input::Button {
                    down: reply.words[0] == 4,
                    changed,
                })
            },
            6 => char::from_u32(a as u32).map(Input::Text),
            7 => Some(Input::Wheel {
                delta: b as u32 as i32,
            }),
            _ => None,
        }
    }
}
