//! Opt-in UI stall localization. One sampled atomic, no per-iteration logging.
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Duration;
static ENABLED: AtomicBool = AtomicBool::new(false);
static PHASE: AtomicU64 = AtomicU64::new(0);
pub fn mark(phase: u8) {
    if ENABLED.load(Ordering::Relaxed) {
        let old = PHASE.load(Ordering::Relaxed);
        PHASE.store((old.wrapping_add(256) & !255) | u64::from(phase), Ordering::Relaxed);
    }
}
pub struct Probe(Option<std::thread::JoinHandle<()>>);
impl Probe {
    pub fn start() -> Self {
        if !std::path::Path::new("/servo/profile-check").exists() { return Self(None); }
        ENABLED.store(true, Ordering::Relaxed);
        Self(Some(std::thread::spawn(|| {
            let mut previous = 0;
            while ENABLED.load(Ordering::Relaxed) {
                std::thread::sleep(Duration::from_secs(1));
                let current = PHASE.load(Ordering::Relaxed);
                if current != 0 && current == previous {
                    crate::say(&format!("PENNY-STALL: phase={} sequence={}", current & 255, current >> 8));
                }
                previous = current;
            }
        })))
    }
}
impl Drop for Probe {
    fn drop(&mut self) {
        ENABLED.store(false, Ordering::Relaxed);
        if let Some(thread) = self.0.take() { let _ = thread.join(); }
    }
}
