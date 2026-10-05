//! Opt-in engine reports. One outstanding request, no UI-thread waits.
use std::sync::mpsc::{self, Receiver, SyncSender};
use std::time::{Duration, Instant};
use servo_base::generic_channel::GenericCallback;
use profile_traits::mem::MemoryReportResult;
pub struct Probe(Option<State>);
struct State {
    tx: SyncSender<Option<MemoryReportResult>>,
    rx: Receiver<Option<MemoryReportResult>>,
    next: Instant,
    pending: Option<Instant>,
    sequence: u64,
}
impl Probe {
    pub fn new() -> Self {
        if !std::path::Path::new("/servo/memory-check").exists() { return Self(None); }
        let (tx, rx) = mpsc::sync_channel(1);
        Self(Some(State { tx, rx, next: Instant::now(), pending: None, sequence: 0 }))
    }
    pub fn poll(&mut self, servo: &servo::Servo) {
        let Some(s) = self.0.as_mut() else { return; };
        if let Ok(response) = s.rx.try_recv() {
            s.pending = None;
            s.next = Instant::now() + Duration::from_secs(30);
            if let Some(response) = response {
                let mut reports: Vec<_> = response.results.into_iter().flat_map(|p| p.reports).collect();
                reports.sort_unstable_by_key(|r| std::cmp::Reverse(r.size));
                crate::say(&format!("PENNY-MEMORY-REPORT: sequence={} entries={} (top 20; categories may overlap)", s.sequence, reports.len()));
                for r in reports.into_iter().take(20) {
                    let label: String = r.path.join("/").chars().take(180)
                        .map(|c| if c.is_control() { '_' } else { c }).collect();
                    crate::say(&format!("PENNY-MEMORY-ENTRY: sequence={} bytes={} kind={:?} path={label}", s.sequence, r.size, r.kind));
                }
            } else { crate::say("PENNY-MEMORY-REPORT: collection failed"); }
        }
        if let Some(start) = s.pending {
            if start.elapsed() >= Duration::from_secs(60) {
                crate::say("PENNY-MEMORY-REPORT: timed out; disabled"); self.0 = None;
            }
            return;
        }
        if Instant::now() < s.next { return; }
        let tx = s.tx.clone();
        let Ok(callback) = GenericCallback::new(move |result| { let _ = tx.try_send(result.ok()); }) else {
            crate::say("PENNY-MEMORY-REPORT: callback unavailable; disabled"); self.0 = None; return;
        };
        s.sequence += 1;
        s.pending = Some(Instant::now());
        servo.create_memory_report(callback);
    }
}
