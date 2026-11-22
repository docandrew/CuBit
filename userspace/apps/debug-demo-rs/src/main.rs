// CuBit
// Copyright (C) 2026 Jon Andrew
// SPDX-License-Identifier: GPL-3.0-or-later

//! The debugger preview's Rust demo target (DBG-001): a job queue (`Vec` of
//! structs with `String`s and `&str`s), a sensor tree (`Option<Box<_>>`,
//! the niche-optimized enum), readings shared through `Arc`, a data-carrying
//! `enum`, a fixed array and a `HashMap`, changed all the time by two
//! threads, so a debugger's live views have something to show.

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use cubit_debug_stub as stub;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Priority {
    Low,
    Normal,
    High,
    Critical,
}

pub struct Job {
    pub id: u32,
    pub name: String,
    pub owner: &'static str,
    pub priority: Priority,
    pub progress: u8,
}

/// A data-carrying enum with a separate tag.
pub enum Shape {
    Circle { radius: u32 },
    Rect { width: u32, height: u32 },
    Point,
}

/// One reading, shared between the tree and the history (strong count 2).
pub struct Reading {
    pub sensor: u32,
    pub celsius: i32,
}

/// A binary search tree: `Option<Box<Sensor>>` has no tag (null is None).
pub struct Sensor {
    pub key: u32,
    pub readings: u32,
    pub last: Arc<Reading>,
    pub left: Option<Box<Sensor>>,
    pub right: Option<Box<Sensor>>,
}

/// How many rounds both threads have made.
pub static GENERATION: AtomicU64 = AtomicU64::new(0);
pub static PROGRAM: &str = "debug-demo-rs";
/// Startup defaults, fixed in the image (a debugger reads them from the
/// file as well as from the running program).
pub static DEFAULT_SHAPE: Shape = Shape::Rect { width: 3, height: 4 };
pub static CALIBRATION: Reading = Reading { sensor: 7, celsius: 21 };
pub static CALIBRATED: Option<&Reading> = Some(&CALIBRATION);
pub static EMPTY_TREE: Option<Box<Sensor>> = None;
pub static OWNER_NAMES: [&str; 4] = OWNERS;

const JOB_STEP: Duration = Duration::from_millis(200);
const SENSOR_STEP: Duration = Duration::from_millis(150);
const JOBS_WANTED: usize = 6;
const SENSOR_KEYS: u32 = 40;
const HISTORY_KEPT: usize = 6;
const NAMES: [&str; 8] = ["index-files", "compress", "send-report", "resize-image",
                          "spell-check", "sync-mail", "render-page", "backup-notes"];
const OWNERS: [&str; 4] = ["ada", "grace", "linus", "barbara"];

/// A small deterministic generator (each thread its own).
struct Seed(u32);
impl Seed {
    fn next(&mut self) -> u32 {
        self.0 = self.0.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
        self.0 >> 8
    }
}

#[inline(never)]
fn advance(job: &mut Job, step: u8) -> bool {
    let before = job.progress;
    let after = before.saturating_add(step).min(100);
    job.progress = after;
    if after >= 90 && job.priority < Priority::Critical {
        job.priority = match job.priority {
            Priority::Low => Priority::Normal,
            Priority::Normal => Priority::High,
            _ => Priority::Critical,
        };
    }
    stub::checkpoint();
    // Steps cut short at 100 %.
    after - before < step
}

fn run_jobs() {
    let mut seed = Seed(7);
    let mut queue: Vec<Job> = Vec::with_capacity(JOBS_WANTED);
    let mut owners: HashMap<u32, String> = HashMap::new();
    let mut next_id = 1u32;
    let mut clipped = 0u32;
    loop {
        while queue.len() < JOBS_WANTED {
            let name = NAMES[seed.next() as usize % NAMES.len()];
            let owner = OWNERS[seed.next() as usize % OWNERS.len()];
            let priority = match seed.next() % 4 {
                0 => Priority::Low,
                1 => Priority::Normal,
                2 => Priority::High,
                _ => Priority::Critical,
            };
            owners.insert(next_id, format!("{owner}@cubit"));
            queue.push(Job { id: next_id, name: format!("{name}-{next_id}"), owner, priority, progress: 0 });
            next_id += 1;
        }
        for job in queue.iter_mut() {
            if advance(job, (seed.next() % 23) as u8) {
                clipped += 1;
            }
        }
        queue.retain(|job| {
            let done = job.progress == 100;
            if done {
                owners.remove(&job.id);
            }
            !done
        });
        GENERATION.fetch_add(1, Ordering::Relaxed);
        std::hint::black_box((&queue, &owners, clipped));
        std::thread::sleep(JOB_STEP);
    }
}

#[inline(never)]
fn insert(node: &mut Option<Box<Sensor>>, key: u32, reading: &Arc<Reading>, depth: u32) -> u32 {
    match node {
        None => {
            *node = Some(Box::new(Sensor { key, readings: 1, last: Arc::clone(reading), left: None, right: None }));
            stub::checkpoint();
            depth
        }
        Some(sensor) if key < sensor.key => insert(&mut sensor.left, key, reading, depth + 1),
        Some(sensor) if key > sensor.key => insert(&mut sensor.right, key, reading, depth + 1),
        Some(sensor) => {
            sensor.readings += 1;
            sensor.last = Arc::clone(reading);
            stub::checkpoint();
            depth
        }
    }
}

fn run_sensors() {
    let mut seed = Seed(11);
    let mut tree: Option<Box<Sensor>> = None;
    let mut history: Vec<Arc<Reading>> = Vec::with_capacity(HISTORY_KEPT + 1);
    let mut window: [i32; 8] = [20; 8];
    let mut shape: Shape;
    let mut label: &str;
    let mut deepest = 0u32;
    let mut tick = 0u32;
    loop {
        let key = seed.next() % SENSOR_KEYS;
        let reading = Arc::new(Reading { sensor: key, celsius: -10 + (seed.next() % 40) as i32 });
        history.push(Arc::clone(&reading));
        if history.len() > HISTORY_KEPT {
            history.remove(0);
        }
        deepest = deepest.max(insert(&mut tree, key, &reading, 0));
        window.rotate_left(1);
        window[window.len() - 1] = reading.celsius;
        let side = 1 + tick % 9;
        (shape, label) = match tick / 4 % 3 {
            0 => (Shape::Circle { radius: side }, "circle"),
            1 => (Shape::Rect { width: side, height: 2 * side }, "rectangle"),
            _ => (Shape::Point, "point"),
        };
        tick += 1;
        std::hint::black_box((&tree, &history, &window, &shape, label, deepest));
        std::thread::sleep(SENSOR_STEP);
    }
}

fn main() {
    std::hint::black_box((&PROGRAM, &DEFAULT_SHAPE, &CALIBRATED, &EMPTY_TREE, &OWNER_NAMES));
    if !stub::start(PROGRAM) {
        println!("debug-demo-rs: stub FAIL");
    }
    let started = stub::spawn("jobs", run_jobs) && stub::spawn("sensors", run_sensors);
    println!("debug-demo-rs: {}", if started { "running" } else { "spawn FAIL" });
    loop {
        std::thread::sleep(Duration::from_secs(1));
    }
}
