use crate::time::Duration;

unsafe extern "C" {
    fn cubit_std_time(wall: bool, secs: *mut u64, nanos: *mut u32) -> bool;
}
fn now(wall: bool) -> Duration {
    let mut secs = 0;
    let mut nanos = 0;
    // SAFETY: the native bridge writes exactly these two live scalars.
    if !unsafe { cubit_std_time(wall, &mut secs, &mut nanos) } || nanos >= 1_000_000_000 {
        panic!("CuBit clock authority unavailable");
    }
    Duration::new(secs, nanos)
}
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Instant(Duration);
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct SystemTime(Duration);
pub const UNIX_EPOCH: SystemTime = SystemTime(Duration::ZERO);
impl Instant {
    pub fn now() -> Self {
        Self(now(false))
    }
    pub fn checked_sub_instant(&self, other: &Self) -> Option<Duration> {
        self.0.checked_sub(other.0)
    }
    pub fn checked_add_duration(&self, other: &Duration) -> Option<Self> {
        self.0.checked_add(*other).map(Self)
    }
    pub fn checked_sub_duration(&self, other: &Duration) -> Option<Self> {
        self.0.checked_sub(*other).map(Self)
    }
}
impl SystemTime {
    pub const MAX: Self = Self(Duration::MAX);
    pub const MIN: Self = Self(Duration::ZERO);
    pub fn now() -> Self {
        Self(now(true))
    }
    pub fn sub_time(&self, other: &Self) -> Result<Duration, Duration> {
        self.0.checked_sub(other.0).ok_or_else(|| other.0 - self.0)
    }
    pub fn checked_add_duration(&self, other: &Duration) -> Option<Self> {
        self.0.checked_add(*other).map(Self)
    }
    pub fn checked_sub_duration(&self, other: &Duration) -> Option<Self> {
        self.0.checked_sub(*other).map(Self)
    }
}
