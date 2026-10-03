//! C layout shared with Servo_Tab_Projection. Only visible native rows cross
//! this boundary; stable logical IDs are independent of local widget IDs.
pub const CAPACITY: usize = 32;

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Row {
    pub id: u64,
    pub text: [u8; 64],
    pub length: u32,
    pub reserved: u32,
}

impl Default for Row {
    fn default() -> Self { Self { id: 0, text: [0; 64], length: 0, reserved: 0 } }
}

#[repr(C)]
#[derive(Clone, Debug, PartialEq)]
pub struct Snapshot {
    pub total: u64,
    pub active: u64,
    pub count: u32,
    pub reserved: u32,
    pub items: [Row; CAPACITY],
}

impl Default for Snapshot {
    fn default() -> Self {
        Self { total: 0, active: 0, count: 0, reserved: 0, items: [Row::default(); CAPACITY] }
    }
}

const _: () = {
    assert!(std::mem::size_of::<Row>() == 80);
    assert!(std::mem::offset_of!(Row, text) == 8);
    assert!(std::mem::offset_of!(Row, length) == 72);
    assert!(std::mem::offset_of!(Row, reserved) == 76);
    assert!(std::mem::size_of::<Snapshot>() == 2584);
    assert!(std::mem::offset_of!(Snapshot, active) == 8);
    assert!(std::mem::offset_of!(Snapshot, count) == 16);
    assert!(std::mem::offset_of!(Snapshot, reserved) == 20);
    assert!(std::mem::offset_of!(Snapshot, items) == 24);
};
