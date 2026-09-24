unsafe extern "C" {
    fn cubit_std_random(bytes: *mut u8, len: usize) -> bool;
}
pub fn fill_bytes(bytes: &mut [u8]) {
    // SAFETY: the bridge writes only this exclusively borrowed live slice.
    if !unsafe { cubit_std_random(bytes.as_mut_ptr(), bytes.len()) } {
        panic!("CuBit random source unavailable");
    }
}
