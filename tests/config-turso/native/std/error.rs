// CuBit has typed IPC outcomes, not an ambient errno slot. Never report fake
// success for a std API that asks for an unavailable OS error facility.
pub fn errno() -> i32 {
    -1
}
pub fn is_interrupted(_code: i32) -> bool {
    false
}
pub fn decode_error_kind(_code: i32) -> crate::io::ErrorKind {
    crate::io::ErrorKind::Unsupported
}
pub fn error_string(_code: i32) -> String {
    "ambient OS error facility unavailable on CuBit".to_string()
}
