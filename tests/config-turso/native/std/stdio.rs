use crate::io;
pub struct Stdin;
pub struct Stdout;
pub type Stderr = Stdout;
impl Stdin {
    pub const fn new() -> Self {
        Self
    }
}
impl Stdout {
    pub const fn new() -> Self {
        Self
    }
}
impl io::Read for Stdin {
    fn read(&mut self, _: &mut [u8]) -> io::Result<usize> {
        Err(io::Error::UNSUPPORTED_PLATFORM)
    }
}
impl io::Write for Stdout {
    fn write(&mut self, _: &[u8]) -> io::Result<usize> {
        Err(io::Error::UNSUPPORTED_PLATFORM)
    }
    fn flush(&mut self) -> io::Result<()> {
        Err(io::Error::UNSUPPORTED_PLATFORM)
    }
}
pub const STDIN_BUF_SIZE: usize = 0;
pub fn is_ebadf(_: &io::Error) -> bool {
    false
}
pub struct PanicOutput;
unsafe extern "C" {
    fn cubit_std_diagnostic(bytes: *const u8, len: usize);
}
impl io::Write for PanicOutput {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        // SAFETY: diagnostic hook borrows these bytes synchronously.
        unsafe { cubit_std_diagnostic(bytes.as_ptr(), bytes.len()) };
        Ok(bytes.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
pub fn panic_output() -> Option<PanicOutput> {
    Some(PanicOutput)
}
