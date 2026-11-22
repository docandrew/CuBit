//! Rust std::env::args on CuBit (docs/process-arguments.md): started by
//! spawn-check with arguments; reports on the kernel console and exits
//! with 44 when every argument arrived.

const EXPECTED_EXIT: i32 = 44;
const FAILED_EXIT: i32 = 99;

unsafe extern "C" {
    fn cubit_debug_write(text: *const u8, length: usize);
}

fn say(text: &str) {
    unsafe { cubit_debug_write(text.as_ptr(), text.len()) }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let ok = args == ["rust-args-check.app", "alpha", "two words", ""]
        && std::env::var("CUBIT_TEST").as_deref() == Ok("1")
        && std::env::var("EMPTY").as_deref() == Ok("")
        && std::env::vars().count() == 2;
    if ok {
        say("rust-args-check: std::env::args and env::var PASS\n");
        std::process::exit(EXPECTED_EXIT);
    }
    say("rust-args-check: std::env::args FAIL\n");
    std::process::exit(FAILED_EXIT);
}
