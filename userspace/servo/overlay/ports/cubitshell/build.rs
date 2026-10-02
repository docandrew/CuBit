fn main() {
    println!("cargo:rerun-if-env-changed=CUBIT_SERVO_NATIVE_DIR");
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() != Ok("cubit") { return; }
    println!("cargo:rustc-link-arg=-Wl,--wrap=__gnat_get_secondary_stack");
    let native = std::path::PathBuf::from(std::env::var_os("CUBIT_SERVO_NATIVE_DIR")
        .expect("build through userspace/servo/build-cubitshell.sh"));
    let userspace = native.parent().unwrap().parent().unwrap();
    for (dir, library) in [
        (native.join("build/lib"), "servo_shell_host"),
        (userspace.join("runtime/adalib"), "gnat-user"),
    ] {
        println!("cargo:rerun-if-changed={}", dir.join(format!("lib{library}.a")).display());
        println!("cargo:rustc-link-search=native={}", dir.display());
        println!("cargo:rustc-link-lib=static={library}");
    }
}
