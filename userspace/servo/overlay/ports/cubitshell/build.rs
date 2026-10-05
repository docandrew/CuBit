fn main() {
    println!("cargo:rerun-if-env-changed=CUBIT_SERVO_NATIVE_DIR");
    if std::env::var("CARGO_CFG_TARGET_OS").as_deref() != Ok("cubit") { return; }
    println!("cargo:rustc-link-arg=-Wl,--wrap=__gnat_get_secondary_stack");
    println!("cargo:rustc-link-arg=-Wl,--wrap=abort,--wrap=mozalloc_abort,--wrap=__cubit_fd_writev");
    let native = std::path::PathBuf::from(std::env::var_os("CUBIT_SERVO_NATIVE_DIR")
        .expect("build through userspace/servo/build-cubitshell.sh"));
    let userspace = native.parent().unwrap().parent().unwrap();
    if std::env::var_os("CARGO_FEATURE_MEDIA").is_some() {
        let media = native.parent().unwrap().join("media");
        let out = std::path::PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
        let flags = std::process::Command::new("pkg-config")
            .args(["--cflags", "gstreamer-base-1.0", "gstreamer-app-1.0"])
            .output().expect("query static media headers");
        assert!(flags.status.success(), "media headers: {}", String::from_utf8_lossy(&flags.stderr));
        let flags = String::from_utf8(flags.stdout).expect("media header flags");
        let mut objects = Vec::new();
        for name in ["penny-audio-sink", "penny-audio-hub", "penny-audio-player", "penny-browser-audio"] {
            let source = media.join(format!("{name}.c"));
            for extension in ["c", "h"] {
                println!("cargo:rerun-if-changed={}", media.join(format!("{name}.{extension}")).display());
            }
            let object = out.join(format!("{name}.o"));
            let status = std::process::Command::new(userspace.join("libc/cubit-cc"))
                .args(["-O2", "-Wall", "-Wextra", "-Werror"])
                .args(flags.split_whitespace()).arg("-I").arg(&media)
                .arg("-c").arg(source).arg("-o").arg(&object)
                .status().expect("compile Penny audio adapter");
            assert!(status.success(), "Penny audio adapter compilation failed");
            objects.push(object);
        }
        let archive = out.join("libpenny_browser_audio.a");
        if archive.exists() { std::fs::remove_file(&archive).expect("replace audio archive"); }
        let status = std::process::Command::new("ar").arg("rcs").arg(archive)
            .args(objects).status().expect("archive Penny audio adapter");
        assert!(status.success(), "Penny audio archive failed");
        println!("cargo:rustc-link-search=native={}", out.display());
        println!("cargo:rustc-link-lib=static=penny_browser_audio");
    }
    for (dir, library) in [
        (native.join("build/lib"), "servo_shell_host"),
        (userspace.join("runtime/adalib"), "gnat-user"),
    ] {
        println!("cargo:rerun-if-changed={}", dir.join(format!("lib{library}.a")).display());
        println!("cargo:rustc-link-search=native={}", dir.display());
        println!("cargo:rustc-link-lib=static={library}");
    }
}
