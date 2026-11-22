//! Links Penny's SPARK cryptography (userspace/servo/crypto/penny_crypto.gpr),
//! built by build-cubitshell.sh (CuBit runtime) or
//! build-cubitshell-hosted.sh (host, runtime encapsulated) before cargo.
fn main() {
    println!("cargo:rerun-if-env-changed=PENNY_CRYPTO_LIB_DIR");
    println!("cargo:rerun-if-env-changed=PENNY_CRYPTO_RUNTIME_DIR");
    let dir = std::env::var("PENNY_CRYPTO_LIB_DIR")
        .expect("PENNY_CRYPTO_LIB_DIR: build Penny through userspace/servo/build-cubitshell*.sh");
    println!("cargo:rerun-if-changed={dir}/libpenny_crypto.a");
    println!("cargo:rustc-link-search=native={dir}");
    println!("cargo:rustc-link-lib=static=penny_crypto");
    // On CuBit the library uses CuBit's Ada runtime; name it here too, after
    // this library, whatever order the final link puts the crates in.
    if let Ok(runtime) = std::env::var("PENNY_CRYPTO_RUNTIME_DIR") {
        println!("cargo:rustc-link-search=native={runtime}");
        println!("cargo:rustc-link-lib=static=gnat-user");
    }
}
