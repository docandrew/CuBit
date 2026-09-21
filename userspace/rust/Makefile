# Invoke through nix develop. Cargo builds Rust only; GNU ld attaches the same
# CCL ELF metadata and stack contract used by native Ada applications.
TARGET := x86_64-unknown-none
MANIFEST := ../ccl/build/manifest/ccl-manifest
CATALOG := ../ccl/catalogs/native-runtime-services.ccl
LIB := build/cargo/$(TARGET)/release/libcubit_rust_probe.a

.PHONY: all
all:
	mkdir -p build
	cd ../../kernel && alr exec -- gprbuild -p -P ../userspace/allocator/allocator_native.gpr
	$(MANIFEST) $(CATALOG) probe/manifest.ccl --rust-output build/bindings.rs > build/manifest.S
	$(MANIFEST) $(CATALOG) probe/denied.ccl > build/denied.S
	as --64 build/manifest.S -o build/manifest.o
	as --64 build/denied.S -o build/denied.o
	CUBIT_BINDINGS_DIR=$(CURDIR)/build cargo build --locked --offline --release -p cubit-rust-probe
	ld -nostdlib -static -z noexecstack -z stack-size=1048576 -T ../c/link.ld --start-group $(LIB) ../allocator/build/native-lib/libcubit_allocator.a --end-group build/manifest.o -o build/rust-probe.app
	ld -nostdlib -static -z noexecstack -z stack-size=1048576 -T ../c/link.ld --start-group $(LIB) ../allocator/build/native-lib/libcubit_allocator.a --end-group build/denied.o -o build/rust-probe-denied.app
