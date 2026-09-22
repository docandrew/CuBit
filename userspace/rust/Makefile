# Invoke through nix develop. Cargo builds Rust only; GNU ld attaches the same
# CCL ELF metadata and stack contract used by native Ada applications.
TARGET := x86_64-unknown-none
MANIFEST := ../ccl/build/manifest/ccl-manifest
CATALOG := ../ccl/catalogs/native-runtime-services.ccl
LIB := build/cargo/$(TARGET)/release/libcubit_rust_probe.a
CARGO := cargo --config 'source.crates-io.replace-with="cubit-vendor"' --config 'source.cubit-vendor.directory="$(CUBIT_RUST_VENDOR)"'

.PHONY: all
all:
	mkdir -p build
	cd ../../kernel && alr exec -- gprbuild -p -P ../userspace/allocator/allocator_native.gpr
	$(MANIFEST) $(CATALOG) probe/manifest.ccl --rust-output build/bindings.rs > build/manifest.S
	$(MANIFEST) $(CATALOG) probe/denied.ccl > build/denied.S
	as --64 build/manifest.S -o build/manifest.o
	as --64 build/denied.S -o build/denied.o
	CUBIT_BINDINGS_DIR=$(CURDIR)/build $(CARGO) build --locked --offline --release -p cubit-rust-probe
	ld -nostdlib -static -z noexecstack -z stack-size=1048576 -T ../c/link.ld --start-group $(LIB) ../allocator/build/native-lib/libcubit_allocator.a --end-group build/manifest.o -o build/rust-probe.app
	ld -nostdlib -static -z noexecstack -z stack-size=1048576 -T ../c/link.ld --start-group $(LIB) ../allocator/build/native-lib/libcubit_allocator.a --end-group build/denied.o -o build/rust-probe-denied.app

.PHONY: fonts-host fonts-native fonts-test
.PHONY: fonts-bench
fonts-bench:
	$(CARGO) run --locked --offline --release --target x86_64-unknown-linux-gnu -p cubit-fonts --example cache_bench

fonts-host:
	$(CARGO) build --locked --offline --release --target x86_64-unknown-linux-gnu -p cubit-fonts
	mkdir -p build/font-host
	cp build/cargo/x86_64-unknown-linux-gnu/release/libcubit_fonts.a build/font-host/

fonts-native:
	cd ../../kernel && alr exec -- gprbuild -p -P ../userspace/allocator/allocator_native.gpr
	$(CARGO) build --locked --offline --release -p cubit-fonts
	mkdir -p build/font-native
	printf '%s\n' 'CREATE build/font-native/libcubit_fonts.a' 'ADDLIB build/cargo/$(TARGET)/release/libcubit_fonts.a' 'ADDLIB ../allocator/build/native-lib/libcubit_allocator.a' 'SAVE' 'END' | ar -M

fonts-test:
	$(CARGO) test --locked --offline --release --target x86_64-unknown-linux-gnu -p cubit-fonts
