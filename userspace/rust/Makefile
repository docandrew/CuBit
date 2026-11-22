# Invoke through nix develop. Cargo builds Rust only; GNU ld attaches the same
# CCL ELF metadata and stack contract used by native Ada applications.
TARGET := x86_64-unknown-none
# The font engine is floating-point work: it builds for CuBit's userspace
# target (hardware SSE2 floating point, std/x86_64-unknown-cubit.json), with
# core and alloc built for it, not for the soft-float bootstrap target, where
# every float operation was a library call (~10x slower rasterization).
# Only integers and pointers cross its C ABI, so Ada links it unchanged.
FONT_TARGET := $(abspath std/x86_64-unknown-cubit.json)
MANIFEST := ../ccl/build/manifest/ccl-manifest
CATALOG := ../ccl/catalogs/native-runtime-services.ccl
LIB := build/cargo/$(TARGET)/release/libcubit_rust_probe.a
DENIED_LIB := build/cargo-denied/$(TARGET)/release/libcubit_rust_probe.a
# CuAlloc and the call census its system calls count into (the probes link
# neither libc.a nor libgnat-user.a, which carry the census themselves).
ALLOCATOR_LIBS := ../allocator/build/native-lib/libcubit_allocator.a ../allocator/build/calls-native-lib/libcubit_allocator_calls.a
# The emphasis face, from the same pinned ibm-plex package as Regular (also
# exported by flake.nix; derived here for shells entered before it was).
IBM_PLEX_SANS_SEMIBOLD_FONT ?= $(dir $(IBM_PLEX_SANS_FONT))IBMPlexSans-SemiBold.ttf
export IBM_PLEX_SANS_SEMIBOLD_FONT
# The checkout's absolute path is remapped to "." (as gnat-15.cgpr does for
# Ada), so the archives built here are the same bytes in every checkout.
REPO_ROOT := $(abspath ../..)
CARGO := cargo --config 'source.crates-io.replace-with="cubit-vendor"' --config 'source.cubit-vendor.directory="$(CUBIT_RUST_VENDOR)"' \
	--config 'target.x86_64-unknown-none.rustflags=["--remap-path-prefix=$(REPO_ROOT)=."]'
# The native GNAT configuration (GNAT 15.3, kernel/Makefile exports it): the
# allocator built here is the one user_runtime builds. Without it gprbuild
# compiles with alr's default GNAT and mixes that object into the library.
NATIVE_GPRBUILD_CONFIG := $(or $(CUBIT_NATIVE_GPRBUILD_CONFIG),$(shell $(MAKE) -s --no-print-directory -C ../../kernel print-native-gprbuild-config))

.PHONY: all
all:
	mkdir -p build
	cd ../../kernel && alr exec -- gprbuild $(NATIVE_GPRBUILD_CONFIG) -p -P ../userspace/allocator/allocator_native.gpr
	cd ../../kernel && alr exec -- gprbuild $(NATIVE_GPRBUILD_CONFIG) -p -P ../userspace/allocator/allocator_calls_native.gpr
	mkdir -p build/denied
	$(MANIFEST) $(CATALOG) probe/manifest.ccl --rust-output build/bindings.rs > build/manifest.S
	$(MANIFEST) $(CATALOG) probe/denied.ccl --rust-output build/denied/bindings.rs > build/denied.S
	as --64 build/manifest.S -o build/manifest.o
	as --64 build/denied.S -o build/denied.o
	# Each variant is built against its own manifest's generated bindings
	# (its accessors and startup check, CAP-003).
	CUBIT_BINDINGS_DIR=$(CURDIR)/build $(CARGO) build --locked --offline --release -p cubit-rust-probe
	CUBIT_BINDINGS_DIR=$(CURDIR)/build/denied $(CARGO) build --locked --offline --release -p cubit-rust-probe --target-dir build/cargo-denied
	# -u: the generated startup check is only weakly referenced by the start
	# code, which would not pull its archive member in.
	ld -nostdlib -static --build-id=sha1 -z noexecstack -z stack-size=1048576 -u __cubit_startup_check -T ../c/link.ld --start-group $(LIB) $(ALLOCATOR_LIBS) --end-group build/manifest.o -o build/rust-probe.app
	ld -nostdlib -static --build-id=sha1 -z noexecstack -z stack-size=1048576 -u __cubit_startup_check -T ../c/link.ld --start-group $(DENIED_LIB) $(ALLOCATOR_LIBS) --end-group build/denied.o -o build/rust-probe-denied.app
	# A deliberate mismatch: code built for manifest.ccl, linked with
	# denied.ccl's sections. Its startup check must stop it (CAP-003).
	ld -nostdlib -static --build-id=sha1 -z noexecstack -z stack-size=1048576 -u __cubit_startup_check -T ../c/link.ld --start-group $(LIB) $(ALLOCATOR_LIBS) --end-group build/denied.o -o build/rust-probe-mismatch.app

.PHONY: fonts-host fonts-native fonts-test
.PHONY: fonts-bench
fonts-bench:
	$(CARGO) run --locked --offline --release --target x86_64-unknown-linux-gnu -p cubit-fonts --example cache_bench

fonts-host:
	$(CARGO) build --locked --offline --release --target x86_64-unknown-linux-gnu -p cubit-fonts
	mkdir -p build/font-host
	cp build/cargo/x86_64-unknown-linux-gnu/release/libcubit_fonts.a build/font-host/

fonts-native:
	cd ../../kernel && alr exec -- gprbuild $(NATIVE_GPRBUILD_CONFIG) -p -P ../userspace/allocator/allocator_native.gpr
	RUSTC_BOOTSTRAP=1 $(CARGO) --config 'target.x86_64-unknown-cubit.rustflags=["--remap-path-prefix=$(REPO_ROOT)=."]' \
	  build --locked --offline --profile fonts -p cubit-fonts --features native --target $(FONT_TARGET) -Zjson-target-spec -Zbuild-std=core,alloc
	mkdir -p build/font-native
	printf '%s\n' 'CREATE build/font-native/libcubit_fonts.a' 'ADDLIB build/cargo/x86_64-unknown-cubit/fonts/libcubit_fonts.a' 'ADDLIB ../allocator/build/native-lib/libcubit_allocator.a' 'SAVE' 'END' | ar -M
	# memcpy, memmove, memset and memcmp are the runtime's (CuBit.String),
	# for the Rust code too. Rust's core brings weak copies of them
	# (compiler_builtins, one per archive member); this archive is linked
	# before the runtime, so the linker took those for the whole program.
	# Each starts three string instructions where the runtime's starts one
	# (PERF-011: 63 cycles against 26 for a 16-byte copy). Their members go;
	# the program then has one definition of each (tests/runtime-string/linked.sh).
	ar d build/font-native/libcubit_fonts.a $$(nm -A --defined-only build/font-native/libcubit_fonts.a 2>/dev/null \
	  | sed -n 's/^[^:]*:\([^:]*\):[0-9a-f]* [TW] \(memcpy\|memmove\|memset\|memcmp\)$$/\1/p' | sort -u)

fonts-test:
	$(CARGO) test --locked --offline --release --target x86_64-unknown-linux-gnu -p cubit-fonts
