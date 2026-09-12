# CuBitOS Makefile
GIT_HASH := $(shell git rev-parse HEAD)
BUILD_DATE := $(shell date)
SHADOW_SCHEDULING ?= 0
DEADLINE_TIMER_TEST ?= 0
LATENCY_TRACE ?= 0
ifneq ($(filter $(LATENCY_TRACE),0 1),$(LATENCY_TRACE))
$(error LATENCY_TRACE must be 0 or 1)
endif
# Measured adaptive scheduling defaults; zero remains available for A/B runs.
WAKEUP_SCHEDULING ?= 1
ONESHOT_SCHEDULING ?= 1
ifneq ($(filter $(ONESHOT_SCHEDULING),0 1),$(ONESHOT_SCHEDULING))
$(error ONESHOT_SCHEDULING must be 0 or 1)
endif
ifneq ($(filter $(WAKEUP_SCHEDULING),0 1),$(WAKEUP_SCHEDULING))
$(error WAKEUP_SCHEDULING must be 0 or 1)
endif
ifneq ($(filter $(DEADLINE_TIMER_TEST),0 1),$(DEADLINE_TIMER_TEST))
$(error DEADLINE_TIMER_TEST must be 0 or 1)
endif
ifneq ($(filter $(SHADOW_SCHEDULING),0 1),$(SHADOW_SCHEDULING))
$(error SHADOW_SCHEDULING must be 0 or 1)
endif

# Use Alire to get toolchain on PATH
ALR := alr exec --

# Interactive runs should exercise CuBit at native CPU speed.  Override with
# QEMU_ACCEL=tcg when instruction emulation is specifically desired; headless
# tests select their accelerator independently in tests/headless/run.sh.
QEMU_ACCEL ?= kvm
QEMU_ACCEL_ARGS := $(if $(strip $(QEMU_ACCEL)),-accel $(QEMU_ACCEL),)

all: cubit_kernel iso

# Runtime library used by the CuBit Kernel itself (not the runtime used by
# software running on it!)
build/libcubit.a:
	mkdir -p runtime/adalib
	mkdir -p build

# Text-substitute string literals used in build.ads
	-rm src/build.ads

# Generate a hash list of all the source files and then a root hash from that
	$(eval SRC_HASH = $(shell find src/ -type f -print0 | xargs -0 sha256sum | sha256sum))

	sed -e 's/SED_GIT_HASH/$(GIT_HASH)/' \
		-e 's/SED_BUILD_DATE/$(BUILD_DATE)/' \
		-e 's/SED_SHADOW_SCHEDULING/$(if $(filter 1,$(SHADOW_SCHEDULING)),True,False)/' \
		-e 's/SED_DEADLINE_TIMER_TEST/$(if $(filter 1,$(DEADLINE_TIMER_TEST)),True,False)/' \
		-e 's/SED_LATENCY_TRACE/$(if $(filter 1,$(LATENCY_TRACE)),True,False)/' \
		-e 's/SED_WAKEUP_SCHEDULING/$(if $(filter 1,$(WAKEUP_SCHEDULING)),True,False)/' \
		-e 's/SED_ONESHOT_SCHEDULING/$(if $(filter 1,$(ONESHOT_SCHEDULING)),True,False)/' \
		-e 's/SED_SRC_HASH/$(SRC_HASH)/' src/build.pre > src/build.ads

	$(ALR) gprbuild -Pkernel_runtime.gpr

# init usermode binary embedded in the kernel ELF object
build/init.bin: src/init.asm
	yasm --arch=x86 -fbin src/init.asm -o build/init.bin
#	ld --omagic -e start -Ttext 0 -o build/init.out build/init.o
#	objcopy -I binary -O elf64-x86-64 -B i386

# Maximum allowed stack usage for any single kernel function (bytes).
# Kernel stacks are 4KB (usable ~3.3KB). Leave headroom for interrupt frames.
MAX_STACK_USAGE := 2048

# kernel ELF object
cubit_kernel: build/libcubit.a build/init.bin
# Compile and bind only
	$(ALR) gprbuild -b -c -Pcubit.gpr
# Check kernel stack usage - fail if any function exceeds threshold.
# Excluded: boot-context functions that run on the 8MB boot stack, not
# on 4KB process kernel stacks.
	@awk -F'\t' '$$2+0 > $(MAX_STACK_USAGE) && \
		$$1 !~ /:kmain$$/ && \
		$$1 !~ /:[Cc]reatekernelthread$$/ && \
		$$1 !~ /:[Ss]tartkernelthread$$/ && \
		$$1 !~ /:[Ss]tart[Rr]eaper$$/ && \
		$$1 !~ /:[Aa]penter$$/ { \
		printf "STACK OVERFLOW RISK: %s uses %s bytes (max %d)\n", $$1, $$2, $(MAX_STACK_USAGE); \
		fail=1 } \
		END { if (fail) exit 1 }' build/*.su
# Link - GRUB fails to identify this as a multiboot object without the -n
	ld -n -o cubit_kernel -T linker.ld \
		build/*.o \
		-b binary build/init.bin \
		-Map cubit_kernel.map

# TODO: make sure kernel is multiboot-compliant with grub-file --is-x86-multiboot cubit_kernel

# tests/cubit_tests: tests/cubit_tests.adb
# 	-gnatA : ignore gnat.adc (we don't care about pragmas there for testing)
#	gnat make -gnatA -aI. tests/cubit_tests.adb -D tests/build

.PHONY : prove
prove:
	$(ALR) gnatprove -Pcubit.gpr --mode=all --level=0 --no-axiom-guard

# Legality is separate from proof coverage. Include every buildable source;
# unsupported hardware glue remains explicitly outside the SPARK core.
.PHONY : check-spark-boundaries prove-interrupt-state
check-spark-boundaries:
	$(ALR) gnatprove -Pcubit.gpr -U --subdirs=spark-legality --mode=check_all -j2

prove-interrupt-state:
	$(ALR) gnatprove -Pcubit.gpr -u interrupt_state.adb \
		--subdirs=proof-interrupt-state --mode=all --level=1 -j2
	@cat build/proof-interrupt-state/gnatprove/gnatprove.out

.PHONY : prove-lock-ownership
prove-lock-ownership:
	$(ALR) gnatprove -Pcubit.gpr -u locks.adb \
		--subdirs=proof-lock-ownership --mode=all --level=1 -j2
	@cat build/proof-lock-ownership/gnatprove/gnatprove.out

.PHONY : prove-process-lifetime
prove-process-lifetime:
	$(ALR) gnatprove -Pcubit.gpr -u process_lifetime.adb \
		--subdirs=proof-process-lifetime --mode=all --level=1 -j2
	@cat build/proof-process-lifetime/gnatprove/gnatprove.out

.PHONY : prove-frame-pins
prove-frame-pins:
	$(ALR) gnatprove -Pcubit.gpr -u frame_pins.adb \
		--subdirs=proof-frame-pins --mode=all --level=1 -j2
	@cat build/proof-frame-pins/gnatprove/gnatprove.out

.PHONY : prove-tlb-reclamation
prove-tlb-reclamation:
	$(ALR) gnatprove -Pcubit.gpr -u tlb_reclamation.adb \
		--subdirs=proof-tlb-reclamation --mode=all --level=1 -j2
	@cat build/proof-tlb-reclamation/gnatprove/gnatprove.out

.PHONY : test-reclamation-state
test-reclamation-state:
	$(ALR) gprbuild -p -P../tests/kernel-reclamation/reclamation_tests.gpr
	../tests/kernel-reclamation/build/main

.PHONY : test-locking
.PHONY : test-scheduler-shadow prove-scheduler-shadow
.PHONY : test-deadline-ownership prove-deadline-ownership
test-deadline-ownership:
	$(ALR) gprbuild -p -P../tests/deadline-ownership/deadlines.gpr
	../tests/deadline-ownership/build/main

prove-deadline-ownership:
	$(ALR) gnatprove -P../tests/deadline-ownership/deadlines.gpr -u proof_driver.adb \
		--level=1 --report=all --checks-as-errors=on -j2
	@cat ../tests/deadline-ownership/build/gnatprove/gnatprove.out

test-scheduler-shadow:
	$(ALR) gprbuild -p -P../tests/scheduler-shadow/shadow.gpr
	../tests/scheduler-shadow/build/main

prove-scheduler-shadow:
	$(ALR) gnatprove -P../tests/scheduler-shadow/shadow.gpr -u scheduling_shadow.adb \
		--level=1 --report=all --checks-as-errors=on -j2
	@cat ../tests/scheduler-shadow/build/gnatprove/gnatprove.out

.PHONY : test-execution-accounting prove-execution-accounting
test-execution-accounting:
	$(ALR) gprbuild -p -P../tests/execution-accounting/accounting.gpr
	../tests/execution-accounting/build/main

prove-execution-accounting:
	$(ALR) gnatprove -P../tests/execution-accounting/accounting.gpr -u proof_driver.adb \
		--level=1 --report=all --checks-as-errors=on -j2
	@cat ../tests/execution-accounting/build/gnatprove/gnatprove.out

.PHONY : test-scheduler-budgets prove-scheduler-budgets
test-scheduler-budgets:
	$(ALR) gprbuild -p -P../tests/scheduler-budgets/budgets.gpr
	../tests/scheduler-budgets/build/main

prove-scheduler-budgets:
	$(ALR) gnatprove -P../tests/scheduler-budgets/budgets.gpr -u scheduling_budgets.adb \
		--level=1 --report=all --checks-as-errors=on -j2
	@cat ../tests/scheduler-budgets/build/gnatprove/gnatprove.out

.PHONY : test-scheduler-turns prove-scheduler-turns
test-scheduler-turns:
	$(ALR) gprbuild -p -P../tests/scheduler-turns/turns.gpr
	../tests/scheduler-turns/build/main

prove-scheduler-turns:
	$(ALR) gnatprove -P../tests/scheduler-turns/turns.gpr -u scheduling_turns.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/scheduler-turns/build/gnatprove/gnatprove.out

.PHONY : test-scheduler-timing prove-scheduler-timing
test-scheduler-timing:
	$(ALR) gprbuild -p -P../tests/scheduler-timing/timing.gpr
	../tests/scheduler-timing/build/main

prove-scheduler-timing:
	$(ALR) gnatprove -P../tests/scheduler-timing/timing.gpr -u scheduler_timing.adb \
		--level=1 --report=all --checks-as-errors=on -j2
	@cat ../tests/scheduler-timing/build/gnatprove/gnatprove.out

.PHONY : test-doom-audio
test-doom-audio:
	bash ../tests/doom-audio/test.sh

test-locking:
	$(ALR) gprbuild -p -P../tests/kernel-locking/locking_tests.gpr
	timeout 30 ../tests/kernel-locking/build/main

.PHONY: test-ccl-cbor prove-ccl-cbor prove-ccl-cbor-core measure-ccl-cbor check-ccl-cbor-native
test-ccl-cbor:
	@test -n "$(CBOR_ADA_SRC)" || { echo "Run inside nix develop (CBOR_ADA_SRC missing)"; exit 1; }
	$(ALR) gprbuild -p -P../tests/ccl-cbor/upstream_tests.gpr
	../tests/ccl-cbor/build/upstream-off/test_cbor
	$(ALR) gprbuild -p -P../tests/ccl-cbor/evaluation.gpr
	../tests/ccl-cbor/build/on/main

prove-ccl-cbor-core:
	@test -n "$(CBOR_ADA_SRC)" || { echo "Run inside nix develop (CBOR_ADA_SRC missing)"; exit 1; }
	$(ALR) gnatprove -P../tests/ccl-cbor/evaluation.gpr \
		-u cbor-encoding.adb cbor-decoding.adb cbor-model.ads sample_profile.adb \
		--level=2 --timeout=30 --report=all --checks-as-errors=on -j2
	@cat ../tests/ccl-cbor/build/on/gnatprove/gnatprove.out

# Also reproduces upstream's property lemmas. Keep this separate from the
# core gate: three float lemma assertions currently remain unproved locally.
prove-ccl-cbor: prove-ccl-cbor-core
	$(ALR) gnatprove -P../tests/ccl-cbor/upstream_tests.gpr \
		-u cbor-encoding.adb cbor-decoding.adb cbor-properties.adb cbor-model.ads \
		--level=2 --timeout=30 --report=all --checks-as-errors=on -j2
	@cat ../tests/ccl-cbor/build/upstream-off/gnatprove/gnatprove.out

measure-ccl-cbor:
	$(ALR) gprbuild -p -P../tests/ccl-cbor/evaluation.gpr -XCBOR_CHECKS=off
	../tests/ccl-cbor/build/off/main

check-ccl-cbor-native:
	@test -n "$(CBOR_ADA_SRC)" || { echo "Run inside nix develop (CBOR_ADA_SRC missing)"; exit 1; }
	$(ALR) gprbuild -p -c -P../tests/ccl-cbor/native_probe.gpr

.PHONY: test-usb-optical prove-usb-optical test-usb-hid
test-usb-hid:
	bash ../tests/usb-optical/run-hid.sh

test-usb-optical:
	$(ALR) gprbuild -p -P../tests/usb-optical/usb_optical_tests.gpr
	../tests/usb-optical/build/main
	../tests/usb-optical/build/configuration_tests
	../tests/usb-optical/build/completion_tests
	../tests/usb-optical/build/iso_tests
	../tests/usb-optical/build/capability_tests
	../tests/usb-optical/build/dma_layout_tests
	../tests/usb-optical/build/port_tests
	../tests/usb-optical/build/legacy_tests

prove-usb-optical:
	$(ALR) gnatprove -P../tests/usb-optical/usb_optical_tests.gpr \
		-u usb_optical.adb usb_configurations.adb xhci_completions.adb iso_records.adb xhci_capabilities.adb xhci_dma_layout.adb xhci_ports.adb xhci_legacy.ads \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/usb-optical/build/gnatprove/gnatprove.out

.PHONY : test-tcp-session prove-tcp-session
test-network-authority:
	$(ALR) gprbuild -p -P../tests/network-authority/network_authority_tests.gpr
	../tests/network-authority/build/main

prove-network-authority:
	$(ALR) gnatprove -P../tests/network-authority/network_authority_tests.gpr \
		-u cubit-network_authority.adb network_grants.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/network-authority/build/gnatprove/gnatprove.out

.PHONY: network-check test-network-authority prove-network-authority
network-check: user_runtime
	mkdir -p ../userspace/apps/network-check/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/network-check/manifest.c -o ../userspace/apps/network-check/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/network-check/network_check.gpr
	cp ../userspace/apps/network-check/build/network-check.app isodir/boot/network-check.app

test-tcp-session:
	$(ALR) gprbuild -p -P../tests/tcp-session/tcp_session_tests.gpr
	../tests/tcp-session/build/main

prove-tcp-session:
	$(ALR) gnatprove -P../tests/tcp-session/tcp_session_tests.gpr \
		-u tcpsession.adb tcp_listeners.adb network_channel_handles.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/tcp-session/build/gnatprove/gnatprove.out

.PHONY : test-filesystem-policy
test-filesystem-policy:
	$(ALR) gprbuild -p -P../userspace/runtime/tests/directory-paths/paths_tests.gpr
	../userspace/runtime/tests/directory-paths/build/main
	$(ALR) gprbuild -p -P../userspace/runtime/tests/file-access/access_tests.gpr
	../userspace/runtime/tests/file-access/build/main
	$(ALR) gprbuild -p -P../tests/filesystem-rename/rename_tests.gpr
	../tests/filesystem-rename/build/main

.PHONY : prove-filesystem-policy
prove-filesystem-policy:
	$(ALR) gnatprove -P../userspace/runtime/tests/directory-paths/paths_tests.gpr \
		-u cubit-directory_paths.adb --level=2 --report=all --checks-as-errors=on -j2
	$(ALR) gnatprove -P../userspace/runtime/tests/file-access/access_tests.gpr \
		-u cubit-file_access.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../userspace/runtime/tests/directory-paths/build/gnatprove/gnatprove.out
	@cat ../userspace/runtime/tests/file-access/build/gnatprove/gnatprove.out
	$(ALR) gnatprove -P../tests/filesystem-rename/rename_tests.gpr \
		-u directory_blocks.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/filesystem-rename/build/gnatprove/gnatprove.out

# Focused proof gate for the capability-policy core.  This is intentionally
# small enough to run on every CI boot test rather than only in full proofs.
.PHONY : prove-capability-policy
prove-capability-policy:
	$(ALR) gnatprove -Pcubit.gpr -u capabilities-operations.adb \
		--mode=all --level=1 --function-sandboxing=off
	@cat build/gnatprove/gnatprove.out

.PHONY : prove-capability-derive
prove-capability-derive:
	$(ALR) gnatprove -Pcubit.gpr -u capabilities.ads \
		--mode=all --level=1 --function-sandboxing=off
	@cat build/gnatprove/gnatprove.out

.PHONY : prove-memory-grants
prove-memory-grants:
	$(ALR) gnatprove -Pcubit.gpr -u memory_grants.adb \
		--mode=all --level=1 --function-sandboxing=off --checks-as-errors=on -j2
	@cat build/gnatprove/gnatprove.out

.PHONY : clean
clean:
	$(ALR) gprclean -Pcubit.gpr
	$(ALR) gprclean -Pkernel_runtime.gpr
	-rm build/*.o
	-rm build/*.ali
	-rm -rf build/gnatdoc/*
	-rm -rf build/gnatprove/*
	-rm -rf build/gnathtml/*
	-rm isodir/boot/cubit_kernel
	-rm cubit_kernel
	-rm cubit_kernel.iso
	-rm cubit_laptop_live.iso
	-rm laptop_sata_disk.img
	-rm laptop_sata_run.img
	-rm isodir/boot/laptop-live-initrd.img
	-rm -rf laptop_live_staging laptop_live_isodir

# Build DOOM and copy ELF into ISO directory
.PHONY : doom
doom: user_runtime
	$(ALR) $(MAKE) -C ../userspace/c -f Makefile.doomgeneric
	cp ../userspace/c/doom_build/doom.elf isodir/boot/doom.elf

.PHONY : sameboy
sameboy: user_runtime
	$(ALR) $(MAKE) -C ../userspace/c -f Makefile.sameboy
	cp ../userspace/c/sameboy_build/sameboy.app isodir/boot/sameboy.app

.PHONY : netsurf
netsurf:
	../userspace/c/netsurf/build-freestanding-deps.sh
	cp ../userspace/c/netsurf/build/freestanding/netsurf.app isodir/boot/netsurf.app

.PHONY : sleep
sleep: user_runtime
	mkdir -p ../userspace/apps/sleep/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/sleep/manifest.c \
		-o ../userspace/apps/sleep/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/sleep/sleep.gpr
	cp ../userspace/apps/sleep/build/sleep.app isodir/boot/sleep.app

.PHONY : capability-test
capability-test: user_runtime
	mkdir -p ../userspace/apps/capability-test/build
	yasm -f elf64 ../userspace/apps/capability-test/syscall_registers.asm \
		-o ../userspace/apps/capability-test/build/syscall_registers.o
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/capability-test/manifest.c \
		-o ../userspace/apps/capability-test/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/capability-test/capability_test.gpr
	cp ../userspace/apps/capability-test/build/capability-test.app isodir/boot/capability-test.app

# Portable protocol checks and an unprivileged native desktop adversary.
.PHONY : desktop-check prepare-desktop-protocol test-desktop-protocol prove-desktop-protocol
desktop-check: user_runtime
	mkdir -p ../userspace/apps/desktop-check/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/desktop-check/manifest.c -o ../userspace/apps/desktop-check/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/apps/desktop-check/desktop_check.gpr
	cp ../userspace/apps/desktop-check/build/desktop-check.app isodir/boot/desktop-check.app

.PHONY: display-check
display-check: user_runtime
	mkdir -p ../userspace/apps/display-check/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/display-check/manifest.c -o ../userspace/apps/display-check/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/apps/display-check/display_check.gpr
	cp ../userspace/apps/display-check/build/display-check.app isodir/boot/display-check.app

prepare-desktop-protocol:
	mkdir -p ../tests/desktop-protocol/build/source
	cp ../userspace/runtime/gnat/cubit.ads ../userspace/runtime/gnat/cubit-desktop_protocol.ads \
		../userspace/runtime/gnat/cubit-grant_references.ads \
		../userspace/runtime/gnat/cubit-display_protocol.ads \
		../userspace/runtime/gnat/cubit-display_protocol.adb \
		../userspace/runtime/gnat/cubit-desktop_protocol.adb ../tests/desktop-protocol/build/source/

test-desktop-protocol: prepare-desktop-protocol
	$(ALR) gprbuild -p -P ../tests/desktop-protocol/protocol.gpr
	../tests/desktop-protocol/build/main

prove-desktop-protocol: prepare-desktop-protocol
	$(ALR) gnatprove -P ../tests/desktop-protocol/protocol.gpr \
		-u cubit-desktop_protocol.adb cubit-display_protocol.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/desktop-protocol/build/obj/gnatprove/gnatprove.out

.PHONY: prepare-desktop-state test-desktop-state prove-desktop-state
prepare-desktop-state:
	mkdir -p ../tests/desktop-state/build/source
	cp ../userspace/runtime/gnat/cubit.ads \
		../userspace/runtime/gnat/cubit-click_sequences.ads \
		../userspace/runtime/gnat/cubit-presentation_state.ads \
		../userspace/runtime/gnat/cubit-presentation_state.adb \
		../userspace/runtime/gnat/cubit-click_sequences.adb ../tests/desktop-state/build/source/

test-desktop-state: prepare-desktop-state
	$(ALR) gprbuild -p -P ../tests/desktop-state/state.gpr
	../tests/desktop-state/build/main

prove-desktop-state: prepare-desktop-state
	$(ALR) gnatprove -P ../tests/desktop-state/state.gpr \
		-u cubit-click_sequences.adb presentation_proof.ads --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/desktop-state/build/obj/gnatprove/gnatprove.out

.PHONY: prepare-ipc-request-ids test-ipc-request-ids prove-ipc-request-ids
prepare-ipc-request-ids:
	mkdir -p ../tests/ipc-request-ids/build/source
	cp src/ipc_request_ids.ads src/ipc_request_ids.adb ../tests/ipc-request-ids/build/source/

test-ipc-request-ids: prepare-ipc-request-ids
	$(ALR) gprbuild -p -P ../tests/ipc-request-ids/ids.gpr
	../tests/ipc-request-ids/build/main

prove-ipc-request-ids: prepare-ipc-request-ids
	$(ALR) gnatprove -P ../tests/ipc-request-ids/ids.gpr \
		-u ipc_request_ids.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/ipc-request-ids/build/obj/gnatprove/gnatprove.out

# Deterministic typed input stream publisher used by the headless recovery gate.
.PHONY : input-stress
input-stress: user_runtime
	mkdir -p ../userspace/apps/input-stress/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/input-stress/manifest.c \
		-o ../userspace/apps/input-stress/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/input-stress/input_stress.gpr
	cp ../userspace/apps/input-stress/build/input-stress.app isodir/boot/input-stress.app

.PHONY : ipctest-server
ipctest-server: user_runtime
	mkdir -p ../userspace/apps/ipctest-server/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/ipctest-server/manifest.c \
		-o ../userspace/apps/ipctest-server/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/ipctest-server/ipctest_server.gpr
	cp ../userspace/apps/ipctest-server/build/ipctest-server.app isodir/boot/ipctest-server.app

.PHONY : ipctest-client
ipctest-client: user_runtime
	mkdir -p ../userspace/apps/ipctest-client/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/ipctest-client/manifest.c \
		-o ../userspace/apps/ipctest-client/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/ipctest-client/ipctest_client.gpr
	cp ../userspace/apps/ipctest-client/build/ipctest-client.app isodir/boot/ipctest-client.app
	cp ../userspace/apps/ipctest-client/build/ipctest-departing.app isodir/boot/ipctest-departing.app

.PHONY : bench-load
.PHONY : bench-storage
bench-storage: user_runtime
	mkdir -p ../userspace/apps/bench-storage/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/bench-storage/manifest.c -o ../userspace/apps/bench-storage/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/apps/bench-storage/bench_storage.gpr
	cp ../userspace/apps/bench-storage/build/bench-storage.app isodir/boot/bench-storage.app

bench-load: user_runtime
	mkdir -p ../userspace/apps/bench-load/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/bench-load/manifest.c -o ../userspace/apps/bench-load/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/apps/bench-load/bench_load.gpr
	cp ../userspace/apps/bench-load/build/bench-load.app isodir/boot/bench-load.app
	cp ../userspace/apps/bench-load/build/bench-input-load.app isodir/boot/bench-input-load.app
	cp ../userspace/apps/bench-load/build/bench-cpu.app isodir/boot/bench-cpu.app

.PHONY : bench-audio
bench-audio: user_runtime
	mkdir -p ../userspace/apps/bench-audio/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/bench-audio/manifest.c -o ../userspace/apps/bench-audio/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/apps/bench-audio/bench_audio.gpr
	cp ../userspace/apps/bench-audio/build/bench-audio.app isodir/boot/bench-audio.app

.PHONY : bench-input
bench-input: user_runtime
	mkdir -p ../userspace/apps/bench-input/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/bench-input/manifest.c -o ../userspace/apps/bench-input/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/apps/bench-input/bench_input.gpr
	cp ../userspace/apps/bench-input/build/bench-input.app isodir/boot/bench-input.app

.PHONY : bench-ipc-server
bench-ipc-server: user_runtime
	mkdir -p ../userspace/apps/bench-ipc-server/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/bench-ipc-server/manifest.c \
		-o ../userspace/apps/bench-ipc-server/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/bench-ipc-server/bench_ipc_server.gpr
	cp ../userspace/apps/bench-ipc-server/build/bench-ipc-server.app isodir/boot/bench-ipc-server.app

.PHONY : bench-ipc-client
bench-ipc-client: user_runtime
	mkdir -p ../userspace/apps/bench-ipc-client/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/bench-ipc-client/manifest.c \
		-o ../userspace/apps/bench-ipc-client/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/bench-ipc-client/bench_ipc_client.gpr
	cp ../userspace/apps/bench-ipc-client/build/bench-ipc-client.app isodir/boot/bench-ipc-client.app

# Build the freestanding CuBit Control Language VM self-test app.
.PHONY : ccl-run
ccl-run:
	$(ALR) gprbuild -P ../userspace/ccl/ccl_run.gpr -p

.PHONY : ccl-control
ccl-control: user_runtime
	mkdir -p ../userspace/ccl/apps/ccl-control/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/ccl/apps/ccl-control/manifest.c -o ../userspace/ccl/apps/ccl-control/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/ccl/apps/ccl-control/ccl_control.gpr
	cp ../userspace/ccl/apps/ccl-control/build/ccl-control.app isodir/boot/ccl-control.app
	mkdir -p isodir/licenses
	cp ../userspace/ccl/remote/CBOR_LICENSE.txt isodir/licenses/cbor_ada.txt

.PHONY : test-ccl-remote ccl-web-preview test-ccl-web-preview
test-ccl-remote:
	$(ALR) gprbuild -p -P../tests/ccl-remote/remote.gpr
	../tests/ccl-remote/build/main
	../tests/ccl-remote/build/host_tests
	../tests/ccl-remote/build/periodic_tests

.PHONY: prove-ccl-interpreter-host
prove-ccl-interpreter-host:
	$(ALR) gnatprove -P../tests/ccl-remote/remote.gpr \
		-u ccl-language.adb ccl-catalog.adb interpreter_host.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/ccl-remote/build/gnatprove/gnatprove.out

.PHONY: prove-ccl-periodic
prove-ccl-periodic:
	$(ALR) gnatprove -P../tests/ccl-remote/remote.gpr --subdirs=periodic-proof \
		-u ccl-periodic_programs.adb interpreter_host.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/ccl-remote/build/periodic-proof/gnatprove/gnatprove.out

.PHONY: prove-ccl-remote ccl-remote-lab
prove-ccl-remote:
	$(ALR) gnatprove -P../tests/ccl-remote/remote.gpr --subdirs=wire-proof \
		-u control_http.adb control_wire.adb ccl-control.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/ccl-remote/build/wire-proof/gnatprove/gnatprove.out

CCL_LAB_ACCEL ?= kvm
ccl-remote-lab: ccl-control clock netstack
	../tests/headless/run.sh --test ccl-remote --accel $(CCL_LAB_ACCEL) --timeout 3600 --keep-logs

ccl-web-preview:
	cd ../userspace/ccl/tools/ccl-observatory && npm run preview

test-ccl-web-preview:
	cd ../userspace/ccl/tools/ccl-observatory && npm test

.PHONY : ccl-ui-preview
ccl-ui-preview:
	$(ALR) gprbuild -P ../userspace/ccl/ccl_ui_preview.gpr -p
	cd .. && userspace/ccl/build/ccl-ui-preview/ccl-ui-preview

.PHONY : ccl-workspace-test ccl-workspace-prove
ccl-workspace-test:
	$(ALR) gprbuild -p -P ../tests/ccl-workspace/workspace_tests.gpr
	../tests/ccl-workspace/build/main

ccl-workspace-prove:
	$(ALR) gnatprove -P ../tests/ccl-workspace/workspace_tests.gpr \
		-u ccl_workspace_names.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/ccl-workspace/build/gnatprove/gnatprove.out

.PHONY : ui-editor-test
ui-editor-test:
	$(ALR) gprbuild -P ../userspace/lib/ui/tests/editor/editor_test.gpr -p
	../userspace/lib/ui/tests/editor/build/main

.PHONY : ccl-test-native
ccl-test-native:
	$(ALR) gprbuild -P ../userspace/ccl/ccl_native.gpr -p
	../userspace/ccl/build/native/main

.PHONY : ccl-vm
.PHONY : ccl-manifest
ccl-manifest:
	$(ALR) gprbuild -p -P ../userspace/ccl/ccl_manifest.gpr

.PHONY : test-ccl-manifests
test-ccl-manifests: ccl-vm
	$(ALR) python3 ../tests/ccl-manifests/test-manifests.py -v

ccl-vm: user_runtime ccl-manifest
	mkdir -p ../userspace/ccl/apps/ccl-vm/build/generated
	../userspace/ccl/build/manifest/ccl-manifest \
		../userspace/ccl/catalogs/bootstrap-services.ccl \
		../userspace/ccl/apps/ccl-vm/manifest.ccl \
		--ada-output ../userspace/ccl/apps/ccl-vm/build/generated/ccl_manifest_bindings.ads \
		> ../userspace/ccl/apps/ccl-vm/build/manifest.S
	$(ALR) gcc -c ../userspace/ccl/apps/ccl-vm/build/manifest.S \
		-o ../userspace/ccl/apps/ccl-vm/build/manifest.o
	$(ALR) gprbuild -P ../userspace/ccl/apps/ccl-vm/ccl_vm.gpr
	cp ../userspace/ccl/apps/ccl-vm/build/ccl-vm.app isodir/boot/ccl-vm.app

# Native CCL editor/compiler/debugger application.
.PHONY : ccl-workbench
ccl-workbench: user_runtime
	mkdir -p ../userspace/ccl/apps/ccl-workbench/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/ccl/apps/ccl-workbench/manifest.c \
		-o ../userspace/ccl/apps/ccl-workbench/build/manifest.o
	$(ALR) gprbuild -P ../userspace/ccl/apps/ccl-workbench/ccl_workbench.gpr
	cp ../userspace/ccl/apps/ccl-workbench/build/ccl-workbench.app isodir/boot/ccl-workbench.app

# Test-only service used to exercise CCL's typed host-import IPC adapter.
.PHONY : ccl-test-host
ccl-test-host: user_runtime
	mkdir -p ../userspace/ccl/services/test-host/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/ccl/services/test-host/manifest.c \
		-o ../userspace/ccl/services/test-host/build/manifest.o
	$(ALR) gprbuild -P ../userspace/ccl/services/test-host/ccl_test_host.gpr
	cp ../userspace/ccl/services/test-host/build/ccl-test-host.svc isodir/boot/ccl-test-host.svc

# Capability-addressed monotonic clock service.
.PHONY : clock
clock: user_runtime ../userspace/ccl/interfaces/clock.ccl-interface
	mkdir -p ../userspace/services/clock/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/services/clock/manifest.c \
		-o ../userspace/services/clock/build/manifest.o
	$(ALR) gprbuild -P ../userspace/services/clock/clock.gpr
	$(ALR) objcopy \
		--add-section .cubit.interfaces=../userspace/ccl/interfaces/clock.ccl-interface \
		--set-section-flags .cubit.interfaces=readonly,data \
		../userspace/services/clock/build/clock.svc isodir/boot/clock.svc

# Build userspace Ada runtime (needed by filesystem server)
.PHONY : user_runtime
user_runtime:
	$(ALR) $(MAKE) -C ../userspace/c build/crt0.o
	mkdir -p ../userspace/runtime/build
	mkdir -p ../userspace/runtime/adalib
	$(ALR) gprbuild -P ../userspace/runtime/user_runtime.gpr

# Build the userspace filesystem server
.PHONY : filesystem
filesystem: user_runtime
	mkdir -p ../userspace/services/filesystem/build
	$(ALR) gprbuild -P ../userspace/services/filesystem/filesystem.gpr
	cp ../userspace/services/filesystem/build/filesystem.svc isodir/boot/filesystem.svc

# Build the userspace ATA driver
.PHONY : ata
ata: user_runtime
	mkdir -p ../userspace/services/ata/build
	$(ALR) gprbuild -P ../userspace/services/ata/ata.gpr
	cp ../userspace/services/ata/build/ata.drv isodir/boot/ata.drv

# Build the userspace NVMe driver
.PHONY : nvme
nvme: user_runtime
	mkdir -p ../userspace/services/nvme/build
	$(ALR) gprbuild -P ../userspace/services/nvme/nvme_drv.gpr
	cp ../userspace/services/nvme/build/nvme.drv isodir/boot/nvme.drv

# Build the userspace virtio-net driver
.PHONY : virtio-net
virtio-net: user_runtime
	mkdir -p ../userspace/services/virtio-net/build
	$(ALR) gprbuild -P ../userspace/services/virtio-net/virtio_net.gpr
	cp ../userspace/services/virtio-net/build/virtio-net.drv isodir/boot/virtio-net.drv

# Build the userspace virtio-gpu driver
.PHONY : virtio-gpu
virtio-gpu: user_runtime
	mkdir -p ../userspace/services/virtio-gpu/build
	$(ALR) gprbuild -P ../userspace/services/virtio-gpu/virtio_gpu.gpr
	cp ../userspace/services/virtio-gpu/build/virtio-gpu.drv isodir/boot/virtio-gpu.drv

# Build the userspace network stack service
.PHONY : netstack
netstack: user_runtime
	mkdir -p ../userspace/services/netstack/build
	$(ALR) gprbuild -P ../userspace/services/netstack/netstack.gpr
	cp ../userspace/services/netstack/build/netstack.svc isodir/boot/netstack.svc

# Build the userspace wget app
.PHONY : wget
wget: user_runtime
	mkdir -p ../userspace/apps/wget/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/wget/manifest.c \
		-o ../userspace/apps/wget/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/wget/wget.gpr
	cp ../userspace/apps/wget/build/wget.app isodir/boot/wget.app

# Build the unified hardware inventory and diagnostics app
.PHONY : devices
devices: user_runtime
	mkdir -p ../userspace/apps/devices/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/devices/manifest.c \
		-o ../userspace/apps/devices/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/devices/devices.gpr
	cp ../userspace/apps/devices/build/devices.app isodir/boot/devices.app

# Build the read-only, authority-scoped filesystem browser.
.PHONY : files
files: user_runtime
	mkdir -p ../userspace/apps/files/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/files/manifest.c \
		-o ../userspace/apps/files/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/files/files.gpr
	cp ../userspace/apps/files/build/files.app isodir/boot/files.app

# Build the live writable-storage diagnostic app.
.PHONY : storage-check
storage-check: user_runtime filesystem-scope-check
	mkdir -p ../userspace/apps/storage-check/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/storage-check/manifest.c \
		-o ../userspace/apps/storage-check/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/storage-check/storage_check.gpr
	cp ../userspace/apps/storage-check/build/storage-check.app isodir/boot/storage-check.app

.PHONY : filesystem-scope-check
filesystem-scope-check: user_runtime
	mkdir -p ../userspace/apps/filesystem-scope-check/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/filesystem-scope-check/manifest.c \
		-o ../userspace/apps/filesystem-scope-check/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/filesystem-scope-check/filesystem_scope_check.gpr
	cp ../userspace/apps/filesystem-scope-check/build/filesystem-scope-check.app isodir/boot/filesystem-scope-check.app

# Build the first desktop shell client
.PHONY : desktop-shell
desktop-shell: user_runtime
	mkdir -p ../userspace/apps/desktop-shell/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/desktop-shell/manifest.c \
		-o ../userspace/apps/desktop-shell/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/desktop-shell/desktop_shell.gpr
	cp ../userspace/apps/desktop-shell/build/desktop-shell.app isodir/boot/desktop-shell.app

# Build the userspace process manager service
.PHONY : procmgr
procmgr: user_runtime
	mkdir -p ../userspace/services/procmgr/build
	$(ALR) gprbuild -P ../userspace/services/procmgr/procmgr.gpr
	cp ../userspace/services/procmgr/build/procmgr.svc isodir/boot/procmgr.svc

# Build the userspace HDA audio driver
.PHONY : hda
hda: user_runtime
	mkdir -p ../userspace/services/hda/build
	$(ALR) gprbuild -P ../userspace/services/hda/hda_drv.gpr
	cp ../userspace/services/hda/build/hda.drv isodir/boot/hda.drv

# Build the userspace audio mixer service
.PHONY : mixer
mixer: user_runtime
	mkdir -p ../userspace/services/mixer/build
	$(ALR) gprbuild -P ../userspace/services/mixer/mixer.gpr
	cp ../userspace/services/mixer/build/mixer.svc isodir/boot/mixer.svc

# Build the userspace PS/2 keyboard + mouse driver
.PHONY : ps2
ps2: user_runtime
	mkdir -p ../userspace/services/ps2/build
	$(ALR) gprbuild -P ../userspace/services/ps2/ps2.gpr
	cp ../userspace/services/ps2/build/ps2.drv isodir/boot/ps2.drv

# Build the userspace xHCI controller driver
.PHONY : xhci
xhci: user_runtime
	mkdir -p ../userspace/services/xhci/build
	$(ALR) gprbuild -P ../userspace/services/xhci/xhci.gpr
	cp ../userspace/services/xhci/build/xhci.drv isodir/boot/xhci.drv

# Build the userspace config store service
.PHONY : config
config: user_runtime
	mkdir -p ../userspace/services/config/build
	$(ALR) gprbuild -P ../userspace/services/config/config.gpr
	cp ../userspace/services/config/build/config.svc isodir/boot/config.svc

# Build the userspace log store service
.PHONY : logstore
logstore: user_runtime
	mkdir -p ../userspace/services/logstore/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/services/logstore/manifest.c \
		-o ../userspace/services/logstore/build/manifest.o
	$(ALR) gprbuild -P ../userspace/services/logstore/logstore.gpr
	cp ../userspace/services/logstore/build/logstore.svc isodir/boot/logstore.svc

# Build the userspace desktop compositor/session service prototype
.PHONY : desktop
desktop: user_runtime
	mkdir -p ../userspace/services/desktop/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/services/desktop/manifest.c \
		-o ../userspace/services/desktop/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/services/desktop/desktop.gpr -XCUBIT_DISPLAY_TEST_MODE=$(CUBIT_DISPLAY_TEST_MODE)
	cp ../userspace/services/desktop/$(PRESENTATION_BUILD_DIR)/desktop.svc isodir/boot/desktop.svc

# Build the userspace display scanout service prototype
.PHONY : display
CUBIT_DISPLAY_TEST_MODE ?= production
PRESENTATION_BUILD_DIR = $(if $(filter delayed,$(CUBIT_DISPLAY_TEST_MODE)),build-delayed,build)
display: user_runtime
	mkdir -p ../userspace/services/display/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/services/display/manifest.c \
		-o ../userspace/services/display/build/manifest.o
	$(ALR) gprbuild -p -P ../userspace/services/display/display.gpr -XCUBIT_DISPLAY_TEST_MODE=$(CUBIT_DISPLAY_TEST_MODE)
	cp ../userspace/services/display/$(PRESENTATION_BUILD_DIR)/display.svc isodir/boot/display.svc

# Build the userspace network manager service
.PHONY : netmgr
netmgr: user_runtime
	mkdir -p ../userspace/services/netmgr/build
	$(ALR) gprbuild -P ../userspace/services/netmgr/netmgr.gpr
	cp ../userspace/services/netmgr/build/netmgr.svc isodir/boot/netmgr.svc

# Build the userspace device manager
.PHONY : devmgr
devmgr: user_runtime
	mkdir -p ../userspace/services/devmgr/build
	$(ALR) gprbuild -P ../userspace/services/devmgr/devmgr.gpr
	cp ../userspace/services/devmgr/build/devmgr.svc isodir/boot/devmgr.svc

# Build the userspace shell app
.PHONY : shell
shell: user_runtime
	mkdir -p ../userspace/apps/shell/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/shell/manifest.c \
		-o ../userspace/apps/shell/build/manifest.o
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/shell/manifest_access.c \
		-o ../userspace/apps/shell/build/manifest_access.o
	$(ALR) gprbuild -P ../userspace/apps/shell/shell.gpr
	cp ../userspace/apps/shell/build/shell.app isodir/boot/shell.app

# Files that live on ext2 disk images (stage-2 content).
# Disk images rebuild automatically when any of these change.
DOOM_WAD ?= ../doom1.wad

DISK_CONTENTS := isodir/boot/shell.app isodir/boot/doom.elf isodir/boot/netsurf.app isodir/boot/sleep.app \
	isodir/boot/sameboy.app ../userspace/c/sameboy_build/test.gb \
	isodir/boot/capability-test.app \
	isodir/boot/wget.app isodir/boot/devices.app isodir/boot/files.app \
	isodir/boot/ccl-vm.app isodir/boot/ccl-workbench.app \
	isodir/boot/ccl-test-host.svc \
	isodir/boot/clock.svc \
	isodir/boot/desktop-shell.app \
	isodir/boot/ipctest-server.app isodir/boot/ipctest-client.app \
	isodir/boot/bench-ipc-server.app isodir/boot/bench-ipc-client.app \
	isodir/boot/logstore.svc isodir/boot/display.svc isodir/boot/desktop.svc \
	../init.conf ../userspace/motd.txt $(DOOM_WAD)

# Every stage-1 binary copied into initrd.img.  Keep this dependency list next
# to the staging targets: copying an old service beside a newly rebuilt driver
# can silently violate their capability/protocol contract.
INITRD_SERVICES := filesystem ata nvme netstack virtio-net virtio-gpu hda \
	mixer ps2 xhci config netmgr procmgr devmgr

# Binaries used by the graphical desktop session. Keep this as a separate
# staging target so run-desktop can build/copy them into isodir first, then
# rebuild the ext2 disk image from those freshly staged files.
.PHONY : desktop-session-content
desktop-session-content: $(INITRD_SERVICES) desktop display logstore \
	desktop-shell devices files ccl-workbench clock doom netsurf sameboy

# Create ext2 ATA disk image containing doom1.wad, doom.elf, init.conf, shell.app
# Requires: mke2fs, debugfs (from e2fsprogs)
ata_disk.img: $(DISK_CONTENTS)
	dd if=/dev/zero of=ata_disk.img bs=1K count=65536
	mke2fs -t ext2 ata_disk.img
	debugfs -w -R "write $(DOOM_WAD) doom1.wad" ata_disk.img
	debugfs -w -R "write ../userspace/motd.txt motd.txt" ata_disk.img
	debugfs -w -R "write ../init.conf init.conf" ata_disk.img
	debugfs -w -R "write isodir/boot/shell.app shell.app" ata_disk.img
	debugfs -w -R "write isodir/boot/doom.elf doom.elf" ata_disk.img
	debugfs -w -R "write isodir/boot/sameboy.app sameboy.app" ata_disk.img
	debugfs -w -R "mkdir sameboy" ata_disk.img
	debugfs -w -R "write ../userspace/c/sameboy_build/test.gb sameboy/00.gb" ata_disk.img
	debugfs -w -R "write isodir/boot/netsurf.app netsurf.app" ata_disk.img
	debugfs -w -R "write isodir/boot/sleep.app sleep.app" ata_disk.img
	debugfs -w -R "write isodir/boot/capability-test.app capability-test.app" ata_disk.img
	debugfs -w -R "write isodir/boot/wget.app wget.app" ata_disk.img
	debugfs -w -R "write isodir/boot/devices.app devices.app" ata_disk.img
	debugfs -w -R "write isodir/boot/files.app files.app" ata_disk.img
	debugfs -w -R "write isodir/boot/desktop-shell.app desktop-shell.app" ata_disk.img
	debugfs -w -R "write isodir/boot/ipctest-server.app ipctest-server.app" ata_disk.img
	debugfs -w -R "write isodir/boot/ipctest-client.app ipctest-client.app" ata_disk.img
	debugfs -w -R "write isodir/boot/bench-ipc-server.app bench-ipc-server.app" ata_disk.img
	debugfs -w -R "write isodir/boot/bench-ipc-client.app bench-ipc-client.app" ata_disk.img
	debugfs -w -R "write isodir/boot/ccl-vm.app ccl-vm.app" ata_disk.img
	debugfs -w -R "write isodir/boot/ccl-workbench.app ccl-workbench.app" ata_disk.img
	debugfs -w -R "write isodir/boot/ccl-test-host.svc ccl-test-host.svc" ata_disk.img
	debugfs -w -R "write isodir/boot/clock.svc clock.svc" ata_disk.img
	debugfs -w -R "write isodir/boot/logstore.svc logstore.svc" ata_disk.img
	debugfs -w -R "write isodir/boot/display.svc display.svc" ata_disk.img
	debugfs -w -R "write isodir/boot/desktop.svc desktop.svc" ata_disk.img
	debugfs -w -R "write ../userspace/motd.txt config.dat" ata_disk.img

# Create ext2 NVMe disk image containing motd.txt, doom1.wad, doom.elf, init.conf, shell.app, wget.app
# Requires: mke2fs, debugfs (from e2fsprogs)
nvme_disk.img: $(DISK_CONTENTS)
	dd if=/dev/zero of=nvme_disk.img bs=1K count=65536
	mke2fs -t ext2 nvme_disk.img
	debugfs -w -R "mkdir work" nvme_disk.img
	debugfs -w -R "write ../userspace/motd.txt motd.txt" nvme_disk.img
	debugfs -w -R "write $(DOOM_WAD) doom1.wad" nvme_disk.img
	debugfs -w -R "write isodir/boot/doom.elf doom.elf" nvme_disk.img
	debugfs -w -R "write isodir/boot/sameboy.app sameboy.app" nvme_disk.img
	debugfs -w -R "mkdir sameboy" nvme_disk.img
	debugfs -w -R "write ../userspace/c/sameboy_build/test.gb sameboy/00.gb" nvme_disk.img
	debugfs -w -R "write isodir/boot/netsurf.app netsurf.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/sleep.app sleep.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/capability-test.app capability-test.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/logstore.svc logstore.svc" nvme_disk.img
	debugfs -w -R "write ../init.conf init.conf" nvme_disk.img
	debugfs -w -R "write isodir/boot/shell.app shell.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/wget.app wget.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/devices.app devices.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/files.app files.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/desktop-shell.app desktop-shell.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/ipctest-server.app ipctest-server.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/ipctest-client.app ipctest-client.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/bench-ipc-server.app bench-ipc-server.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/bench-ipc-client.app bench-ipc-client.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/ccl-vm.app ccl-vm.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/ccl-workbench.app ccl-workbench.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/ccl-test-host.svc ccl-test-host.svc" nvme_disk.img
	debugfs -w -R "write isodir/boot/clock.svc clock.svc" nvme_disk.img
	debugfs -w -R "write isodir/boot/display.svc display.svc" nvme_disk.img
	debugfs -w -R "write isodir/boot/desktop.svc desktop.svc" nvme_disk.img
	debugfs -w -R "write ../userspace/motd.txt config.dat" nvme_disk.img

# Convenience aliases for manual rebuilds (e.g. make ata_disk)
.PHONY : ata_disk nvme_disk
ata_disk: ata_disk.img
nvme_disk: nvme_disk.img

# Minimal stage-2 disk for the authority non-amplification boot test.  Keeping
# this separate from nvme_disk.img prevents CI from building desktop, browser,
# and game payloads that the security test never launches.
CAPABILITY_SECURITY_DISK := capability_security_disk.img

$(CAPABILITY_SECURITY_DISK): capability-test ../tests/headless/init-capability-security.conf
	dd if=/dev/zero of=$(CAPABILITY_SECURITY_DISK) bs=1K count=16384
	mke2fs -t ext2 -F $(CAPABILITY_SECURITY_DISK)
	debugfs -w -R "write ../tests/headless/init-capability-security.conf init.conf" $(CAPABILITY_SECURITY_DISK)
	debugfs -w -R "write isodir/boot/capability-test.app capability-test.app" $(CAPABILITY_SECURITY_DISK)

# Build the real kernel, normal stage-1 service graph, and only the stage-2
# payload needed by the capability test.  The recipe creates the initrd after
# all services have been staged so a parallel caller cannot capture a partial
# archive.
CAPABILITY_SECURITY_STAGE1 := filesystem ata nvme netstack virtio-net \
	virtio-gpu hda mixer ps2 config netmgr procmgr devmgr

.PHONY : capability-security-image
capability-security-image: cubit_kernel $(CAPABILITY_SECURITY_STAGE1) $(CAPABILITY_SECURITY_DISK)
	$(MAKE) initrd
	cp cubit_kernel isodir/boot/
	grub-mkrescue -o cubit_kernel.iso isodir

# Create CPIO newc initrd image containing stage-1 service binaries.
# The kernel parses this at boot and loads services from it.
# Stage-2 content (shell.app, doom.elf, motd.txt, etc.) lives on disk.
.PHONY : initrd
initrd: $(INITRD_SERVICES)
	@rm -rf ramdisk_staging
	mkdir -p ramdisk_staging
	-cp isodir/boot/devmgr.svc ramdisk_staging/
	-cp isodir/boot/filesystem.svc ramdisk_staging/
	-cp isodir/boot/ata.drv ramdisk_staging/
	-cp isodir/boot/nvme.drv ramdisk_staging/
	-cp isodir/boot/netstack.svc ramdisk_staging/
	-cp isodir/boot/virtio-net.drv ramdisk_staging/
	-cp isodir/boot/virtio-gpu.drv ramdisk_staging/
	-cp isodir/boot/hda.drv ramdisk_staging/
	-cp isodir/boot/mixer.svc ramdisk_staging/
	-cp isodir/boot/ps2.drv ramdisk_staging/
	-cp isodir/boot/xhci.drv ramdisk_staging/
	-cp isodir/boot/config.svc ramdisk_staging/
	-cp isodir/boot/netmgr.svc ramdisk_staging/
	-cp isodir/boot/procmgr.svc ramdisk_staging/
	-cp ../system.conf ramdisk_staging/
	cd ramdisk_staging && ls | cpio -o -H newc > ../isodir/boot/initrd.img
	rm -rf ramdisk_staging

# Self-contained, read-only userspace image for real-hardware bring-up.  Keep
# this separate from the normal two-stage image: the laptop must reach a
# desktop and diagnostic UI before AHCI or any other block driver works.
LAPTOP_LIVE_STAGE1 := filesystem ata nvme netstack virtio-net virtio-gpu hda \
	mixer ps2 xhci config netmgr procmgr devmgr
LAPTOP_LIVE_STAGE2 := logstore clock display desktop ccl-workbench devices files \
	storage-check doom sameboy
LAPTOP_LIVE_RW_DISK := laptop_live_rw.img

# Native USB optical edition. Keep the original initrd-only laptop image as
# an independent fallback; no ATA/NVMe driver is admitted in this profile.
.PHONY: usb-live-iso run-usb-live
usb-live-iso: cubit_kernel $(LAPTOP_LIVE_STAGE1) $(LAPTOP_LIVE_STAGE2) $(LAPTOP_LIVE_RW_DISK)
	bash ../tests/usb-optical/build-live.sh "$(DOOM_WAD)"

run-usb-live: usb-live-iso
	qemu-system-x86_64 $(QEMU_ACCEL_ARGS) -machine q35 -cpu host -smp 4 -m 8G \
		-nodefaults -device VGA -device qemu-xhci,id=xhci \
		-drive file=cubit_laptop_usb.iso,if=none,id=livecd,media=cdrom,format=raw,readonly=on \
		-device usb-bot,id=usbcd,bus=xhci.0,port=1 \
		-device scsi-cd,bus=usbcd.0,lun=0,drive=livecd,bootindex=1 \
		-device usb-mouse,bus=xhci.0,port=2 \
		-audiodev sdl,id=snd0 -device ich9-intel-hda -device hda-output,audiodev=snd0 \
		-serial file:/tmp/cubit-usb-live-serial.log -display sdl -no-reboot

$(LAPTOP_LIVE_RW_DISK):
	dd if=/dev/zero of=$(LAPTOP_LIVE_RW_DISK) bs=1M count=8
	mke2fs -t ext2 -F -m 0 $(LAPTOP_LIVE_RW_DISK)
	debugfs -w -R "mkdir work" $(LAPTOP_LIVE_RW_DISK)
	debugfs -w -R "mkdir tmp" $(LAPTOP_LIVE_RW_DISK)

.PHONY : laptop-live-initrd
laptop-live-initrd: $(LAPTOP_LIVE_STAGE1) $(LAPTOP_LIVE_STAGE2) $(LAPTOP_LIVE_RW_DISK)
	rm -rf laptop_live_staging
	mkdir -p laptop_live_staging
	cp isodir/boot/devmgr.svc laptop_live_staging/
	cp isodir/boot/filesystem.svc laptop_live_staging/
	cp isodir/boot/ata.drv laptop_live_staging/
	cp isodir/boot/nvme.drv laptop_live_staging/
	cp isodir/boot/netstack.svc laptop_live_staging/
	cp isodir/boot/virtio-net.drv laptop_live_staging/
	cp isodir/boot/virtio-gpu.drv laptop_live_staging/
	cp isodir/boot/hda.drv laptop_live_staging/
	cp isodir/boot/mixer.svc laptop_live_staging/
	cp isodir/boot/ps2.drv laptop_live_staging/
	cp isodir/boot/xhci.drv laptop_live_staging/
	cp isodir/boot/config.svc laptop_live_staging/
	cp isodir/boot/netmgr.svc laptop_live_staging/
	cp isodir/boot/procmgr.svc laptop_live_staging/
	cp isodir/boot/logstore.svc laptop_live_staging/
	cp isodir/boot/clock.svc laptop_live_staging/
	cp isodir/boot/display.svc laptop_live_staging/
	cp isodir/boot/desktop.svc laptop_live_staging/
	cp isodir/boot/ccl-workbench.app laptop_live_staging/
	cp isodir/boot/devices.app laptop_live_staging/
	cp isodir/boot/files.app laptop_live_staging/
	cp isodir/boot/storage-check.app laptop_live_staging/
	cp isodir/boot/doom.elf laptop_live_staging/
	cp $(DOOM_WAD) laptop_live_staging/doom1.wad
	cp $(LAPTOP_LIVE_RW_DISK) laptop_live_staging/live-rw.ext2
	cp ../tests/hardware/init-laptop-live.conf laptop_live_staging/init.conf
	cp ../tests/hardware/system-live.conf laptop_live_staging/system.conf
	cd laptop_live_staging && ls | cpio -o -H newc > ../isodir/boot/laptop-live-initrd.img
	rm -rf laptop_live_staging

# A blank SATA disk is attached to QEMU through Q35's ICH9 AHCI controller.
# Runtime boots must use a copy so driver development cannot damage the base.
laptop_sata_disk.img:
	dd if=/dev/zero of=laptop_sata_disk.img bs=1M count=64
	mke2fs -t ext2 -F laptop_sata_disk.img

.PHONY : laptop-live-iso
laptop-live-iso: cubit_kernel laptop-live-initrd
	rm -rf laptop_live_isodir
	cp -a isodir laptop_live_isodir
	cp cubit_kernel laptop_live_isodir/boot/cubit_kernel
	sed -i 's/^set default=.*/set default=7/' laptop_live_isodir/boot/grub/grub.cfg
	grub-mkrescue -o cubit_laptop_live.iso laptop_live_isodir
	rm -rf laptop_live_isodir

# Refresh only the xHCI/devmgr binaries inside an already-built laptop image.
# This keeps controller bring-up iterations short without weakening the normal
# from-source laptop-live-iso target used for distributable artifacts.
.PHONY : laptop-live-refresh-xhci
laptop-live-refresh-xhci: xhci devmgr
	test -f isodir/boot/laptop-live-initrd.img
	rm -rf laptop_live_refresh
	mkdir -p laptop_live_refresh
	cd laptop_live_refresh && cpio -id -F ../isodir/boot/laptop-live-initrd.img
	cp isodir/boot/xhci.drv laptop_live_refresh/xhci.drv
	cp isodir/boot/devmgr.svc laptop_live_refresh/devmgr.svc
	cd laptop_live_refresh && ls | cpio -o -H newc > ../isodir/boot/laptop-live-initrd.img
	rm -rf laptop_live_refresh laptop_live_isodir
	cp -a isodir laptop_live_isodir
	cp cubit_kernel laptop_live_isodir/boot/cubit_kernel
	sed -i 's/^set default=.*/set default=7/' laptop_live_isodir/boot/grub/grub.cfg
	grub-mkrescue -o cubit_laptop_live.iso laptop_live_isodir
	rm -rf laptop_live_isodir

# Q35 is not a cycle-accurate Sunrise Point model, but AHCI, xHCI, HDA, PCIe
# bridges, and USB HID exercise the standardized interfaces needed by the
# target laptop.  The first USB milestone intentionally enumerates one boot
# mouse; retain Q35's built-in PS/2 keyboard until hubs/multiple devices and
# the USB boot-keyboard protocol are implemented.
LAPTOP_RAM ?= 8G
LAPTOP_CPU ?= Skylake-Client-v4
LAPTOP_CPUS ?= 4
LAPTOP_INPUT_DEVICES ?= -device usb-mouse,bus=xhci.0
LAPTOP_MONITOR ?= /tmp/cubit-laptop-monitor.sock
.PHONY : run-laptop
run-laptop: laptop-live-iso laptop_sata_disk.img
	$(MAKE) run-laptop-fast

# Reuse an already-built live ISO while iterating on QEMU behavior.  The
# regular run-laptop target remains the safe, from-source entry point.
.PHONY : run-laptop-fast
run-laptop-fast:
	test -f cubit_laptop_live.iso
	test -f laptop_sata_disk.img
	rm -f serial_output.log laptop_sata_run.img $(LAPTOP_MONITOR)
	cp laptop_sata_disk.img laptop_sata_run.img
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu $(LAPTOP_CPU) \
		-smp $(LAPTOP_CPUS) \
		-m $(LAPTOP_RAM) \
		-nodefaults \
		-drive file=cubit_laptop_live.iso,if=none,id=livecd,media=cdrom,format=raw,readonly=on \
		-device ide-cd,drive=livecd,bus=ide.1 \
		-drive file=laptop_sata_run.img,if=none,id=sata0,format=raw \
		-device ide-hd,drive=sata0,bus=ide.0 \
		-device VGA \
		-device qemu-xhci,id=xhci \
		$(LAPTOP_INPUT_DEVICES) \
		-audiodev sdl,id=snd0 \
		-device ich9-intel-hda \
		-device hda-output,audiodev=snd0 \
		-device pcie-root-port,id=net_root,bus=pcie.0 \
		-netdev user,id=net0 \
		-device e1000e,bus=net_root,netdev=net0 \
		-serial file:serial_output.log \
		-monitor unix:$(LAPTOP_MONITOR),server,nowait \
		-display gtk,zoom-to-fit=on \
		-no-reboot

# Keep xHCI visible while using Q35's built-in i8042 controller for interactive
# desktop testing before CuBit has a USB HID service.  Do not instantiate an
# additional i8042 or USB HID device: either can become QEMU's selected pointer
# while CuBit is listening to the platform controller's IRQ.
.PHONY : run-laptop-interactive-fast
run-laptop-interactive-fast:
	$(MAKE) run-laptop-fast LAPTOP_INPUT_DEVICES=""

iso: cubit_kernel initrd
	cp cubit_kernel isodir/boot/
	grub-mkrescue -o cubit_kernel.iso isodir

# Build everything including DOOM, then create ISO and disk images
.PHONY : world
world: cubit_kernel doom sameboy netsurf sleep capability-test storage-check ipctest-server ipctest-client bench-ipc-server bench-ipc-client ccl-test-host clock ccl-vm ccl-workbench filesystem ata nvme virtio-net virtio-gpu netstack wget devices files desktop-shell procmgr shell hda mixer ps2 xhci config netmgr logstore display desktop devmgr initrd ata_disk.img nvme_disk.img iso

.PHONY : headless-test
headless-test: world
	bash ../tests/headless/run.sh --test boot-shell-nvme
	bash ../tests/headless/run.sh --test async-ipc
	bash ../tests/headless/run.sh --test bench-ipc
	bash ../tests/headless/run.sh --test ccl-vm
	bash ../tests/headless/run.sh --test ccl-workbench
	bash ../tests/headless/run.sh --test capability-security
	bash ../tests/headless/run.sh --test storage-grants
	bash ../tests/headless/run.sh --test audio-grants
	bash ../tests/headless/run.sh --test desktop-display
	bash ../tests/headless/run.sh --test input-stream
	bash ../tests/headless/run.sh --test virtio-gpu
	bash ../tests/headless/run.sh --test virtio-vga-primary

# Run QEMU with NVMe + virtio-net + HDA audio, shell (everything)
.PHONY : run
run: nvme_disk.img
	@rm -f serial_output.log net.pcap
	sed -i 's/^set default=.*/set default=4/' isodir/boot/grub/grub.cfg
	grub-mkrescue -o cubit_kernel.iso isodir
	sed -i 's/^set default=.*/set default=0/' isodir/boot/grub/grub.cfg
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu Broadwell \
		-smp 4 \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-serial file:serial_output.log \
		-display gtk,zoom-to-fit=on \
		-drive file=nvme_disk.img,if=none,id=nvme0,format=raw \
		-device nvme,serial=cubitnvme,drive=nvme0 \
		-device virtio-net-pci,netdev=net0 \
		-device virtio-gpu-pci \
		-netdev user,id=net0 \
		-object filter-dump,id=f0,netdev=net0,file=net.pcap \
		-audiodev sdl,id=snd0 \
		-device intel-hda \
		-device hda-output,audiodev=snd0 \
		-no-reboot

# Run QEMU with an init profile that starts display.svc + desktop.svc.
# This keeps GUI iteration out of the CuBASIC/CLI launch path.
.PHONY : run-desktop
run-desktop: cubit_kernel desktop-session-content
	$(MAKE) nvme_disk.img
	$(MAKE) initrd
	cp cubit_kernel isodir/boot/cubit_kernel
	@rm -f serial_output.log net.pcap desktop_disk.img
	cp nvme_disk.img desktop_disk.img
	debugfs -w -R "mkdir work" desktop_disk.img
	-debugfs -w -R "rm init.conf" desktop_disk.img >/dev/null 2>&1
	debugfs -w -R "write ../tests/headless/init-desktop-session.conf init.conf" desktop_disk.img
	sed -i 's/^set default=.*/set default=4/' isodir/boot/grub/grub.cfg
	grub-mkrescue -o cubit_kernel.iso isodir
	sed -i 's/^set default=.*/set default=0/' isodir/boot/grub/grub.cfg
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu Broadwell \
		-smp 4 \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-serial file:serial_output.log \
		-vga none \
		-device virtio-vga,xres=1024,yres=768 \
		-display gtk,zoom-to-fit=on \
		-drive file=desktop_disk.img,if=none,id=nvme0,format=raw \
		-device nvme,serial=cubitnvme,drive=nvme0 \
		-device virtio-net-pci,netdev=net0 \
		-netdev user,id=net0 \
		-object filter-dump,id=f0,netdev=net0,file=net.pcap \
		-audiodev sdl,id=snd0 \
		-device intel-hda \
		-device hda-output,audiodev=snd0 \
		-no-reboot

# Fast desktop launcher for UI iteration. This intentionally has no build
# prerequisites: it reuses the current cubit_kernel.iso and nvme_disk.img,
# then overlays the current staged desktop binaries into a temporary disk
# image. Use run-desktop when binaries need to be rebuilt first.
.PHONY : run-desktop-fast
run-desktop-fast:
	@test -f cubit_kernel.iso || (echo "missing cubit_kernel.iso; run make -C kernel run-desktop once first" && false)
	@test -f nvme_disk.img || (echo "missing nvme_disk.img; run make -C kernel run-desktop once first" && false)
	@rm -f serial_output.log net.pcap desktop_disk.img
	cp nvme_disk.img desktop_disk.img
	-debugfs -w -R "rm init.conf" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm desktop.svc" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm display.svc" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm netstack.svc" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm desktop-shell.app" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm devices.app" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm files.app" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm doom.elf" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm netsurf.app" desktop_disk.img >/dev/null 2>&1
	debugfs -w -R "write ../tests/headless/init-desktop-session.conf init.conf" desktop_disk.img
	debugfs -w -R "write isodir/boot/desktop.svc desktop.svc" desktop_disk.img
	debugfs -w -R "write isodir/boot/display.svc display.svc" desktop_disk.img
	debugfs -w -R "write isodir/boot/netstack.svc netstack.svc" desktop_disk.img
	debugfs -w -R "write isodir/boot/desktop-shell.app desktop-shell.app" desktop_disk.img
	debugfs -w -R "write isodir/boot/devices.app devices.app" desktop_disk.img
	debugfs -w -R "write isodir/boot/files.app files.app" desktop_disk.img
	debugfs -w -R "write isodir/boot/doom.elf doom.elf" desktop_disk.img
	debugfs -w -R "write isodir/boot/netsurf.app netsurf.app" desktop_disk.img
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu Broadwell \
		-smp 4 \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-serial file:serial_output.log \
		-vga none \
		-device virtio-vga,xres=1024,yres=768 \
		-display gtk,zoom-to-fit=on \
		-drive file=desktop_disk.img,if=none,id=nvme0,format=raw \
		-device nvme,serial=cubitnvme,drive=nvme0 \
		-device virtio-net-pci,netdev=net0 \
		-netdev user,id=net0 \
		-object filter-dump,id=f0,netdev=net0,file=net.pcap \
		-audiodev sdl,id=snd0 \
		-device intel-hda \
		-device hda-output,audiodev=snd0 \
		-no-reboot

# Interactive desktop launcher for UI inspection. This keeps the GTK window for
# manual interaction and exposes monitor sockets so screenshots can be captured
# from the current guest framebuffer without disturbing the session.
QEMU_QMP ?= /tmp/cubit-qmp.sock
QEMU_MONITOR ?= /tmp/cubit-monitor.sock
QEMU_SCREENSHOT ?= /tmp/cubit-screen.ppm

.PHONY : run-desktop-inspect
run-desktop-inspect:
	@test -f cubit_kernel.iso || (echo "missing cubit_kernel.iso; run make -C kernel run-desktop once first" && false)
	@test -f nvme_disk.img || (echo "missing nvme_disk.img; run make -C kernel run-desktop once first" && false)
	@rm -f serial_output.log net.pcap desktop_disk.img $(QEMU_QMP) $(QEMU_MONITOR) $(QEMU_SCREENSHOT)
	cp nvme_disk.img desktop_disk.img
	-debugfs -w -R "rm init.conf" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm desktop.svc" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm display.svc" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm netstack.svc" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm desktop-shell.app" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm devices.app" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm files.app" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm doom.elf" desktop_disk.img >/dev/null 2>&1
	-debugfs -w -R "rm netsurf.app" desktop_disk.img >/dev/null 2>&1
	debugfs -w -R "write ../tests/headless/init-desktop-session.conf init.conf" desktop_disk.img
	debugfs -w -R "write isodir/boot/desktop.svc desktop.svc" desktop_disk.img
	debugfs -w -R "write isodir/boot/display.svc display.svc" desktop_disk.img
	debugfs -w -R "write isodir/boot/netstack.svc netstack.svc" desktop_disk.img
	debugfs -w -R "write isodir/boot/desktop-shell.app desktop-shell.app" desktop_disk.img
	debugfs -w -R "write isodir/boot/devices.app devices.app" desktop_disk.img
	debugfs -w -R "write isodir/boot/files.app files.app" desktop_disk.img
	debugfs -w -R "write isodir/boot/doom.elf doom.elf" desktop_disk.img
	debugfs -w -R "write isodir/boot/netsurf.app netsurf.app" desktop_disk.img
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu Broadwell \
		-smp 4 \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-serial file:serial_output.log \
		-vga none \
		-device virtio-vga,xres=1024,yres=768 \
		-display gtk,zoom-to-fit=on \
		-qmp unix:$(QEMU_QMP),server=on,wait=off \
		-monitor unix:$(QEMU_MONITOR),server,nowait \
		-drive file=desktop_disk.img,if=none,id=nvme0,format=raw \
		-device nvme,serial=cubitnvme,drive=nvme0 \
		-device virtio-net-pci,netdev=net0 \
		-netdev user,id=net0 \
		-object filter-dump,id=f0,netdev=net0,file=net.pcap \
		-audiodev sdl,id=snd0 \
		-device intel-hda \
		-device hda-output,audiodev=snd0 \
		-no-reboot

.PHONY : capture-desktop
capture-desktop:
	@test -S $(QEMU_MONITOR) || (echo "missing QEMU monitor socket: $(QEMU_MONITOR); run make run-desktop-inspect first" && false)
	@command -v socat >/dev/null 2>&1 || (echo "missing socat; install it or send 'screendump $(QEMU_SCREENSHOT)' to $(QEMU_MONITOR)" && false)
	printf 'screendump $(QEMU_SCREENSHOT)\n' | socat - UNIX-CONNECT:$(QEMU_MONITOR)
	@echo "wrote $(QEMU_SCREENSHOT)"

# Experimental primary VirtIO VGA path. This is useful for debugging the
# virtio-gpu display backend, but it is not the default run target yet because
# this path can currently wedge QEMU hard on some hosts.
.PHONY : run-virtio-vga
run-virtio-vga: nvme_disk.img
	@rm -f serial_output.log net.pcap
	sed -i 's/^set default=.*/set default=4/' isodir/boot/grub/grub.cfg
	grub-mkrescue -o cubit_kernel.iso isodir
	sed -i 's/^set default=.*/set default=0/' isodir/boot/grub/grub.cfg
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu Broadwell \
		-smp 4 \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-serial file:serial_output.log \
		-vga none \
		-device virtio-vga,xres=1024,yres=768 \
		-display gtk,zoom-to-fit=on \
		-drive file=nvme_disk.img,if=none,id=nvme0,format=raw \
		-device nvme,serial=cubitnvme,drive=nvme0 \
		-device virtio-net-pci,netdev=net0 \
		-netdev user,id=net0 \
		-object filter-dump,id=f0,netdev=net0,file=net.pcap \
		-audiodev sdl,id=snd0 \
		-device intel-hda \
		-device hda-output,audiodev=snd0 \
		-no-reboot

.PHONY : docs
docs:
	$(ALR) gnatdoc -l -d --preserve-source-formatting -w -P cubit.gpr

.PHONY : html
html:
#	gnathtml.pl -l1 -I build/ -d -f -o documentation/html src/*.adb src/*.ads
	$(ALR) gnathtml.pl -cc grey -sc red -o build/gnathtml src/*.adb src/*.ads
