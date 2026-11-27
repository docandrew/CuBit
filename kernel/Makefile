# CuBitOS Makefile
GIT_HASH := $(shell git rev-parse HEAD)
# The source date (BUILD_EPOCH, the commit time), not the wall clock, so the
# same sources build the same kernel (KERN-012).
BUILD_DATE = $(shell date -u -d @$(BUILD_EPOCH))
SHADOW_SCHEDULING ?= 0
DEADLINE_TIMER_TEST ?= 0
LATENCY_TRACE ?= 0
ifneq ($(filter $(LATENCY_TRACE),0 1),$(LATENCY_TRACE))
$(error LATENCY_TRACE must be 0 or 1)
endif
# Measured adaptive scheduling defaults; zero remains available for A/B runs.
WAKEUP_SCHEDULING ?= 1
ONESHOT_SCHEDULING ?= 1
# Idle CPUs take ready work from busy CPUs (docs/threads.md).
WORK_STEALING ?= 1
# The futex pause (IPC-010, UMWAIT): off by default until proved on the
# laptop; USER_WAIT_PAUSE=1 make -C kernel world turns it on.
USER_WAIT_PAUSE ?= 0
# Address-space tags (PCIDs, src/tlb_context.ads): off by default until proved
# on the laptop. No test host here has PCID (AMD KVM guests, TCG), so the
# tagged path has not run yet; TAGGED_TLB=1 make -C kernel world turns it on.
TAGGED_TLB ?= 0
# System call census (ABI-002, src/syscall_census.ads); diagnostic builds only.
SYSCALL_CENSUS ?= 0
# Interrupt-entry SMAP probe (SEC-026, Interrupts.Probe_User_Access): test
# kernels only, for headless --test alignment-flag.
SMAP_ENTRY_PROBE ?= 0
# Kernel-map probe (KERN-014, Process.Map_Probe): test kernels only, for
# headless --test kernel-map-probe. Off by default.
KERN014_PROBE ?= 0
# Debug-register probe (DBG-001, src/debug_register_probe.ads): test kernels
# only, for headless --test mov-ss-debug. Off by default.
DEBUG_REGISTER_PROBE ?= 0
# Clock-stall probe (src/clock_stall_probe.ads): test kernels only, for
# headless --test clock-keeper (CPU 0 runs no timer work for a while). Off by
# default.
CLOCK_STALL_PROBE ?= 0
# TSC skew probe (src/tsc_sync.adb): test kernels only, for headless --test
# tsc-skew and tsc-skew-noadjust (CPU 1's counter moved before its TSC_ADJUST
# step and warp check). Off by default.
TSC_SKEW_PROBE ?= 0
# Records in the kernel log ring (LOG-004, src/kernel_log.ads): at most
# Kernel_Log_Publication.Maximum_Records (4096). Tests shrink it to overflow
# the ring before logstore starts (tests/kernel-log-ring/run.sh); src/build.ads
# is regenerated on every build, so the next build without it is full size.
KERNEL_LOG_RECORDS ?= 4096
ifeq ($(shell test "$(KERNEL_LOG_RECORDS)" -ge 1 2>/dev/null && test "$(KERNEL_LOG_RECORDS)" -le 4096 2>/dev/null && echo ok),)
$(error KERNEL_LOG_RECORDS must be a number in 1 .. 4096)
endif
ifneq ($(filter $(WORK_STEALING),0 1),$(WORK_STEALING))
$(error WORK_STEALING must be 0 or 1)
endif
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

# Use Alire to get toolchain on PATH: gprbuild, gnatprove's GNAT and hosted
# test programs (alire.toml pins gnat_native to exactly 15.3.1).
ALR := alr exec --

# The kernel compiler (KERN-012): exactly the GCC/GNAT 15.3 that native CuBit
# Ada uses, nixpkgs' musl cross GNAT (flake.nix exports CUBIT_GNAT and
# CUBIT_BINUTILS), through the same gprbuild configuration,
# userspace/toolchain/gnat-15.cgpr, with the kernel's own runtime. Never the
# GNAT that Alire happens to have installed. KERNEL_GNAT_IDENT is the .comment
# string this compiler writes into every object it compiles; the linked kernel
# must carry no other.
KERNEL_GNAT_VERSION := 15.3.0
KERNEL_GNAT_TRIPLET := x86_64-unknown-linux-musl
KERNEL_GNAT_IDENT := GCC: (GNU) $(KERNEL_GNAT_VERSION)
KERNEL_GCC = $(CUBIT_GNAT)/bin/$(KERNEL_GNAT_TRIPLET)-gcc
KERNEL_LD = $(CUBIT_BINUTILS)/bin/ld
KERNEL_GPRBUILD = $(ALR) gprbuild --config=$(abspath ../userspace/toolchain/gnat-15.cgpr) \
	-XCUBIT_GNAT=$(CUBIT_GNAT) -XCUBIT_BINUTILS=$(CUBIT_BINUTILS) \
	-XCUBIT_GPRBIND=$(GPRBUILD_LIBEXEC)/gprbind -XCUBIT_GPRLIB=$(GPRBUILD_LIBEXEC)/gprlib \
	-XCUBIT_RUNTIME=$(abspath runtime) -XCUBIT_SOURCE_ROOT=$(abspath ..)

# Stop before compiling anything unless the kernel compiler is that GNAT.
define check_kernel_gnat
@test -n "$(CUBIT_GNAT)" -a -n "$(CUBIT_BINUTILS)" || \
  { echo "KERN-012: CUBIT_GNAT/CUBIT_BINUTILS are not set; build inside nix develop" >&2; exit 1; }
@version=$$($(KERNEL_GCC) -dumpfullversion) || \
  { echo "KERN-012: no kernel compiler at $(KERNEL_GCC)" >&2; exit 1; }; \
if [ "$$version" != "$(KERNEL_GNAT_VERSION)" ]; then \
  echo "KERN-012: kernel compiler $(KERNEL_GCC) is GCC $$version, not the pinned $(KERNEL_GNAT_VERSION)" >&2; exit 1; fi
endef

# After linking: every object in the image was compiled by that GNAT (a stale
# object from another compiler, or a different build of 15.3, fails here).
define check_kernel_image_gnat
@idents=$$(readelf -p .comment $(1) | sed -n 's/^ *\[ *[0-9a-f]*\] *//p' | sort -u); \
if [ "$$idents" != "$(KERNEL_GNAT_IDENT)" ]; then \
  echo "KERN-012: $(1) contains code from compilers other than '$(KERNEL_GNAT_IDENT)':" >&2; \
  echo "$$idents" >&2; echo "(objects from an earlier compiler: make clean)" >&2; rm -f $(1); exit 1; fi
endef

# Interactive runs should exercise CuBit at native CPU speed.  Override with
# QEMU_ACCEL=tcg when instruction emulation is specifically desired; headless
# tests select their accelerator independently in tests/headless/run.sh.
QEMU_ACCEL ?= kvm
QEMU_ACCEL_ARGS := $(if $(strip $(QEMU_ACCEL)),-accel $(QEMU_ACCEL),)
# The guest CPU of the interactive run targets (run, run-desktop and its
# variants). tests/headless/run.sh --test desktop-session boots with this
# same model (print-run-cpu). +invtsc exposes the host's invariant TSC, as
# real hardware and every KVM headless test do (QEMU masks it otherwise):
# only then does the kernel publish the clock conversion user space reads.
# Without it user space falls back to the kernel's millisecond word while
# the kernel's timer waits keep the HPET's epoch; sleeps then return at
# once, and virtio-gpu.drv's boot commands time out (run-desktop never
# reached the desktop). TCG drops the flag with a warning.
QEMU_RUN_CPU ?= Broadwell,vendor=GenuineIntel,+invtsc

.PHONY : print-run-cpu
print-run-cpu:
	@echo $(QEMU_RUN_CPU)

all: cubit_kernel iso

# A step make cannot see the inputs of runs every time: FORCE. Its tool is
# its own up-to-date check, and leaves an unchanged output untouched.
.PHONY: FORCE
FORCE:

# ccl_program DIR,PROGRAM,PREREQUISITES (BLD-001): a component ccl-build
# builds from DIR/component.ccl, staged as isodir/boot/PROGRAM.
#   - ccl-build runs only when one of the component's inputs changed: each
#     run writes the files and directories in the checkout it read (ccl-build
#     --inputs), and build/ccl/PROGRAM.d makes them the prerequisites of
#     the step's stamp. What it reads outside the checkout is named by the
#     command line (build/ccl-build.args, rewritten only when it changes:
#     the toolchains' store paths, the signing directory) and the signing
#     key, or is a Storehouse entry, which never changes. ccl-build stays
#     the judge: a step whose inputs are unchanged by content is answered
#     by the cache, and an output whose bytes are the same is left
#     untouched, so neither DIR/build/PROGRAM nor anything after it is
#     redone.
#   - isodir/boot/PROGRAM is a copy, made again only when DIR/build/PROGRAM
#     changed. The sign step (SIGN_PROGRAMS) strips and signs it in place.
#   - PREREQUISITES and the phony tool targets are order-only: built first;
#     the files they write that ccl-build reads are inputs like any other
#     (CCL_TOOL_OUTPUTS below).
# The host signing directory (SIGN-001; see SIGN_PROGRAMS below).
CUBIT_SIGNING ?= $(or $(XDG_CONFIG_HOME),$(HOME)/.config)/cubit/signing
CCL_STEP_INPUTS = ../userspace/ccl/build/ccl-build/ccl-build build/ccl-build.args \
	$(wildcard $(CUBIT_SIGNING)/builder.der $(CUBIT_SIGNING)/builder.key)
define ccl_program
build/ccl/$(2).stamp: $$(CCL_STEP_INPUTS) | ccl-build user_runtime ccl-manifest $(3)
	@mkdir -p build/ccl $(1)/build
	$$(CCL_BUILD) --inputs build/ccl/$(2).inputs $(1)/component.ccl
	@{ printf '%s: \\\n' $$@; sed 's|.*|  ../& \\|' build/ccl/$(2).inputs; echo; \
	  sed 's|.*|../&:|' build/ccl/$(2).inputs; } > build/ccl/$(2).d
	@touch $$@
-include build/ccl/$(2).d
$(1)/build/$(2): build/ccl/$(2).stamp ;
isodir/boot/$(2): $(1)/build/$(2)
	cp $$< $$@
endef

# Built by its script (BLD-001 interim): rebuilt, musl included, only when
# one of its inputs changed: the libc tree, the runtime and allocator units
# it compiles, the shared units, and the flake (the compilers, the C++
# runtime it installs). LIBC_STAMP is touched after a build that succeeded.
LIBC_STAMP := ../userspace/libc/build/.built
LIBC_INPUTS := $(shell find ../userspace/libc -path ../userspace/libc/build -prune -o -type f -print) \
	$(wildcard ../userspace/runtime/gnat/*.ad? ../userspace/runtime/start/* \
		../userspace/allocator/src/*.ad? ../userspace/allocator/process/*.ad? ../shared/*/*.ad?) \
	../flake.lock ../flake.nix
CCL_MANIFEST_TOOL := ../userspace/ccl/build/manifest/ccl-manifest

# stage SOURCE,NAME (BLD-001), for programs a phony target builds: put
# SOURCE in isodir/boot/NAME only when its bytes differ from the ones staged
# there last (kept as build/staged/NAME), since the sign step strips and
# signs the staged copy in place. An unchanged program then keeps its
# signed file and its time, and nothing that contains it is redone.
# UNSTAGED is where such recipes write a program before it is staged.
STAGED := build/staged
UNSTAGED := build/unstaged
define stage
@mkdir -p $(STAGED)
@if [ -e isodir/boot/$(2) ] && cmp -s $(1) $(STAGED)/$(2); then :; else \
  echo "stage $(2)"; cp $(1) $(STAGED)/$(2).new && cp $(1) isodir/boot/$(2) && mv $(STAGED)/$(2).new $(STAGED)/$(2); fi
endef

# Runtime library used by the CuBit Kernel itself (not the runtime used by
# software running on it!)
build/libcubit.a:
	$(check_kernel_gnat)
	mkdir -p runtime/adalib
	mkdir -p build

# Text-substitute string literals used in build.ads
	-rm src/build.ads

# Generate a hash list of all the source files and then a root hash from that
# (in name order: find's directory order differs between checkouts; without
# the generated build.ads, which make expands this before the rm above runs)
	$(eval SRC_HASH = $(shell find src/ -type f ! -path src/build.ads -print0 | LC_ALL=C sort -z | xargs -0 sha256sum | sha256sum))

	sed -e 's/SED_GIT_HASH/$(GIT_HASH)/' \
		-e 's/SED_BUILD_DATE/$(BUILD_DATE)/' \
		-e 's/SED_SHADOW_SCHEDULING/$(if $(filter 1,$(SHADOW_SCHEDULING)),True,False)/' \
		-e 's/SED_DEADLINE_TIMER_TEST/$(if $(filter 1,$(DEADLINE_TIMER_TEST)),True,False)/' \
		-e 's/SED_LATENCY_TRACE/$(if $(filter 1,$(LATENCY_TRACE)),True,False)/' \
		-e 's/SED_WAKEUP_SCHEDULING/$(if $(filter 1,$(WAKEUP_SCHEDULING)),True,False)/' \
		-e 's/SED_ONESHOT_SCHEDULING/$(if $(filter 1,$(ONESHOT_SCHEDULING)),True,False)/' \
		-e 's/SED_WORK_STEALING/$(if $(filter 1,$(WORK_STEALING)),True,False)/' \
		-e 's/SED_USER_WAIT_PAUSE/$(if $(filter 1,$(USER_WAIT_PAUSE)),True,False)/' \
		-e 's/SED_TAGGED_TLB/$(if $(filter 1,$(TAGGED_TLB)),True,False)/' \
		-e 's/SED_SYSCALL_CENSUS/$(if $(filter 1,$(SYSCALL_CENSUS)),True,False)/' \
		-e 's/SED_SMAP_ENTRY_PROBE/$(if $(filter 1,$(SMAP_ENTRY_PROBE)),True,False)/' \
		-e 's/SED_KERN014_PROBE/$(if $(filter 1,$(KERN014_PROBE)),True,False)/' \
		-e 's/SED_DEBUG_REGISTER_PROBE/$(if $(filter 1,$(DEBUG_REGISTER_PROBE)),True,False)/' \
		-e 's/SED_CLOCK_STALL_PROBE/$(if $(filter 1,$(CLOCK_STALL_PROBE)),True,False)/' \
		-e 's/SED_TSC_SKEW_PROBE/$(if $(filter 1,$(TSC_SKEW_PROBE)),True,False)/' \
		-e 's/SED_KERNEL_LOG_RECORDS/$(KERNEL_LOG_RECORDS)/' \
		-e 's/SED_SRC_HASH/$(SRC_HASH)/' src/build.pre > src/build.ads

	$(KERNEL_GPRBUILD) -Pkernel_runtime.gpr

# init usermode binary embedded in the kernel ELF object
build/init.bin: src/init.asm
	yasm --arch=x86 -fbin src/init.asm -o build/init.bin
#	ld --omagic -e start -Ttext 0 -o build/init.out build/init.o
#	objcopy -I binary -O elf64-x86-64 -B i386

# Kernel assembly, assembled here rather than by gprbuild so that the objects
# record no checkout path (KERN-012): yasm writes the source name it is given
# into the ELF FILE symbol, so it gets names relative to src/. No DWARF: yasm
# always records its absolute working directory as DW_AT_comp_dir and has no
# prefix map. The include files are prerequisites (boot.asm's entry gate macro
# is shared with the hosted tests/multiboot-entry).
KERNEL_ASM_OBJECTS := build/boot.o build/boot_ap.o build/context_switch.o build/svm_run.o build/vmx_entry.o \
	build/interrupt_handlers.o build/syscall_entry.o
$(KERNEL_ASM_OBJECTS): build/%.o: src/%.asm src/cubit.inc src/multiboot_entry.inc
	mkdir -p build
	cd src && yasm --arch=x86 -felf64 $*.asm -o ../build/$*.o

# Maximum allowed stack usage for any single kernel function (bytes).
# Kernel stacks are 4KB (usable ~3.3KB). Leave headroom for interrupt frames.
MAX_STACK_USAGE := 2048

# kernel ELF object
cubit_kernel: build/libcubit.a build/init.bin $(KERNEL_ASM_OBJECTS) cubit.gpr gnat.adc linker.ld Makefile \
              $(wildcard ../shared/firmware/*.ads ../shared/firmware/*.adb) \
              $(wildcard ../shared/time/*.ads ../shared/time/*.adb) \
              $(wildcard ../shared/interrupts/*.ads ../shared/interrupts/*.adb) \
              $(wildcard ../shared/boot/*.ads ../shared/boot/*.adb) \
              $(wildcard ../shared/capabilities/*.ads ../shared/capabilities/*.adb) \
              $(wildcard ../shared/launch/*.ads ../shared/launch/*.adb) \
              $(wildcard ../shared/publications/*.ads ../shared/publications/*.adb) \
              $(wildcard src/*.ads src/*.adb src/*.asm src/*.adc \
                         src/services/*.ads src/services/*.adb src/*.inc)
# Compile and bind only
	$(check_kernel_gnat)
	$(KERNEL_GPRBUILD) -b -c -Pcubit.gpr
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
	$(KERNEL_LD) -n --build-id=sha1 -o cubit_kernel -T linker.ld \
		build/*.o \
		-b binary build/init.bin \
		-Map cubit_kernel.map
	$(call check_kernel_image_gnat,cubit_kernel)

# TODO: make sure kernel is multiboot-compliant with grub-file --is-x86-multiboot cubit_kernel

# tests/cubit_tests: tests/cubit_tests.adb
# 	-gnatA : ignore gnat.adc (we don't care about pragmas there for testing)
#	gnat make -gnatA -aI. tests/cubit_tests.adb -D tests/build

.PHONY : prove
# Whole kernel at level 2: every SPARK_Mode On unit, including those the
# focused prove-* targets also cover (level 0 left most of them unproved).
# cvc5_int is the buddy proofs' extra encoding of the same obligations
# (tests/buddy-bitmap/why3.conf: bit-vectors as integers), not an axiom.
prove:
	$(ALR) gnatprove -Pcubit.gpr --mode=all --level=2 --no-axiom-guard \
		--why3-conf=$(CURDIR)/../tests/buddy-bitmap/why3.conf \
		--prover=cvc5,cvc5_int,z3 -j2

# Every proof target in the repository (tests/prove-all.targets), with one
# summary table: the proof checkpoint. Narrow it with
# PROVE_ALL_ARGS="--group desktop" or "--only qoi"; see tests/prove-all.sh.
.PHONY : prove-all
prove-all:
	bash ../tests/prove-all.sh $(PROVE_ALL_ARGS)

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

# Boot memory reclaim: the boot module lifetime (mapping counts, no revival
# of a released page) and the boot allocator's retirement (no use after).
.PHONY : prove-boot-reclaim
prove-boot-reclaim:
	$(ALR) gnatprove -Pcubit.gpr -u boot_module_lifetime.adb bootallocator.adb \
		--subdirs=proof-boot-reclaim --mode=all --level=2 --prover=cvc5,z3 \
		--checks-as-errors=on -j2
	@cat build/proof-boot-reclaim/gnatprove/gnatprove.out

.PHONY : prove-process-lifetime
prove-process-lifetime:
	$(ALR) gnatprove -Pcubit.gpr -u process_lifetime.adb \
		--subdirs=proof-process-lifetime --mode=all --level=1 -j2
	@cat build/proof-process-lifetime/gnatprove/gnatprove.out

# SEC-025: decoders read peer-writable memory once (a private copy); every
# Import/Address overlay is reviewed; procmgr's images are in private pages.
# FS-033: places are named by alias (@system, @boot), never by device,
# outside the reviewed allowlist (tests/fs033/check_device_locators.py).
.PHONY : test-fs033
test-fs033:
	python3 -I ../tests/fs033/check_device_locators.py ..

# Programs that compile CCL sources from an explicit list must list every
# unit the shared settings catalogue (ccl-setting_kinds.ads) withs
# (tests/ccl-source-lists/check.py).
.PHONY : test-ccl-source-lists
test-ccl-source-lists:
	python3 ../tests/ccl-source-lists/check.py

.PHONY : test-sec025
test-sec025:
	python3 -I ../tests/sec025/check_shared_overlays.py
	python3 -I ../tests/sec025/check_procmgr_image_buffer.py
	$(ALR) gprbuild -p -q -P ../tests/sec025/shared_copies.gpr
	../tests/sec025/build/shared_copies_tests

.PHONY: test-boot-framebuffer prove-boot-framebuffer
.PHONY: test-boot-panel prove-boot-panel
test-boot-panel:
	$(ALR) gprbuild -p -P ../tests/boot-panel/panel_tests.gpr
	../tests/boot-panel/build/main
	$(ALR) gprbuild -p -P ../tests/boot-panel/adapter/adapter_tests.gpr
	../tests/boot-panel/adapter/build/main 1 1
	../tests/boot-panel/adapter/build/main 1 1 retired
	../tests/boot-panel/adapter/build/main 640 480
	../tests/boot-panel/adapter/build/main 1024 768
	../tests/boot-panel/adapter/build/main 1920 1080

prove-boot-panel:
	$(ALR) gnatprove -P ../tests/boot-panel/panel_tests.gpr -u boot_panel.adb boot_font.adb --mode=all --level=1 --report=all --checks-as-errors=on -j2

test-boot-framebuffer:
	bash ../tests/boot-framebuffer/test.sh

prove-boot-framebuffer:
	bash ../tests/boot-framebuffer/prove.sh

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

.PHONY : test-object-table
test-object-table:
	$(ALR) gprbuild -p -P../tests/object-table/object_table_tests.gpr
	../tests/object-table/build/main

.PHONY : test-quiescent-reclamation prove-quiescent-reclamation
test-quiescent-reclamation:
	$(ALR) gprbuild -p -P../tests/quiescent-reclamation/reclamation.gpr
	../tests/quiescent-reclamation/build/main

prove-quiescent-reclamation:
	$(ALR) gnatprove -P../tests/quiescent-reclamation/reclamation.gpr -u quiescent_reclamation.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/quiescent-reclamation/build/gnatprove/gnatprove.out

.PHONY : test-id-ledger prove-id-ledger
test-id-ledger:
	$(ALR) gprbuild -p -P../tests/id-ledger/ledger.gpr
	../tests/id-ledger/build/main

prove-id-ledger:
	$(ALR) gnatprove -P../tests/id-ledger/ledger.gpr -u test_ledger.ads \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/id-ledger/build/gnatprove/gnatprove.out

.PHONY : test-capability-tables prove-capability-tables
test-capability-tables:
	$(ALR) gprbuild -p -P../tests/capability-tables/tables.gpr
	../tests/capability-tables/build/main
	$(ALR) gprbuild -p -P../tests/capability-tables/geometry.gpr
	../tests/capability-tables/build/geometry

# CAP-002: copying a capability into another table (CSPACE installs and
# reply transfers): hosted tests, and the level-2 proof (--prove).
.PHONY : test-capability-install prove-capability-install
test-capability-install:
	bash ../tests/capability-install/run.sh
prove-capability-install:
	bash ../tests/capability-install/run.sh --prove

prove-capability-tables:
	$(ALR) gnatprove -P../tests/capability-tables/tables.gpr -u test_tables.ads \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/capability-tables/build/gnatprove/gnatprove.out

.PHONY : test-work-stealing prove-work-stealing
test-work-stealing:
	$(ALR) gprbuild -p -P../tests/work-stealing/stealing.gpr
	../tests/work-stealing/build/main

prove-work-stealing:
	$(ALR) gnatprove -P../tests/work-stealing/stealing.gpr -u work_stealing.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/work-stealing/build/gnatprove/gnatprove.out

.PHONY : test-address-space-tags prove-address-space-tags
test-address-space-tags:
	$(ALR) gprbuild -p -P../tests/address-space-tags/address_space_tags_tests.gpr
	../tests/address-space-tags/build/main

prove-address-space-tags:
	$(ALR) gnatprove -P../tests/address-space-tags/address_space_tags_tests.gpr \
		-u address_space_tags.adb tag_protocol.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/address-space-tags/build/gnatprove/gnatprove.out

.PHONY : test-thread-slots prove-thread-slots
test-thread-slots:
	$(ALR) gprbuild -p -P../tests/thread-slots/thread_slots_tests.gpr
	../tests/thread-slots/build/main

prove-thread-slots:
	$(ALR) gnatprove -P../tests/thread-slots/thread_slots_tests.gpr \
		-u thread_slots.adb slot_protocol.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/thread-slots/build/gnatprove/gnatprove.out

.PHONY : test-trace-object prove-trace-object
test-trace-object:
	$(ALR) gprbuild -p -P../tests/trace-object/trace_object_tests.gpr
	../tests/trace-object/build/main

prove-trace-object:
	$(ALR) gnatprove -P../tests/trace-object/trace_object_tests.gpr \
		-u trace_abi.ads --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/trace-object/build/gnatprove/gnatprove.out

.PHONY : test-test-console prove-test-console
# The test console object's decision and the debug-text framing (ABI-002).
test-test-console:
	$(ALR) gprbuild -p -P../tests/test-console/test_console_tests.gpr
	../tests/test-console/build/main

prove-test-console:
	$(ALR) gnatprove -P../tests/test-console/test_console_tests.gpr \
		-u test_console_abi.ads cubit-console_lines.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/test-console/build/gnatprove/gnatprove.out

.PHONY : test-process-list-object prove-process-list-object
test-process-list-object:
	$(ALR) gprbuild -p -P../tests/process-list-object/process_list_object_tests.gpr
	../tests/process-list-object/build/main

prove-process-list-object:
	$(ALR) gnatprove -P../tests/process-list-object/process_list_object_tests.gpr \
		-u process_list_abi.ads --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/process-list-object/build/gnatprove/gnatprove.out

.PHONY : test-notifications prove-notifications
test-notifications:
	$(ALR) gprbuild -p -P../tests/notifications/notifications_tests.gpr
	../tests/notifications/build/main

prove-notifications:
	$(ALR) gnatprove -P../tests/notifications/notifications_tests.gpr \
		-u notification_objects.adb fan_in_model.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/notifications/build/gnatprove/gnatprove.out

.PHONY : test-private-futexes prove-private-futexes
test-private-futexes:
	$(ALR) gprbuild -p -P../tests/private-futexes/private_futexes_tests.gpr
	../tests/private-futexes/build/main

prove-private-futexes:
	$(ALR) gnatprove -P../tests/private-futexes/private_futexes_tests.gpr \
		-u private_futexes.adb word_protocol.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/private-futexes/build/gnatprove/gnatprove.out

.PHONY : test-wait-deadlines prove-wait-deadlines
test-wait-deadlines:
	$(ALR) gprbuild -p -P../tests/wait-deadlines/wait_deadlines_tests.gpr
	../tests/wait-deadlines/build/main

prove-wait-deadlines:
	$(ALR) gnatprove -P../tests/wait-deadlines/wait_deadlines_tests.gpr \
		-u wait_deadlines.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/wait-deadlines/build/gnatprove/gnatprove.out

.PHONY : test-futex-queues prove-futex-queues
test-futex-queues:
	$(ALR) gprbuild -p -P../tests/futex-queues/futex_queues_tests.gpr
	../tests/futex-queues/build/main
	$(ALR) python3 ../tests/futex-queues/explore.py

prove-futex-queues:
	$(ALR) gnatprove -P../tests/futex-queues/futex_queues_tests.gpr \
		-u futex_queues_small.ads futex_queues_large.ads futex_keys.adb futex_protocol.adb wake_word_protocol.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/futex-queues/build/gnatprove/gnatprove.out

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
	$(NATIVE_GPRBUILD) -p -c -P../tests/ccl-cbor/native_probe.gpr

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
		-u usb_optical.adb usb_configurations.adb xhci_completions.adb iso_records.adb xhci_capabilities.adb xhci_dma_layout.adb xhci_ports.adb xhci_legacy.ads scsi.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/usb-optical/build/gnatprove/gnatprove.out

# HW-001: the shared Block.Device.V1 front end (hosted tests, the RAM
# disk's regression on it, and its SPARK proofs).
.PHONY: test-block-requests prove-block-requests
test-block-requests:
	$(ALR) gprbuild -p -P../tests/block-requests/block_requests_tests.gpr
	../tests/block-requests/build/main
	$(ALR) gprbuild -p -P../tests/ram-block/ram_block_tests.gpr
	../tests/ram-block/build/main
	$(ALR) gprbuild -p -P../tests/block-driver/block_driver_tests.gpr
	../tests/block-driver/build/main

prove-block-requests:
	$(ALR) gnatprove -P../tests/block-requests/block_requests_tests.gpr \
		-u cubit-block_requests.adb cubit-disk_install.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	$(ALR) gnatprove -P../tests/block-requests/runtime_proof.gpr \
		-u cubit-block_requests.adb cubit-disk_install.adb cubit-device_identities.adb cubit-installer_protocol.adb --level=2 --report=all --checks-as-errors=on -j2

# SYS-001: the installer's disk layout writer (GPT, FAT32 ESP, ext2 root):
# Linux-hosted images checked by sgdisk, fsck.fat and e2fsck, and proofs.
.PHONY: test-disk-layout prove-disk-layout
test-disk-layout:
	bash ../tests/disk-layout/run.sh

# SYS-003: service switches, generation records, and the boot-time and
# next-boot generation decisions (hosted); prove-service-switch proves them.
.PHONY: test-service-switch prove-service-switch
test-service-switch:
	bash ../tests/service-switch/run.sh
prove-service-switch:
	bash ../tests/service-switch/run.sh --prove

# SYS-001: system.disks and system.install as CCL over a stand-in installer.
.PHONY: test-ccl-install
test-ccl-install:
	$(ALR) gprbuild -q -p -P../tests/ccl-install/ccl_install_tests.gpr
	../tests/ccl-install/build/main
	# Every interface the host environment installs fits the catalog.
	$(ALR) gprbuild -q -p -P../tests/ccl-install/environment/probe.gpr
	../tests/ccl-install/environment/build/probe

prove-disk-layout:
	$(ALR) gnatprove -P../tests/disk-layout/disk_layout_tests.gpr \
		-u disk_layout.adb gpt_image.adb fat32_image.adb ext2_image.adb \
		install_tree.adb partition_tables.adb proof_writer.adb \
		--level=2 --report=all --checks-as-errors=on -j2

# HW-001: typed SCSI command blocks and response decoders (shared by USB
# optical and UFS).
.PHONY: test-scsi prove-scsi
test-scsi:
	$(ALR) gprbuild -p -P../tests/scsi/scsi_tests.gpr
	../tests/scsi/build/main

prove-scsi:
	$(ALR) gnatprove -P../tests/scsi/scsi_tests.gpr -u scsi.adb \
		--level=2 --report=all --checks-as-errors=on -j2

# SELF-003: USB mass-storage disks: the Bulk-Only Transport codec, the
# SCSI disk decoders and the probe (hosted tests, level-2 proof; the
# mutation check is tests/usb-disk/mutate.sh).
.PHONY: test-usb-disk prove-usb-disk
test-usb-disk:
	$(ALR) gprbuild -p -P../tests/usb-disk/usb_disk_tests.gpr
	../tests/usb-disk/build/main

prove-usb-disk:
	$(ALR) gnatprove -P../tests/usb-disk/usb_disk_tests.gpr \
		-u scsi.adb scsi_disks.adb usb_bulk_only.adb usb_disk_probe.adb \
		--level=2 --report=all --checks-as-errors=on -j2

# FS-034: USB stick hot-plug and Eject: the unit badge, the disk presence
# report, xhci's report state, devmgr's USB disk names, the eject codec
# and plan (hosted tests, level-2 proof; the mutation check is
# tests/usb-eject/mutate.sh).
.PHONY: test-usb-eject prove-usb-eject
test-usb-eject:
	$(ALR) gprbuild -p -P../tests/usb-eject/usb_eject_tests.gpr
	../tests/usb-eject/build/main

prove-usb-eject:
	$(ALR) gnatprove -P../tests/usb-eject/usb_eject_tests.gpr \
		-u cubit-block_unit_badges.ads cubit-disk_presence.adb cubit-volume_ejection.adb test_reports.ads \
		devmgr_usb_disks.adb \
		--level=2 --report=all --checks-as-errors=on -j2

# HW-001: the UFS driver's codecs (UFSHCI, UPIU, descriptors, GPT).
.PHONY: test-ufs prove-ufs
test-ufs:
	$(ALR) gprbuild -p -P../tests/ufs/ufs_tests.gpr
	../tests/ufs/build/main

prove-ufs:
	$(ALR) gnatprove -P../tests/ufs/ufs_tests.gpr \
		-u ufshci.adb ufs_upiu.adb ufs_descriptors.adb partition_tables.adb ufs_text.adb \
		--level=2 --report=all --checks-as-errors=on -j2

.PHONY : test-tcp-session prove-tcp-session
test-network-authority:
	$(ALR) gprbuild -p -P../tests/network-authority/network_authority_tests.gpr
	../tests/network-authority/build/main

prove-network-authority:
	$(ALR) gnatprove -P../tests/network-authority/network_authority_tests.gpr \
		-u cubit-network_authority.adb network_grants.adb udp_channels.adb --level=1 --report=all --checks-as-errors=on -j2
	@cat ../tests/network-authority/build/gnatprove/gnatprove.out

test-timesync:
	$(ALR) gprbuild -p -P../tests/timesync/timesync_tests.gpr
	../tests/timesync/build/main

# The clock-control round trip (Decode's postcondition) needs level 4 for
# its integer/bit-vector conversion; the other cores prove at level 2.
prove-timesync:
	$(ALR) gnatprove -P../tests/timesync/timesync_tests.gpr \
		-u sntp.adb server_list.adb clock_discipline.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	$(ALR) gnatprove -P../tests/timesync/timesync_tests.gpr \
		-u cubit-clock_control.adb \
		--level=4 --report=all --checks-as-errors=on -j2
	@cat ../tests/timesync/build/gnatprove/gnatprove.out

.PHONY: test-timesync prove-timesync
.PHONY: network-check test-network-authority prove-network-authority
network-check: isodir/boot/network-check.app
$(eval $(call ccl_program,../userspace/apps/network-check,network-check.app,))

# The net-link and net-multi headless tests' guests (tests/net-link).
.PHONY: net-link-check net-multi-check
net-link-check: isodir/boot/net-link-check.app
$(eval $(call ccl_program,../userspace/apps/net-link-check,net-link-check.app,))

net-multi-check: isodir/boot/net-multi-check.app
$(eval $(call ccl_program,../userspace/apps/net-multi-check,net-multi-check.app,))

test-tcp-session:
	$(ALR) gprbuild -p -P../tests/tcp-session/tcp_session_tests.gpr
	../tests/tcp-session/build/main

prove-tcp-session:
	$(ALR) gnatprove -P../tests/tcp-session/tcp_session_tests.gpr \
		-u tcp_listeners.adb network_channel_handles.adb tcp_time_wait.adb tcp_wire.adb ipv6_link_proof.adb ipv4_icmp_proof.adb internet_checksum.adb --level=1 --report=all --checks-as-errors=on -j2
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
		--subdirs=proof-capability-policy --mode=all --level=1 --function-sandboxing=off
	@cat build/proof-capability-policy/gnatprove/gnatprove.out

.PHONY : prove-capability-derive
prove-capability-derive:
	$(ALR) gnatprove -Pcubit.gpr -u capabilities.ads \
		--subdirs=proof-capability-derive --mode=all --level=1 --function-sandboxing=off
	@cat build/proof-capability-derive/gnatprove/gnatprove.out

.PHONY : prove-memory-grants
prove-memory-grants:
	$(ALR) gnatprove -Pcubit.gpr -u memory_grants.adb \
		--subdirs=proof-memory-grants --mode=all --level=1 --function-sandboxing=off --checks-as-errors=on -j2
	@cat build/proof-memory-grants/gnatprove/gnatprove.out

.PHONY: test-grant-loans prove-grant-loans
test-grant-loans:
	$(ALR) gprbuild -p -P ../tests/grant-loans/loans.gpr
	../tests/grant-loans/build/main
	../tests/grant-loans/build/parent_lifetime_test
	../tests/grant-loans/build/parent_scope_test
	../tests/grant-loans/build/mapping_test
	../tests/grant-loans/build/output_lifetime_test

prove-grant-loans:
	$(ALR) gnatprove -P ../tests/grant-loans/loans.gpr \
		-u loan_proof.ads memory_grants.adb --level=2 --timeout=15 --report=all --checks-as-errors=on -j2
	python3 ../tests/grant-loans/check-proof.py

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

# DOOM: doomgeneric on the CuBit libc, platform layer in Ada
# (userspace/ports/doom).
# The ccl-build command line as a file, rewritten only when it changes.
build/ccl-build.args: FORCE
	@mkdir -p build
	@printf '%s\n' '$(CCL_BUILD)' > $@.new
	@if cmp -s $@.new $@; then rm $@.new; else mv $@.new $@; fi

# CCL_TOOL_OUTPUTS: files phony targets write that ccl-build steps read
# (the user runtime, the C start file, the allocator, the font archive).
# Each depends on the target that writes it, with an empty recipe, so make
# runs that target before it compares the file's time with a step's stamp.
../userspace/runtime/adalib/%: user_runtime ;
../userspace/runtime/build/%: user_runtime ;
../userspace/c/build/%: user_runtime ;
../userspace/allocator/build/%: user_runtime ;
../userspace/rust/build/font-native/%: ui-fonts-native ;

# gpr_component stamps (BLD-001 interim, until these move onto ccl-build):
# the programs gprbuild still links run their recipe only when an input
# changed. tools/gpr_inputs.py writes build/gpr/NAME.d after each run from
# gprls: the Ada sources of the project and its imports, the other files
# beside them, and their directories. GPR_STEP_INPUTS are the rest: the
# runtime and its archive, the C start file and linker script, the font
# archive, the manifest catalogs and tool, the GNAT configuration, the
# flake (the compilers, the SPARK crates), and build/gpr-step.args, the -X
# values and compiler paths, rewritten only when they change.
NATIVE_GPRLS = $(ALR) gprls $(CUBIT_NATIVE_GPRBUILD_CONFIG) -d -s
GPR_STEP_VARIABLES := CUBIT_GNAT CUBIT_BINUTILS CUBIT_SPARK_CRATES CBOR_ADA_SRC CUBIT_DISPLAY_TEST_MODE \
	CUBIT_GPU_TEST_MODE CUBIT_COMPOSITOR CUBIT_COMPOSITOR_TIMING CUBIT_COMPOSITOR_STORAGE CUBIT_DESKTOP_FAULT \
	CUBIT_INPUT_OVERLAY CUBIT_NVME_IO_PROFILE CUBIT_MESA_BUILD CUBIT_MESA_BUNDLE CUBIT_MESA_SOURCE CUBIT_MESA_CACHE
GPR_STEP_INPUTS = build/gpr-step.args $(wildcard ../userspace/runtime/adalib/*) ../userspace/c/build/crt0.o \
	../userspace/c/link.ld $(wildcard ../userspace/rust/build/font-native/libcubit_fonts.a) \
	$(wildcard ../userspace/ccl/catalogs/*.ccl ../userspace/ccl/interfaces/*) $(CCL_MANIFEST_TOOL) \
	../userspace/toolchain/gnat-15.cgpr ../flake.lock ../flake.nix
# Desktop's inputs: what tools/build_desktop_vulkan_compositor.py copies
# into its clean build tree (the same directories serve the other
# compositor variants), its builders, and the Mesa bundle teapots' build
# leaves for it (its inputs.json names the Mesa build by content).
DESKTOP_SOURCE_DIRECTORIES := $(addprefix ../userspace/,services/desktop lib/compositor lib/display lib/image \
	lib/theme lib/ui lib/input lib/time ccl/src allocator/src services/display/production runtime/gnat mesa) \
	$(addprefix ../shared/,time interrupts launch publications capabilities boot)
DESKTOP_STEP_INPUTS = ../tools/desktop_build_variant.py ../tools/build_vulkan_desktop.py \
	../tools/build_desktop_vulkan_compositor.py ../tools/build_mesa_desktop.py \
	../tests/compositor/build-vulkan-affine-shaders.py ../userspace/c/cubit_handles.h \
	$(shell find $(DESKTOP_SOURCE_DIRECTORIES) -type d -name 'build*' -prune -o -type f -print 2>/dev/null) \
	$(addprefix ../userspace/runtime/,ada_source_path ada_object_path runtime.xml target_properties) $(TIME_ZONE_SOURCES) \
	$(wildcard ../userspace/mesa/build/combined/*/bundle/inputs.json)
build/gpr-step.args: FORCE
	@mkdir -p build
	@printf '%s\n' $(foreach v,$(GPR_STEP_VARIABLES),'$(v)=$($(v))') > $@.new
	@if cmp -s $@.new $@; then rm $@.new; else mv $@.new $@; fi

# For images other tools also write in place (the headless tests write
# test variants of isodir/boot/initrd.img and cubit_kernel.iso): such a
# rule also depends on FORCE, and its recipe runs only unless_made TARGET
# says otherwise (BLD-001). That is true when no prerequisite is newer and
# TARGET is still the file the rule made: its size, inode and times are
# those made_record TARGET kept in build/made/ after making it.
define unless_made
[ -z "$(filter-out FORCE,$?)" ] && [ -e $(1) ] && \
  [ "$$(stat -c '%s %i %Y %Z' $(1))" = "$$(cat build/made/$(notdir $(1)) 2>/dev/null)" ]
endef
define made_record
mkdir -p build/made && stat -c '%s %i %Y %Z' $(1) > build/made/$(notdir $(1))
endef

# Built by a script, not ccl-build yet (BLD-001): a file target over its
# inputs, so it is rebuilt only when one changed (PORT_INPUTS: the CuBit
# libc, the runtime and shared units the script compiles, the manifest
# tool, and the flake, which pins the upstream sources and the compilers).
PORT_INPUTS = $(LIBC_STAMP) $(CCL_MANIFEST_TOOL) ../userspace/ccl/catalogs/native-runtime-services.ccl \
	$(wildcard ../userspace/runtime/gnat/*.ad? ../userspace/libc/ada/*.ad? ../shared/*/*.ad?) \
	../userspace/libc/libc-ada.adc ../flake.lock ../flake.nix
.PHONY : doom
doom: isodir/boot/doom.elf
$(UNSTAGED)/doom.elf: $(wildcard ../userspace/ports/doom/*) $(PORT_INPUTS) | libc ccl-manifest
	mkdir -p $(UNSTAGED)
	bash ../userspace/ports/doom/build.sh $(abspath $@)
isodir/boot/doom.elf: $(UNSTAGED)/doom.elf
	cp $< $@

# SameBoy: the upstream core on the CuBit libc, frontend in Ada
# (userspace/ports/sameboy).
.PHONY : sameboy
sameboy: isodir/boot/sameboy.app
$(UNSTAGED)/sameboy.app: $(wildcard ../userspace/ports/sameboy/*) $(PORT_INPUTS) | libc ccl-manifest
	mkdir -p $(UNSTAGED)
	bash ../userspace/ports/sameboy/build.sh $(abspath $@)
isodir/boot/sameboy.app: $(UNSTAGED)/sameboy.app
	cp $< $@
../userspace/ports/sameboy/build/test.gb: $(UNSTAGED)/sameboy.app ;

.PHONY : sleep
sleep: isodir/boot/sleep.app
$(eval $(call ccl_program,../userspace/apps/sleep,sleep.app,))

.PHONY : capability-test
capability-test: isodir/boot/capability-test.app
	$(ALR) python3 ../tests/process-construction/make-fixtures.py \
		isodir/boot/capability-test.app ../tests/process-construction/build
$(eval $(call ccl_program,../userspace/apps/capability-test,capability-test.app,../userspace/apps/capability-test/build/syscall_registers.o))
../userspace/apps/capability-test/build/syscall_registers.o: ../userspace/apps/capability-test/syscall_registers.asm
	mkdir -p $(@D)
	yasm -f elf64 $< -o $@

# The launch gate check (CAP-012) and its two children
# (tests/headless/run.sh --test launch-gate).
LAUNCH_GATE_APPS := launch-gate-check launch-gate-child launch-gate-greedy
.PHONY : launch-gate
launch-gate: $(patsubst %,isodir/boot/%.app,$(LAUNCH_GATE_APPS)) capability-test
$(foreach app,$(LAUNCH_GATE_APPS),$(eval $(call ccl_program,../userspace/apps/$(app),$(app).app,)))

# The SPAWN rules check (CAP-012): runs in procmgr's place from a test
# initrd only (tests/headless/run.sh --test spawn-rules).
.PHONY : debug-observe-check
# The Debug_Observe check (DBG-001): headless --test debug-observe puts it in
# procmgr's place.
debug-observe-check: isodir/boot/debug-observe-check.app
$(eval $(call ccl_program,../userspace/apps/debug-observe-check,debug-observe-check.app,))

.PHONY : vm-hello-check
# The VM mechanism check (VMM-001 slice 1): headless --test vm-hello puts it
# in procmgr's place.
vm-hello-check: isodir/boot/vm-hello-check.app
$(eval $(call ccl_program,../userspace/apps/vm-hello-check,vm-hello-check.app,))

.PHONY : graphics-report-check
# GPU-014: reads devmgr's renderer report once a second (headless
# graphics-report-desktop-down).
graphics-report-check: isodir/boot/graphics-report-check.app
$(eval $(call ccl_program,../userspace/apps/graphics-report-check,graphics-report-check.app,))

.PHONY : vm-linux-check
# The Linux guest check (VMM-001 slice 1): headless --test vm-linux puts it
# in procmgr's place. The guest image is pinned (tests/vm-linux).
vm-linux-check: ccl-build user_runtime ccl-manifest
	bash ../tests/vm-linux/guest-image.sh >/dev/null
	mkdir -p ../userspace/apps/vm-linux-check/build
	$(CCL_BUILD) ../userspace/apps/vm-linux-check/component.ccl
	cp ../userspace/apps/vm-linux-check/build/vm-linux-check.app isodir/boot/vm-linux-check.app

.PHONY : spawn-rules-check
spawn-rules-check: isodir/boot/spawn-rules-check.app
$(eval $(call ccl_program,../userspace/apps/spawn-rules-check,spawn-rules-check.app,))

# Portable protocol checks and an unprivileged native desktop adversary.
.PHONY: prepare-stream-policies test-stream-policies prove-stream-policies
prepare-stream-policies:
	mkdir -p ../tests/stream-policies/build/source
	cp ../userspace/runtime/gnat/cubit.ads ../userspace/runtime/gnat/cubit-protocols.ads \
		../userspace/runtime/gnat/cubit-protocols-stream_policies.ads ../tests/stream-policies/build/source/

test-stream-policies: prepare-stream-policies
	$(ALR) gprbuild -p -P ../tests/stream-policies/policies.gpr
	../tests/stream-policies/build/main

prove-stream-policies: prepare-stream-policies
	$(ALR) gnatprove -P ../tests/stream-policies/policies.gpr \
		-u cubit-protocols-stream_policies.ads proof_cases.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/stream-policies/build/obj/gnatprove/gnatprove.out

# The streaming QOI decoder and the wallpaper store (tests/qoi/README.md).
.PHONY: test-qoi prove-qoi
test-qoi: wallpaper-assets icon-assets
	python3 ../tests/qoi/make_fixtures.py ../tests/qoi/build/fixtures
	python3 ../tools/build_wallpaper_assets.py ../assets/wallpapers ../tests/qoi/build/Assets \
		--reference ../tests/qoi/build/reference >/dev/null
	$(ALR) gprbuild -q -p -P ../tests/qoi/qoi.gpr
	../tests/qoi/build/qoi_tests ../tests/qoi/build/fixtures ../tests/qoi/build/Assets ../tests/qoi/build/reference
	python3 ../tools/build_icon_assets.py ../tests/qoi/build/Assets/cubit-icons/5 \
		--reference ../tests/qoi/build/reference >/dev/null
	python3 ../tests/qoi/make_icon_fixtures.py ../tests/qoi/build/fixtures
	$(ALR) gprbuild -q -p -P ../tests/qoi/icon_store.gpr
	bash ../tests/qoi/run-icon-store.sh ../tests/qoi/build

prove-qoi:
	$(ALR) gnatprove -P ../tests/qoi/qoi.gpr -u cubit-qoi.adb --mode=all --level=1 --timeout=10 -j2
	$(ALR) gnatprove -P ../tests/qoi/store.gpr -u desktop_wallpaper_store.adb --mode=all --level=2 --timeout=30 -j2
	$(ALR) gnatprove -P ../tests/qoi/icon_store.gpr -u desktop_icon_store.adb --mode=all --level=2 --timeout=15 -j2

.PHONY: test-appearance prove-appearance
test-appearance: ui-fonts-host
	$(ALR) gprbuild -p -P ../tests/appearance/appearance.gpr
	../tests/appearance/build/main

prove-appearance:
	$(ALR) gnatprove -P ../tests/appearance/appearance.gpr -u \
		cubit-ui-theme_ccl.adb cubit-ui-theme_data.adb cubit-appearance.adb \
		ccl-declarations.adb \
		--mode=all --level=1 --timeout=5 -j2

.PHONY : clipboard-check test-clipboard prove-clipboard
# Desktop clipboard (docs/clipboard.md): the regression client, the hosted
# tests and the level-2 proof of the store, gestures, wire codec and menu.
clipboard-check: isodir/boot/clipboard-check.app
$(eval $(call ccl_program,../userspace/apps/clipboard-check,clipboard-check.app,ui-fonts-native))

test-clipboard:
	bash ../tests/clipboard/run.sh

prove-clipboard:
	bash ../tests/clipboard/run.sh --prove

.PHONY : desktop-check prepare-desktop-protocol test-desktop-protocol prove-desktop-protocol
desktop-check: isodir/boot/desktop-check.app
$(eval $(call ccl_program,../userspace/apps/desktop-check,desktop-check.app,ui-fonts-native))

.PHONY: reap-check
reap-check: isodir/boot/reap-check.app
$(eval $(call ccl_program,../userspace/apps/reap-check,reap-check.app,ui-fonts-native))

# The native font engine's masks, hashed (tests/headless --test font-masks).
.PHONY: font-masks
font-masks: isodir/boot/font-masks.app
$(eval $(call ccl_program,../userspace/apps/font-masks,font-masks.app,ui-fonts-native))

.PHONY: display-check
display-check: build/gpr/display-check.stamp
build/gpr/display-check.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/apps/display-check/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/apps/display-check/build
	$(call compile_ccl_manifest,../userspace/apps/display-check)
	$(NATIVE_GPRBUILD) -p -P ../userspace/apps/display-check/display_check.gpr -XCUBIT_DISPLAY_TEST_MODE=$(CUBIT_DISPLAY_TEST_MODE) -XCUBIT_GPU_TEST_MODE=$(CUBIT_GPU_TEST_MODE)
	$(call stage,../userspace/apps/display-check/$(if $(filter output-rebind,$(CUBIT_DISPLAY_TEST_MODE)),build-output-rebind,$(if $(filter production,$(CUBIT_GPU_TEST_MODE)),build,build-gpu-$(CUBIT_GPU_TEST_MODE)))/display-check.app,display-check.app)
	@python3 ../tools/gpr_inputs.py build/gpr/display-check.d $@ -- $(NATIVE_GPRLS) -P ../userspace/apps/display-check/display_check.gpr -XCUBIT_DISPLAY_TEST_MODE=$(CUBIT_DISPLAY_TEST_MODE) -XCUBIT_GPU_TEST_MODE=$(CUBIT_GPU_TEST_MODE)
	@touch $@
-include build/gpr/display-check.d

prepare-desktop-protocol:
	mkdir -p ../tests/desktop-protocol/build/source
	cp ../userspace/runtime/gnat/cubit.ads ../userspace/runtime/gnat/cubit-desktop_protocol.ads \
		../userspace/runtime/gnat/cubit-grant_references.ads \
		../userspace/runtime/gnat/cubit-display_protocol.ads \
		../userspace/runtime/gnat/cubit-display_protocol.adb \
		../userspace/runtime/gnat/cubit-present_timing.ads \
		../userspace/runtime/gnat/cubit-present_timing.adb \
		../userspace/runtime/gnat/cubit-desktop_protocol.adb ../tests/desktop-protocol/build/source/

test-desktop-protocol: prepare-desktop-protocol
	$(ALR) gprbuild -p -P ../tests/desktop-protocol/protocol.gpr
	../tests/desktop-protocol/build/main

prove-desktop-protocol: prepare-desktop-protocol
	$(ALR) gnatprove -P ../tests/desktop-protocol/protocol.gpr \
		-u cubit-desktop_protocol.adb cubit-display_protocol.adb cubit-present_timing.adb --level=2 --report=all --checks-as-errors=on -j2
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
input-stress: isodir/boot/input-stress.app
$(eval $(call ccl_program,../userspace/apps/input-stress,input-stress.app,))

.PHONY: grant-forward
# BLD-001 interim: remade only when a fixture source, its manifests, the
# Mesa headers it compiles against or a shared input changed.
grant-forward: build/gpr/grant-forward.stamp
build/gpr/grant-forward.stamp: $(GPR_STEP_INPUTS) $(filter-out %/build,$(wildcard ../tests/grant-forward/native/*)) \
		$(wildcard ../userspace/mesa/anv/*) | user_runtime ccl-manifest
	@set -e; for role in owner reader client intermediary_exit owner_exit desktop; do \
	  dir=../tests/grant-forward/native; \
	  manifest=$$role; case $$role in *_exit) manifest=client;; esac; \
	  mkdir -p $$dir/build/$$role/generated; \
	  ../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/native-runtime-services.ccl \
	    $$dir/$$manifest-manifest.ccl --startup-output $$dir/build/$$role/generated/startup.ads \
	    > $$dir/build/$$role/manifest.S; \
	  $(NATIVE_CC) -c $$dir/build/$$role/manifest.S -o $$dir/build/$$role/manifest.o; \
	  $(NATIVE_GPRBUILD) -p -P $$dir/fixture.gpr -XROLE=$$role; \
	  mkdir -p $(STAGED); \
	  if [ -e isodir/boot/grant-forward-$$role.app ] && cmp -s $$dir/build/$$role/grant-forward-$$role.app $(STAGED)/grant-forward-$$role.app; then :; else \
	    echo "stage grant-forward-$$role.app"; cp $$dir/build/$$role/grant-forward-$$role.app isodir/boot/ && cp $$dir/build/$$role/grant-forward-$$role.app $(STAGED)/; fi; \
	done
	@touch $@

.PHONY : ipctest-server
ipctest-server: isodir/boot/ipctest-server.app
$(eval $(call ccl_program,../userspace/apps/ipctest-server,ipctest-server.app,))

.PHONY : ipctest-client
ipctest-client: build/gpr/ipctest-client.stamp
build/gpr/ipctest-client.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/apps/ipctest-client/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/apps/ipctest-client/build
	$(call compile_ccl_manifest,../userspace/apps/ipctest-client)
	$(NATIVE_GPRBUILD) -P ../userspace/apps/ipctest-client/ipctest_client.gpr
	$(call stage,../userspace/apps/ipctest-client/build/ipctest-client.app,ipctest-client.app)
	$(call stage,../userspace/apps/ipctest-client/build/ipctest-departing.app,ipctest-departing.app)
	@python3 ../tools/gpr_inputs.py build/gpr/ipctest-client.d $@ -- $(NATIVE_GPRLS) -P ../userspace/apps/ipctest-client/ipctest_client.gpr
	@touch $@
-include build/gpr/ipctest-client.d

.PHONY : bench-load
.PHONY : bench-storage
bench-storage: isodir/boot/bench-storage.app
$(eval $(call ccl_program,../userspace/apps/bench-storage,bench-storage.app,))

bench-load: user_runtime ccl-manifest
	mkdir -p ../userspace/apps/bench-load/build
	$(call compile_ccl_manifest,../userspace/apps/bench-load)
	$(NATIVE_GPRBUILD) -p -P ../userspace/apps/bench-load/bench_load.gpr
	cp ../userspace/apps/bench-load/build/bench-load.app isodir/boot/bench-load.app
	cp ../userspace/apps/bench-load/build/bench-input-load.app isodir/boot/bench-input-load.app
	cp ../userspace/apps/bench-load/build/bench-cpu.app isodir/boot/bench-cpu.app
	cp ../userspace/apps/bench-load/build/bench-heap-load.app isodir/boot/bench-heap-load.app
	cp ../userspace/apps/bench-load/build/bench-sbrk-load.app isodir/boot/bench-sbrk-load.app
	cp ../userspace/apps/bench-load/build/bench-burn.app isodir/boot/bench-burn.app
	cp ../userspace/apps/bench-load/build/bench-lock-self.app isodir/boot/bench-lock-self.app
	cp ../userspace/apps/bench-load/build/bench-lock-mutual.app isodir/boot/bench-lock-mutual.app

.PHONY : bench-audio
bench-audio: isodir/boot/bench-audio.app
$(eval $(call ccl_program,../userspace/apps/bench-audio,bench-audio.app,))

.PHONY : bench-input
bench-input: isodir/boot/bench-input.app
$(eval $(call ccl_program,../userspace/apps/bench-input,bench-input.app,))

.PHONY : bench-ipc-server
bench-ipc-server: isodir/boot/bench-ipc-server.app
$(eval $(call ccl_program,../userspace/apps/bench-ipc-server,bench-ipc-server.app,))

.PHONY : bench-ipc-client
bench-ipc-client: isodir/boot/bench-ipc-client.app
$(eval $(call ccl_program,../userspace/apps/bench-ipc-client,bench-ipc-client.app,))

# IPC revision 2 S0: the endpoint fairness benchmark's client
# (tests/ipc-fairness; headless bench-fairness-*), against bench-ipc-server.
.PHONY : bench-fairness
bench-fairness: isodir/boot/fairness-client.app isodir/boot/bench-ipc-server.app
$(eval $(call ccl_program,../tests/ipc-fairness/client,fairness-client.app,))

# Build the freestanding CuBit Control Language VM self-test app.
.PHONY : ccl-run
ccl-run:
	$(ALR) gprbuild -P ../userspace/ccl/ccl_run.gpr -p

.PHONY : ccl-control
ccl-control: build/gpr/ccl-control.stamp
build/gpr/ccl-control.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/ccl/apps/ccl-control/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/ccl/apps/ccl-control/build
	$(call compile_ccl_manifest,../userspace/ccl/apps/ccl-control)
	$(NATIVE_GPRBUILD) -p -P ../userspace/ccl/apps/ccl-control/ccl_control.gpr
	$(call stage,../userspace/ccl/apps/ccl-control/build/ccl-control.app,ccl-control.app)
	mkdir -p isodir/licenses
	cp ../userspace/ccl/remote/CBOR_LICENSE.txt isodir/licenses/cbor_ada.txt
	@python3 ../tools/gpr_inputs.py build/gpr/ccl-control.d $@ -- $(NATIVE_GPRLS) -P ../userspace/ccl/apps/ccl-control/ccl_control.gpr
	@touch $@
-include build/gpr/ccl-control.d

# The LAN remote session (tools/cubit-remote is its host client): off unless
# its startup entry's arguments say listen => (Remote_Listen.LAN port).
.PHONY : ccl-remote
ccl-remote: build/gpr/ccl-remote.stamp
build/gpr/ccl-remote.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/ccl/apps/ccl-remote/manifest.ccl) | user_runtime ccl-manifest ui-fonts-native
	mkdir -p ../userspace/ccl/apps/ccl-remote/build
	$(call compile_ccl_manifest,../userspace/ccl/apps/ccl-remote)
	# gprbuild does not track the manifest object it links: relink when it changed.
	[ ../userspace/ccl/apps/ccl-remote/build/ccl-remote.app -nt ../userspace/ccl/apps/ccl-remote/build/manifest.o ] || rm -f ../userspace/ccl/apps/ccl-remote/build/ccl-remote.app
	$(NATIVE_GPRBUILD) -p -P ../userspace/ccl/apps/ccl-remote/ccl_remote.gpr
	$(call stage,../userspace/ccl/apps/ccl-remote/build/ccl-remote.app,ccl-remote.app)
	@python3 ../tools/gpr_inputs.py build/gpr/ccl-remote.d $@ -- $(NATIVE_GPRLS) -P ../userspace/ccl/apps/ccl-remote/ccl_remote.gpr
	@touch $@
-include build/gpr/ccl-remote.d

.PHONY : test-ccl-remote ccl-web-preview test-ccl-web-preview
test-ccl-remote:
	$(ALR) gprbuild -p -P../tests/ccl-remote/remote.gpr
	../tests/ccl-remote/build/main
	../tests/ccl-remote/build/host_tests
	../tests/ccl-remote/build/periodic_tests
	# The LAN remote session's wire profile, and its host client.
	../tests/ccl-remote/build/remote_wire_tests
	../tests/ccl-remote/build/remote_activity_tests
	# Build output as whole lines (byte-by-byte stderr regression).
	../tests/ccl-remote/build/remote_output_lines_tests
	# Screenshots: the PNG encoder, checked by an independent decoder.
	cd ../tests/ccl-remote && build/png_tests && python3 -I check_png.py
	python3 -I ../tools/cubit-remote/test_cubit_remote.py
	# The Observatory decodes these exact bytes (wire.test.mjs).
	../tests/ccl-remote/build/wire_vectors > ../tests/ccl-remote/build/wire-vectors.json
	cmp ../tests/ccl-remote/build/wire-vectors.json ../userspace/ccl/tools/ccl-observatory/wire-vectors.json

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

# The LAN remote session's wire profile: level 2, no assumptions.
.PHONY: prove-ccl-remote-wire
prove-ccl-remote-wire:
	$(ALR) gnatprove -P../tests/ccl-remote/remote.gpr --subdirs=remote-wire-proof \
		-u remote_wire.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/ccl-remote/build/remote-wire-proof/gnatprove/gnatprove.out

# The LAN remote session in QEMU: pair, eval, logs, files, mail, a build job.
.PHONY: test-ccl-remote-session
test-ccl-remote-session: ccl-remote
	../tests/headless/run.sh --test ccl-remote-session --accel $(CCL_LAB_ACCEL) --timeout 240

CCL_LAB_ACCEL ?= kvm
ccl-remote-lab: ccl-control clock netstack
	../tests/headless/run.sh --test ccl-remote --accel $(CCL_LAB_ACCEL) --timeout 3600 --keep-logs

ccl-web-preview:
	cd ../userspace/ccl/tools/ccl-observatory && npm run preview

test-ccl-web-preview:
	cd ../userspace/ccl/tools/ccl-observatory && npm test

.PHONY : ccl-ui-preview
.PHONY: ui-fonts-host ui-fonts-native
.PHONY: test-ui-fonts
.PHONY: test-procmgr-reads
.PHONY: prepare-display-geometry test-display-geometry prove-display-geometry
.PHONY: prepare-monitor-edid test-monitor-edid prove-monitor-edid
prepare-monitor-edid:
	mkdir -p ../tests/monitor-edid/build/source
	cp ../userspace/runtime/gnat/cubit.ads ../userspace/lib/display/cubit-monitor_edid.ad? ../tests/monitor-edid/build/source/

test-monitor-edid: prepare-monitor-edid
	$(ALR) gprbuild -p -P ../tests/monitor-edid/edid.gpr
	../tests/monitor-edid/build/main

prove-monitor-edid: prepare-monitor-edid
	$(ALR) gnatprove -P ../tests/monitor-edid/edid.gpr -u cubit-monitor_edid.adb --level=2 --report=all --checks-as-errors=on -j2
	python3 ../tests/monitor-edid/check-proof.py

prepare-display-geometry:
	mkdir -p ../tests/display-geometry/build/source
	cp ../userspace/runtime/gnat/cubit.ads \
		../userspace/lib/display/cubit-display_geometry.ads \
		../userspace/lib/display/cubit-display_geometry.adb \
		../tests/display-geometry/build/source/

test-display-geometry: prepare-display-geometry
	$(ALR) gprbuild -p -P ../tests/display-geometry/geometry.gpr
	../tests/display-geometry/build/main

prove-display-geometry: prepare-display-geometry
	$(ALR) gnatprove -P ../tests/display-geometry/geometry.gpr \
		-u cubit-display_geometry.adb --level=2 --report=all --checks-as-errors=on -j2
	python3 ../tests/display-geometry/check-proof.py

test-procmgr-reads:
	$(ALR) gprbuild -p -P ../tests/procmgr-reads/read_tests.gpr
	../tests/procmgr-reads/build/read_tests

.PHONY: prepare-display-layouts test-display-layouts prove-display-layouts
prepare-display-layouts: prepare-display-geometry
	mkdir -p ../tests/display-layouts/build/source
	cp ../tests/display-geometry/build/source/*.ad? \
		../userspace/lib/display/cubit-display_layouts.ads \
		../userspace/lib/display/cubit-display_layouts.adb \
		../userspace/lib/display/cubit-display_arrangement.ads \
		../userspace/lib/display/cubit-display_arrangement.adb \
		../tests/display-layouts/build/source/

test-display-layouts: prepare-display-layouts
	$(ALR) gprbuild -p -P ../tests/display-layouts/layouts.gpr
	../tests/display-layouts/build/main
	../tests/display-layouts/build/primary_test
	../tests/display-layouts/build/pointer_test
	../tests/display-layouts/build/arrangement_test

.PHONY: prove-display-arrangement
prove-display-arrangement: prepare-display-layouts
	$(ALR) gnatprove -P ../tests/display-layouts/layouts.gpr \
		-u cubit-display_arrangement.adb --level=1 --no-inlining --no-loop-unrolling \
		--report=all --checks-as-errors=on -j2
	python3 ../tests/display-layouts/check-arrangement-proof.py

prove-display-layouts: prepare-display-layouts
	$(ALR) gnatprove -P ../tests/display-layouts/layouts.gpr \
		-u cubit-display_layouts.adb --level=2 --report=all --checks-as-errors=on -j2
	python3 ../tests/display-layouts/check-proof.py

.PHONY: prepare-window-placement test-window-placement prove-window-placement
prepare-window-placement: prepare-display-layouts
	mkdir -p ../tests/window-placement/build/source
	cp ../tests/display-layouts/build/source/*.ad? \
		../userspace/lib/display/cubit-window_placement.ads \
		../userspace/lib/display/cubit-window_placement.adb \
		../tests/window-placement/build/source/

test-window-placement: prepare-window-placement
	$(ALR) gprbuild -p -P ../tests/window-placement/placement.gpr
	../tests/window-placement/build/main

prove-window-placement: prepare-window-placement
	$(ALR) gnatprove -P ../tests/window-placement/placement.gpr \
		-u cubit-window_placement.adb --level=2 --report=all --checks-as-errors=on -j2
	python3 ../tests/window-placement/check-proof.py

.PHONY: prepare-output-registry test-output-registry prove-output-registry
prepare-output-registry: prepare-window-placement
	mkdir -p ../tests/output-registry/build/source
	cp ../tests/window-placement/build/source/*.ad? \
		../userspace/lib/display/cubit-output_registry.ad? \
		../userspace/lib/display/cubit-display_outputs.ads \
		../userspace/lib/display/cubit-placement_tickets.ad? \
		../tests/output-registry/build/source/

test-output-registry: prepare-output-registry
	$(ALR) gprbuild -p -P ../tests/output-registry/registry.gpr
	../tests/output-registry/build/main

prove-output-registry: prepare-output-registry
	$(ALR) gnatprove -P ../tests/output-registry/registry.gpr \
		-u cubit-display_outputs.ads registry_proof.ads cubit-placement_tickets.adb \
		--level=2 --report=all --checks-as-errors=on -j2
	python3 ../tests/output-registry/check-proof.py

.PHONY: prepare-output-discovery test-output-discovery prove-output-discovery
prepare-output-discovery: prepare-desktop-protocol
	mkdir -p ../tests/output-discovery/build/source
	cp ../tests/desktop-protocol/build/source/*.ad? \
		../userspace/lib/display/cubit-output_discovery.ad? \
		../userspace/lib/display/cubit-detected_outputs.ad? \
		../tests/output-discovery/build/source/

test-output-discovery: prepare-output-discovery
	$(ALR) gprbuild -p -P ../tests/output-discovery/discovery.gpr
	../tests/output-discovery/build/main
	../tests/output-discovery/build/detected_tests

prove-output-discovery: prepare-output-discovery
	$(ALR) gnatprove -P ../tests/output-discovery/discovery.gpr \
		-u cubit-output_discovery.adb cubit-detected_outputs.adb --level=2 --report=all --checks-as-errors=on -j2
	python3 ../tests/output-discovery/check-proof.py

test-ui-fonts: ui-fonts-host
	$(MAKE) -C ../userspace/rust fonts-test fonts-bench
	$(ALR) gprbuild -p -P ../tests/ui-fonts/fonts_tests.gpr
	../tests/ui-fonts/build/main

ui-fonts-host:
	$(MAKE) -C ../userspace/rust fonts-host

# BLD-001 interim: rebuilt only when a Rust source, the allocator it
# archives with, or the flake (the Rust toolchain) changed.
ui-fonts-native: build/gate/ui-fonts-native.stamp
# The font archive carries no runtime unit (tools/check_archive_overlap.py,
# in its recipe, refuses one: BUG-009); the check reads the runtime archive,
# so a changed runtime runs it again.
build/gate/ui-fonts-native.stamp: $(shell find ../userspace/rust ../userspace/allocator -type d \
		\( -name 'build*' -o -name target \) -prune -o -type f -print) $(HOST_TOOL_INPUTS) \
		../tools/check_archive_overlap.py $(wildcard ../userspace/runtime/adalib/libgnat-user.a) | user_runtime
	$(MAKE) -C ../userspace/rust fonts-native
	@mkdir -p $(@D) && touch $@

ccl-ui-preview: ui-fonts-host
	$(ALR) gprbuild -P ../userspace/ccl/ccl_ui_preview.gpr -p
	cd .. && userspace/ccl/build/ccl-ui-preview/ccl-ui-preview

.PHONY : ccl-workspace-test ccl-workspace-prove
.PHONY : ccl-completion-test ccl-completion-prove
ccl-completion-test:
	$(ALR) gprbuild -p -P ../tests/ccl-completion/completion_tests.gpr
	../tests/ccl-completion/build/main

ccl-completion-prove:
	$(ALR) gnatprove -P ../tests/ccl-completion/completion_tests.gpr \
		-u ccl-catalog-completion.adb ccl-call_context.adb --mode=all --level=1 --timeout=5 --checks-as-errors=on -j2
	@cat ../tests/ccl-completion/build/gnatprove/gnatprove.out

ccl-workspace-test:
	$(ALR) gprbuild -p -P ../tests/ccl-workspace/workspace_tests.gpr
	../tests/ccl-workspace/build/main

ccl-workspace-prove:
	$(ALR) gnatprove -P ../tests/ccl-workspace/workspace_tests.gpr \
		-u ccl_workspace_names.adb --level=2 --report=all --checks-as-errors=on -j2
	@cat ../tests/ccl-workspace/build/gnatprove/gnatprove.out

.PHONY : ui-editor-test
ui-editor-test: ui-fonts-host
	$(ALR) gprbuild -P ../userspace/lib/ui/tests/editor/editor_test.gpr -p
	../userspace/lib/ui/tests/editor/build/main

.PHONY : ui-focus-test
ui-focus-test: ui-fonts-host
	$(ALR) gprbuild -P ../userspace/lib/ui/tests/focus/focus_test.gpr -p
	../userspace/lib/ui/tests/focus/build/main

.PHONY : ui-surfaces-test
ui-surfaces-test: ui-fonts-host
	$(ALR) gprbuild -P ../userspace/lib/ui/tests/surfaces/surfaces_test.gpr -p
	../userspace/lib/ui/tests/surfaces/build/main

.PHONY : ccl-test-native
ccl-test-native:
	$(ALR) gprbuild -P ../userspace/ccl/ccl_native.gpr -p
	../userspace/ccl/build/native/main

.PHONY : ccl-vm
.PHONY: rust-probe
rust-probe: ccl-manifest
	$(MAKE) -C ../userspace/rust
	cp ../userspace/rust/build/rust-probe.app isodir/boot/rust-probe.app
	cp ../userspace/rust/build/rust-probe-denied.app isodir/boot/rust-probe-denied.app
	cp ../userspace/rust/build/rust-probe-mismatch.app isodir/boot/rust-probe-mismatch.app
# CCL.Setting_Kinds and the typed settings' decoders (CCL.Setting_Codecs),
# generated from the setting catalog (CAP-003); the generated files are kept
# in the tree (tests/ccl-manifests compares them).
.PHONY : setting-kinds
setting-kinds: ccl-manifest
	../userspace/ccl/build/manifest/ccl-manifest --setting-kinds ../userspace/ccl/src/ccl-setting_kinds.ads \
	  --schema ../userspace/ccl/interfaces/executable-manifest.ccl
	# The decoders are generated from the kinds compiled into the tool.
	$(ALR) gprbuild -p -P ../userspace/ccl/ccl_manifest.gpr
	../userspace/ccl/build/manifest/ccl-manifest --setting-codecs ../userspace/ccl/src \
	  --schema ../userspace/ccl/interfaces/executable-manifest.ccl

# The asset catalogs' entry decoders (CCL.Asset_Codecs: the wallpaper
# package's catalog lines), generated from their CCL types and kept in the
# tree like the setting codecs (tests/wallpaper-catalog compares them).
.PHONY : asset-codecs
asset-codecs: ccl-manifest
	../userspace/ccl/build/manifest/ccl-manifest --asset-codecs ../userspace/ccl/src

.PHONY : ccl-manifest
ccl-manifest: build/gate/ccl-manifest.stamp
build/gate/ccl-manifest.stamp: $(HOST_TOOL_INPUTS)
	$(ALR) gprbuild -p -P ../userspace/ccl/ccl_manifest.gpr
	@python3 ../tools/gpr_inputs.py build/gate/ccl-manifest.d $@ -- $(HOST_GPRLS) -P ../userspace/ccl/ccl_manifest.gpr
	@touch $@
-include build/gate/ccl-manifest.d
$(CCL_MANIFEST_TOOL): ccl-manifest ;

CCL_CONFIG_TOOL := ../userspace/ccl/build/config/ccl-config
CONFIG_SOURCES := ../init.ccl ../system.ccl \
	$(wildcard ../tests/headless/init-*.ccl) \
	$(wildcard ../tests/hardware/init-*.ccl ../tests/hardware/system-*.ccl)

.PHONY: ccl-config configurations test-ccl-configurations
ccl-config: $(CCL_CONFIG_TOOL)
$(CCL_CONFIG_TOOL): $(wildcard ../userspace/ccl/src/*.ad?) \
	../userspace/ccl/ccl_config.gpr ../userspace/ccl/tools/ccl-config/main.adb
	$(ALR) gprbuild -p -P ../userspace/ccl/ccl_config.gpr

# Optional host preflight; native services evaluate these same source files.
configurations: $(CCL_CONFIG_TOOL)
	@for source in $(CONFIG_SOURCES); do $(CCL_CONFIG_TOOL) $$source || exit 1; done

test-ccl-configurations: configurations
	$(ALR) python3 ../tests/ccl-configurations/test-configurations.py -v

.PHONY: test-ccl-configurations-native
test-ccl-configurations-native: iso nvme_disk.img
	$(ALR) python3 ../tests/ccl-configurations/test-native-rejection.py

CCL_IMAGE_TOOL := ../userspace/ccl/build/image/ccl-image
CCL_IMAGE_REALIZER := ../userspace/ccl/tools/ccl-image/realize.py
CCL_IMAGE_CATALOG := ../images/artifacts.ccl
.PHONY: ccl-image test-ccl-images
ccl-image: $(CCL_IMAGE_TOOL) ccl-config
$(CCL_IMAGE_TOOL): $(wildcard ../userspace/ccl/src/*.ad?) \
	../userspace/ccl/ccl_image.gpr ../userspace/ccl/tools/ccl-image/main.adb \
	../userspace/ccl/tools/ccl-image/device_index.adb
	$(ALR) gprbuild -p -P ../userspace/ccl/ccl_image.gpr
test-ccl-images: ccl-image
	$(ALR) python3 ../tests/ccl-images/test-images.py -v

# The build's timestamp: the source commit time (outside Git, the inherited
# SOURCE_DATE_EPOCH), never earlier than 2026-01-01, because the Nix shell
# exports a 1980 SOURCE_DATE_EPOCH. It is the wall-clock floor (UTC never falls
# below it) and is exported as SOURCE_DATE_EPOCH, which mke2fs, debugfs and
# grub-mkrescue honour: every file and directory in a built image carries it
# (the shell's 1980 value made Files list 1980-01-01 00:00 everywhere).
BUILD_EPOCH_MINIMUM := 1767225600
BUILD_EPOCH := $(shell floor=$$(git -C .. log -1 --format=%ct 2>/dev/null || echo "$${SOURCE_DATE_EPOCH:-0}"); \
  if [ "$$floor" -lt $(BUILD_EPOCH_MINIMUM) ]; then floor=$(BUILD_EPOCH_MINIMUM); fi; echo $$floor)
export SOURCE_DATE_EPOCH := $(BUILD_EPOCH)

# For scripts that build native fixtures outside make (tests/headless/run.sh).
.PHONY: print-native-gprbuild-config
print-native-gprbuild-config:
	@echo $(CUBIT_NATIVE_GPRBUILD_CONFIG)

# For scripts that write into built images (tests/headless/run.sh).
.PHONY: print-build-epoch
print-build-epoch:
	@echo $(BUILD_EPOCH)

# The wall-clock floor package. The file is rewritten only when the value
# changes, to avoid needless rebuilds.
define generate_clock_floor
mkdir -p $(dir $(1))
printf -- '--  Generated by kernel/Makefile: build-time wall-clock floor.\npackage Clock_Floor is\n   UTC_Seconds : constant := %s;\nend Clock_Floor;\n' "$(BUILD_EPOCH)" > $(1).tmp; \
if cmp -s $(1).tmp $(1); then rm -f $(1).tmp; else mv $(1).tmp $(1); fi
endef

# A driver's typed manifest against the driver CSPACE layout devmgr mints:
# its .cubit.resources names the slot devmgr installs device interrupts in
# (docs/msi-interrupts.md), and the bindings give the driver the same slot.
define compile_driver_manifest
$(call compile_manifest_with,../userspace/ccl/catalogs/driver-services.ccl,$(1))
endef

# compile_manifest_with CATALOG,DIR: DIR/manifest.ccl's startup binding
# (DIR/build/generated) and manifest object (DIR/build/manifest.o). The
# generated files replace the old ones only when they changed, and the
# object is assembled again only then (BLD-001): gprbuild and the stage
# step see an unchanged manifest as unchanged.
define compile_manifest_with
mkdir -p $(2)/build/generated $(2)/build/generated.new
../userspace/ccl/build/manifest/ccl-manifest $(1) $(2)/manifest.ccl --startup-output $(2)/build/generated.new/startup.ads > $(2)/build/manifest.S.new
for f in $(2)/build/generated.new/*; do \
  if cmp -s $$f $(2)/build/generated/$${f##*/}; then rm $$f; else mv $$f $(2)/build/generated/; fi; done
if cmp -s $(2)/build/manifest.S.new $(2)/build/manifest.S; then rm $(2)/build/manifest.S.new; \
  else mv $(2)/build/manifest.S.new $(2)/build/manifest.S; fi
[ $(2)/build/manifest.o -nt $(2)/build/manifest.S ] || $(NATIVE_CC) -c $(2)/build/manifest.S -o $(2)/build/manifest.o
endef

# Native CuBit Ada (the user runtime and everything built on it) uses
# GCC/GNAT 15.3, the compiler that also runs on CuBit, through
# userspace/toolchain/gnat-15.cgpr (docs/ccl-build.md). Its compile switches
# map the checkout's absolute path to "." in the debug information, so the
# same sources give the same bytes in any checkout and in CCL build's step
# directories. flake.nix exports CUBIT_GNAT and CUBIT_BINUTILS.
GPRBUILD_LIBEXEC := $(dir $(shell $(ALR) which gprbuild))../libexec/gprbuild
# Exported for the build scripts that run gprbuild themselves (tools/build_*).
export CUBIT_NATIVE_GPRBUILD_CONFIG := --config=$(abspath ../userspace/toolchain/gnat-15.cgpr) \
	-XCUBIT_GNAT=$(CUBIT_GNAT) -XCUBIT_BINUTILS=$(CUBIT_BINUTILS) \
	-XCUBIT_GPRBIND=$(GPRBUILD_LIBEXEC)/gprbind -XCUBIT_GPRLIB=$(GPRBUILD_LIBEXEC)/gprlib \
	-XCUBIT_RUNTIME=$(abspath ../userspace/runtime) -XCUBIT_SOURCE_ROOT=$(abspath ..)
NATIVE_GPRBUILD = $(ALR) gprbuild $(CUBIT_NATIVE_GPRBUILD_CONFIG)
# CCL build (docs/ccl-build.md) builds the migrated programs: their
# component.ccl with build-params.ccl and build-catalog.ccl, outputs copied back to where make put them. The
# Storehouse is shared by every checkout; the toolchain names are
# build-params.ccl's pins.
CCL_BUILD_TOOL := ../userspace/ccl/build/ccl-build/ccl-build
CCL_BUILD_STORE ?= $(or $(XDG_CACHE_HOME),$(HOME)/.cache)/cubit/storehouse
CCL_BUILD = $(CCL_BUILD_TOOL) build --store $(CCL_BUILD_STORE) --source-root $(abspath ..) \
	--import gnat-15.3.0-x86_64-linux-musl=$(CUBIT_GNAT) --import binutils-2.46=$(CUBIT_BINUTILS) \
	--signing $(CUBIT_SIGNING) --out $(abspath ..)

# The host tools gprbuild builds (BLD-001 interim): each recipe runs only
# when an input changed, the inputs gprls lists after a build
# (tools/gpr_inputs.py) plus the flake, which pins the compiler and the
# SPARK crates.
HOST_GPRLS = $(ALR) gprls -d -s
HOST_TOOL_INPUTS = ../flake.lock ../flake.nix
.PHONY: ccl-build
ccl-build: build/gate/ccl-build.stamp
build/gate/ccl-build.stamp: $(HOST_TOOL_INPUTS)
	$(ALR) gprbuild -p -q -P ../userspace/ccl/ccl_build.gpr
	@python3 ../tools/gpr_inputs.py build/gate/ccl-build.d $@ -- $(HOST_GPRLS) -P ../userspace/ccl/ccl_build.gpr
	@touch $@
-include build/gate/ccl-build.d
$(CCL_BUILD_TOOL): ccl-build ;


# Signed binaries (SIGN-001, docs/signed-binaries.md section 7): every native
# program is signed as the last step of its build, after strip. The host key
# lives outside the repository, in CUBIT_SIGNING, made once by
# `make signing-key` (ccl-sign init; SIGNING_NAME names the CA); a build
# without a key stops and names that command. Images name the CA through
# their `builder` field and ccl-image's --import, never by a path here, and
# ccl-image refuses any executable that does not verify (IMAGE_SIGNING).
# Signing changes no program's behaviour: loaders read the ELF prefix, and
# nothing verifies on CuBit until procmgr does.
# (CUBIT_SIGNING is set near the top: the component rules name its key.)
SIGNING_NAME ?= $(shell hostname -s)
CCL_SIGN_TOOL := ../userspace/ccl/build/sign/ccl-sign
.PHONY: ccl-sign signing-key sign-programs
ccl-sign: build/gate/ccl-sign.stamp
build/gate/ccl-sign.stamp: $(HOST_TOOL_INPUTS)
	$(ALR) gprbuild -p -q -P ../userspace/ccl/ccl_sign.gpr
	@python3 ../tools/gpr_inputs.py build/gate/ccl-sign.d $@ -- $(HOST_GPRLS) -P ../userspace/ccl/ccl_sign.gpr
	@touch $@
-include build/gate/ccl-sign.d
signing-key: ccl-sign
	$(CCL_SIGN_TOOL) init --signing $(CUBIT_SIGNING) --name "$(SIGNING_NAME)"
# Strip, then sign, every program staged in isodir/boot (tools/sign_programs.py);
# the kernel is the boot trusted base and stays as GRUB loads it. Image
# recipes run it first, so whatever rule placed a program, it is signed.
SIGN_PROGRAMS = $(ALR) python3 ../tools/sign_programs.py isodir/boot --signing $(CUBIT_SIGNING) \
	--sign-tool $(CCL_SIGN_TOOL) --debug-dir debug --exclude cubit_kernel
IMAGE_SIGNING = --import cubit-signing-ca=$(CUBIT_SIGNING)/builder-ca.der --sign-tool $(CCL_SIGN_TOOL)
sign-programs: ccl-sign
	$(SIGN_PROGRAMS)

# The same compiler driver for native assembly (manifest objects).
NATIVE_CC = $(CUBIT_GNAT)/bin/x86_64-unknown-linux-musl-gcc -B$(CUBIT_BINUTILS)/bin/

# The CCL tools on CuBit (SELF-002): ccl-build.app and ccl-manifest.app,
# the same sources as the Linux tools (ccl-build with the CuBit body of
# Build_Platform), on the GCC port's full Ada runtime (libgnat, libgnarl)
# and the CuBit libc, cross-built by the same GNAT 15.3 with the native
# configuration and the port's toolchain tree as the runtime. Build the
# tree first: userspace/ports/gcc/build.sh (tests/gnat/build.sh runs it).
CCL_TOOL_APP_TREE := $(abspath ../userspace/ports/gcc/build/toolchain)
CCL_TOOL_APP_RUNTIME := $(CCL_TOOL_APP_TREE)/lib/gcc/x86_64-linux-musl/15.3.0
# ccl_tool_app TARGET,PROJECT,OBJECTS,MANIFEST_CCL,MANIFEST_VARIABLE: the
# tool's manifest object in ../userspace/ccl/build/OBJECTS/manifest, then
# PROJECT's cubit target linked with it (MANIFEST_VARIABLE names it).
define ccl_tool_app
@test -f $(CCL_TOOL_APP_RUNTIME)/adalib/libgnat.a || \
  { echo "$(1): no toolchain tree; run userspace/ports/gcc/build.sh first" >&2; exit 1; }
mkdir -p ../userspace/ccl/build/$(3)/manifest
../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/native-runtime-services.ccl \
  $(4) --output ../userspace/ccl/build/$(3)/manifest/manifest.S
$(NATIVE_CC) -c ../userspace/ccl/build/$(3)/manifest/manifest.S -o ../userspace/ccl/build/$(3)/manifest/manifest.o
$(ALR) gprbuild $(subst -XCUBIT_RUNTIME=$(abspath ../userspace/runtime),-XCUBIT_RUNTIME=$(CCL_TOOL_APP_RUNTIME),$(CUBIT_NATIVE_GPRBUILD_CONFIG)) \
  -p -q -P $(2) -XCCL_TOOL_TARGET=cubit -XCUBIT_TOOLCHAIN=$(CCL_TOOL_APP_TREE) \
  -X$(5)=$(abspath ../userspace/ccl/build/$(3)/manifest/manifest.o)
endef
.PHONY: ccl-build-app ccl-manifest-app
ccl-build-app: ccl-manifest
	$(call ccl_tool_app,$@,../userspace/ccl/ccl_build.gpr,ccl-build-cubit,../userspace/ccl/tools/ccl-build/cubit/manifest.ccl,CCL_BUILD_MANIFEST)
	cp ../userspace/ccl/build/ccl-build-cubit/ccl-build.app isodir/boot/ccl-build.app
# The manifest step's tool on CuBit (build-params.ccl's CuBit
# manifest_tool), which the device-build payload installs at @system/tools
# (tests/device-builds/build.sh).
ccl-manifest-app: ccl-manifest
	$(call ccl_tool_app,$@,../userspace/ccl/ccl_manifest.gpr,manifest-cubit,../userspace/ccl/tools/ccl-manifest/cubit/manifest.ccl,CCL_MANIFEST_OBJECT)

# One checked result supplies both the ELF requests and optional Ada bindings.
# Named fixed bindings in this catalog preserve shared-runtime ABI conventions.
define compile_ccl_manifest
$(call compile_manifest_with,../userspace/ccl/catalogs/native-runtime-services.ccl,$(1))
endef

.PHONY: ccl-migrated-manifests
ccl-migrated-manifests: ccl-manifest
	$(ALR) python3 ../tests/ccl-manifests/test-migrations.py

# CAP-002 step 2: the startup-table record, the launcher-side builder, the
# capability handles and a generated Startup binding (hosted; --prove: level 2).
.PHONY : test-startup-tables prove-startup-tables
test-startup-tables:
	bash ../tests/startup-tables/run.sh
prove-startup-tables:
	bash ../tests/startup-tables/run.sh --prove

.PHONY : test-ccl-manifests
test-ccl-manifests: ccl-vm
	$(ALR) gprbuild -p -q -P ../tests/ccl-manifests/resources.gpr
	$(ALR) python3 ../tests/ccl-manifests/test-manifests.py -v

# Level-1 proofs: the .cubit.resources decoder and startup grant decision.
.PHONY : prove-ccl-format
# CCLB v8 module codec: level 1, except one 64-bit conversion at level 3.
prove-ccl-format:
	$(ALR) gnatprove -P../tests/ccl-types/types_tests.gpr --subdirs=format-proof -u ccl-format.adb --level=1 --checks-as-errors=off -j0
	$(ALR) gnatprove -P../tests/ccl-types/types_tests.gpr --subdirs=format-proof -u ccl-format.adb \
		--limit-line=ccl-format.adb:$$(grep -n 'Magnitude : constant Integer_64 := Integer_64 (Found);' ../userspace/ccl/modules/ccl-format.adb | cut -d: -f1) \
		--level=3 --checks-as-errors=on -j0

.PHONY : prove-ccl-startup
prove-ccl-startup:
	$(ALR) gnatprove -P../tests/ccl-manifests/resources.gpr -u ccl-resource_sections.adb --level=1 -j0
	$(ALR) gnatprove -P../tests/ccl-manifests/startup_grants.gpr -u ccl-startup_grants.adb --level=1 -j0

ccl-vm: build/gpr/ccl-vm.stamp
build/gpr/ccl-vm.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/ccl/apps/ccl-vm/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/ccl/apps/ccl-vm/build/generated
	../userspace/ccl/build/manifest/ccl-manifest \
		../userspace/ccl/catalogs/bootstrap-services.ccl \
		../userspace/ccl/apps/ccl-vm/manifest.ccl \
		--startup-output ../userspace/ccl/apps/ccl-vm/build/generated/startup.ads \
		> ../userspace/ccl/apps/ccl-vm/build/manifest.S
	$(NATIVE_CC) -c ../userspace/ccl/apps/ccl-vm/build/manifest.S \
		-o ../userspace/ccl/apps/ccl-vm/build/manifest.o
	$(NATIVE_GPRBUILD) -P ../userspace/ccl/apps/ccl-vm/ccl_vm.gpr
	$(call stage,../userspace/ccl/apps/ccl-vm/build/ccl-vm.app,ccl-vm.app)
	@python3 ../tools/gpr_inputs.py build/gpr/ccl-vm.d $@ -- $(NATIVE_GPRLS) -P ../userspace/ccl/apps/ccl-vm/ccl_vm.gpr
	@touch $@
-include build/gpr/ccl-vm.d

# Native CCL editor/compiler/debugger application.
.PHONY : ccl-workbench
ccl-workbench: build/gpr/ccl-workbench.stamp
build/gpr/ccl-workbench.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/ccl/apps/ccl-workbench/manifest.ccl) | user_runtime ccl-manifest ui-fonts-native
	mkdir -p ../userspace/ccl/apps/ccl-workbench/build
	$(call compile_ccl_manifest,../userspace/ccl/apps/ccl-workbench)
	$(NATIVE_GPRBUILD) -P ../userspace/ccl/apps/ccl-workbench/ccl_workbench.gpr
	$(call stage,../userspace/ccl/apps/ccl-workbench/build/ccl-workbench.app,ccl-workbench.app)
	@python3 ../tools/gpr_inputs.py build/gpr/ccl-workbench.d $@ -- $(NATIVE_GPRLS) -P ../userspace/ccl/apps/ccl-workbench/ccl_workbench.gpr
	@touch $@
-include build/gpr/ccl-workbench.d

# Native CCL console: a full-window REPL over the Workbench's host environment.
.PHONY : ccl-console
ccl-console: build/gpr/ccl-console.stamp
build/gpr/ccl-console.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/ccl/apps/ccl-console/manifest.ccl) | user_runtime ccl-manifest ui-fonts-native
	mkdir -p ../userspace/ccl/apps/ccl-console/build
	$(call compile_ccl_manifest,../userspace/ccl/apps/ccl-console)
	# gprbuild does not track the manifest object it links: relink when it changed.
	[ ../userspace/ccl/apps/ccl-console/build/ccl-console.app -nt ../userspace/ccl/apps/ccl-console/build/manifest.o ] || rm -f ../userspace/ccl/apps/ccl-console/build/ccl-console.app
	$(NATIVE_GPRBUILD) -P ../userspace/ccl/apps/ccl-console/ccl_console.gpr
	$(call stage,../userspace/ccl/apps/ccl-console/build/ccl-console.app,ccl-console.app)
	@python3 ../tools/gpr_inputs.py build/gpr/ccl-console.d $@ -- $(NATIVE_GPRLS) -P ../userspace/ccl/apps/ccl-console/ccl_console.gpr
	@touch $@
-include build/gpr/ccl-console.d

# Headless-test variant for net-fetch (SELF-003 step 1): ccl-console with
# only .cubit.access replaced: the production manifest with its one tls
# scope rewritten to the fixture's name and port (tests/net-fetch/server.py)
# instead of "*:443", so nothing else can drift; checked as llm-test is.
# Only the net-fetch test image installs it; no production image has it.
CONSOLE_FETCH_SCOPE := (tls "fetch-test.cubit.internal:18496")
# BLD-001: each test variant below is remade only when its production
# program, its test manifest, the manifest tool or a catalog changed.
VARIANT_INPUTS = $(CCL_MANIFEST_TOOL) $(wildcard ../userspace/ccl/catalogs/*.ccl)
CONSOLE_FETCH_VARIANT := ../tests/net-fetch/build/variant
CONSOLE_FETCH_APP := ../userspace/ccl/apps/ccl-console/build/ccl-console.app
$(CONSOLE_FETCH_APP): ccl-console ;
$(STAGED)/ccl-remote.app: ccl-remote ;
.PHONY : ccl-console-fetch-test
ccl-console-fetch-test: build/gate/ccl-console-fetch-test.stamp
build/gate/ccl-console-fetch-test.stamp: $(VARIANT_INPUTS) $(CONSOLE_FETCH_APP) ../userspace/ccl/apps/ccl-console/manifest.ccl | ccl-console
	mkdir -p $(CONSOLE_FETCH_VARIANT)/build/generated $(UNSTAGED)
	sed 's|(tls "\*:443")|$(CONSOLE_FETCH_SCOPE)|' ../userspace/ccl/apps/ccl-console/manifest.ccl \
		> $(CONSOLE_FETCH_VARIANT)/manifest.ccl
	grep -qF '$(CONSOLE_FETCH_SCOPE)' $(CONSOLE_FETCH_VARIANT)/manifest.ccl
	$(call compile_ccl_manifest,$(CONSOLE_FETCH_VARIANT))
	cmp $(CONSOLE_FETCH_VARIANT)/build/generated/startup.ads \
		../userspace/ccl/apps/ccl-console/build/generated/startup.ads
	$(ALR) objcopy --dump-section .cubit.startup=$(CONSOLE_FETCH_VARIANT)/startup-test.bin \
		$(CONSOLE_FETCH_VARIANT)/build/manifest.o
	$(ALR) objcopy --dump-section .cubit.startup=$(CONSOLE_FETCH_VARIANT)/startup-production.bin \
		$(CONSOLE_FETCH_APP) $(CONSOLE_FETCH_VARIANT)/scratch.out
	cmp $(CONSOLE_FETCH_VARIANT)/startup-test.bin $(CONSOLE_FETCH_VARIANT)/startup-production.bin
	rm -f $(CONSOLE_FETCH_VARIANT)/access-test.bin $(CONSOLE_FETCH_VARIANT)/access-production.bin
	$(ALR) objcopy --dump-section .cubit.access=$(CONSOLE_FETCH_VARIANT)/access-test.bin \
		$(CONSOLE_FETCH_VARIANT)/build/manifest.o
	$(ALR) objcopy --dump-section .cubit.access=$(CONSOLE_FETCH_VARIANT)/access-production.bin \
		$(CONSOLE_FETCH_APP) $(CONSOLE_FETCH_VARIANT)/scratch.out
	test -s $(CONSOLE_FETCH_VARIANT)/access-test.bin
	test $$(stat -c %s $(CONSOLE_FETCH_VARIANT)/access-test.bin) -eq \
		$$(stat -c %s $(CONSOLE_FETCH_VARIANT)/access-production.bin)
	! cmp -s $(CONSOLE_FETCH_VARIANT)/access-test.bin $(CONSOLE_FETCH_VARIANT)/access-production.bin
	$(ALR) objcopy --update-section .cubit.access=$(CONSOLE_FETCH_VARIANT)/access-test.bin \
		$(CONSOLE_FETCH_APP) $(UNSTAGED)/ccl-console-fetch-test.app
	$(ALR) objcopy --dump-section .cubit.access=$(CONSOLE_FETCH_VARIANT)/access-installed.bin \
		$(UNSTAGED)/ccl-console-fetch-test.app
	cmp $(CONSOLE_FETCH_VARIANT)/access-test.bin $(CONSOLE_FETCH_VARIANT)/access-installed.bin
	$(call stage,$(UNSTAGED)/ccl-console-fetch-test.app,ccl-console-fetch-test.app)
	@mkdir -p $(@D) && touch $@

# The Agent app (AGENT-002): the agent's own window over the console's host
# environment and the agent core.
.PHONY : agent
agent: build/gpr/agent.stamp
build/gpr/agent.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/ccl/apps/agent/manifest.ccl) | user_runtime ccl-manifest ui-fonts-native
	mkdir -p ../userspace/ccl/apps/agent/build
	$(call compile_ccl_manifest,../userspace/ccl/apps/agent)
	# gprbuild does not track the manifest object it links: relink when it changed.
	[ ../userspace/ccl/apps/agent/build/agent.app -nt ../userspace/ccl/apps/agent/build/manifest.o ] || rm -f ../userspace/ccl/apps/agent/build/agent.app
	$(NATIVE_GPRBUILD) -P ../userspace/ccl/apps/agent/agent_app.gpr
	$(call stage,../userspace/ccl/apps/agent/build/agent.app,agent.app)
	@python3 ../tools/gpr_inputs.py build/gpr/agent.d $@ -- $(NATIVE_GPRLS) -P ../userspace/ccl/apps/agent/agent_app.gpr
	@touch $@
-include build/gpr/agent.d

# Test-only service used to exercise CCL's typed host-import IPC adapter.
.PHONY : ccl-test-host
ccl-test-host: isodir/boot/ccl-test-host.svc
$(eval $(call ccl_program,../userspace/ccl/services/test-host,ccl-test-host.svc,))

# SNTP time synchronization service.
.PHONY : timesync timesync-test
timesync: isodir/boot/timesync.svc
$(eval $(call ccl_program,../userspace/services/timesync,timesync.svc,))

# Headless-test variant: the same executable with only .cubit.caps replaced by
# tests/timesync/manifest-test.ccl (loopback fixture ports instead of 123).
# Fails unless the startup bindings, the startup table (.cubit.startup) and
# the capability section size are identical.
TIMESYNC_VARIANT := ../tests/timesync/build/variant
timesync-test: build/gate/timesync-test.stamp
build/gate/timesync-test.stamp: $(VARIANT_INPUTS) ../userspace/services/timesync/build/timesync.svc ../tests/timesync/manifest-test.ccl | timesync
	mkdir -p $(TIMESYNC_VARIANT)/build/generated $(UNSTAGED)
	cp ../tests/timesync/manifest-test.ccl $(TIMESYNC_VARIANT)/manifest.ccl
	$(call compile_ccl_manifest,$(TIMESYNC_VARIANT))
	cmp $(TIMESYNC_VARIANT)/build/generated/startup.ads \
		../userspace/services/timesync/build/generated/startup.ads
	$(ALR) objcopy --dump-section .cubit.startup=$(TIMESYNC_VARIANT)/startup-test.bin \
		$(TIMESYNC_VARIANT)/build/manifest.o
	$(ALR) objcopy --dump-section .cubit.startup=$(TIMESYNC_VARIANT)/startup-production.bin \
		../userspace/services/timesync/build/timesync.svc $(TIMESYNC_VARIANT)/scratch.out
	cmp $(TIMESYNC_VARIANT)/startup-test.bin $(TIMESYNC_VARIANT)/startup-production.bin
	rm -f $(TIMESYNC_VARIANT)/caps-test.bin $(TIMESYNC_VARIANT)/caps-production.bin
	$(ALR) objcopy --dump-section .cubit.caps=$(TIMESYNC_VARIANT)/caps-test.bin \
		$(TIMESYNC_VARIANT)/build/manifest.o
	$(ALR) objcopy --dump-section .cubit.caps=$(TIMESYNC_VARIANT)/caps-production.bin \
		../userspace/services/timesync/build/timesync.svc $(TIMESYNC_VARIANT)/scratch.out
	test -s $(TIMESYNC_VARIANT)/caps-test.bin
	test $$(stat -c %s $(TIMESYNC_VARIANT)/caps-test.bin) -eq \
		$$(stat -c %s $(TIMESYNC_VARIANT)/caps-production.bin)
	! cmp -s $(TIMESYNC_VARIANT)/caps-test.bin $(TIMESYNC_VARIANT)/caps-production.bin
	$(ALR) objcopy --update-section .cubit.caps=$(TIMESYNC_VARIANT)/caps-test.bin \
		../userspace/services/timesync/build/timesync.svc $(UNSTAGED)/timesync-test.svc
	$(ALR) objcopy --dump-section .cubit.caps=$(TIMESYNC_VARIANT)/caps-installed.bin \
		$(UNSTAGED)/timesync-test.svc
	cmp $(TIMESYNC_VARIANT)/caps-test.bin $(TIMESYNC_VARIANT)/caps-installed.bin
	$(call stage,$(UNSTAGED)/timesync-test.svc,timesync-test.svc)
	@mkdir -p $(@D) && touch $@

# Headless-test variant of logstore (ABI-002, Test_Console_ABI): logstore.svc
# with only .cubit.caps and .cubit.startup replaced by
# tests/headless/logstore-test/manifest.ccl, which adds the test console
# object (logstore mirrors programs' console text onto the serial console).
# Every other manifest section must equal the shipped logstore's. Only
# tests/headless/run.sh installs it (as logstore.svc, for a plan approving
# (test-console approve-declared)); no shipped image has it, and
# tests/test-console checks that.
LOGSTORE_VARIANT := ../tests/headless/logstore-test/build/variant
LOGSTORE_SECTIONS_SAME := id access resources devices launch description channels parameters
.PHONY : logstore-test
logstore-test: build/gate/logstore-test.stamp
build/gate/logstore-test.stamp: $(VARIANT_INPUTS) ../userspace/services/logstore/build/logstore.svc ../tests/headless/logstore-test/manifest.ccl | logstore
	mkdir -p $(LOGSTORE_VARIANT)/build/generated $(UNSTAGED)
	cp ../tests/headless/logstore-test/manifest.ccl $(LOGSTORE_VARIANT)/manifest.ccl
	$(call compile_ccl_manifest,$(LOGSTORE_VARIANT))
	set -e; for section in $(LOGSTORE_SECTIONS_SAME); do \
		rm -f $(LOGSTORE_VARIANT)/$$section-test.bin $(LOGSTORE_VARIANT)/$$section-production.bin; \
		$(ALR) objcopy --dump-section .cubit.$$section=$(LOGSTORE_VARIANT)/$$section-test.bin \
			$(LOGSTORE_VARIANT)/build/manifest.o $(LOGSTORE_VARIANT)/scratch.o 2>/dev/null || true; \
		$(ALR) objcopy --dump-section .cubit.$$section=$(LOGSTORE_VARIANT)/$$section-production.bin \
			../userspace/services/logstore/build/logstore.svc $(LOGSTORE_VARIANT)/scratch.svc 2>/dev/null || true; \
		if [ -e $(LOGSTORE_VARIANT)/$$section-test.bin ] || [ -e $(LOGSTORE_VARIANT)/$$section-production.bin ]; then \
			cmp $(LOGSTORE_VARIANT)/$$section-test.bin $(LOGSTORE_VARIANT)/$$section-production.bin; \
		fi; \
	done
	set -e; for section in caps startup; do \
		rm -f $(LOGSTORE_VARIANT)/$$section-test.bin $(LOGSTORE_VARIANT)/$$section-production.bin; \
		$(ALR) objcopy --dump-section .cubit.$$section=$(LOGSTORE_VARIANT)/$$section-test.bin \
			$(LOGSTORE_VARIANT)/build/manifest.o $(LOGSTORE_VARIANT)/scratch.o; \
		$(ALR) objcopy --dump-section .cubit.$$section=$(LOGSTORE_VARIANT)/$$section-production.bin \
			../userspace/services/logstore/build/logstore.svc $(LOGSTORE_VARIANT)/scratch.svc; \
		! cmp -s $(LOGSTORE_VARIANT)/$$section-test.bin $(LOGSTORE_VARIANT)/$$section-production.bin; \
	done
	$(ALR) objcopy --update-section .cubit.caps=$(LOGSTORE_VARIANT)/caps-test.bin \
		--update-section .cubit.startup=$(LOGSTORE_VARIANT)/startup-test.bin \
		../userspace/services/logstore/build/logstore.svc $(UNSTAGED)/logstore-test.svc
	$(ALR) objcopy --dump-section .cubit.startup=$(LOGSTORE_VARIANT)/startup-installed.bin \
		$(UNSTAGED)/logstore-test.svc $(LOGSTORE_VARIANT)/scratch.svc
	cmp $(LOGSTORE_VARIANT)/startup-test.bin $(LOGSTORE_VARIANT)/startup-installed.bin
	$(call stage,$(UNSTAGED)/logstore-test.svc,logstore-test.svc)
	@mkdir -p $(@D) && touch $@

# Headless-test variant of ccl-remote (ABI-002): ccl-remote.app with only
# .cubit.caps and .cubit.startup replaced by its manifest plus the test
# console object request, generated here from the shipped manifest (no
# second copy to drift). With display => Pairing_Display.Test_Console it
# shows the pairing code there, never in logstore (the code is a secret).
# Only tests/headless/run.sh (for a plan whose ccl-remote entry approves
# (test-console approve-declared)) and the device-build test images use
# it; tests/test-console checks no shipped image has it.
CCL_REMOTE_VARIANT := ../tests/headless/ccl-remote-test/build/variant
CCL_REMOTE_SECTIONS_SAME := id access resources devices launch description channels parameters
.PHONY : ccl-remote-test
ccl-remote-test: build/gate/ccl-remote-test.stamp
build/gate/ccl-remote-test.stamp: $(VARIANT_INPUTS) $(STAGED)/ccl-remote.app ../userspace/ccl/apps/ccl-remote/manifest.ccl | ccl-remote
	mkdir -p $(CCL_REMOTE_VARIANT)/build/generated $(UNSTAGED)
	sed 's|^requests => \[$$|requests => [\n(Request.Kernel_Object (Kernel_Object_Request Kernel_Object.Test_Console "test-console"))|' \
		../userspace/ccl/apps/ccl-remote/manifest.ccl > $(CCL_REMOTE_VARIANT)/manifest.ccl
	grep -q 'Kernel_Object.Test_Console' $(CCL_REMOTE_VARIANT)/manifest.ccl
	$(call compile_ccl_manifest,$(CCL_REMOTE_VARIANT))
	set -e; for section in $(CCL_REMOTE_SECTIONS_SAME); do \
		rm -f $(CCL_REMOTE_VARIANT)/$$section-test.bin $(CCL_REMOTE_VARIANT)/$$section-production.bin; \
		$(ALR) objcopy --dump-section .cubit.$$section=$(CCL_REMOTE_VARIANT)/$$section-test.bin \
			$(CCL_REMOTE_VARIANT)/build/manifest.o $(CCL_REMOTE_VARIANT)/scratch.o 2>/dev/null || true; \
		$(ALR) objcopy --dump-section .cubit.$$section=$(CCL_REMOTE_VARIANT)/$$section-production.bin \
			$(STAGED)/ccl-remote.app $(CCL_REMOTE_VARIANT)/scratch.app 2>/dev/null || true; \
		if [ -e $(CCL_REMOTE_VARIANT)/$$section-test.bin ] || [ -e $(CCL_REMOTE_VARIANT)/$$section-production.bin ]; then \
			cmp $(CCL_REMOTE_VARIANT)/$$section-test.bin $(CCL_REMOTE_VARIANT)/$$section-production.bin; \
		fi; \
	done
	set -e; for section in caps startup; do \
		$(ALR) objcopy --dump-section .cubit.$$section=$(CCL_REMOTE_VARIANT)/$$section-test.bin \
			$(CCL_REMOTE_VARIANT)/build/manifest.o $(CCL_REMOTE_VARIANT)/scratch.o; \
	done
	$(ALR) objcopy --update-section .cubit.caps=$(CCL_REMOTE_VARIANT)/caps-test.bin \
		--update-section .cubit.startup=$(CCL_REMOTE_VARIANT)/startup-test.bin \
		$(STAGED)/ccl-remote.app $(UNSTAGED)/ccl-remote-test.app
	$(call stage,$(UNSTAGED)/ccl-remote-test.app,ccl-remote-test.app)
	@mkdir -p $(@D) && touch $@

# The Messages API client (AGENT-001 slice 1) and the secret store v0
# (SEC-022) that holds its API key.
.PHONY : llm secrets llm-test
llm: isodir/boot/llm.svc
$(eval $(call ccl_program,../userspace/services/llm,llm.svc,))

secrets: isodir/boot/secrets.svc
$(eval $(call ccl_program,../userspace/services/secrets,secrets.svc,))

# power.svc, the power manager (PWR-003 slice 1, CuBit.Power_Requests).
.PHONY : power
power: isodir/boot/power.svc
$(eval $(call ccl_program,../userspace/services/power,power.svc,))

# Headless-test variant for llm-mock: llm.svc with only .cubit.access
# replaced by tests/llm/manifest-test.ccl (the mock's name and port
# instead of api.anthropic.com:443), checked as timesync-test is. Only the
# llm-mock test image installs it; no production image has it.
LLM_VARIANT := ../tests/llm/build/variant
llm-test: build/gate/llm-test.stamp
build/gate/llm-test.stamp: $(VARIANT_INPUTS) ../userspace/services/llm/build/llm.svc ../tests/llm/manifest-test.ccl | llm
	mkdir -p $(LLM_VARIANT)/build/generated $(UNSTAGED)
	cp ../tests/llm/manifest-test.ccl $(LLM_VARIANT)/manifest.ccl
	$(call compile_ccl_manifest,$(LLM_VARIANT))
	cmp $(LLM_VARIANT)/build/generated/startup.ads \
		../userspace/services/llm/build/generated/startup.ads
	$(ALR) objcopy --dump-section .cubit.startup=$(LLM_VARIANT)/startup-test.bin \
		$(LLM_VARIANT)/build/manifest.o
	$(ALR) objcopy --dump-section .cubit.startup=$(LLM_VARIANT)/startup-production.bin \
		../userspace/services/llm/build/llm.svc $(LLM_VARIANT)/scratch.out
	cmp $(LLM_VARIANT)/startup-test.bin $(LLM_VARIANT)/startup-production.bin
	rm -f $(LLM_VARIANT)/access-test.bin $(LLM_VARIANT)/access-production.bin
	$(ALR) objcopy --dump-section .cubit.access=$(LLM_VARIANT)/access-test.bin \
		$(LLM_VARIANT)/build/manifest.o
	$(ALR) objcopy --dump-section .cubit.access=$(LLM_VARIANT)/access-production.bin \
		../userspace/services/llm/build/llm.svc $(LLM_VARIANT)/scratch.out
	test -s $(LLM_VARIANT)/access-test.bin
	test $$(stat -c %s $(LLM_VARIANT)/access-test.bin) -eq \
		$$(stat -c %s $(LLM_VARIANT)/access-production.bin)
	! cmp -s $(LLM_VARIANT)/access-test.bin $(LLM_VARIANT)/access-production.bin
	$(ALR) objcopy --update-section .cubit.access=$(LLM_VARIANT)/access-test.bin \
		../userspace/services/llm/build/llm.svc $(UNSTAGED)/llm-test.svc
	$(ALR) objcopy --dump-section .cubit.access=$(LLM_VARIANT)/access-installed.bin \
		$(UNSTAGED)/llm-test.svc
	cmp $(LLM_VARIANT)/access-test.bin $(LLM_VARIANT)/access-installed.bin
	$(call stage,$(UNSTAGED)/llm-test.svc,llm-test.svc)
	@mkdir -p $(@D) && touch $@

.PHONY : timesync-check
timesync-check: isodir/boot/timesync-check.app
$(eval $(call ccl_program,../userspace/apps/timesync-check,timesync-check.app,))

# SPARKTLS and its dependencies are pinned flake inputs; nix develop exports
# them as one directory in CUBIT_SPARK_CRATES.
.PHONY : tls-crates tls-roots tls-test-pki tls-probe test-tls-scopes prove-tls-scopes
test-tls-scopes:
	$(ALR) gprbuild -p -P../tests/tls/scopes_tests.gpr
	../tests/tls/build/scopes/scopes_main

prove-tls-scopes:
	$(ALR) gnatprove -P../tests/tls/scopes_tests.gpr -u cubit-tls_scopes.adb \
		--level=2 --report=all --checks-as-errors=on -j2

tls-crates:
	@test -n "$(CUBIT_SPARK_CRATES)" || \
	  { echo "CUBIT_SPARK_CRATES is unset: build inside nix develop" >&2; exit 1; }

# tls.svc's trust store: the pinned Mozilla root set (flake.nix cubitCaCerts)
# as concatenated DER, installed on the development disk as tls/roots.der.
# A store holding a distrusted root (tests/tls-roots/distrusted.txt) is
# never produced.
build/tls-roots.der: $(CUBIT_CA_BUNDLE) ../tools/pem_bundle_to_der.py \
		../tests/tls-roots/check.sh ../tests/tls-roots/distrusted.txt
	@test -n "$(CUBIT_CA_BUNDLE)" || \
	  { echo "CUBIT_CA_BUNDLE is unset: build inside nix develop" >&2; exit 1; }
	mkdir -p build
	python3 ../tools/pem_bundle_to_der.py $(CUBIT_CA_BUNDLE) $@.new
	../tests/tls-roots/check.sh $@.new
	mv $@.new $@
tls-roots: build/tls-roots.der

# Test-only PKI (git-ignored) for the TLS regressions.
tls-test-pki:
	../tests/tls/make-pki.sh

.PHONY : bench-spread
bench-spread: isodir/boot/bench-spread.app
$(eval $(call ccl_program,../userspace/apps/bench-spread,bench-spread.app,))

# CuBit Teapots (userspace/apps/teapots): the Vulkan teapot gallery on a GPU
# canvas (CuBit.UI.GPU_Canvas), linked against the Desktop's combined Mesa
# (ANV) bundle. Also stages the Mesa notice bundle the live images carry.
.PHONY: teapots
# BLD-001 interim: Teapots and Neon Blast are linked against the combined
# Mesa bundle by their scripts; each runs only when an input changed: its
# Ada closure (gprls, tools/gpr_inputs.py), its directory, the shared
# inputs, the CuBit libc and the Mesa bundle selected (current.json).
MESA_STEP_INPUTS = $(wildcard ../userspace/mesa/build/combined/current.json) \
	$(wildcard ../userspace/mesa/*.sh ../userspace/mesa/*.py) ../userspace/lib/ui/gpu_canvas_vk.c
teapots: build/gpr/teapots.stamp
build/gpr/teapots.stamp: $(GPR_STEP_INPUTS) $(MESA_STEP_INPUTS) $(LIBC_STAMP) ../tools/build_teapots.py \
		$(filter-out %/build,$(wildcard ../userspace/apps/teapots/*)) | libc user_runtime ccl-manifest ui-fonts-native
	bash ../userspace/mesa/build.sh
	@python3 ../tools/gpr_inputs.py build/gpr/teapots.d $@ -- $(NATIVE_GPRLS) -P ../userspace/apps/teapots/teapots.gpr
	@touch $@
-include build/gpr/teapots.d

# Neon Blast (userspace/apps/neon-blast, APP-008): the musical vaporwave
# arena shooter, Vulkan on a GPU canvas with a software fallback, linked
# against the same combined Mesa (ANV) bundle as Teapots (whose build
# stages the Mesa notices the images carry).
.PHONY: neon-blast
neon-blast: build/gpr/neon-blast.stamp
build/gpr/neon-blast.stamp: $(GPR_STEP_INPUTS) $(MESA_STEP_INPUTS) $(LIBC_STAMP) ../tools/build_neon_blast.py \
		$(filter-out %/build,$(wildcard ../userspace/apps/neon-blast/*)) | libc user_runtime ccl-manifest ui-fonts-native
	mkdir -p $(UNSTAGED)
	python3 ../tools/build_neon_blast.py --output $(UNSTAGED)/neon-blast.app
	$(call stage,$(UNSTAGED)/neon-blast.app,neon-blast.app)
	@python3 ../tools/gpr_inputs.py build/gpr/neon-blast.d $@ -- $(NATIVE_GPRLS) -P ../userspace/apps/neon-blast/neon_blast.gpr
	@touch $@
-include build/gpr/neon-blast.d

# CuBit libc (userspace/libc): musl over CuBit system calls, and its checks.

.PHONY : libc libc-check cxx-check
libc: $(LIBC_STAMP)
$(LIBC_STAMP): $(LIBC_INPUTS)
	bash ../userspace/libc/build.sh
	touch $@
libc-check: build/gate/libc-check.stamp
build/gate/libc-check.stamp: $(LIBC_STAMP) $(CCL_MANIFEST_TOOL) $(wildcard ../userspace/ccl/catalogs/*.ccl) ../userspace/ccl/interfaces/executable-manifest.ccl $(wildcard ../userspace/libc/tests/libc-check.c ../userspace/libc/tests/manifest.ccl ../userspace/c/*.h) | libc ccl-manifest libc-protection-check
	mkdir -p ../userspace/libc/build/libc-check
	../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/native-runtime-services.ccl ../userspace/libc/tests/manifest.ccl --schema ../userspace/ccl/interfaces/executable-manifest.ccl --c-output ../userspace/libc/build/libc-check/startup.h > ../userspace/libc/build/libc-check-manifest.S
	as --64 ../userspace/libc/build/libc-check-manifest.S -o ../userspace/libc/build/libc-check-manifest.o
	../userspace/libc/cubit-cc -O2 -g -pthread -I../userspace/libc/build/libc-check -I../userspace/c -o $(UNSTAGED)/libc-check.app ../userspace/libc/tests/libc-check.c --manifest ../userspace/libc/build/libc-check-manifest.o
	$(call stage,$(UNSTAGED)/libc-check.app,libc-check.app)
	@mkdir -p $(@D) && touch $@
cxx-check: build/gate/cxx-check.stamp
build/gate/cxx-check.stamp: $(LIBC_STAMP) $(CCL_MANIFEST_TOOL) $(wildcard ../userspace/ccl/catalogs/*.ccl) ../userspace/ccl/interfaces/executable-manifest.ccl $(wildcard ../userspace/libc/tests/cxx-check.cpp ../userspace/libc/tests/cxx-manifest.ccl) | libc ccl-manifest
	mkdir -p $(UNSTAGED)
	../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/native-runtime-services.ccl ../userspace/libc/tests/cxx-manifest.ccl --schema ../userspace/ccl/interfaces/executable-manifest.ccl > ../userspace/libc/build/cxx-check-manifest.S
	as --64 ../userspace/libc/build/cxx-check-manifest.S -o ../userspace/libc/build/cxx-check-manifest.o
	../userspace/libc/cubit-c++ -O2 -g -pthread -o $(UNSTAGED)/cxx-check.app ../userspace/libc/tests/cxx-check.cpp --manifest ../userspace/libc/build/cxx-check-manifest.o
	$(call stage,$(UNSTAGED)/cxx-check.app,cxx-check.app)
	@mkdir -p $(@D) && touch $@

.PHONY: libc-protection-check
libc-protection-check: build/gate/libc-protection-check.stamp
build/gate/libc-protection-check.stamp: $(LIBC_STAMP) $(CCL_MANIFEST_TOOL) $(wildcard ../userspace/ccl/catalogs/*.ccl) ../userspace/ccl/interfaces/executable-manifest.ccl $(wildcard ../userspace/libc/tests/protection-fault.c ../userspace/libc/tests/protection-manifest.ccl) | libc ccl-manifest
	mkdir -p $(UNSTAGED)
	../userspace/ccl/build/manifest/ccl-manifest ../userspace/ccl/catalogs/native-runtime-services.ccl ../userspace/libc/tests/protection-manifest.ccl > ../userspace/libc/build/protection-manifest.S
	as --64 ../userspace/libc/build/protection-manifest.S -o ../userspace/libc/build/protection-manifest.o
	../userspace/libc/cubit-cc -O2 -g -o $(UNSTAGED)/guard-fault.app ../userspace/libc/tests/protection-fault.c --manifest ../userspace/libc/build/protection-manifest.o
	$(call stage,$(UNSTAGED)/guard-fault.app,guard-fault.app)
	../userspace/libc/cubit-cc -O2 -g -DFAULT_READ_ONLY=1 -o $(UNSTAGED)/readonly-fault.app ../userspace/libc/tests/protection-fault.c --manifest ../userspace/libc/build/protection-manifest.o
	$(call stage,$(UNSTAGED)/readonly-fault.app,readonly-fault.app)
	../userspace/libc/cubit-cc -O2 -g -DFAULT_DISCARD_READ_ONLY=1 -o $(UNSTAGED)/discard-readonly-fault.app ../userspace/libc/tests/protection-fault.c --manifest ../userspace/libc/build/protection-manifest.o
	$(call stage,$(UNSTAGED)/discard-readonly-fault.app,discard-readonly-fault.app)
	../userspace/libc/cubit-cc -O2 -g -DFAULT_DISCARD_RESERVED=1 -o $(UNSTAGED)/discard-reserved-fault.app ../userspace/libc/tests/protection-fault.c --manifest ../userspace/libc/build/protection-manifest.o
	$(call stage,$(UNSTAGED)/discard-reserved-fault.app,discard-reserved-fault.app)
	@mkdir -p $(@D) && touch $@

# Rust std on CuBit (docs/rust-std.md): patched std, built with -Zbuild-std.
.PHONY : rust-std-hello
rust-std-hello: ccl-manifest
	bash ../userspace/rust/std/build-hello.sh
	cp ../userspace/rust/build/rust-std-hello.app isodir/boot/rust-std-hello.app

# rtw89's WiFi_Crypto self-test without the device (headless --test
# wifi-crypto); SPARKNaCl and SPARKTLSCrypto are fetched, pinned trees.
.PHONY : wifi-crypto-check
wifi-crypto-check: isodir/boot/wifi-crypto-check.app
$(eval $(call ccl_program,../userspace/apps/wifi-crypto-check,wifi-crypto-check.app,))

.PHONY : avx-check
avx-check: isodir/boot/avx-check.app
$(eval $(call ccl_program,../userspace/apps/avx-check,avx-check.app,))

# CAP-001 diagnostics check: device memory without authority is refused
# and recorded (tests/headless: devmem-denials).
.PHONY : devmem-check
devmem-check: isodir/boot/devmem-check.app
$(eval $(call ccl_program,../userspace/apps/devmem-check,devmem-check.app,))

# Process-kill diagnostics check: faults on purpose (tests/headless:
# process-kill).
.PHONY : stall-check
stall-check: isodir/boot/stall-check.app
$(eval $(call ccl_program,../userspace/apps/stall-check,stall-check.app,))

.PHONY : fault-check
fault-check: isodir/boot/fault-check.app
$(eval $(call ccl_program,../userspace/apps/fault-check,fault-check.app,))

# Syscall-entry trap flag regression (SEC-023 #1): headless --test trap-flag.
.PHONY : trap-flag-check
trap-flag-check: isodir/boot/trap-flag-check.app
$(eval $(call ccl_program,../userspace/apps/trap-flag-check,trap-flag-check.app,))

# NMI survival regression (SEC-023): headless --test nmi.
.PHONY : nmi-check
nmi-check: isodir/boot/nmi-check.app
$(eval $(call ccl_program,../userspace/apps/nmi-check,nmi-check.app,))

# Interrupt-entry direction flag regression (WEB-001 guest resets): headless
# --test direction-flag.
.PHONY : direction-flag-check
direction-flag-check: isodir/boot/direction-flag-check.app
$(eval $(call ccl_program,../userspace/apps/direction-flag-check,direction-flag-check.app,))

# Interrupt-entry alignment-check flag regression (SEC-026): headless
# --test alignment-flag, with a kernel built SMAP_ENTRY_PROBE=1.
.PHONY : alignment-flag-check
alignment-flag-check: isodir/boot/alignment-flag-check.app
$(eval $(call ccl_program,../userspace/apps/alignment-flag-check,alignment-flag-check.app,))

# Kernel-map probe (KERN-014): headless --test kernel-map-probe, with a
# kernel built KERN014_PROBE=1.
.PHONY : kernel-map-probe
kernel-map-probe: isodir/boot/kernel-map-probe.app
$(eval $(call ccl_program,../userspace/apps/kernel-map-probe,kernel-map-probe.app,))

# MOV SS / POP SS debug-exception check (DBG-001, CVE-2018-8897 class):
# headless --test mov-ss-debug, with a kernel built DEBUG_REGISTER_PROBE=1.
.PHONY : mov-ss-check
mov-ss-check: isodir/boot/mov-ss-check.app
$(eval $(call ccl_program,../userspace/apps/mov-ss-check,mov-ss-check.app,))

# Timekeeping check (headless clock-keeper): deadlines and msTicks keep going
# while CPU 0 runs no timer work, with a kernel built CLOCK_STALL_PROBE=1.
.PHONY : clock-keeper-check
clock-keeper-check: isodir/boot/clock-keeper-check.app
$(eval $(call ccl_program,../userspace/apps/clock-keeper-check,clock-keeper-check.app,))

# Out-of-memory reclaim check (headless oom-reclaim): teardown allocates nothing.
.PHONY : oom-check
oom-check: isodir/boot/oom-check.app
$(eval $(call ccl_program,../userspace/apps/oom-check,oom-check.app,))

.PHONY : wait-check readiness-writer
wait-check: isodir/boot/wait-check.app
$(eval $(call ccl_program,../userspace/apps/wait-check,wait-check.app,))
# Shared futexes (IPC-010): headless --test futex-share.
.PHONY : futex-share
FUTEX_SHARE_APPS := futex-share-server futex-share-client futex-share-outsider
futex-share: $(patsubst %,isodir/boot/%.app,$(FUTEX_SHARE_APPS))
$(foreach app,$(FUTEX_SHARE_APPS),$(eval $(call ccl_program,../userspace/apps/$(app),$(app).app,)))
# The debugger preview (DBG-001): its demo target and the engine's native
# check, headless --test debug-engine.
.PHONY : debug-demo debug-engine-check
debug-demo: isodir/boot/debug-demo.app
$(eval $(call ccl_program,../userspace/apps/debug-demo,debug-demo.app,))
debug-engine-check: isodir/boot/debug-engine-check.app
$(eval $(call ccl_program,../userspace/apps/debug-engine-check,debug-engine-check.app,))
# The debugger engine through the kernel (DBG-001, CAP-012): a target with
# no stub and the engine's kernel-path check, headless --test debug-kernel.
DEBUG_KERNEL_APPS := debug-plain debug-kernel-check debug-gate-check
.PHONY : debug-kernel
debug-kernel: $(patsubst %,isodir/boot/%.app,$(DEBUG_KERNEL_APPS))
$(foreach app,$(DEBUG_KERNEL_APPS),$(eval $(call ccl_program,../userspace/apps/$(app),$(app).app,)))
# The debugger preview's Rust target and the engine's native Rust check,
# headless --test debug-engine-rs.
.PHONY : debug-demo-rs debug-engine-rs-check
debug-demo-rs: ccl-manifest
	bash ../userspace/apps/debug-demo-rs/build.sh
	cp ../userspace/rust/build/debug-demo-rs.app isodir/boot/debug-demo-rs.app
debug-engine-rs-check: isodir/boot/debug-engine-rs-check.app
$(eval $(call ccl_program,../userspace/apps/debug-engine-rs-check,debug-engine-rs-check.app,))
# The Debugger app (DBG-001 preview): headless --test debugger-gui.
.PHONY : debugger
debugger: isodir/boot/debugger.app
$(eval $(call ccl_program,../userspace/apps/debugger,debugger.app,ui-fonts-native))
# Address-space tags (PCIDs): headless --test tlb-tags.
.PHONY : tlb-tags
TLB_TAG_APPS := tlb-check tlb-probe
tlb-tags: $(patsubst %,isodir/boot/%.app,$(TLB_TAG_APPS))
$(foreach app,$(TLB_TAG_APPS),$(eval $(call ccl_program,../userspace/apps/$(app),$(app).app,)))
readiness-writer: isodir/boot/readiness-writer.app
$(eval $(call ccl_program,../userspace/apps/readiness-writer,readiness-writer.app,))

.PHONY : futex-check
futex-check: isodir/boot/futex-check.app
$(eval $(call ccl_program,../userspace/apps/futex-check,futex-check.app,))

# Per-thread growable secondary stacks (CuBit.Thread_Areas): headless
# --test secondary-stacks.
# User GNAT runtime language features (RT-008; headless --test ada-runtime).
.PHONY : ada-runtime-check
ada-runtime-check: isodir/boot/ada-runtime-check.app
$(eval $(call ccl_program,../userspace/apps/ada-runtime-check,ada-runtime-check.app,))

.PHONY : secondary-stack-check
secondary-stack-check: isodir/boot/secondary-stack-check.app
$(eval $(call ccl_program,../userspace/apps/secondary-stack-check,secondary-stack-check.app,))

# FIRMWARE_SNAPSHOT checks (shared/boot/firmware_snapshot_abi.ads):
# headless --test firmware-snapshot.
.PHONY : firmware-check firmware-undeclared
firmware-check: isodir/boot/firmware-check.app
$(eval $(call ccl_program,../userspace/apps/firmware-check,firmware-check.app,))

# The one acpi.svc (userspace/services/acpi/native; ACPI-ONE-001): AML plus
# the hardware it holds itself, and the observer metrics client, for
# headless --test acpi-one-service.
.PHONY : acpi-native acpi-metrics-check
acpi-native: isodir/boot/acpi.svc
$(eval $(call ccl_program,../userspace/services/acpi/native,acpi.svc,))

acpi-metrics-check: isodir/boot/acpi-metrics-check.app
$(eval $(call ccl_program,../userspace/apps/acpi-metrics-check,acpi-metrics-check.app,))

# Test-only stand-in for acpi.svc's "acpi" service (never in world/images).
.PHONY : acpi-standin
acpi-standin: ccl-build user_runtime ccl-manifest
	mkdir -p ../userspace/apps/acpi-standin/build
	$(CCL_BUILD) ../userspace/apps/acpi-standin/component.ccl

# ACPI hardware windows (coordination/acpi-region-boundary.md): headless
# --test acpi-hardware. Test-only (never in images).
.PHONY : acpi-hardware-check
acpi-hardware-check: isodir/boot/acpi-hardware-check.app
$(eval $(call ccl_program,../userspace/apps/acpi-hardware-check,acpi-hardware-check.app,))

# ACPI-PCI-001: the opt-in read-only PCI segment 0 bus 0 window of the ACPI
# hardware grant (headless --test acpi-pci-config). Test-only (never in
# world/images).
.PHONY : acpi-pci-check
acpi-pci-check: isodir/boot/acpi-pci-check.app
$(eval $(call ccl_program,../userspace/apps/acpi-pci-check,acpi-pci-check.app,))

# Table-named firmware data: devmgr's narrowed, uncached windows over the
# kernel's Firmware_Data_Tag roots (headless --test acpi-firmware-data).
# Test-only (never in world/images).
.PHONY : acpi-data-check
acpi-data-check: isodir/boot/acpi-data-check.app
$(eval $(call ccl_program,../userspace/apps/acpi-data-check,acpi-data-check.app,))

# The platform-register pool (ACPI-POOL-001): devmgr's pool, mapped once,
# refused over a bound driver's BAR, revoked when the driver binds after the
# mapping (headless --test platform-pool). Test-only (never in world/images).
.PHONY : platform-pool-check
platform-pool-check: isodir/boot/platform-pool-check.app
$(eval $(call ccl_program,../userspace/apps/platform-pool-check,platform-pool-check.app,))

# The ACPI launch (docs/device-matching.md "ACPI launch"): the stand-in for
# acpi.svc that asks devmgr to admit the test GPIO controller's spans
# (headless --test gpio-launch). Test-only (never in world/images).
.PHONY : gpio-bootstrap-check
gpio-bootstrap-check: isodir/boot/gpio-bootstrap-check.app
$(eval $(call ccl_program,../tests/gpio-launch/requester,gpio-bootstrap-check.app,))

firmware-undeclared: isodir/boot/firmware-undeclared.app
$(eval $(call ccl_program,../userspace/apps/firmware-undeclared,firmware-undeclared.app,))

# Clock publication checks (docs/fast-clock.md): headless --test clock.
.PHONY : clock-check
clock-check: isodir/boot/clock-check.app
$(eval $(call ccl_program,../userspace/apps/clock-check,clock-check.app,))

# The per-CPU times publication (KERN-002, run.sh --test cpu-times).
.PHONY : cpu-times-check
cpu-times-check: isodir/boot/cpu-times-check.app
$(eval $(call ccl_program,../userspace/apps/cpu-times-check,cpu-times-check.app,))

# Boot timeline reader (BOOT-001, docs/boot-timeline.md)
.PHONY : boot-timeline
boot-timeline: isodir/boot/boot-timeline.app
$(eval $(call ccl_program,../userspace/apps/boot-timeline,boot-timeline.app,))

# Boot timeline bookkeeping proof (kernel/src/boot_timeline_records.ad?) and
# the reader's analysis (userspace/apps/boot-timeline): tests/boot-timeline.
.PHONY : prove-boot-timeline
prove-boot-timeline:
	$(ALR) bash ../tests/boot-timeline/run.sh --prove

.PHONY : thread-check
thread-check: isodir/boot/thread-check.app
$(eval $(call ccl_program,../userspace/apps/thread-check,thread-check.app,))

.PHONY : tls-check
tls-check: isodir/boot/tls-check.app
$(eval $(call ccl_program,../userspace/apps/tls-check,tls-check.app,))

# SPARKTLS's FIPS 140-3 post-link step (the pinned crate's tools/fips_inject,
# built for the host): writes the software integrity test's HMACs into a
# program that links SPARKTLS (userspace/lib/tls/sparktls_fips_cubit.ld).
# Without it SPARKTLS.RBG.Init fails its FIPS self-test and no session starts.
FIPS_INJECT := ../tools/fips-inject/build/fips_inject
.PHONY : fips-inject
fips-inject: build/gate/fips-inject.stamp
build/gate/fips-inject.stamp: $(HOST_TOOL_INPUTS) | tls-crates
	$(ALR) gprbuild -p -j8 -P ../tools/fips-inject/fips_inject.gpr \
		-XCUBIT_SPARK_CRATES=$(CUBIT_SPARK_CRATES)
	@python3 ../tools/gpr_inputs.py build/gate/fips-inject.d $@ -- $(HOST_GPRLS) -P ../tools/fips-inject/fips_inject.gpr \
		-XCUBIT_SPARK_CRATES=$(CUBIT_SPARK_CRATES)
	@touch $@
-include build/gate/fips-inject.d
$(FIPS_INJECT): fips-inject ;

# TLS client service.
.PHONY : tls
tls: build/gpr/tls.stamp
build/gpr/tls.stamp: $(FIPS_INJECT) $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/tls/manifest.ccl) | user_runtime ccl-manifest tls-crates fips-inject
	mkdir -p ../userspace/services/tls/build
	$(call compile_ccl_manifest,../userspace/services/tls)
	$(NATIVE_GPRBUILD) -j8 -P ../userspace/services/tls/tls.gpr \
		-XCUBIT_SPARK_CRATES=$(CUBIT_SPARK_CRATES)
	$(FIPS_INJECT) ../userspace/services/tls/build/tls.svc
	$(call stage,../userspace/services/tls/build/tls.svc,tls.svc)
	@python3 ../tools/gpr_inputs.py build/gpr/tls.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/tls/tls.gpr -XCUBIT_SPARK_CRATES=$(CUBIT_SPARK_CRATES)
	@touch $@
-include build/gpr/tls.d

# Native SPARKTLS probe for the headless tls-probe regression; embeds the
# generated test root.
tls-probe: build/gpr/tls-probe.stamp
build/gpr/tls-probe.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/apps/tls-probe/manifest.ccl) | user_runtime ccl-manifest tls-crates tls-test-pki fips-inject
	mkdir -p ../userspace/apps/tls-probe/build/generated
	$(call compile_ccl_manifest,../userspace/apps/tls-probe)
	python3 ../tools/der_to_ada.py TLS_Probe_Roots DER ../tests/tls/build/pki/ca.der \
		../userspace/apps/tls-probe/build/generated/tls_probe_roots.ads
	$(NATIVE_GPRBUILD) -j8 -P ../userspace/apps/tls-probe/tls_probe.gpr \
		-XCUBIT_SPARK_CRATES=$(CUBIT_SPARK_CRATES)
	$(FIPS_INJECT) ../userspace/apps/tls-probe/build/tls-probe.app
	$(call stage,../userspace/apps/tls-probe/build/tls-probe.app,tls-probe.app)
	@python3 ../tools/gpr_inputs.py build/gpr/tls-probe.d $@ -- $(NATIVE_GPRLS) -P ../userspace/apps/tls-probe/tls_probe.gpr -XCUBIT_SPARK_CRATES=$(CUBIT_SPARK_CRATES)
	@touch $@
-include build/gpr/tls-probe.d

# Capability-addressed monotonic clock service.
.PHONY : clock
clock: build/gpr/clock.stamp
build/gpr/clock.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/clock/manifest.ccl) | user_runtime ../userspace/ccl/interfaces/clock.ccl-interface ccl-manifest time-zones
	mkdir -p ../userspace/services/clock/build $(UNSTAGED)
	$(call compile_ccl_manifest,../userspace/services/clock)
	$(call generate_clock_floor,../userspace/services/clock/build/generated/clock_floor.ads)
	$(NATIVE_GPRBUILD) -P ../userspace/services/clock/clock.gpr
	$(ALR) objcopy \
		--add-section .cubit.interfaces=../userspace/ccl/interfaces/clock.ccl-interface \
		--set-section-flags .cubit.interfaces=readonly,data \
		../userspace/services/clock/build/clock.svc $(UNSTAGED)/clock.svc
	$(call stage,$(UNSTAGED)/clock.svc,clock.svc)
	@python3 ../tools/gpr_inputs.py build/gpr/clock.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/clock/clock.gpr
	@touch $@
-include build/gpr/clock.d

# The bundled IANA zones: names (Time_Zones: clock, Settings) and offset
# rules (Time_Zone_Rules: clock only).
TIME_ZONE_SOURCES := $(addprefix ../userspace/lib/time/build/generated/,time_zones.ads time_zones.adb time_zone_rules.ads time_zone_rules.adb)
.PHONY : time-zones
time-zones: $(TIME_ZONE_SOURCES)
$(TIME_ZONE_SOURCES) &: ../tools/generate_time_zones.py ../flake.lock ../flake.nix
	$(ALR) python3 ../tools/generate_time_zones.py ../userspace/lib/time/build/generated

# libgcc_eh.a's unwinder: the DWARF frame walker and the .eh_frame_hdr
# lookup through dl_iterate_phdr (not emutls, SJLJ or the C personality).
UNWINDER_OBJECTS := unwind-dw2.o unwind-dw2-fde-dip.o

# Build userspace Ada runtime (needed by filesystem server)
# BLD-001 interim: rebuilt only when one of its sources (the runtime, the
# allocator, the shared units, the C start code) or the flake changed.
.PHONY : user_runtime
user_runtime: build/gate/user_runtime.stamp
build/gate/user_runtime.stamp: $(shell find ../userspace/runtime ../userspace/allocator ../shared ../userspace/c -type d \
		\( -name 'build*' -o -name adalib \) -prune -o -type f -print) \
		../userspace/toolchain/gnat-15.cgpr $(HOST_TOOL_INPUTS)
	$(ALR) $(MAKE) -C ../userspace/c build/crt0.o
	mkdir -p ../userspace/runtime/build
	mkdir -p ../userspace/runtime/adalib
	$(NATIVE_GPRBUILD) -P ../userspace/runtime/user_runtime.gpr
	# CuAlloc, the process heap System.Memory allocates from: the same
	# objects as in libc.a and libcubit_allocator.a.
	$(NATIVE_GPRBUILD) -p -P ../userspace/allocator/allocator_native.gpr
	# Assembled as a copy, which replaces the archive only when its
	# contents changed (BLD-001): nothing that links it looks newer than it is.
	cp ../userspace/runtime/adalib/libgnat-user.a ../userspace/runtime/adalib/libgnat-user.a.new
	ar rcs ../userspace/runtime/adalib/libgnat-user.a.new ../userspace/allocator/build/native-objects/*.o
	# Zero-cost exceptions (EXC-001). GNAT's personality routine, upstream's
	# raise-gcc.c unchanged, compiled as libgnat does (IN_RTS), with no
	# libgcc configuration headers (STANDALONE: runtime.h), no C++
	# exception interoperation, and the unwinder's abort (no libc).
	mkdir -p ../userspace/runtime/build/unwinder
	$(KERNEL_GCC) -B$(CUBIT_BINUTILS)/bin/ -c -O2 -g -mno-red-zone -fno-pic -fexceptions \
	  -ffile-prefix-map=$(abspath ..)=. -DIN_RTS -DSTANDALONE -DGXX_EH_INTEROP=0 \
	  -Dabort=__cubit_unwind_abort -I../userspace/runtime/gnat \
	  ../userspace/runtime/gnat/raise-gcc.c -o ../userspace/runtime/build/unwinder/raise-gcc.o
	ar rcs ../userspace/runtime/adalib/libgnat-user.a.new ../userspace/runtime/build/unwinder/raise-gcc.o
	# libgcc's DWARF unwinder, for zero-cost exceptions and tracebacks: the
	# pinned compiler's own objects, unchanged except that their libc calls
	# are renamed to CuBit.Unwinder_Support's
	# (userspace/runtime/unwinder-symbols.txt).
	cd ../userspace/runtime/build/unwinder && ar x $$($(KERNEL_GCC) -print-file-name=libgcc_eh.a) $(UNWINDER_OBJECTS)
	for object in $(UNWINDER_OBJECTS); do \
	  $(CUBIT_BINUTILS)/bin/objcopy --redefine-syms=../userspace/runtime/unwinder-symbols.txt \
	    ../userspace/runtime/build/unwinder/$$object ../userspace/runtime/build/unwinder/cubit-$$object || exit 1; \
	done
	ar rcs ../userspace/runtime/adalib/libgnat-user.a.new $(addprefix ../userspace/runtime/build/unwinder/cubit-,$(UNWINDER_OBJECTS))
	if cmp -s ../userspace/runtime/adalib/libgnat-user.a.new ../userspace/runtime/adalib/libgnat-user.a; then \
	  rm ../userspace/runtime/adalib/libgnat-user.a.new; else \
	  mv ../userspace/runtime/adalib/libgnat-user.a.new ../userspace/runtime/adalib/libgnat-user.a; fi
	@mkdir -p $(@D) && touch $@

# RAM storage uses the same block-device protocol as hardware drivers.
.PHONY : ramdisk
ramdisk: build/gpr/ramdisk.stamp
build/gpr/ramdisk.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/ramdisk/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/services/ramdisk/build
	$(call compile_driver_manifest,../userspace/services/ramdisk)
	$(NATIVE_GPRBUILD) -P ../userspace/services/ramdisk/ramdisk.gpr
	$(call stage,../userspace/services/ramdisk/build/ramdisk.drv,ramdisk.drv)
	@python3 ../tools/gpr_inputs.py build/gpr/ramdisk.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/ramdisk/ramdisk.gpr
	@touch $@
-include build/gpr/ramdisk.d

# Build the userspace filesystem server
.PHONY : filesystem
filesystem: isodir/boot/filesystem.svc
$(eval $(call ccl_program,../userspace/services/filesystem,filesystem.svc,))

# Build the userspace ATA driver
.PHONY : ata
ata: isodir/boot/ata.drv
$(eval $(call ccl_program,../userspace/services/ata,ata.drv,))

# Build the userspace UFS host controller driver (HW-001)
.PHONY : ufs
ufs: isodir/boot/ufs.drv
$(eval $(call ccl_program,../userspace/services/ufs,ufs.drv,))

# Build the userspace NVMe driver
.PHONY : nvme
nvme: build/gpr/nvme.stamp
build/gpr/nvme.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/nvme/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/services/nvme/build
	$(call compile_driver_manifest,../userspace/services/nvme)
	$(NATIVE_GPRBUILD) -P ../userspace/services/nvme/nvme_drv.gpr
	$(call stage,../userspace/services/nvme/build/nvme.drv,nvme.drv)
	@python3 ../tools/gpr_inputs.py build/gpr/nvme.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/nvme/nvme_drv.gpr
	@touch $@
-include build/gpr/nvme.d

# Build the userspace virtio-net driver
.PHONY : virtio-net
virtio-net: isodir/boot/virtio-net.drv
$(eval $(call ccl_program,../userspace/services/virtio-net,virtio-net.drv,))

# Build the userspace virtio-gpu driver
CUBIT_GPU_TEST_MODE ?= production
GPU_BUILD_DIR = $(if $(filter production,$(CUBIT_GPU_TEST_MODE)),build,build-$(CUBIT_GPU_TEST_MODE))
.PHONY : virtio-gpu
virtio-gpu: build/gpr/virtio-gpu.stamp
build/gpr/virtio-gpu.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/virtio-gpu/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/services/virtio-gpu/build
	$(call compile_driver_manifest,../userspace/services/virtio-gpu)
	$(NATIVE_GPRBUILD) -p -P ../userspace/services/virtio-gpu/virtio_gpu.gpr -XCUBIT_GPU_TEST_MODE=$(CUBIT_GPU_TEST_MODE)
	$(call stage,../userspace/services/virtio-gpu/$(GPU_BUILD_DIR)/virtio-gpu.drv,virtio-gpu.drv)
	@python3 ../tools/gpr_inputs.py build/gpr/virtio-gpu.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/virtio-gpu/virtio_gpu.gpr -XCUBIT_GPU_TEST_MODE=$(CUBIT_GPU_TEST_MODE)
	@touch $@
-include build/gpr/virtio-gpu.d

# Build the userspace network stack service
.PHONY : netstack
netstack: build/gpr/netstack.stamp
build/gpr/netstack.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/netstack/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/services/netstack/build
	$(call compile_driver_manifest,../userspace/services/netstack)
	$(NATIVE_GPRBUILD) -P ../userspace/services/netstack/netstack.gpr
	$(call stage,../userspace/services/netstack/build/netstack.svc,netstack.svc)
	@python3 ../tools/gpr_inputs.py build/gpr/netstack.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/netstack/netstack.gpr
	@touch $@
-include build/gpr/netstack.d

# Build the userspace wget app
.PHONY : wget
wget: isodir/boot/wget.app
$(eval $(call ccl_program,../userspace/apps/wget,wget.app,))

# Build the unified hardware inventory and diagnostics app
.PHONY : devices
.PHONY : config-inspector
.PHONY: intel-gpu
intel-gpu: isodir/boot/intel-gpu.drv
$(eval $(call ccl_program,../userspace/services/intel-gpu,intel-gpu.drv,intel-buffer-layout-check))

config-inspector: isodir/boot/config-inspector.app
$(eval $(call ccl_program,../userspace/apps/config-inspector,config-inspector.app,ui-fonts-native))

devices: isodir/boot/devices.app
$(eval $(call ccl_program,../userspace/apps/devices,devices.app,ui-fonts-native))

.PHONY: render-launch-policy
render-launch-policy: user_runtime ccl-manifest
	mkdir -p ../tests/mesa-anv/native-launch-policy/build
	$(call compile_ccl_manifest,../tests/mesa-anv/native-launch-policy)
	$(NATIVE_GPRBUILD) -p -P ../tests/mesa-anv/native-launch-policy/fixture.gpr
	cp ../tests/mesa-anv/native-launch-policy/build/render-denied.app isodir/boot/render-denied.app
	cp ../tests/mesa-anv/native-launch-policy/build/render-denied.app isodir/boot/render-unavailable.app

.PHONY: mesa-log-test
mesa-log-test: user_runtime ccl-manifest
	$(call compile_ccl_manifest,../tests/mesa-anv/native-log)
	$(NATIVE_GPRBUILD) -p -P ../tests/mesa-anv/native-log/fixture.gpr
	cp ../tests/mesa-anv/native-log/build/mesa-log-check.app isodir/boot/mesa-log-check.app

.PHONY: render-session-test
render-session-test: user_runtime ccl-manifest
	mkdir -p ../tests/mesa-anv/native-session/build
	$(call compile_ccl_manifest,../tests/mesa-anv/native-session)
	$(NATIVE_GPRBUILD) -p -P ../tests/mesa-anv/native-session/fixture.gpr
	cp ../tests/mesa-anv/native-session/build/render-session.app isodir/boot/render-session.app

# Build the read-only, authority-scoped filesystem browser.
.PHONY : files
.PHONY: gpu-viewer-test
gpu-viewer-test: user_runtime ccl-manifest
	@set -e; for role in owner viewer; do \
	  dir=../tests/gpu-viewer/native; \
	  mkdir -p $$dir/build/$$role; \
	  ../userspace/ccl/build/manifest/ccl-manifest $$dir/catalog.ccl \
	    $$dir/$$role-manifest.ccl > $$dir/build/$$role/manifest.S; \
	  $(NATIVE_CC) -c $$dir/build/$$role/manifest.S -o $$dir/build/$$role/manifest.o; \
	  $(NATIVE_GPRBUILD) -p -P $$dir/fixture.gpr -XROLE=$$role; \
	  case $$role in owner) app=gpu-viewer-owner;; viewer) app=gpu-viewer-test;; esac; \
	  cp $$dir/build/$$role/$$app.app isodir/boot/; \
	done

.PHONY: gpu-viewer
gpu-viewer: isodir/boot/gpu-viewer.app
$(eval $(call ccl_program,../userspace/apps/gpu-viewer,gpu-viewer.app,))

.PHONY : logs
# The Logs app (docs/logs-app.md): every service's records, live.
logs: isodir/boot/logs.app
$(eval $(call ccl_program,../userspace/apps/logs,logs.app,ui-fonts-native))

.PHONY : boot-logs
boot-logs: isodir/boot/boot-logs.app
$(eval $(call ccl_program,../userspace/apps/boot-logs,boot-logs.app,ui-fonts-native))

files: isodir/boot/files.app
$(eval $(call ccl_program,../userspace/apps/files,files.app,ui-fonts-native))

# The text editor (docs/text-editor.md): the editor core in userspace/lib/text.
.PHONY : text
text: isodir/boot/text.app
$(eval $(call ccl_program,../userspace/apps/text,text.app,ui-fonts-native))

# A test window animating every refresh (LAT-002; display-latency with
# CUBIT_DISPLAY_LATENCY_ANIMATION=1 runs it beside the Calculator).
.PHONY : frame-ticker
frame-ticker: isodir/boot/frame-ticker.app
$(eval $(call ccl_program,../userspace/apps/frame-ticker,frame-ticker.app,ui-fonts-native))

# UI-061: two windows titled as other apps (tests/headless --test task-cards).
.PHONY : task-card-check
task-card-check: isodir/boot/task-card-check.app
$(eval $(call ccl_program,../userspace/apps/task-card-check,task-card-check.app,ui-fonts-native))

# The calculator (docs/calculator.md): the calculator core in userspace/lib/calc.
# The UI latency probe (tests/headless --test ui-latency).
.PHONY : ui-latency-probe
ui-latency-probe: isodir/boot/ui-latency-probe.app
$(eval $(call ccl_program,../userspace/apps/ui-latency-probe,ui-latency-probe.app,ui-fonts-native))

.PHONY : calculator
# On gprbuild until CCL capacity M1: its declaration exceeds the program
# size cap (docs/ccl-build.md), like text's.
calculator: isodir/boot/calculator.app
$(eval $(call ccl_program,../userspace/apps/calculator,calculator.app,ui-fonts-native))

.PHONY : vmm
# vmm (VMM-001): the per-VM monitor as a desktop app, with the pinned guest.
vmm: ccl-build user_runtime ccl-manifest ui-fonts-native
	bash ../tests/vm-linux/guest-image.sh >/dev/null || { \
	  echo 'vmm: tests/vm-linux/guest-image.sh could not build the pinned guest (firmware/vm-guest/bzImage, initramfs.cpio); vmm.app embeds both' >&2; \
	  exit 1; }
	mkdir -p ../userspace/apps/vmm/build
	$(CCL_BUILD) ../userspace/apps/vmm/component.ccl
	cp ../userspace/apps/vmm/build/vmm.app isodir/boot/vmm.app

.PHONY : metricsview
# Metrics (MET-002's app): dashboards over metrics.svc's series; the charts
# core in userspace/lib/charts.
metricsview: isodir/boot/metricsview.app
$(eval $(call ccl_program,../userspace/apps/metricsview,metricsview.app,ui-fonts-native))

# The dictionary, thesaurus and spelling service (docs/lexicon.md): it reads
# the lexicon asset package and answers Lexicon_Channel channels.
.PHONY : lexicon
lexicon: isodir/boot/lexicon.svc
$(eval $(call ccl_program,../userspace/services/lexicon,lexicon.svc,))

# Dictionary (docs/lexicon.md), with Dictionary and Thesaurus modes, asking
# lexicon.svc.
.PHONY : dictionary
dictionary: isodir/boot/dictionary.app
$(eval $(call ccl_program,../userspace/apps/dictionary,dictionary.app,ui-fonts-native))

# The Bible (APP-005): its proved core in userspace/lib/bible, the reading
# face (Literata) linked in from its pinned download, the texts read from
# the cubit-bible asset package (bible-assets).
.PHONY : bible
bible: isodir/boot/bible.app
$(eval $(call ccl_program,../userspace/apps/bible,bible.app,ui-fonts-native))

# Build the live writable-storage diagnostic app.
# HW-001 step 4: whole-disk writes behind the block write grant (headless
# ufs-write); the installer's stand-in, holding disk-install.
.PHONY : disk-write-check
disk-write-check: isodir/boot/disk-write-check.app
$(eval $(call ccl_program,../userspace/apps/disk-write-check,disk-write-check.app,))

# SYS-001: the installed system's boot check (headless install-reboot).
.PHONY : install-boot-check
install-boot-check: isodir/boot/install-boot-check.app
$(eval $(call ccl_program,../userspace/apps/install-boot-check,install-boot-check.app,))

# SYS-001: the install payload's pieces. The UEFI fallback loader is GRUB,
# the same loader the live medium uses, standalone with its configuration
# (tests/hardware/grub-installed.cfg) inside. The installed system's
# bootstrap archive (its startup plan, init.ccl, is on the installed root).
INSTALL_EFI_MODULES := part_gpt fat search search_fs_file multiboot2 all_video serial terminal echo normal
.PHONY : install-efi installed-initrd
install-efi: ../tests/hardware/grub-installed.cfg
	grub-mkstandalone -O x86_64-efi --directory="$(CUBIT_GRUB_EFI_DIR)" \
		--modules="$(INSTALL_EFI_MODULES)" -o isodir/boot/install-bootx64.efi \
		"boot/grub/grub.cfg=../tests/hardware/grub-installed.cfg"
installed-initrd: $(INITRD_SERVICES) ccl-image ccl-sign ../images/installed-initrd.ccl $(CCL_IMAGE_CATALOG)
	$(SIGN_PROGRAMS)
	$(ALR) python3 $(CCL_IMAGE_REALIZER) ../images/installed-initrd.ccl $(IMAGE_SIGNING) \
		--debug-dir debug --output isodir/boot/installed-initrd.img

# SYS-001: installer.svc, system.disks and system.install (holds disk-install).
.PHONY : installer
installer: isodir/boot/installer.svc
$(eval $(call ccl_program,../userspace/services/installer,installer.svc,))

.PHONY : storage-check
storage-check: isodir/boot/storage-check.app
$(eval $(call ccl_program,../userspace/apps/storage-check,storage-check.app,filesystem-scope-check))

.PHONY : filesystem-scope-check
filesystem-scope-check: isodir/boot/filesystem-scope-check.app
$(eval $(call ccl_program,../userspace/apps/filesystem-scope-check,filesystem-scope-check.app,))

# Build the first desktop shell client
.PHONY : desktop-shell
desktop-shell: isodir/boot/desktop-shell.app
$(eval $(call ccl_program,../userspace/apps/desktop-shell,desktop-shell.app,ui-fonts-native))

# Build the userspace process manager service
.PHONY : procmgr
procmgr: isodir/boot/procmgr.svc
$(eval $(call ccl_program,../userspace/services/procmgr,procmgr.svc,))

# Typed program parameters (docs/ccl-launch-parameters.md): the headless
# program-parameters test's programs, one that declares parameters and one
# that declares none.
.PHONY : program-parameters-test
program-parameters-test: isodir/boot/parameters-check.app isodir/boot/parameters-plain.app
$(eval $(call ccl_program,../tests/program-parameters/check,parameters-check.app,))
$(eval $(call ccl_program,../tests/program-parameters/plain,parameters-plain.app,))

# LIVE-002: the headless exit-status test's program, which ends as its typed
# parameter says (tests/exit-status).
.PHONY : exit-status-test
exit-status-test: isodir/boot/exit-status-check.app
$(eval $(call ccl_program,../tests/exit-status/check,exit-status-check.app,))

# PWR-003 slice 1: the headless shutdown tests' fixtures: a TEST STAND-IN
# for acpi.svc's side of CuBit.Power_Event_Exchange (button watch, S5 on
# q35), a service that obeys Stop and one that does not.
.PHONY : shutdown-test
shutdown-test: isodir/boot/shutdown-standin.app isodir/boot/shutdown-cooperative.app isodir/boot/shutdown-stubborn.app
$(eval $(call ccl_program,../tests/shutdown/standin,shutdown-standin.app,))
$(eval $(call ccl_program,../tests/shutdown/cooperative,shutdown-cooperative.app,))
$(eval $(call ccl_program,../tests/shutdown/stubborn,shutdown-stubborn.app,))

# PWR-004 slice 1 (headless power-sources): the TEST STAND-IN for acpi.svc's
# AC adapter and battery observer pages (tests/power-sources/standin). Test-only.
.PHONY : power-sources-test
power-sources-test: isodir/boot/power-standin.app
$(eval $(call ccl_program,../tests/power-sources/standin,power-standin.app,))

# Start_Child (SUP-001): the headless supervision test's supervisor and
# the child it starts with typed parameters. Test-only.
.PHONY : supervision-test
supervision-test: isodir/boot/supervision-check.app isodir/boot/supervision-child.app
$(eval $(call ccl_program,../tests/supervision/supervisor,supervision-check.app,))
$(eval $(call ccl_program,../tests/supervision/child,supervision-child.app,))

# HID launch fixture (SUP-001): a test driver declaring ACPI_ID PNP0C50 and
# hid-i2c's HID_Parameters, which devmgr starts through Start_Child in the
# headless gpio-launch test. Test-only (never in world/images).
.PHONY : hid-launch-check
hid-launch-check: isodir/boot/hid-launch-check.drv
$(eval $(call ccl_program,../tests/gpio-launch/hid-driver,hid-launch-check.drv,))

# The laptop touchpad (SUP-001): the INTC1057 GPIO driver (ACPI launch),
# the DesignWare I2C controller driver (generic PCI launch, 8086:54E8/54E9)
# and the HID-over-I2C driver (PNP0C50, started by devmgr through
# procmgr's Start_Child). Hardware images only (LAPTOP_LIVE_STAGE1); the
# development image keeps hid-launch-check as its PNP0C50 driver.
.PHONY : gpio i2c-dw hid-i2c
gpio: isodir/boot/gpio.drv
$(eval $(call ccl_program,../userspace/services/gpio,gpio.drv,))
i2c-dw: isodir/boot/i2c-dw.drv
$(eval $(call ccl_program,../userspace/services/i2c-dw,i2c-dw.drv,))
hid-i2c: isodir/boot/hid-i2c.drv
$(eval $(call ccl_program,../userspace/services/hid-i2c,hid-i2c.drv,))

# The headless large-manifest test's program: a typed manifest well past the
# CCL limits the manifest compiler had (tests/large-manifest-probe).
.PHONY : large-manifest-probe
large-manifest-probe: isodir/boot/large-manifest-probe.app
$(eval $(call ccl_program,../tests/large-manifest-probe,large-manifest-probe.app,))

# procmgr's startup waits (CAP-003): the headless startup-wait test's late
# provider of the test-only startup-wait-test service and its client.
.PHONY : startup-wait-test
startup-wait-test: isodir/boot/startup-wait-provider.app isodir/boot/startup-wait-client.app
$(eval $(call ccl_program,../tests/startup-wait/provider,startup-wait-provider.app,))
$(eval $(call ccl_program,../tests/startup-wait/client,startup-wait-client.app,))

# Build the userspace HDA audio driver
.PHONY : hda
hda: isodir/boot/hda.drv
$(eval $(call ccl_program,../userspace/services/hda,hda.drv,))

# Build the Realtek RTL8852BE Wi-Fi bring-up driver (docs/wifi.md). Its
# firmware is embedded at build time from firmware/rtw89 (D6).
.PHONY : rtw89
rtw89: isodir/boot/rtw89.drv
$(eval $(call ccl_program,../userspace/services/rtw89,rtw89.drv,))

# Build the userspace audio mixer service
.PHONY : mixer
mixer: isodir/boot/mixer.svc
$(eval $(call ccl_program,../userspace/services/mixer,mixer.svc,))

# Build the userspace PS/2 keyboard + mouse driver
.PHONY : ps2
ps2: isodir/boot/ps2.drv
$(eval $(call ccl_program,../userspace/services/ps2,ps2.drv,))

# Build the userspace virtio-input driver (QEMU virtio-tablet absolute pointer)
.PHONY : virtio-input
# The generic PCI launch's fixture driver (HW-002): bound only when a test
# adds QEMU's edu device (tests/headless/run.sh --test generic-launch).
.PHONY: generic-check
generic-check: isodir/boot/generic-check.drv
$(eval $(call ccl_program,../tests/generic-launch/driver,generic-check.drv,))

# The ACPI launch's fixture driver (docs/device-matching.md "ACPI launch"):
# launched only when devmgr admits an ACPI device with _HID CBIT1057 (the
# headless gpio-launch profile's test controller).
.PHONY: gpio-launch-check
gpio-launch-check: isodir/boot/gpio-launch-check.drv
$(eval $(call ccl_program,../tests/gpio-launch/driver,gpio-launch-check.drv,))

virtio-input: isodir/boot/virtio-input.drv
$(eval $(call ccl_program,../userspace/services/virtio-input,virtio-input.drv,))

# Build the userspace xHCI controller driver
.PHONY : xhci
xhci: isodir/boot/xhci.drv
$(eval $(call ccl_program,../userspace/services/xhci,xhci.drv,))

# Build the userspace config store service
.PHONY : config
.PHONY: config-check
config-check: isodir/boot/config-check.app build/gpr/config-check.stamp
$(eval $(call ccl_program,../userspace/apps/config-check,config-check.app,))
build/gpr/config-check.stamp: $(GPR_STEP_INPUTS) $(filter-out %/build,$(wildcard ../userspace/apps/config-denied/*)) | user_runtime ccl-manifest
	mkdir -p ../userspace/apps/config-denied/build
	$(call compile_ccl_manifest,../userspace/apps/config-denied)
	$(NATIVE_GPRBUILD) -P ../userspace/apps/config-denied/config_denied.gpr
	$(call stage,../userspace/apps/config-denied/build/config-denied.app,config-denied.app)
	@python3 ../tools/gpr_inputs.py build/gpr/config-check.d $@ -- $(NATIVE_GPRLS) -P ../userspace/apps/config-denied/config_denied.gpr
	@touch $@
-include build/gpr/config-check.d

config: build/gpr/config.stamp
build/gpr/config.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/config/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/services/config/build $(UNSTAGED)
	$(call compile_driver_manifest,../userspace/services/config)
	$(NATIVE_GPRBUILD) -P ../userspace/services/config/config.gpr
	$(ALR) objcopy \
		--add-section .cubit.interfaces=../userspace/ccl/interfaces/config.ccl-interface \
		--set-section-flags .cubit.interfaces=readonly,data \
		../userspace/services/config/build/config.svc $(UNSTAGED)/config.svc
	$(call stage,$(UNSTAGED)/config.svc,config.svc)
	@python3 ../tools/gpr_inputs.py build/gpr/config.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/config/config.gpr
	@touch $@
-include build/gpr/config.d

# Build the userspace log store service
.PHONY : log-check
.PHONY : log-retire
log-retire: isodir/boot/log-retire.app
$(eval $(call ccl_program,../userspace/apps/log-retire,log-retire.app,))

# ABI-002: a program whose debug text reaches the serial console only
# through test builds' logstore mirror (tests/headless console-route).
.PHONY : console-route-check
console-route-check: isodir/boot/console-route-check.app
$(eval $(call ccl_program,../userspace/apps/console-route-check,console-route-check.app,))

.PHONY : late-intro-check
late-intro-check: isodir/boot/late-intro-check.app
$(eval $(call ccl_program,../userspace/apps/late-intro-check,late-intro-check.app,))

log-check: isodir/boot/log-check.app
$(eval $(call ccl_program,../userspace/apps/log-check,log-check.app,log-retire))

.PHONY : logstore
logstore: isodir/boot/logstore.svc
$(eval $(call ccl_program,../userspace/services/logstore,logstore.svc,))

# Observability (docs/metrics-service.md). metrics.svc is not yet in world or
# generic world images; desktop sessions explicitly include it below.
.PHONY : metricsvc
metricsvc: isodir/boot/metrics.svc
$(eval $(call ccl_program,../userspace/services/metricsvc,metrics.svc,))

.PHONY : metrics-check
metrics-check: isodir/boot/metrics-check.app
$(eval $(call ccl_program,../userspace/apps/metrics-check,metrics-check.app,))

.PHONY : log-fields-check
log-fields-check: isodir/boot/log-fields-check.app
$(eval $(call ccl_program,../userspace/apps/log-fields-check,log-fields-check.app,))

# LOG-002: per-source log retention (headless --test log-retention).
.PHONY : log-flood
log-flood: isodir/boot/log-flood.app
$(eval $(call ccl_program,../userspace/apps/log-flood,log-flood.app,))

.PHONY : log-retention-check
log-retention-check: isodir/boot/log-retention-check.app
$(eval $(call ccl_program,../userspace/apps/log-retention-check,log-retention-check.app,log-flood))

# LOG-003: an exited service's records keep its name (headless
# CCL_CONSOLE_DEMO=log-exited --test ccl-console).
.PHONY : log-exit-probe
log-exit-probe: isodir/boot/log-exit-probe.app
$(eval $(call ccl_program,../userspace/apps/log-exit-probe,log-exit-probe.app,))

# LOG-005: a bring-up's worth of records, then more than its retention
# ceiling (headless CCL_CONSOLE_DEMO=log-burst --test ccl-console).
.PHONY : log-burst-probe
log-burst-probe: isodir/boot/log-burst-probe.app
$(eval $(call ccl_program,../userspace/apps/log-burst-probe,log-burst-probe.app,))

# LOG-004: dies of an unhandled exception; its last words reach logstore
# through the kernel log ring (run.sh --test ccl-console, CCL_CONSOLE_DEMO=kernel-log).
.PHONY : crash-probe
crash-probe: isodir/boot/crash-probe.app
$(eval $(call ccl_program,../userspace/apps/crash-probe,crash-probe.app,))

# LOG-004: overflows its stack; the kernel's typed fault report must reach
# logstore under its name (CCL_CONSOLE_DEMO=kernel-log).
.PHONY : stack-probe
stack-probe: isodir/boot/stack-probe.app
$(eval $(call ccl_program,../userspace/apps/stack-probe,stack-probe.app,))

# Build the userspace desktop compositor/session service prototype
.PHONY : desktop
desktop: build/gpr/desktop.stamp
build/gpr/desktop.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/desktop/manifest.ccl) $(DESKTOP_STEP_INPUTS) | user_runtime ccl-manifest ui-fonts-native time-zones
	mkdir -p ../userspace/services/desktop/build
	$(call compile_ccl_manifest,../userspace/services/desktop)
	@python3 ../tools/desktop_build_variant.py >/dev/null
	@if [ "$(CUBIT_COMPOSITOR)" = mesa ]; then \
	  test -n "$(CUBIT_MESA_BUILD)" || { echo 'Set CUBIT_MESA_BUILD to the existing native Mesa build' >&2; exit 2; }; \
	  python3 ../tools/build_mesa_desktop.py "$(abspath $(CUBIT_MESA_BUILD))" --scenario-output; \
	elif [ "$(CUBIT_COMPOSITOR)" = vulkan ]; then \
	  python3 ../tools/build_vulkan_desktop.py; \
	else \
	  $(NATIVE_GPRBUILD) -p -P ../userspace/services/desktop/desktop.gpr -XCUBIT_DISPLAY_TEST_MODE=$(CUBIT_DISPLAY_TEST_MODE); \
	fi
	$(call stage,../userspace/services/desktop/$$(python3 ../tools/desktop_build_variant.py)/desktop.svc,desktop.svc)
	@touch $@

# Read-only asset packages (docs/assets.md): Assets/<name>/<version>/ on each
# image's system volume. ASSET_ROOT is the one place the root is spelled for
# the disk builders; Desktop reads it from the system.assets.root setting.
ASSET_ROOT := Assets
WALLPAPER_PACKAGE := $(ASSET_ROOT)/cubit-wallpapers/2
# The package is whatever its source list declares (assets/wallpapers/
# wallpapers.ccl): its catalog.ccl and one <name>.qoi per wallpaper. Make
# names no picture: the stamp stands for the whole directory, and the disk
# placements list the files the build wrote (expanded when a recipe runs).
WALLPAPER_STAMP := build/$(ASSET_ROOT)/.cubit-wallpapers-2.stamp
WALLPAPER_ASSETS := $(WALLPAPER_STAMP)
WALLPAPER_SOURCES := ../assets/wallpapers/wallpapers.ccl $(wildcard ../assets/wallpapers/*.png)
WALLPAPER_DISK_FILES = $(foreach f,$(sort $(wildcard build/$(WALLPAPER_PACKAGE)/*)),--file "$(WALLPAPER_PACKAGE)/$(notdir $(f))=$(f)")

.PHONY : wallpaper-assets
wallpaper-assets: $(WALLPAPER_ASSETS)
$(WALLPAPER_STAMP): $(WALLPAPER_SOURCES) ../tools/build_wallpaper_assets.py ../tools/qoi.py | ccl-manifest
	$(ALR) python3 ../tools/build_wallpaper_assets.py ../assets/wallpapers build/$(WALLPAPER_PACKAGE)
	touch $@

# Desktop's application icon atlas (Desktop_Icon_Assets): the CuBit icon
# PNGs at every exported edge, in Desktop_Icons.Icon_ID order, packed by
# tools/build_icon_assets.py.
ICON_PACKAGE := $(ASSET_ROOT)/cubit-icons/5
ICON_ASSETS := build/$(ICON_PACKAGE)/applications.qoi
ICON_SOURCES := $(wildcard ../assets/desktop-icons/png/*.png) $(wildcard ../assets/penny/penny-*.png)
ICON_DISK_FILES = --file "$(ICON_PACKAGE)/applications.qoi=$(ICON_ASSETS)"

.PHONY : icon-assets
icon-assets: $(ICON_ASSETS)
$(ICON_ASSETS): $(ICON_SOURCES) ../tools/build_icon_assets.py ../tools/generate_desktop_icons.py ../tools/qoi.py
	$(ALR) python3 ../tools/build_icon_assets.py build/$(ICON_PACKAGE)

# The lexicon (docs/lexicon.md): WordNet 3.0 and SCOWL from the pinned
# nixpkgs, compiled into one read-only image; their notices ship beside it.
LEXICON_PACKAGE := $(ASSET_ROOT)/cubit-lexicon/1
LEXICON_ASSETS := build/$(LEXICON_PACKAGE)/english.lexicon build/$(LEXICON_PACKAGE)/LEXICON-NOTICE
LEXICON_DISK_FILES = $(foreach f,$(LEXICON_ASSETS),--file "$(LEXICON_PACKAGE)/$(notdir $(f))=$(f)")

.PHONY : lexicon-assets
lexicon-assets: $(LEXICON_ASSETS)
$(LEXICON_ASSETS) &: ../tools/build_lexicon.py ../flake.lock ../flake.nix
	$(ALR) python3 -I ../tools/build_lexicon.py build/$(LEXICON_PACKAGE)/english.lexicon \
		--notice build/$(LEXICON_PACKAGE)/LEXICON-NOTICE

# The Bible (APP-005): the KJV with the Apocrypha, the BSB, the WEB with
# the deuterocanon and the Clementine Vulgate (Latin), from the public-domain
# sources pinned by SHA-256 in assets/bible/sources.json (fetched once, https
# only; a changed source is refused), compiled into one read-only image each;
# their provenance and licences in BIBLE-NOTICE.
BIBLE_PACKAGE := $(ASSET_ROOT)/cubit-bible/1
BIBLE_ASSETS := $(foreach t,kjv bsb web vul,build/$(BIBLE_PACKAGE)/$(t).bible) build/$(BIBLE_PACKAGE)/BIBLE-NOTICE
BIBLE_DISK_FILES = $(foreach f,$(BIBLE_ASSETS),--file "$(BIBLE_PACKAGE)/$(notdir $(f))=$(f)")

.PHONY : bible-assets
bible-assets: $(BIBLE_ASSETS)
$(BIBLE_ASSETS) &: ../tools/build_bible.py ../assets/bible/sources.json
	$(ALR) python3 -I ../tools/build_bible.py build/$(BIBLE_PACKAGE)

# Every asset package the desktop session reads (Desktop: wallpapers and
# icons; lexicon.svc: the lexicon; bible.app: the Bible), and their disk
# placements.
DESKTOP_ASSETS = $(WALLPAPER_ASSETS) $(ICON_ASSETS) $(LEXICON_ASSETS) $(BIBLE_ASSETS)
DESKTOP_ASSET_DISK_FILES = $(WALLPAPER_DISK_FILES) $(ICON_DISK_FILES) $(LEXICON_DISK_FILES) $(BIBLE_DISK_FILES)

# Build the userspace display scanout service prototype
.PHONY : display
CUBIT_COMPOSITOR ?= vulkan
CUBIT_COMPOSITOR_TIMING ?= off
CUBIT_COMPOSITOR_STORAGE ?= production
CUBIT_DISPLAY_TEST_MODE ?= production
export CUBIT_MESA_BUILD CUBIT_MESA_BUNDLE CUBIT_MESA_SOURCE CUBIT_MESA_CACHE
# Both libraries must be complete before the combined Mesa bundle is selected.
ifeq ($(CUBIT_COMPOSITOR),vulkan)
build/gpr/desktop.stamp: $(LIBC_STAMP) | libc
endif
# Test only: the headless desktop-restart test's desktop fails once (BUG-001).
CUBIT_DESKTOP_FAULT ?= off
export CUBIT_COMPOSITOR CUBIT_COMPOSITOR_TIMING CUBIT_COMPOSITOR_STORAGE CUBIT_DISPLAY_TEST_MODE CUBIT_DESKTOP_FAULT
PRESENTATION_BUILD_DIR = $(if $(filter output-rebind,$(CUBIT_DISPLAY_TEST_MODE)),build-output-rebind,$(if $(filter delayed,$(CUBIT_DISPLAY_TEST_MODE)),build-delayed,build))
display: build/gpr/display.stamp
build/gpr/display.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/display/manifest.ccl) | user_runtime ccl-manifest
	mkdir -p ../userspace/services/display/build
	$(call compile_ccl_manifest,../userspace/services/display)
	$(NATIVE_GPRBUILD) -p -P ../userspace/services/display/display.gpr -XCUBIT_DISPLAY_TEST_MODE=$(CUBIT_DISPLAY_TEST_MODE)
	$(call stage,../userspace/services/display/$(PRESENTATION_BUILD_DIR)/display.svc,display.svc)
	@python3 ../tools/gpr_inputs.py build/gpr/display.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/display/display.gpr -XCUBIT_DISPLAY_TEST_MODE=$(CUBIT_DISPLAY_TEST_MODE)
	@touch $@
-include build/gpr/display.d

# Build the userspace network manager service
.PHONY : netmgr
netmgr: isodir/boot/netmgr.svc
$(eval $(call ccl_program,../userspace/services/netmgr,netmgr.svc,))

# wifi.svc: the Wi-Fi connection manager and supplicant (docs/wifi.md).
.PHONY : wifi
wifi: isodir/boot/wifi.svc
$(eval $(call ccl_program,../userspace/services/wifi,wifi.svc,))

# Build the userspace device manager
.PHONY : devmgr
devmgr: build/gpr/devmgr.stamp
build/gpr/devmgr.stamp: $(GPR_STEP_INPUTS) $(wildcard ../userspace/services/devmgr/manifest.ccl) | user_runtime intel-buffer-layout-check
	mkdir -p ../userspace/services/devmgr/build
	$(NATIVE_GPRBUILD) -P ../userspace/services/devmgr/devmgr.gpr
	$(call stage,../userspace/services/devmgr/build/devmgr.svc,devmgr.svc)
	@python3 ../tools/gpr_inputs.py build/gpr/devmgr.d $@ -- $(NATIVE_GPRLS) -P ../userspace/services/devmgr/devmgr.gpr
	@touch $@
-include build/gpr/devmgr.d

.PHONY: intel-buffer-layout-check
intel-buffer-layout-check:
	python3 ../tools/check-intel-buffer-layout.py

# Build the userspace shell app
.PHONY : shell
shell: isodir/boot/shell.app
$(eval $(call ccl_program,../userspace/apps/shell,shell.app,))

# The Rust/Turso worker is a stage-2 service, not part of the boot initrd.
.PHONY : config-storage
# BLD-001 interim: its script (gprbuild, the Turso crate, cargo, ld) runs
# only when one of its sources, the Rust std port or a shared input changed.
config-storage: build/gpr/config-storage.stamp
build/gpr/config-storage.stamp: $(GPR_STEP_INPUTS) $(shell find ../userspace/services/config-storage \
		../tests/config-turso/native ../userspace/rust/std -type d \( -name 'build*' -o -name target \) -prune \
		-o -type f -print) | user_runtime ccl-manifest
	bash ../userspace/services/config-storage/build.sh
	$(call stage,../userspace/services/config-storage/build/config-storage.svc,config-storage.svc)
	@touch $@

# Files that live on ext2 disk images (stage-2 content).
# Disk images rebuild automatically when any of these change.
DOOM_WAD ?= ../doom1.wad

# Servo (docs/servo-port.md) joins the disk once built (`make servo`; it
# fetches its pinned Servo checkout into userspace/rust/build/servo-work):
# cubitshell.app and its start page (servo/pages). Its fonts are linked in.
SERVO_DISK_CONTENTS := $(wildcard isodir/boot/cubitshell.app)
DISK_CONTENTS := isodir/boot/shell.app isodir/boot/doom.elf isodir/boot/sleep.app \
	isodir/boot/sameboy.app ../userspace/ports/sameboy/build/test.gb \
	isodir/boot/capability-test.app \
	isodir/boot/wget.app isodir/boot/devices.app isodir/boot/files.app isodir/boot/text.app \
	isodir/boot/calculator.app \
	isodir/boot/config-inspector.app isodir/boot/ccl-vm.app isodir/boot/ccl-workbench.app \
	isodir/boot/ccl-test-host.svc \
	isodir/boot/clock.svc isodir/boot/timesync.svc isodir/boot/tls.svc isodir/boot/config-storage.svc \
	isodir/boot/lexicon.svc isodir/boot/dictionary.app isodir/boot/bible.app \
	isodir/boot/desktop-shell.app \
	isodir/boot/ipctest-server.app isodir/boot/ipctest-client.app \
	isodir/boot/bench-ipc-server.app isodir/boot/bench-ipc-client.app \
	isodir/boot/logstore.svc isodir/boot/display.svc isodir/boot/desktop.svc \
	build/tls-roots.der \
	../init.ccl ../userspace/motd.txt $(DOOM_WAD) \
	$(SERVO_DISK_CONTENTS) $(DESKTOP_ASSETS)

SERVO_DISK_FILES = $(if $(SERVO_DISK_CONTENTS), \
	--file "servo/pages=../userspace/servo/start-pages")

# Guest memory for the interactive desktop targets (Servo needs well over
# the 128 MB the headless tests use).
DESKTOP_MEMORY ?= 2G

# Shared, verified scratch preparation for all desktop launchers. Each invocation
# resets the scratch disk from the base; this is NOT persistent across launches.
# Override both paths for tests; never point the output at a persistent user disk.
DESKTOP_BASE_DISK ?= nvme_disk.img
DESKTOP_SCRATCH_DISK ?= desktop_disk.img
DESKTOP_CONFIG_SAMPLES := ../userspace/ccl/samples/config-counter.ccl \
	../userspace/ccl/samples/config-counter-read.ccl
DESKTOP_OVERLAY := isodir/boot/desktop.svc isodir/boot/display.svc isodir/boot/metrics.svc \
	isodir/boot/netstack.svc isodir/boot/tls.svc isodir/boot/desktop-shell.app \
	isodir/boot/devices.app isodir/boot/config-inspector.app isodir/boot/files.app isodir/boot/text.app \
	isodir/boot/calculator.app isodir/boot/doom.elf isodir/boot/ccl-workbench.app \
	isodir/boot/ccl-console.app isodir/boot/logs.app isodir/boot/metricsview.app isodir/boot/config-storage.svc \
	isodir/boot/lexicon.svc isodir/boot/dictionary.app isodir/boot/bible.app \
	isodir/boot/neon-blast.app $(SERVO_DISK_CONTENTS)
DESKTOP_SAMPLE_ARGS = $(foreach sample,$(DESKTOP_CONFIG_SAMPLES),--file "work/$(notdir $(sample))=$(sample)")

# The session's startup profile (the Bible demo starts the Bible too).
DESKTOP_INIT ?= ../tests/headless/init-desktop-session.ccl

DESKTOP_DISK_ARGS = --boot $(DESKTOP_OVERLAY) \
		--file "init.ccl=$(DESKTOP_INIT)" $(DESKTOP_SAMPLE_ARGS) \
		$(DESKTOP_ASSET_DISK_FILES)

# The launchers that never build (run-desktop-fast, run-desktop-inspect):
# overlay what is staged now onto a fresh copy of the base.
.PHONY : prepare-desktop-disk
prepare-desktop-disk: $(DESKTOP_ASSETS)
	python3 ../tools/prepare_desktop_disk.py "$(DESKTOP_BASE_DISK)" "$(DESKTOP_SCRATCH_DISK)" --replace \
		$(DESKTOP_DISK_ARGS)

# run-desktop (BLD-001): the same overlay, made once into
# DESKTOP_PREPARED_DISK and again only when the base, a payload or the
# overlay's arguments (build/desktop-disk.args, rewritten only when they
# change) changed. Every launch still resets the scratch disk, to a copy of
# that prepared disk: nothing persists between runs.
DESKTOP_PREPARED_DISK ?= $(basename $(DESKTOP_SCRATCH_DISK))-prepared.img
build/desktop-disk.args: FORCE $(DESKTOP_ASSETS)
	@mkdir -p build
	@printf '%s\n' '$(DESKTOP_BASE_DISK) $(strip $(DESKTOP_DISK_ARGS))' > $@.new
	@if cmp -s $@.new $@; then rm $@.new; else mv $@.new $@; fi
$(DESKTOP_PREPARED_DISK): $(DESKTOP_BASE_DISK) $(DESKTOP_OVERLAY) $(DESKTOP_INIT) $(DESKTOP_CONFIG_SAMPLES) \
		$(DESKTOP_ASSETS) ../tools/prepare_desktop_disk.py build/desktop-disk.args
	python3 ../tools/prepare_desktop_disk.py "$(DESKTOP_BASE_DISK)" "$@" --replace $(DESKTOP_DISK_ARGS)
.PHONY : desktop-disk-reset
desktop-disk-reset: $(DESKTOP_PREPARED_DISK)
	cp --sparse=always $(DESKTOP_PREPARED_DISK) $(DESKTOP_SCRATCH_DISK).new
	mv $(DESKTOP_SCRATCH_DISK).new $(DESKTOP_SCRATCH_DISK)
	@echo "Desktop scratch disk: reset from $(DESKTOP_PREPARED_DISK)"

.PHONY : servo
servo: libc ccl-manifest
	bash ../userspace/servo/build-cubitshell.sh
	cp ../userspace/rust/build/cubitshell.app isodir/boot/cubitshell.app

# Programs still staged by a phony target (gprbuild or a build script, not
# ccl-build; BLD-001): each staged file names its target, with an empty
# recipe, so make runs the target before it compares the file's time with
# an image's. The targets stage content-stably (stage above).
# Their own up-to-date checks (gprbuild, the scripts) run on every make.
isodir/boot/ccl-console.app: ccl-console ;
isodir/boot/ccl-vm.app: ccl-vm ;
isodir/boot/ccl-workbench.app: ccl-workbench ;
isodir/boot/clock.svc: clock ;
isodir/boot/config-storage.svc: config-storage ;
isodir/boot/config.svc: config ;
isodir/boot/desktop.svc: desktop ;
isodir/boot/devmgr.svc: devmgr ;
isodir/boot/display.svc: display ;
isodir/boot/ipctest-client.app: ipctest-client ;
isodir/boot/ipctest-departing.app: ipctest-client ;
isodir/boot/neon-blast.app: neon-blast ;
isodir/boot/netstack.svc: netstack ;
isodir/boot/nvme.drv: nvme ;
isodir/boot/ramdisk.drv: ramdisk ;
isodir/boot/tls.svc: tls ;
isodir/boot/virtio-gpu.drv: virtio-gpu ;

# Every stage-1 binary copied into initrd.img.  Keep this dependency list next
# to the staging targets: copying an old service beside a newly rebuilt driver
# can silently violate their capability/protocol contract.
INITRD_SERVICES := filesystem ramdisk ata nvme netstack virtio-net virtio-gpu hda \
	mixer ps2 xhci virtio-input intel-gpu ufs generic-check gpio-launch-check hid-launch-check rtw89 config netmgr wifi procmgr devmgr

# Binaries used by the graphical desktop session. Keep this as a separate
# staging target so run-desktop can build/copy them into isodir first, then
# rebuild the ext2 disk image from those freshly staged files.
.PHONY : desktop-session-content
desktop-session-content: $(INITRD_SERVICES) desktop metricsvc display logstore \
	desktop-shell devices files text calculator config-inspector ccl-workbench ccl-console logs config-storage clock timesync tls doom sameboy \
	lexicon dictionary bible neon-blast

# Create ext2 ATA disk image containing doom1.wad, doom.elf, init.ccl, shell.app
# Requires: mke2fs, debugfs (from e2fsprogs)
# Both generated development disks use the same verified payload builder.
# ccl-sign is order-only: a rebuilt signer makes the same signatures.
ata_disk.img nvme_disk.img: $(DISK_CONTENTS) $(DESKTOP_CONFIG_SAMPLES) ../tools/build_development_disk.py | ccl-sign
	$(SIGN_PROGRAMS)
	$(ALR) python3 ../tools/build_development_disk.py $@ \
		--boot $(filter isodir/boot/%,$(DISK_CONTENTS)) \
		--file "motd.txt=../userspace/motd.txt" \
		--file "doom1.wad=$(DOOM_WAD)" \
		--file "sameboy/00.gb=../userspace/ports/sameboy/build/test.gb" \
		--file "tls/roots.der=build/tls-roots.der" \
		--file "init.ccl=../init.ccl" \
		--file "Documents/notes.txt=../userspace/apps/text/notes.txt" \
		$(DESKTOP_SAMPLE_ARGS) \
		$(SERVO_DISK_FILES) \
		$(DESKTOP_ASSET_DISK_FILES)

# Convenience aliases for manual rebuilds (e.g. make ata_disk)
.PHONY : ata_disk nvme_disk
ata_disk: ata_disk.img
nvme_disk: nvme_disk.img

# Minimal stage-2 disk for the authority non-amplification boot test.  Keeping
# this separate from nvme_disk.img prevents CI from building desktop, browser,
# and game payloads that the security test never launches.
CAPABILITY_SECURITY_DISK := capability_security_disk.img

$(CAPABILITY_SECURITY_DISK): capability-test logstore-test ../tests/headless/init-capability-security.ccl ccl-sign
	$(SIGN_PROGRAMS)
	dd if=/dev/zero of=$(CAPABILITY_SECURITY_DISK) bs=1K count=16384
	mke2fs -t ext2 -F $(CAPABILITY_SECURITY_DISK)
	debugfs -w -R "write ../tests/headless/init-capability-security.ccl init.ccl" $(CAPABILITY_SECURITY_DISK)
	debugfs -w -R "write isodir/boot/capability-test.app capability-test.app" $(CAPABILITY_SECURITY_DISK)
	# Services' debug text reaches serial only through logstore's mirror (ABI-002).
	debugfs -w -R "write isodir/boot/logstore-test.svc logstore.svc" $(CAPABILITY_SECURITY_DISK)

# Build the real kernel, normal stage-1 service graph, and only the stage-2
# payload needed by the capability test.  The recipe creates the initrd after
# all services have been staged so a parallel caller cannot capture a partial
# archive.
CAPABILITY_SECURITY_STAGE1 := filesystem ramdisk ata nvme netstack virtio-net \
	virtio-gpu hda mixer ps2 xhci virtio-input intel-gpu ufs generic-check gpio-launch-check hid-launch-check rtw89 config netmgr wifi procmgr devmgr

.PHONY : capability-security-image
capability-security-image: cubit_kernel $(CAPABILITY_SECURITY_STAGE1) $(CAPABILITY_SECURITY_DISK)
	$(MAKE) initrd
	cp cubit_kernel isodir/boot/
	grub-mkrescue -o cubit_kernel.iso isodir

# Create CPIO newc initrd image containing stage-1 service binaries.
# The kernel parses this at boot and loads services from it.
# Stage-2 content (shell.app, doom.elf, motd.txt, etc.) lives on disk.
# A file target (BLD-001): the realizer writes build/initrd.img.d, a
# rule naming every file the archive was made from, so the archive is made
# again only when one of them changed. The services are also order-only
# prerequisites: built first, even before the first archive names them.
.PHONY : initrd
initrd: isodir/boot/initrd.img
-include build/initrd.img.d
isodir/boot/initrd.img: ../images/development-initrd.ccl $(CCL_IMAGE_CATALOG) ../system.ccl \
		$(CCL_IMAGE_REALIZER) ../tools/image_debug.py $(CCL_IMAGE_TOOL) $(CCL_CONFIG_TOOL) FORCE \
		| $(INITRD_SERVICES) ccl-image ccl-sign
	@if $(call unless_made,$@); then :; else set -e; \
	  echo "image: $@ (realize.py)"; mkdir -p build; \
	  $(SIGN_PROGRAMS); \
	  $(ALR) python3 $(CCL_IMAGE_REALIZER) ../images/development-initrd.ccl $(IMAGE_SIGNING) \
	    --debug-dir debug --output $@ --depfile build/initrd.img.d; \
	  $(call made_record,$@); fi

# Self-contained, read-only userspace image for real-hardware bring-up.  Keep
# this separate from the normal two-stage image: the laptop must reach a
# desktop and diagnostic UI before AHCI or any other block driver works.
LAPTOP_LIVE_STAGE1 := filesystem ramdisk ata nvme netstack virtio-net virtio-gpu hda \
	mixer ps2 xhci virtio-input intel-gpu ufs rtw89 config netmgr wifi procmgr devmgr \
	gpio i2c-dw hid-i2c
LAPTOP_LIVE_STAGE2 := logstore clock display desktop ccl-workbench ccl-console logs devices files config-inspector boot-logs \
	storage-check doom sameboy tls timesync secrets llm agent tls-roots
LAPTOP_LIVE_RW_DISK := laptop_live_rw.img
# All runnable examples belong in the live workspace. The image regression
# also requires matching explicit optical placements in the CCL profile.
CCL_WORKSPACE_SAMPLES := $(sort $(wildcard ../userspace/ccl/samples/*.ccl))

# Native USB optical edition. Keep the original initrd-only laptop image as
# an independent fallback.
.PHONY: usb-live-iso run-usb-live run-usb-live-fast
usb-live-iso: ccl-image ccl-sign ../images/laptop-usb.ccl $(CCL_IMAGE_CATALOG) cubit_kernel $(LAPTOP_LIVE_STAGE1) $(LAPTOP_LIVE_STAGE2) intel-gpu config-storage teapots calculator lexicon dictionary bible debugger $(LAPTOP_LIVE_RW_DISK) $(DESKTOP_ASSETS)
	@test -s isodir/boot/cubitshell.app || (echo "missing Servo payload; run make -C kernel servo first" && false)
	$(SIGN_PROGRAMS)
	CUBIT_SIGNING=$(CUBIT_SIGNING) bash ../tests/usb-optical/build-live.sh "$(DOOM_WAD)"

run-usb-live: usb-live-iso
	$(MAKE) run-usb-live-fast

# UEFI-only companion for machines without CSM, e.g. the N95 reference NUC.
# Retain the existing legacy image for the laptop/IODD regression path. One
# live image for every machine (HW-002): every driver is in it, devmgr
# starts those whose devices are present, and Desktop's GPU rendering is a
# setting (desktop.render.gpu), not an image variant.
.PHONY: usb-live-uefi-iso
usb-live-uefi-iso: usb-live-iso
	bash ../tests/usb-optical/build-live.sh "$(DOOM_WAD)" --uefi

# RELEASE-001: the public preview image (images/preview.ccl), UEFI. It
# ships vmm.app (VMM-001, experimental) with its pinned Linux guest.
.PHONY: preview-uefi-iso
preview-uefi-iso: check-image-apps usb-live-iso debug-demo debug-demo-rs vmm
	bash ../tests/usb-optical/build-live.sh "$(DOOM_WAD)" --uefi --preview

# SYS-001: the UEFI live medium that installs CuBit (installer.svc, the
# console, and the install payload at @cd:0/install).
.PHONY: usb-live-install-iso
usb-live-install-iso: usb-live-iso installer install-efi installed-initrd
	bash ../tests/usb-optical/build-live.sh "$(DOOM_WAD)" --uefi --install

# SELF-002: the device-build payload, the one tree an image needs to build
# apps on the device (tests/device-builds/build.sh): the toolchain at
# /toolchain, the manifest step's tool at /tools (ccl-manifest.app), an
# empty Storehouse, the source subset in work/remote and the
# mailbox, as tests/device-builds/build/root/ and its ext2 image root.ext2
# (also the Linux reference builds the tests compare with).
.PHONY: device-build-root
device-build-root: libc user_runtime ccl-manifest ccl-build ccl-remote
	bash ../tests/device-builds/build.sh isodir/boot

# SELF-002: the UEFI live medium that builds on the device: its live RAM
# root (@mem:0, @system) is the device-build payload, ccl-remote listens
# with builds granted, and its installer carries the same payload onto the
# installed root (cubit_live_build_uefi.img). The -test image differs only
# in showing the pairing code on the serial console (tests/device-builds/run-live.sh).
# Every program the Apps menu can launch ships in every laptop image.
.PHONY: check-image-apps
check-image-apps:
	python3 -I ../tests/image-apps/check_launch_programs.py ..

.PHONY: usb-live-build-uefi-iso usb-live-build-test-uefi-iso
# VMM-001 slice 1, the N150 run: the UEFI live medium plus vmm.app, started
# with (vm-create approve-declared) (tests/hardware/vmm-laptop-build.sh).
.PHONY: usb-live-vmm-uefi-iso
usb-live-vmm-uefi-iso: usb-live-iso vmm
	bash ../tests/usb-optical/build-live.sh "$(DOOM_WAD)" --uefi --vmm
usb-live-build-uefi-iso: check-image-apps usb-live-iso installer install-efi installed-initrd device-build-root
	bash ../tests/usb-optical/build-live.sh "$(DOOM_WAD)" --uefi --device-build
usb-live-build-test-uefi-iso: check-image-apps usb-live-iso installer install-efi installed-initrd device-build-root
	bash ../tests/usb-optical/build-live.sh "$(DOOM_WAD)" --uefi --device-build-test

# Match the working desktop frontend: SDL's native Wayland display can leave
# a running guest without a visible window on the development workstation.
# This target only reuses the completed ISO; it never refreshes its contents.
run-usb-live-fast:
	@test -f cubit_laptop_usb.img || (echo "missing cubit_laptop_usb.img; run make -C kernel usb-live-iso first" && false)
	GDK_BACKEND=x11 qemu-system-x86_64 $(QEMU_ACCEL_ARGS) -machine q35 -cpu host -smp 4 -m 8G \
		-nodefaults -device VGA -device qemu-xhci,id=xhci \
		-drive file=cubit_laptop_usb.img,if=none,id=livecd,media=cdrom,format=raw,readonly=on \
		-device usb-bot,id=usbcd,bus=xhci.0,port=1 \
		-device scsi-cd,bus=usbcd.0,lun=0,drive=livecd,bootindex=1 \
		-device usb-mouse,bus=xhci.0,port=2 \
		-audiodev sdl,id=snd0 -device ich9-intel-hda -device hda-output,audiodev=snd0 \
		-serial file:/tmp/cubit-usb-live-serial.log -display gtk,zoom-to-fit=on -no-reboot

# This is the generated boot template, never an attached persistent user disk.
# Both laptop images copy it into RAM; rebuilding refreshes the demo workspace.
$(LAPTOP_LIVE_RW_DISK): $(CCL_WORKSPACE_SAMPLES) ../userspace/ccl/samples Makefile
	dd if=/dev/zero of=$(LAPTOP_LIVE_RW_DISK) bs=1M count=8
	mke2fs -t ext2 -F -m 0 $(LAPTOP_LIVE_RW_DISK)
	debugfs -w -R "mkdir work" $(LAPTOP_LIVE_RW_DISK)
	debugfs -w -R "mkdir tmp" $(LAPTOP_LIVE_RW_DISK)
	for sample in $(CCL_WORKSPACE_SAMPLES); do \
		debugfs -w -R "write $$sample /work/$${sample##*/}" $(LAPTOP_LIVE_RW_DISK); \
	done

test-ccl-images: $(LAPTOP_LIVE_RW_DISK)

.PHONY : laptop-live-initrd
laptop-live-initrd: ccl-image ccl-sign ../images/laptop-initrd.ccl $(CCL_IMAGE_CATALOG) $(LAPTOP_LIVE_STAGE1) $(LAPTOP_LIVE_STAGE2) $(LAPTOP_LIVE_RW_DISK)
	$(SIGN_PROGRAMS)
	$(ALR) python3 $(CCL_IMAGE_REALIZER) ../images/laptop-initrd.ccl $(IMAGE_SIGNING) \
		--input "doom-wad=$(DOOM_WAD)" --debug-dir debug --output isodir/boot/laptop-live-initrd.img

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

# The ISOs (BLD-001): grub-mkrescue packs the whole isodir, so each depends
# on build/isodir.stamp, a listing of isodir by content that is rewritten
# only when a file in it was added, removed or changed (tools/content_stamp.py;
# the headless tests copy the kernel in and edit grub.cfg and restore it,
# which changes times, not content). cubit_kernel.iso boots GRUB's first
# entry; RUN_ISO, which the run targets boot, the NVMe entry (4): the same
# isodir with a grub.cfg grafted in its place, so the shared grub.cfg is
# never edited.
RUN_ISO := cubit_run.iso
RUN_GRUB_ENTRY := 4
.PHONY : iso
iso: cubit_kernel.iso
# The headless tests copy the same kernel in: copied only when it differs.
isodir/boot/cubit_kernel: cubit_kernel FORCE
	@cmp -s $< $@ || { echo "stage cubit_kernel"; cp $< $@; }
# Signed first: an ISO never packs a program the sign step has not seen.
build/isodir.stamp: FORCE isodir/boot/cubit_kernel isodir/boot/initrd.img | ccl-sign
	@$(SIGN_PROGRAMS)
	@python3 ../tools/content_stamp.py $@ isodir
cubit_kernel.iso: build/isodir.stamp FORCE
	@if $(call unless_made,$@); then :; else set -e; \
	  echo "grub-mkrescue -o $@ isodir"; grub-mkrescue -o $@ isodir; \
	  $(call made_record,$@); fi
$(RUN_ISO): build/isodir.stamp FORCE
	@if $(call unless_made,$@); then :; else set -e; \
	  echo "grub-mkrescue -o $@ (isodir, GRUB entry $(RUN_GRUB_ENTRY))"; \
	  sed 's/^set default=.*/set default=$(RUN_GRUB_ENTRY)/' isodir/boot/grub/grub.cfg > build/run-grub.cfg; \
	  grub-mkrescue -o $@ isodir boot/grub/grub.cfg=build/run-grub.cfg; \
	  $(call made_record,$@); fi

# Build everything including DOOM, then create ISO and disk images (last, so
# they hold every program world stages: BLD-001)
.PHONY : world
# Test programs the headless images pack. world builds them too, so no test
# runs a binary left over from an older ABI (a stale one passes or fails for
# the wrong reason).
TEST_PROGRAMS := logs boot-logs timesync-test llm-test ccl-console-fetch-test logstore-test ccl-remote-test console-route-check late-intro-check ccl-console log-check \
	log-fields-check log-retention-check log-exit-probe crash-probe stack-probe libc-check cxx-check network-check net-link-check \
	net-multi-check timesync-check \
	tls-probe tls-check config-check desktop-check display-check clipboard-check \
	metrics-check grant-forward ccl-control input-stress bench-storage \
	bench-input disk-write-check install-boot-check

.PHONY : test-programs
test-programs: $(TEST_PROGRAMS)

# Binaries only the hardware images (usb-live-iso and its UEFI/GPU variants)
# pack. world builds them too, so a world pass means every image binary
# compiled and links; the image targets name the same phony targets, so one
# make invocation builds each once.
IMAGE_ONLY_BINARIES := ramdisk intel-gpu gpu-viewer teapots neon-blast gpio i2c-dw hid-i2c acpi-native

# CAP-002: capabilities are named by handles, never numbers. Runs first in
# world; --self-test shows it rejecting each forbidden form.
.PHONY : check-declarations
# BLD-001: the checks run again only when a file they read changed: the
# tree's sources of the kinds check_declarations.py reads (as git lists
# them, tracked or not), the staged executables check-shipped.py reads,
# and the scripts themselves.
DECLARATION_INPUTS := $(wildcard $(addprefix ../,$(shell git -C .. ls-files --cached --others --exclude-standard -- \
	'*.ads' '*.adb' '*.c' '*.h' '*.rs' '*.ccl' '*.S' '*.s' '*.asm' '*.patch' '*.diff' 2>/dev/null)))
check-declarations: build/gate/check-declarations.stamp
build/gate/check-declarations.stamp: $(DECLARATION_INPUTS) $(wildcard isodir/boot/*) \
		../tools/check_declarations.py ../tests/test-console/check-shipped.py
	python3 ../tools/check_declarations.py --self-test
	python3 ../tools/check_declarations.py --root ..
	python3 ../tests/test-console/check-shipped.py
	@mkdir -p $(@D) && touch $@

# No shipped manifest, plan or image declares, approves or contains the test
# console object (Test_Console_ABI); after world, every image's executables
# that is built is checked too (logstore.svc among them).
.PHONY : test-console-shipped
test-console-shipped: world
	python3 ../tests/test-console/check-shipped.py

world: check-declarations test-programs frame-ticker task-card-check $(IMAGE_ONLY_BINARIES) cubit_kernel doom sameboy sleep thread-check futex-check wait-check readiness-writer futex-share secondary-stack-check ada-runtime-check firmware-check firmware-undeclared clock-check avx-check wifi-crypto-check devmem-check fault-check reap-check font-masks trap-flag-check nmi-check direction-flag-check alignment-flag-check kernel-map-probe mov-ss-check clock-keeper-check oom-check stall-check bench-spread capability-test storage-check ipctest-server ipctest-client bench-ipc-server bench-ipc-client ccl-test-host clock timesync tls ccl-vm ccl-workbench filesystem ata nvme ufs installer virtio-net virtio-gpu netstack wget devices files text calculator lexicon dictionary bible config-inspector desktop-shell procmgr shell hda mixer ps2 xhci virtio-input config config-storage netmgr wifi logstore display desktop devmgr llm secrets power agent ccl-remote cpu-times-check metricsvc metricsview tlb-tags ccl-sign initrd ata_disk.img nvme_disk.img iso
	$(SIGN_PROGRAMS)

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
run: nvme_disk.img $(RUN_ISO)
	@rm -f serial_output.log net.pcap
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu $(QEMU_RUN_CPU) \
		-smp 4 \
		-m 128M \
		-cdrom $(RUN_ISO) \
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
# Start the native desktop directly, without the CLI launch path.
QEMU_DESKTOP_DEVICE ?= virtio-vga,xres=1024,yres=768
# Pointer for the desktop run targets (docs/vm-guide.md): a virtio tablet
# reports absolute positions, so QEMU's window never grabs the mouse and the
# guest cursor is the host's pointer. The PS/2 mouse stays as a relative
# pointer; QEMU_POINTER_DEVICE= (empty) leaves only that one.
QEMU_POINTER_DEVICE ?= virtio-tablet-pci
QEMU_POINTER_ARGS := $(if $(strip $(QEMU_POINTER_DEVICE)),-device $(QEMU_POINTER_DEVICE),)
# GTK window: zoom-to-fit (QEMU's default for virtio-gpu) lets the window be
# resized freely; every resize is offered to the guest as a new preferred
# mode, and CuBit follows it (docs/vm-guide.md). zoom-to-fit=off would pin
# the window's minimum size to the guest mode, so it could only grow.
QEMU_DESKTOP_DISPLAY ?= gtk,zoom-to-fit=on
.PHONY: run-desktop-dual
run-desktop-dual: QEMU_DESKTOP_DEVICE = '{"driver":"virtio-vga","id":"multi-gpu","max_outputs":2,"outputs":[{"name":"CuBit Main","xres":1024,"yres":768},{"name":"CuBit Side","xres":1024,"yres":768}]}'
run-desktop-dual: run-desktop

.PHONY: test-desktop-mixed
test-desktop-mixed: desktop display virtio-gpu display-check ccl-workbench
	CUBIT_TEST_MIXED_OUTPUTS=1 ../tests/headless/run.sh --test desktop-dual-output --accel kvm --cpus 4 --timeout 60 --keep-logs

# Everything run-desktop does before QEMU starts (BLD-001), as one target
# that can be timed and tested headless (tests/build-noop): build and stage
# what changed, remake the images that contain it, and reset the scratch
# disk from the prepared base.
#
# CONSOLE_MIRROR=1, the default (development sessions, BUG-008): boot
# logstore's test variant (logstore-test, with the test console object that
# DESKTOP_INIT's (test-console approve-declared) approves), so every
# program's console text, Desktop's launch lines and notices included, is
# mirrored onto serial_output.log, and a failed launch can be read there.
# Programs reach serial only through the test console (ABI-002), so this is
# the way. It is written onto the freshly reset scratch disk only; the
# prepared base and every shipped image are unchanged (tests/test-console).
# CONSOLE_MIRROR=0 boots the shipped logstore.
CONSOLE_MIRROR ?= 1
.PHONY : run-desktop desktop-stage
desktop-stage: desktop-session-content isodir/boot/cubit_kernel nvme_disk.img isodir/boot/initrd.img \
		$(RUN_ISO) desktop-disk-reset
run-desktop: desktop-stage
	$(if $(filter 1,$(CONSOLE_MIRROR)),$(MAKE) logstore-test && \
		debugfs -w -R "rm logstore.svc" $(DESKTOP_SCRATCH_DISK) >/dev/null 2>&1; \
		debugfs -w -R "write isodir/boot/logstore-test.svc logstore.svc" $(DESKTOP_SCRATCH_DISK) && \
		echo "run-desktop: CONSOLE_MIRROR=1: console text is mirrored to serial_output.log")
	@rm -f serial_output.log net.pcap
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu $(QEMU_RUN_CPU) \
		-smp 4 \
		-m $(DESKTOP_MEMORY) \
		-cdrom $(RUN_ISO) \
		-serial file:serial_output.log \
		-vga none \
		-device $(QEMU_DESKTOP_DEVICE) \
		-display $(QEMU_DESKTOP_DISPLAY) \
		$(QEMU_POINTER_ARGS) \
		-drive file=$(DESKTOP_SCRATCH_DISK),if=none,id=nvme0,format=raw \
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
	@test -f $(RUN_ISO) || (echo "missing $(RUN_ISO); run make -C kernel run-desktop once first" && false)
	@test -f nvme_disk.img || (echo "missing nvme_disk.img; run make -C kernel run-desktop once first" && false)
	@rm -f net.pcap
	$(MAKE) prepare-desktop-disk
	GDK_BACKEND=x11 python3 ../tools/run_logged_qemu.py \
		--log-root desktop-logs --latest serial_output.log \
		--hash isodir/boot/cubitshell.app --hash isodir/boot/desktop.svc \
		--hash $(RUN_ISO) -- qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu $(QEMU_RUN_CPU) \
		-smp 4 \
		-m $(DESKTOP_MEMORY) \
		-cdrom $(RUN_ISO) \
		-serial stdio \
		-vga none \
		-device virtio-vga,xres=1024,yres=768 \
		-display $(QEMU_DESKTOP_DISPLAY) \
		$(QEMU_POINTER_ARGS) \
		-drive file=$(DESKTOP_SCRATCH_DISK),if=none,id=nvme0,format=raw \
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
	@test -f $(RUN_ISO) || (echo "missing $(RUN_ISO); run make -C kernel run-desktop once first" && false)
	@test -f nvme_disk.img || (echo "missing nvme_disk.img; run make -C kernel run-desktop once first" && false)
	@rm -f serial_output.log net.pcap $(QEMU_QMP) $(QEMU_MONITOR) $(QEMU_SCREENSHOT)
	$(MAKE) prepare-desktop-disk
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu $(QEMU_RUN_CPU) \
		-smp 4 \
		-m $(DESKTOP_MEMORY) \
		-cdrom $(RUN_ISO) \
		-serial file:serial_output.log \
		-vga none \
		-device virtio-vga,xres=1024,yres=768 \
		-display $(QEMU_DESKTOP_DISPLAY) \
		$(QEMU_POINTER_ARGS) \
		-qmp unix:$(QEMU_QMP),server=on,wait=off \
		-monitor unix:$(QEMU_MONITOR),server,nowait \
		-drive file=$(DESKTOP_SCRATCH_DISK),if=none,id=nvme0,format=raw \
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
run-virtio-vga: nvme_disk.img $(RUN_ISO)
	@rm -f serial_output.log net.pcap
	GDK_BACKEND=x11 qemu-system-x86_64 \
		$(QEMU_ACCEL_ARGS) \
		-machine q35 \
		-cpu $(QEMU_RUN_CPU) \
		-smp 4 \
		-m 128M \
		-cdrom $(RUN_ISO) \
		-serial file:serial_output.log \
		-vga none \
		-device virtio-vga,xres=1024,yres=768 \
		-display $(QEMU_DESKTOP_DISPLAY) \
		$(QEMU_POINTER_ARGS) \
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
