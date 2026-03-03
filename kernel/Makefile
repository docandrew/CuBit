# CuBitOS Makefile
GIT_HASH := $(shell git rev-parse HEAD)
BUILD_DATE := $(shell date)

# Use Alire to get toolchain on PATH
ALR := alr exec --

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
		-e 's/SED_SRC_HASH/$(SRC_HASH)/' src/build.pre > src/build.ads

	$(ALR) gprbuild -Pkernel_runtime.gpr

# init usermode binary embedded in the kernel ELF object
build/init.bin: src/init.asm
	yasm --arch=x86 -fbin src/init.asm -o build/init.bin
#	ld --omagic -e start -Ttext 0 -o build/init.out build/init.o
#	objcopy -I binary -O elf64-x86-64 -B i386

# kernel ELF object
cubit_kernel: build/libcubit.a build/init.bin
# Compile and bind only
	$(ALR) gprbuild -b -c -Pcubit.gpr
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

.PHONY : run-raw
run-raw:
	qemu-system-x86_64 -kernel cubit_kernel

.PHONY : run-qemu
run-qemu:
# add -S to pause simulation at startup to allow debugger connection
# add -d int,cpu_reset,guest_errors for exception debugging
	qemu-system-x86_64 \
		-machine q35 \
		-cpu Broadwell \
		-d cpu_reset \
		-s \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-serial stdio \
		-drive file=nvm.qcow2,if=none,id=nvm \
		-device nvme,serial=MyNVMeDrive,drive=nvm

# Headless QEMU for CLI testing - serial output to file, no GUI
.PHONY : run-headless
run-headless:
	@rm -f serial_output.log
	timeout 45 qemu-system-x86_64 \
		-machine q35 \
		-cpu Broadwell \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-display none \
		-serial file:serial_output.log \
		-no-reboot; \
	echo "--- Serial Output ---"; \
	cat serial_output.log

# Build DOOM and copy ELF into ISO directory
.PHONY : doom
doom:
	$(MAKE) -C ../userspace/c -f Makefile.doomgeneric
	cp ../userspace/c/doom_build/doom.elf isodir/boot/doom.elf

# Build userspace Ada runtime (needed by filesystem server)
.PHONY : user_runtime
user_runtime:
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

# Create ext2 ATA disk image containing doom1.wad
# Requires: mke2fs, debugfs (from e2fsprogs)
.PHONY : ata_disk
ata_disk:
	dd if=/dev/zero of=ata_disk.img bs=1K count=8192
	mke2fs -t ext2 ata_disk.img
	debugfs -w -R "write ../doom1.wad doom1.wad" ata_disk.img

# Create ext2 ramdisk image containing doom1.wad
# Requires: mke2fs, debugfs (from e2fsprogs)
.PHONY : doom_ramdisk
doom_ramdisk:
	dd if=/dev/zero of=isodir/boot/doom_ramdisk.img bs=1K count=4500
	mke2fs -t ext2 isodir/boot/doom_ramdisk.img
	debugfs -w -R "write ../doom1.wad doom1.wad" isodir/boot/doom_ramdisk.img

iso: cubit_kernel
	cp cubit_kernel isodir/boot/
	grub-mkrescue -o cubit_kernel.iso isodir

# Build everything including DOOM, then create ISO
# TODO: update docs for all build targets (doom, filesystem, doom_ramdisk, world)
.PHONY : world
world: cubit_kernel doom filesystem ata doom_ramdisk iso

# Run QEMU with display (for WSL2 with GUI support)
.PHONY : run
run:
	@rm -f serial_output.log
	qemu-system-x86_64 \
		-machine q35 \
		-cpu Broadwell \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-serial file:serial_output.log \
		-no-reboot

# Run QEMU with an ATA disk image (for testing userspace ATA driver)
# q35 uses AHCI by default; add a PIIX3 IDE controller so the PIO driver
# can access the disk on legacy ports 0x1F0/0x3F6.
.PHONY : run-ata
run-ata:
	@rm -f serial_output.log
	qemu-system-x86_64 \
		-machine q35 \
		-cpu Broadwell \
		-m 128M \
		-cdrom cubit_kernel.iso \
		-serial file:serial_output.log \
		-drive file=ata_disk.img,format=raw,if=none,id=ata0 \
		-device piix3-ide,id=pata \
		-device ide-hd,drive=ata0,bus=pata.0 \
		-no-reboot

.PHONY : docs
docs:
	$(ALR) gnatdoc -l -d --preserve-source-formatting -w -P cubit.gpr

.PHONY : html
html:
#	gnathtml.pl -l1 -I build/ -d -f -o documentation/html src/*.adb src/*.ads
	$(ALR) gnathtml.pl -cc grey -sc red -o build/gnathtml src/*.adb src/*.ads
