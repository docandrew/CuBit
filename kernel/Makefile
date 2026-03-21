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

# Build DOOM and copy ELF into ISO directory
.PHONY : doom
doom:
	$(MAKE) -C ../userspace/c -f Makefile.doomgeneric
	cp ../userspace/c/doom_build/doom.elf isodir/boot/doom.elf

.PHONY : sleep
sleep: user_runtime
	mkdir -p ../userspace/apps/sleep/build
	$(ALR) gcc -c -fno-pic -mno-sse -mno-sse2 -mno-red-zone \
		../userspace/apps/sleep/manifest.c \
		-o ../userspace/apps/sleep/build/manifest.o
	$(ALR) gprbuild -P ../userspace/apps/sleep/sleep.gpr
	cp ../userspace/apps/sleep/build/sleep.app isodir/boot/sleep.app

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

# Build the userspace config store service
.PHONY : config
config: user_runtime
	mkdir -p ../userspace/services/config/build
	$(ALR) gprbuild -P ../userspace/services/config/config.gpr
	cp ../userspace/services/config/build/config.svc isodir/boot/config.svc

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
DISK_CONTENTS := isodir/boot/shell.app isodir/boot/doom.elf isodir/boot/sleep.app \
	../init.conf ../motd.txt ../doom1.wad

# Create ext2 ATA disk image containing doom1.wad, doom.elf, init.conf, shell.app
# Requires: mke2fs, debugfs (from e2fsprogs)
ata_disk.img: $(DISK_CONTENTS)
	dd if=/dev/zero of=ata_disk.img bs=1K count=8192
	mke2fs -t ext2 ata_disk.img
	debugfs -w -R "write ../doom1.wad doom1.wad" ata_disk.img
	debugfs -w -R "write ../motd.txt motd.txt" ata_disk.img
	debugfs -w -R "write ../init.conf init.conf" ata_disk.img
	debugfs -w -R "write isodir/boot/shell.app shell.app" ata_disk.img
	debugfs -w -R "write isodir/boot/doom.elf doom.elf" ata_disk.img
	debugfs -w -R "write isodir/boot/sleep.app sleep.app" ata_disk.img
	debugfs -w -R "write isodir/boot/wget.app wget.app" ata_disk.img
	dd if=/dev/zero of=/tmp/config.dat bs=8192 count=1
	debugfs -w -R "write /tmp/config.dat config.dat" ata_disk.img
	rm -f /tmp/config.dat

# Create ext2 NVMe disk image containing motd.txt, doom1.wad, doom.elf, init.conf, shell.app, wget.app
# Requires: mke2fs, debugfs (from e2fsprogs)
nvme_disk.img: $(DISK_CONTENTS)
	dd if=/dev/zero of=nvme_disk.img bs=1K count=16384
	mke2fs -t ext2 nvme_disk.img
	debugfs -w -R "write ../motd.txt motd.txt" nvme_disk.img
	debugfs -w -R "write ../doom1.wad doom1.wad" nvme_disk.img
	debugfs -w -R "write isodir/boot/doom.elf doom.elf" nvme_disk.img
	debugfs -w -R "write isodir/boot/sleep.app sleep.app" nvme_disk.img
	debugfs -w -R "write ../init.conf init.conf" nvme_disk.img
	debugfs -w -R "write isodir/boot/shell.app shell.app" nvme_disk.img
	debugfs -w -R "write isodir/boot/wget.app wget.app" nvme_disk.img
	dd if=/dev/zero of=/tmp/config.dat bs=8192 count=1
	debugfs -w -R "write /tmp/config.dat config.dat" nvme_disk.img
	rm -f /tmp/config.dat

# Convenience aliases for manual rebuilds (e.g. make ata_disk)
.PHONY : ata_disk nvme_disk
ata_disk: ata_disk.img
nvme_disk: nvme_disk.img

# Create CPIO newc initrd image containing stage-1 service binaries.
# The kernel parses this at boot and loads services from it.
# Stage-2 content (shell.app, doom.elf, motd.txt, etc.) lives on disk.
.PHONY : initrd
initrd:
	@rm -rf ramdisk_staging
	mkdir -p ramdisk_staging
	-cp isodir/boot/devmgr.svc ramdisk_staging/
	-cp isodir/boot/filesystem.svc ramdisk_staging/
	-cp isodir/boot/ata.drv ramdisk_staging/
	-cp isodir/boot/nvme.drv ramdisk_staging/
	-cp isodir/boot/netstack.svc ramdisk_staging/
	-cp isodir/boot/virtio-net.drv ramdisk_staging/
	-cp isodir/boot/hda.drv ramdisk_staging/
	-cp isodir/boot/mixer.svc ramdisk_staging/
	-cp isodir/boot/ps2.drv ramdisk_staging/
	-cp isodir/boot/config.svc ramdisk_staging/
	-cp isodir/boot/netmgr.svc ramdisk_staging/
	-cp isodir/boot/procmgr.svc ramdisk_staging/
	-cp ../system.conf ramdisk_staging/
	cd ramdisk_staging && ls | cpio -o -H newc > ../isodir/boot/initrd.img
	rm -rf ramdisk_staging

iso: cubit_kernel
	cp cubit_kernel isodir/boot/
	grub-mkrescue -o cubit_kernel.iso isodir

# Build everything including DOOM, then create ISO and disk images
.PHONY : world
world: cubit_kernel doom sleep filesystem ata nvme virtio-net netstack wget procmgr shell hda mixer ps2 config netmgr devmgr initrd ata_disk.img nvme_disk.img iso

# Run QEMU with NVMe + virtio-net + HDA audio, shell (everything)
.PHONY : run
run: nvme_disk.img
	@rm -f serial_output.log net.pcap
	sed -i 's/^set default=.*/set default=4/' isodir/boot/grub/grub.cfg
	grub-mkrescue -o cubit_kernel.iso isodir
	sed -i 's/^set default=.*/set default=0/' isodir/boot/grub/grub.cfg
	qemu-system-x86_64 \
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
