# CuBitOS Makefile
GIT_HASH := $(shell git rev-parse HEAD)
BUILD_DATE := $(shell date)

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

	gprbuild -Pkernel_runtime.gpr

# init usermode binary embedded in the kernel ELF object
build/init.bin: src/init.asm
	yasm --arch=x86 -fbin src/init.asm -o build/init.bin
#	ld --omagic -e start -Ttext 0 -o build/init.out build/init.o
#	objcopy -I binary -O elf64-x86-64 -B i386 

# kernel ELF object
cubit_kernel: build/libcubit.a build/init.bin
# Compile and bind only
	gprbuild -b -c -Pcubit.gpr
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
	gnatprove -Pcubit.gpr --mode=all --level=0 --no-axiom-guard

.PHONY : clean
clean:
	gprclean -Pcubit.gpr
	gprclean -Pkernel_runtime.gpr
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

.PHONY : run
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

iso: cubit_kernel
	cp cubit_kernel isodir/boot/
	grub-mkrescue -o cubit_kernel.iso isodir

.PHONY : docs
docs:
	gnatdoc -l -d --preserve-source-formatting -w -P cubit.gpr

.PHONY : html
html:
#	gnathtml.pl -l1 -I build/ -d -f -o documentation/html src/*.adb src/*.ads
	gnathtml.pl -cc grey -sc red -o build/gnathtml src/*.adb src/*.ads
