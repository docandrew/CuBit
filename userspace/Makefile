#
# CuBit user-space build
#

# Build everything
.PHONY: all
all: runtime drivers services cprograms doom initrd
	cp drivers/ramdisk/build/ramdisk.drv ../kernel/isodir/boot/ramdisk.drv
	cp init.img ../kernel/isodir/boot/init.img
	cp c/build/hello.elf ../kernel/isodir/boot/hello.elf
	-cp services/filesystem/build/filesystem.svc ../kernel/isodir/boot/filesystem.svc 2>/dev/null
	-cp drivers/ata/build/ata.drv ../kernel/isodir/boot/ata.drv 2>/dev/null
	-cp c/doom_src/build/doom.elf ../kernel/isodir/boot/doom.elf 2>/dev/null

###############################################################################
# Runtime library
###############################################################################
.PHONY: runtime
runtime: runtime/adalib/libgnat-user.a runtime/adalib/art0.o

runtime/adalib/libgnat-user.a:
	gprbuild -p -Pruntime/user_runtime.gpr

runtime/adalib/art0.o:
	yasm --arch=x86 -felf64 --dformat=dwarf2 runtime/gnat/art0.asm -o runtime/adalib/art0.o

###############################################################################
# Ramdisk Driver
###############################################################################
.PHONY: ramdisk
ramdisk: drivers/ramdisk/build/ramdisk.drv

drivers/ramdisk/build/ramdisk.drv: runtime
	gprbuild -b -c -p -Pdrivers/ramdisk/ramdisk.gpr

	ld drivers/ramdisk/build/*.o runtime/adalib/art0.o runtime/adalib/libgnat-user.a -o drivers/ramdisk/build/ramdisk.drv

###############################################################################
# ATA Driver
###############################################################################
.PHONY: ata
ata: drivers/ata/build/ata.drv

drivers/ata/build/ata.drv: runtime
	gprbuild -b -c -p -Pdrivers/ata/ata.gpr

	ld drivers/ata/build/*.o runtime/adalib/art0.o runtime/adalib/libgnat-user.a -o drivers/ata/build/ata.drv

###############################################################################
# Drivers meta-target
###############################################################################
.PHONY: drivers
drivers: ramdisk ata

###############################################################################
# Filesystem Service
###############################################################################
.PHONY: filesystem
filesystem: services/filesystem/build/filesystem.svc

services/filesystem/build/filesystem.svc: runtime
	gprbuild -b -c -p -Pservices/filesystem/filesystem.gpr

	ld services/filesystem/build/*.o runtime/adalib/art0.o runtime/adalib/libgnat-user.a -o services/filesystem/build/filesystem.svc

###############################################################################
# Services meta-target
###############################################################################
.PHONY: services
services: filesystem

###############################################################################
# C Programs
###############################################################################
.PHONY: cprograms
cprograms:
	$(MAKE) -C c

###############################################################################
# DOOM
###############################################################################
.PHONY: doom
doom: cprograms
	$(MAKE) -C c/doom_src

###############################################################################
# Initial Ramdisk Image (initrd)
###############################################################################
.PHONY: initrd
initrd:
	cpio -o < init.manifest > init.img

###############################################################################
# Clean
###############################################################################
.PHONY: clean
clean:
	-gprclean -Pdrivers/ata/ata.gpr
	-gprclean -Pdrivers/ramdisk/ramdisk.gpr
	-gprclean -Pservices/filesystem/filesystem.gpr
	-gprclean -Pruntime/user_runtime.gpr
	rm -f runtime/adalib/art0.o
	$(MAKE) -C c clean
	$(MAKE) -C c/doom_src clean
