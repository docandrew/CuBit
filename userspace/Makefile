#
# CuBit user-space build
#

# Build everything
.PHONY: all
all: runtime drivers services initrd
	cp drivers/ramdisk/build/ramdisk.drv ../kernel/isodir/boot/ramdisk.drv
	cp init.img ../kernel/isodir/boot/init.img

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
# Drivers meta-target
###############################################################################
.PHONY: drivers
drivers: ramdisk

###############################################################################
# Filesystem Service
###############################################################################
.PHONY: fileservice
fileservice: services/fileservice/build/fileservice.svc

services/fileservice/build/fileservice.svc: runtime
	gprbuild -b -c -p -Pservices/fileservice/fileservice.gpr

	ld services/fileservice/build/*.o runtime/adalib/art0.o runtime/adalib/libgnat-user.a -o services/fileservice/build/fileservice.svc

###############################################################################
# Services meta-target
###############################################################################
.PHONY: services
services: fileservice

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
	gprclean -Pdrivers/ramdisk/ramdisk.gpr
	gprclean -Pservices/fileservice/fileservice.gpr
	gprclean -Pruntime/user_runtime.gpr
	rm runtime/adalib/art0.o
