#
# CuBit user-space build
#

# Build everything
.PHONY: all
all: runtime drivers

# Runtime library
.PHONY: runtime
runtime: runtime/adalib/libgnat-user.a runtime/adalib/art0.o

runtime/adalib/libgnat-user.a:
	gprbuild -p -Pruntime/user_runtime.gpr

runtime/adalib/art0.o:
	yasm --arch=x86 -felf64 --dformat=dwarf2 runtime/gnat/art0.asm -o runtime/adalib/art0.o

# Ramdisk Driver
.PHONY: ramdisk
ramdisk: drivers/ramdisk/build/ramdisk.drv

drivers/ramdisk/build/ramdisk.drv: runtime
	gprbuild -b -c -p -Pdrivers/ramdisk/ramdisk.gpr

	ld drivers/ramdisk/build/*.o runtime/adalib/art0.o runtime/adalib/libgnat-user.a -o drivers/ramdisk/build/ramdisk.drv

# Drivers
.PHONY: drivers
drivers: ramdisk


.PHONY: clean
clean:
	gprclean -Pdrivers/ramdisk/ramdisk.gpr
	gprclean -Pruntime/user_runtime.gpr