#
# CuBit OS - shared start code
# Copyright (C) 2026 Jon Andrew
#
# crt0.o is the start code every Ada service and Rust program links (with
# link.ld). C programs use the CuBit libc (userspace/libc) instead; the
# headers here only declare CuBit's C interfaces.
#

AS      = gcc
ASFLAGS = -ffreestanding -nostdlib
BUILDDIR = build
CRT_OBJ = $(BUILDDIR)/crt0.o

.PHONY: all clean

all: $(CRT_OBJ)

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(CRT_OBJ): crt0.S | $(BUILDDIR)
	$(AS) $(ASFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILDDIR)
