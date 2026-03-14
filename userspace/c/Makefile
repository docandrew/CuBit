#
# CuBit OS - C Userspace Build
# Copyright (C) 2026 Jon Andrew
#

CC      = gcc
AS      = gcc
LD      = ld
AR      = ar

CFLAGS  = -ffreestanding -nostdlib -nostartfiles -fno-builtin \
          -fno-stack-protector -fno-pic -fno-pie \
          -mcmodel=small -mno-red-zone \
          -isystem include -I. \
          -Wall -Wextra -O2

ASFLAGS = -ffreestanding -nostdlib

LDFLAGS = -nostdlib -static -T link.ld

BUILDDIR = build

# CRT startup object
CRT_OBJ = $(BUILDDIR)/crt0.o

# Runtime library objects
LIB_SRCS = cubit_syscalls.c cubit_mem.c cubit_time.c cubit_video.c cubit_input.c stdio.c string.c libc_stubs.c cubit_config.c
LIB_OBJS = $(patsubst %.c,$(BUILDDIR)/%.o,$(LIB_SRCS))
LIB      = $(BUILDDIR)/libcubit.a

# Programs to build
HELLO_ELF      = $(BUILDDIR)/hello.elf
TEST_MALLOC_ELF = $(BUILDDIR)/test_malloc.elf
SPIN_ELF       = $(BUILDDIR)/spin.elf

.PHONY: all clean

all: $(HELLO_ELF) $(TEST_MALLOC_ELF) $(SPIN_ELF)

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

# Assembly
$(CRT_OBJ): crt0.S | $(BUILDDIR)
	$(AS) $(ASFLAGS) -c $< -o $@

# C sources -> objects
$(BUILDDIR)/%.o: %.c cubit.h | $(BUILDDIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Static library of runtime functions
$(LIB): $(LIB_OBJS)
	$(AR) rcs $@ $^

# Programs link: crt0 + program object + libcubit
# Programs link: crt0 + program object + libcubit
$(HELLO_ELF): $(CRT_OBJ) $(BUILDDIR)/hello.o $(LIB)
	$(LD) $(LDFLAGS) $(CRT_OBJ) $(BUILDDIR)/hello.o $(LIB) -o $@

$(TEST_MALLOC_ELF): $(CRT_OBJ) $(BUILDDIR)/test_malloc.o $(LIB)
	$(LD) $(LDFLAGS) $(CRT_OBJ) $(BUILDDIR)/test_malloc.o $(LIB) -o $@

$(SPIN_ELF): $(CRT_OBJ) $(BUILDDIR)/spin.o $(LIB)
	$(LD) $(LDFLAGS) $(CRT_OBJ) $(BUILDDIR)/spin.o $(LIB) -o $@

clean:
	rm -rf $(BUILDDIR)
