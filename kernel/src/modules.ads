-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- Multiboot Module Loading. CuBit kernel modules are ELF binaries loaded by
-- GRUB and available here for our use. We use these to bootstrap required
-- user-mode OS features before the filesystem is available.
-------------------------------------------------------------------------------
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;


package Modules with
    SPARK_Mode => On
is
    ModuleException : exception;

    MAGIC_RAMDISK_ADDRESS : System.Address;
    MAGIC_RAMDISK_SIZE    : Storage_Count := 0;

    -- The boot allocator permanently excludes the multiboot image from its
    -- free pool. This recognizes only actual initrd bytes, not page padding.
    -- It confers no mapping authority: callers must first validate a readable
    -- user mapping to these bytes.
    function residentInitrdRange
      (Physical : Integer_Address; Length : Storage_Count) return Boolean;

    ---------------------------------------------------------------------------
    -- setup
    -- See if GRUB has loaded any modules, and if so, print any information
    -- about them.
    ---------------------------------------------------------------------------
    procedure setup;

end Modules;
