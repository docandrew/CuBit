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

with Multiboot;

package Modules with
    SPARK_Mode => On
is
    ModuleException : exception;

    MAGIC_RAMDISK_ADDRESS : System.Address;

    ---------------------------------------------------------------------------
    -- setup
    -- See if GRUB has loaded any modules, and if so, print any information
    -- about them.
    ---------------------------------------------------------------------------
    procedure setup (mbinfo : in Multiboot.MultibootInfo);

end Modules;
