-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- ELF Module Loading. CuBit kernel modules are ELF binaries loaded by GRUB and
-- available here for our use. We use these to bootstrap required user-mode OS
-- features before the filesystem is available.
-------------------------------------------------------------------------------
with Multiboot;

package Modules with
    SPARK_Mode => On
is
    ModuleException : exception;

    ---------------------------------------------------------------------------
    -- setup
    -- See if GRUB has loaded any modules, and if so, print any information
    -- about them.
    ---------------------------------------------------------------------------
    procedure setup (mbinfo : in Multiboot.MultibootInfo);

end Modules;