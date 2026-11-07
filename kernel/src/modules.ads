-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- Multiboot Module Loading. CuBit kernel modules are ELF binaries loaded by
-- GRUB and available here for our use. We use these to bootstrap required
-- user-mode OS features before the filesystem is available.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Process;


-- Physical memory, page tables and locks: kernel glue around the proved
-- Boot_Module_Lifetime.
package Modules with
    SPARK_Mode => Off
is
    ModuleException : exception;

    ---------------------------------------------------------------------------
    -- Boot module lifetime (Boot_Module_Lifetime). Every loader module is
    -- reserved exactly from boot. At setup the kernel releases each module
    -- nobody consumes; the boot image (init.img) is Shared: the device
    -- manager gets the root mapping of every page, MAP_INTO hands further
    -- read-only mappings out by offset, and each page goes back to the buddy
    -- allocator when its last mapping is released (Release_View) or its
    -- holder exits (Forget_Process). A released page is never mapped again.
    ---------------------------------------------------------------------------

    -- The physical pages of the boot image from byte Offset for Bytes, for
    -- MAP_INTO's boot-image kind: callers name an offset into the image,
    -- never a physical address. Valid only for a page-aligned Offset and a
    -- nonzero Bytes inside the image's pages. A check only: mapping them
    -- takes Acquire_Initrd_Pages, which also refuses released pages.
    procedure Initrd_Pages (Offset, Bytes : Unsigned_64;
                            Physical      : out Unsigned_64;
                            Valid         : out Boolean);

    -- MAP_INTO's boot-image kind: one more mapping of each page from byte
    -- Offset for Bytes (a page-aligned offset and a nonzero span inside the
    -- image's pages, none released). Physical is the first page. All or
    -- nothing; the caller installs the mappings, then reports them with
    -- Note_Mapped, or gives back those it did not install.
    procedure Acquire_Initrd_Pages (Offset, Bytes : Unsigned_64;
                                    Physical      : out Unsigned_64;
                                    Granted       : out Boolean);
    procedure Return_Initrd_Pages (Physical, Bytes : Unsigned_64);
    procedure Note_Mapped (Holder : Process.ProcessID; Pages : Unsigned_64);

    -- Copy Length bytes of the boot image at Physical (a frame found in the
    -- caller's own read-only mapping) while those pages are still the
    -- image's. Under the image lock, so no concurrent release can give them
    -- to the buddy allocator mid-copy. False (nothing copied) for anything
    -- that is not live boot-image memory.
    procedure Copy_Resident (Physical    : Integer_Address;
                             Length      : Storage_Count;
                             Destination : System.Address;
                             Copied      : out Boolean);

    -- The holder's own boot-image mappings at Virtual, Bytes long (whole
    -- pages, at most Maximum_Release_Pages): removed, and every page whose
    -- last mapping that was goes to the buddy allocator. All or nothing:
    -- each page must be a present, read-only boot-image mapping.
    Maximum_Release_Pages : constant := 1024;
    procedure Release_View (Holder   : Process.ProcessID;
                            Virtual  : Unsigned_64;
                            Bytes    : Unsigned_64;
                            Released : out Boolean);

    -- The holder is gone (no CPU runs it; its page tables are about to be
    -- deleted): its boot-image mappings no longer count.
    procedure Forget_Process (Holder : Process.ProcessID);

    -- Diagnostics and tests: boot-module bytes returned so far.
    function Released_Bytes return Unsigned_64;

    ---------------------------------------------------------------------------
    -- setup
    -- See if GRUB has loaded any modules, and if so, print any information
    -- about them.
    ---------------------------------------------------------------------------
    procedure setup;

end Modules;
