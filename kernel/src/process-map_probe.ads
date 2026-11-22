-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel-map probe (KERN-014, docs/kernel-memory-map.md): test kernels
-- only (Build.Kernel_Map_Probe, KERN014_PROBE=1), for headless
-- --test kernel-map-probe.
--
-- @description
-- Evidence for what the kernel's own map reaches, reported on the console:
--  * Report_Process_Page: on a process's request (its console write
--    "KERN014-PROBE-READ " and 16 hex digits), the word at that address of
--    its own page, read as a kernel bug would, through the kernel's linear
--    map. "readable" means the kernel map exposes the process's page;
--    "unmapped" means the read faulted (recovered).
--  * Check_Free_Links: at boot, whether a freed frame holds the buddy
--    allocator's free-list links (they move out of the frames, KERN-014
--    slice 1, so process pool frames can leave the kernel map).
--  * "KERN014-PROBE-WRITE " and an address: the kernel stores a word there
--    through Process.User_Memory.Copy_To_User (the caller's own mapping,
--    KERN-014 slice 2) and reports whether the store went through;
--    "KERN014-PROBE-FAULTS": how many kernel accesses to user memory
--    faulted and were refused (recovered) since boot.
-- Nothing here is reachable in a normal kernel.
-------------------------------------------------------------------------------
package Process.Map_Probe is
   Request_Prefix : constant String := "KERN014-PROBE-READ ";
   Write_Prefix   : constant String := "KERN014-PROBE-WRITE ";
   Faults_Request : constant String := "KERN014-PROBE-FAULTS";
   -- What a probe store writes.
   Written_Word   : constant Unsigned_64 := 16#5752_4954_5445_4E21#;
   Hex_Digits     : constant := 16;

   -- Text is the caller's console write (a chunk): a request when it is the
   -- prefix and Hex_Digits hex digits. Handled reports True.
   procedure Handle_Request
     (Caller : ProcessID; Text : String; Handled : out Boolean);

   procedure Report_Process_Page (Caller : ProcessID; Address : Unsigned_64);

   -- Boot, after the buddy allocator is set up, before any process.
   procedure Check_Free_Links;
end Process.Map_Probe;
