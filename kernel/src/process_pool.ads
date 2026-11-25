-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The process pool (KERN-014 slice 4, docs/kernel-memory-map.md): every
-- frame a process owns comes from 2 MiB regions the kernel's linear map no
-- longer covers. A kernel read bug, a speculative gadget or a Meltdown-class
-- read through the linear map finds no process data there.
--
-- @description
-- Growth: wholly free 2 MiB blocks are taken from the kernel's lists
-- (zeroed while still mapped), their linear-map entries cleared in one
-- batch, ONE TLB round for the batch, and only then are their frames handed
-- out (Kernel_Map_Ownership: Begin_Unmap, Complete_Round, Finish_Unmap).
-- The pool grows geometrically (an eighth of its size, at least a region)
-- and has no fixed split with the kernel.
-- Shrink: when a kernel allocation finds no block, wholly free regions go
-- back: their dirty frames are zeroed through the kernel window first, then
-- the entry is restored (no round: a not-present entry is never cached).
-- Allocation: only clean frames reach a process: zeroed, with a TLB round
-- since (a region joins zeroed, before its round; a freed frame is dirty
-- until laundered: zeroed through the window in a batch, then one round),
-- so no CPU can still translate it through a window slot.
--
-- Not yet: DMA blocks larger than 2 MiB still come from the kernel's lists
-- (Syscall.IPC); a pool frame has no kernel address (Process_Frames).
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Virtmem;

package Process_Pool is
   -- After the buddy allocator, before any process frame is allocated.
   procedure Setup;

   -- Before taking any lock: make Frames clean frames available (launder
   -- dirty ones, else grow by the regions still missing), so the
   -- allocations that follow under spinlocks find them and do no zeroing
   -- or TLB rounds of their own. Best effort: another CPU may take them
   -- first, and Alloc_Frame still launders or grows (one region) then.
   procedure Prepare (Frames : Natural);

   -- One zeroed frame for a process; 0 when memory is exhausted.
   procedure Alloc_Frame (Frame : out Virtmem.PhysAddress);

   -- 2**Order zeroed, contiguous frames aligned to their size, below
   -- Ceiling (0: anywhere); 0 when none. Order at most 9 (one region).
   procedure Alloc_Run
     (Order : Natural; Ceiling : Unsigned_64; Base : out Virtmem.PhysAddress)
     with Pre => Order <= 9;

   -- Regions in the pool now (diagnostics).
   function Regions return Natural;
end Process_Pool;
