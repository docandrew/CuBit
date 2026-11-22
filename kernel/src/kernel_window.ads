-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The per-CPU window (KERN-014 slice 3, docs/kernel-memory-map.md): the
-- only way the kernel reaches a frame a process owns that is not the
-- current process's mapped memory (a child's image at spawn, launch
-- arguments, boot loads, zeroing a frame before a process gets it).
--
-- @description
-- Each CPU has 512 slots of 4 KiB in its own 2 MiB of the window area, the
-- kernel half's P4 entry Window_P4_Index (shared by every address space, as
-- the rest of the kernel half). A section maps one frame at this CPU's slot
-- with interrupts disabled from Open to Close: it cannot migrate, nor change
-- CR3, so the INVLPG at Close drops every translation this CPU made of the
-- slot (the entries are not global: TLB_Context keeps CR4.PGE clear, and a
-- tagged CPU caches them under the one tag current throughout). Another CPU
-- could hold a speculative translation of this CPU's slot until its next
-- flush; that exposes only the frame's current owner's data (a stated
-- residual, docs/kernel-memory-map.md "The window").
--
-- Sections do not nest. Window_Model (tests/kernel-map-ownership) states
-- the ledger these operations keep.
-------------------------------------------------------------------------------
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Process_Frames;

package Kernel_Window is
   Window_P4_Index : constant := 510;
   Slot_Bytes      : constant := 4096;

   -- Boot, after Mem_mgr.setup and the buddy allocator, before any process:
   -- the window's P4 entry must exist before the kernel half is copied.
   procedure Setup;

   -- Map Frame at this CPU's slot; Where is its address until Close.
   procedure Open (Frame : Process_Frames.Process_Frame; Where : out System.Address)
     with Pre => Process_Frames.Is_Frame (Frame);
   procedure Close;

   -- One-section conveniences.
   procedure Write
     (Frame : Process_Frames.Process_Frame; Offset : Storage_Offset;
      Source : System.Address; Bytes : Storage_Count)
     with Pre => Process_Frames.Is_Frame (Frame) and then Offset >= 0
                 and then Bytes <= Slot_Bytes and then Offset <= Slot_Bytes - Bytes;
   procedure Zero (Frame : Process_Frames.Process_Frame)
     with Pre => Process_Frames.Is_Frame (Frame);
end Kernel_Window;
