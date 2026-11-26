-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel's side of the readiness page and the thread-count pages
-- (KERN-013, shared/publications/readiness_publication.ads).
--
-- @description
-- Three frames per process, allocated and mapped read-only, non-executable
-- at Readiness_Publication.Page_Address and the two pages after it when the
-- process is created, freed when it is reclaimed. The frames are the
-- kernel's: not in the process's frame list, so no grant, owned-memory call
-- or exit path ever hands them out or writes them from user space. Each
-- word is written with one aligned 64-bit store (an atomic add where two
-- writers may meet); a thread's count line only by that thread's system
-- call entry (Process.threadtab.callCounter).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with Virtmem;
with Readiness_Publication;

package Readiness_Pages with
    SPARK_Mode => Off -- frame allocation and the kernel's page writes
is
    package RP renames Readiness_Publication;

    No_Frame : constant Virtmem.PhysAddress := 0;

    type Count_Page is range 0 .. RP.Thread_Count_Pages - 1;
    type Count_Frames is array (Count_Page) of Virtmem.PhysAddress;
    No_Count_Frames : constant Count_Frames := [others => No_Frame];

    -- A zeroed page holding Layout_Version and the process's Identity word,
    -- and the zeroed thread-count pages, mapped into Root. Frame is No_Frame
    -- (and Counts No_Count_Frames) when a frame or a mapping failed (nothing
    -- kept; a mapping already made goes with Root).
    procedure Create (Root     : in out Virtmem.P4;
                      Identity : Unsigned_64;
                      Frame    : out Virtmem.PhysAddress;
                      Counts   : out Count_Frames);

    -- After the address space is gone: the frames back to the allocator.
    procedure Release (Frame  : in out Virtmem.PhysAddress;
                       Counts : in out Count_Frames);

    -- The kernel address of Slot's count line, zeroed for a thread that
    -- takes the slot. Counts must be Create's (not No_Count_Frames).
    function Line (Counts : Count_Frames; Slot : RP.Thread_Slot)
      return System.Address;
    procedure Clear_Line (Counts : Count_Frames; Slot : RP.Thread_Slot);

    -- The process's own frames, in bytes (No_Frame: nothing).
    procedure Set_Owned (Frame : Virtmem.PhysAddress; Bytes : Unsigned_64);
    -- KERN-006: the bytes of shared image frames the process maps.
    procedure Set_Shared (Frame : Virtmem.PhysAddress; Bytes : Unsigned_64);

    -- One more event its mailbox refused (No_Frame: nothing).
    procedure Note_Drop (Frame : Virtmem.PhysAddress);

    -- One more entry of Class queued (No_Frame: nothing).
    procedure Note (Frame : Virtmem.PhysAddress; Class : RP.Queue_Class);

    -- One more wake of a waiting thread.
    procedure Note_Wake (Frame : Virtmem.PhysAddress);
end Readiness_Pages;
