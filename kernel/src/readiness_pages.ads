-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel's side of the readiness page (KERN-013,
-- shared/publications/readiness_publication.ads).
--
-- @description
-- One frame per process, allocated and mapped read-only, non-executable at
-- Readiness_Publication.Page_Address when the process is created, freed when
-- it is reclaimed. The frame is the kernel's: not in the process's frame
-- list, so no grant, owned-memory call or exit path ever hands it out or
-- writes it from user space. Only Note and Note_Wake write it, each as one
-- aligned 64-bit store, under the receiving mailbox's lock.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Virtmem;
with Readiness_Publication;

package Readiness_Pages with
    SPARK_Mode => Off -- frame allocation and the kernel's page writes
is
    package RP renames Readiness_Publication;

    No_Frame : constant Virtmem.PhysAddress := 0;

    -- A zeroed page holding Layout_Version, mapped into Root. Frame is
    -- No_Frame when there was no frame or the mapping failed (nothing kept).
    procedure Create (Root : in out Virtmem.P4; Frame : out Virtmem.PhysAddress);

    -- After the address space is gone: the frame back to the allocator.
    procedure Release (Frame : in out Virtmem.PhysAddress);

    -- One more entry of Class queued (No_Frame: nothing).
    procedure Note (Frame : Virtmem.PhysAddress; Class : RP.Queue_Class);

    -- One more wake of a waiting thread.
    procedure Note_Wake (Frame : Virtmem.PhysAddress);
end Readiness_Pages;
