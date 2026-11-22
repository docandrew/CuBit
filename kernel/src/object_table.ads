-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Dynamic, two-level table of kernel records (docs/threads.md)
--
-- The process table and, later, the thread table. Records live in pages
-- allocated on demand; a directory maps a page index to its page, so
-- Lookup is two loads and takes no lock. Records never move while their page
-- is live.
--
-- ID and generation bookkeeping is Id_Ledger (SPARK, proved). A page whose
-- last record is released is unlinked from the directory and freed only
-- after a quiescent-state grace period (Quiescent_Reclamation, proved), so a
-- lock-free reader on another CPU never touches freed memory. Rule for
-- callers: never hold a record reference across a quiescent point.
--
-- Allocate, Allocate_Specific, Release and Reclaim serialize on the table
-- lock. Page memory comes from the generic formal Alloc_Page/Free_Page
-- (buddy allocator in the kernel, ordinary memory in hosted tests).
-------------------------------------------------------------------------------
with Interfaces;
with System;
with Id_Ledger;
with Quiescent_Reclamation;

generic
    type Element is limited private;
    Max_Id           : Positive;
    Entries_Per_Page : Positive;
    Reserved_Last    : Natural;
    Generation_Limit : Interfaces.Unsigned_32 := Interfaces.Unsigned_32'Last;
    -- Reset a record to its initial state when its ID is allocated.
    with procedure Reset (E : in out Element);
    -- Page memory: Page_Bytes bytes, suitably aligned for Element; returns
    -- System.Null_Address on exhaustion.
    with procedure Alloc_Page (Page_Bytes : Natural; Addr : out System.Address);
    with procedure Free_Page (Page_Bytes : Natural; Addr : System.Address);
    -- The table lock (a leaf lock in the kernel).
    with procedure Lock;
    with procedure Unlock;
package Object_Table with SPARK_Mode => Off is

    package Ledger is new Id_Ledger
      (Max_Id, Entries_Per_Page, Reserved_Last, Generation_Limit);
    subtype Id is Ledger.Id;
    subtype Valid_Id is Ledger.Valid_Id;
    subtype Generation is Ledger.Generation;

    type Element_Ref (E : not null access Element) is null record
      with Implicit_Dereference => E;

    -- Once, before any other operation (and before other CPUs run).
    procedure Initialize;

    -- True if I is allocated (its page is live). Lock-free snapshot.
    function Present (I : Id) return Boolean;

    -- Reference to a live record. I must be Present; otherwise the reference
    -- is to the table's Absent record, whose contents are the Reset state
    -- (so a reader of an unused ID sees an invalid, empty record). Writes
    -- to it are a caller bug; Absent_Is_Pristine detects them in tests.
    function Lookup (I : Id) return Element_Ref;

    -- Generation of I (live or released), for identity checks.
    function Generation_Of (I : Valid_Id) return Generation;

    -- Lowest free ID above the reserved range, reset and live; 0 if none or
    -- if page memory is exhausted.
    procedure Allocate (I : out Id);

    -- A specific ID (reserved or requested), if free and not retired.
    procedure Allocate_Specific (I : Valid_Id; Success : out Boolean);

    -- Advance a still-allocated ID's generation (teardown while the ID must
    -- stay reserved). Saturated: the generation is at the limit and did not
    -- change; the ID will be retired when released.
    procedure Invalidate (I : Valid_Id; Saturated : out Boolean);

    -- Release I. Its generation advances (unless Advance is False, after
    -- Invalidate); its page is unlinked and retired if this was the page's
    -- last record. An ID at the generation limit is retired forever.
    procedure Release (I : Valid_Id; Advance : Boolean := True);

    -- Quiescent point on CPU: that CPU holds no record reference. Called
    -- from the scheduler loop.
    procedure Quiescent (CPU : Quiescent_Reclamation.CPU_Index);

    -- Free retired pages whose grace period has elapsed. Online lists CPUs
    -- that may be running kernel code.
    procedure Reclaim (Online : Quiescent_Reclamation.CPU_Set);

    -- Iteration over live IDs, lowest first. Present must be rechecked by
    -- callers that act on the record outside the table lock.
    function High_Water return Id;

    -- Diagnostics for tests.
    function Live_Pages return Natural;
    function Retired_Pages return Natural;
    function Absent_Is_Pristine return Boolean;

end Object_Table;
