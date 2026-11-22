-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The kernel's per-process capability table (CAP-002,
-- docs/capability-handles.md): the Capability_Tables instance every process
-- record holds, and the lookups the system calls make on it.
--
-- @description
-- Each process's table is charged to its memory account (Charged_Metadata_
-- Pages, the Metadata budget). Every operation on a table holds its owner's
-- mailbox lock (Process.mailtab (pid).lock), as every edit of the old
-- fixed table did.
--
-- Find is the one way a system call turns a handle word into authority: the
-- handle must name a live entry of this table (its generation included), of
-- the kind the call needs, with the rights it needs. There is no search by
-- kind: a call that needs authority is given the handle for it.
--
-- The generic table and Capability_Install.Placement are proved for this
-- geometry in tests/capability-tables and tests/capability-install; this
-- unit only instantiates them and converts results to ABI words.
-------------------------------------------------------------------------------
pragma Ada_2022;

with Interfaces; use Interfaces;
with System;

with Capabilities; use Capabilities;
with Capability_ABI;
with Capability_Tables;
with Capability_Install.Placement;

package Process_Capabilities with SPARK_Mode => Off is

    package ABI renames Capability_ABI;

    procedure Allocate_Page
      (Owner : Unsigned_64; Page : out System.Address; OK : out Boolean);
    procedure Release_Page (Page : System.Address);

    -- One directory page of leaf pointers: 512 leaves of 64 entries.
    Directory_Leaves : constant := 512;

    package Tables is new Capability_Tables
      (Element              => Capability,
       Empty_Element        => NULL_CAPABILITY,
       Generation_Limit     => Unsigned_32'Last,
       Leaves_Per_Directory => Directory_Leaves,
       Allocate_Page        => Allocate_Page,
       Release_Page         => Release_Page);

    package Placement is new Capability_Install.Placement (Tables);

    use type Tables.Handle_Check;

    ---------------------------------------------------------------------------
    -- Handle limits from the ABI (SPAWN's request).
    ---------------------------------------------------------------------------
    -- The limit a request names, or Valid = False.
    procedure Limit_Of
      (Kind, Count : Unsigned_64;
       Limit       : out Tables.Handle_Limit;
       Valid       : out Boolean);

    ---------------------------------------------------------------------------
    -- Lookups
    ---------------------------------------------------------------------------
    function Refusal_Of (Check : Tables.Handle_Check) return ABI.Refusal is
        (case Check is
            when Tables.Current => ABI.Not_Permitted,
            when Tables.Invalid => ABI.Invalid_Handle,
            when Tables.Stale   => ABI.Stale_Handle,
            when Tables.Revoked => ABI.Revoked_Handle);

    type Found (OK : Boolean := False) is record
        case OK is
            when True  =>
                Cap    : Capability;
                Handle : Tables.Handle;
            when False =>
                Why    : ABI.Refusal := ABI.Invalid_Handle;
        end case;
    end record;

    -- The live entry Word names, whatever its kind.
    function Find_Any (T : Tables.Table; Word : Unsigned_64) return Found
    with Inline_Always;   -- the IPC path (resolveEndpointHandle) on every call

    -- The live entry Word names, if it is of Kind and holds Rights.
    function Find
      (T      : Tables.Table;
       Word   : Unsigned_64;
       Kind   : CapabilityType;
       Rights : CapabilityRights) return Found
    with Inline;

    ---------------------------------------------------------------------------
    -- Changes (the owner's mailbox lock held)
    ---------------------------------------------------------------------------
    -- Place Cap where the table chooses. Result: the handle word, or a
    -- Refusal code (Capability_ABI.Handle_Result encoding).
    procedure Insert (T : in out Tables.Table; Cap : Capability; Result : out Unsigned_64);

    -- Remove the entry Word names (the process gave it up). Removed is its
    -- former contents when Result is zero; else Result is a Refusal code.
    procedure Close
      (T : in out Tables.Table; Word : Unsigned_64;
       Removed : out Capability; Result : out Unsigned_64);

    -- Remove the entry at a live slot from outside (revocation).
    procedure Revoke_At (T : in out Tables.Table; Slot : Tables.Slot_Index);

    -- The handle word of a placement, or its refusal code.
    function Result_Word (R : Placement.Place_Result) return Unsigned_64 is
        (case R.Status is
            when Placement.Placed  => Tables.To_Word (R.Handle),
            when Placement.Refused => ABI.Refusal_Word (R.Why));

end Process_Capabilities;
