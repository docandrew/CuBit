-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- FIRMWARE_SNAPSHOT's operations over the sealed boot snapshot
-- (Firmware_Snapshot_ABI), after the authority check.
--
-- @description
-- Generic over where the snapshot and the caller's memory are, so the same
-- body serves the kernel (kernel/src/firmware_snapshot_export.adb: acpi's
-- Boot_Snapshot and Process.User_Memory.Copy_To_User) and the hosted tests
-- (tests/firmware-snapshot: a snapshot built in the test, a user memory
-- that can refuse after a prefix).
--
-- Serve decodes the request words at full width (Firmware_Snapshot_ABI.
-- Decode) before narrowing anything, checks the snapshot (ready, the table
-- known, the offset within it) before copying, then copies Capacity bytes
-- through a bounded, initialized scratch piece filled by Copy_Part (the
-- table's bytes, then zeros), Piece_Bytes at a time, each piece written
-- with Write_User. It writes the reply (Write_Reply) only for Ok. It never
-- reads the caller's memory and never touches a user address itself.
-------------------------------------------------------------------------------
pragma Ada_2022;

with Interfaces; use Interfaces;
with Firmware_Tables;
with Firmware_Tables.Snapshots;
with Firmware_Tables.Snapshots.Transfers;
with Firmware_Snapshot_ABI;

generic
    -- The snapshot's phase and table count (0 unless ready).
    with function Snapshot_Ready return Boolean;
    with function Table_Count return Natural;
    -- Firmware_Tables.Snapshots.Describe / Transfers.Copy_Part over it.
    with function Describe (Index : Natural)
      return Firmware_Tables.Snapshots.Metadata_Result;
    with procedure Copy_Part
      (Index, Offset : Natural; Destination : out Firmware_Tables.Bytes;
       Result : out Firmware_Tables.Snapshots.Transfers.Transfer_Result);
    -- Writes Source to the caller's memory at Destination (all of it, or
    -- reports failure, possibly after a prefix).
    with procedure Write_User
      (Destination : Unsigned_64; Source : Firmware_Tables.Bytes; Success : out Boolean);
    -- Writes the reply record to the caller's memory.
    with procedure Write_Reply
      (Item : Firmware_Snapshot_ABI.Reply; Success : out Boolean);
package Firmware_Snapshot_Service with SPARK_Mode is

    package ABI renames Firmware_Snapshot_ABI;
    package Transfers renames Firmware_Tables.Snapshots.Transfers;
    use type ABI.Outcome, ABI.Operation;

    -- The scratch piece: a stack bound for the kernel's 4 KiB stacks, not
    -- a limit of the interface (Chunk_Max is).
    Piece_Bytes : constant := 512;

    -- What a helper refusal means to the caller.
    function Transfer_Outcome (Status : Transfers.Transfer_Status) return ABI.Outcome is
        (case Status is
            when Transfers.Source_Not_Ready    => ABI.Not_Ready,
            when Transfers.No_Table            => ABI.Unknown_Table,
            when Transfers.Offset_Out_Of_Range => ABI.Beyond_End,
            when Transfers.Buffer_Empty        => ABI.Empty_Before_End,
            when Transfers.Transferred         => ABI.Ok);

    function Metadata_Outcome (Status : Firmware_Tables.Snapshots.Metadata_Status)
      return ABI.Outcome is
        (case Status is
            when Firmware_Tables.Snapshots.Snapshot_Not_Ready => ABI.Not_Ready,
            when Firmware_Tables.Snapshots.Unknown_Table      => ABI.Unknown_Table,
            when Firmware_Tables.Snapshots.Metadata_Available => ABI.Ok);

    -- One request, after its authority was checked against the caller.
    -- Malformed exactly when the words are not well formed; otherwise the
    -- outcome of the operation.
    procedure Serve (Words : ABI.Request_Words; Result : out ABI.Outcome)
    with Post => (if not ABI.Well_Formed (Words) then Result = ABI.Malformed
                  else Result /= ABI.Malformed and then Result /= ABI.Not_Authorized);

end Firmware_Snapshot_Service;
