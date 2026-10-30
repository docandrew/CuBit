------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Read-only access to the kernel's sealed boot snapshot of the firmware
--  (ACPI) tables (FIRMWARE_SNAPSHOT, shared/boot/firmware_snapshot_abi.ads).
--
--  @description
--  A program declares (Request.Firmware_Snapshot "<binding>") in its
--  manifest; its startup entry must approve it, (firmware-snapshot
--  approve-declared), and only acpi.svc is meant to be so approved. The
--  generated Startup.<Binding> returns the Authority; a missing one stops
--  the program at elaboration (CAP-003).
--
--  The snapshot is the kernel's immutable copy of the admitted tables (no
--  FACS, no NVS, no registers, no physical addresses). Tables are numbered
--  1 .. Table_Count. Read a table by describing it (its Extent is its whole
--  size: there is no fixed per-table limit) and copying Chunk_Max bytes at
--  a time from offset 0 until Copied is 0 at the end:
--  * at the end (Offset = Extent): Ok, Copied = 0;
--  * past the end: Beyond_End; an empty Into before the end: Empty_Before_End;
--  * Copy_Failed: discard everything copied into Into;
--  * Not_Authorized: the authority is not a firmware snapshot reader;
--  * Malformed: the kernel refused the request words or answered with a
--    reply that is not well formed (or does not have this call).
--  Into is always fully written: the table's bytes, then zeros; on any
--  outcome but Ok it is all zeros and Copied is 0.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Startup_State;
with Firmware_Snapshot_ABI;

package CuBit.Firmware_Snapshot with SPARK_Mode is

   type Authority is limited private;

   type Table_ID is new Unsigned_32;
   type Byte_Count is new Unsigned_64;

   Chunk_Max : constant := Firmware_Snapshot_ABI.Chunk_Max;

   type Outcome is
     (Ok, Not_Ready, Unknown_Table, Beyond_End, Empty_Before_End, Copy_Failed,
      Not_Authorized, Malformed);

   type Catalog_Info is record
      Version       : Unsigned_32 := 0;
      Table_Count   : Unsigned_32 := 0;
      Ready         : Boolean := False;
      Largest_Table : Byte_Count := 0;
      Total_Bytes   : Byte_Count := 0;
   end record;

   Signature_Length : constant := Firmware_Snapshot_ABI.Signature_Length;
   subtype Signature_Index is Positive range 1 .. Signature_Length;
   type Signature_Bytes is array (Signature_Index) of Unsigned_8;

   type Table_Description is record
      Signature : Signature_Bytes := [others => 0];
      Revision  : Unsigned_8 := 0;
      Extent    : Byte_Count := 0;
   end record;

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   --  The authority the startup table grants under Label (the generated
   --  Startup package calls this; programs use Startup.<Binding>).
   function Named (Label : String) return Authority
   with Global => CuBit.Startup_State.Loaded_Table;

   procedure Info (A : Authority; Result : out Catalog_Info; Status : out Outcome)
   with Post => (if Status = Ok
                 then Result.Version = Firmware_Snapshot_ABI.Interface_Version
                      and then Result.Largest_Table <= Result.Total_Bytes
                      and then (if not Result.Ready then Result.Table_Count = 0)
                 else Result = Catalog_Info'(others => <>));

   procedure Describe
     (A : Authority; Table : Table_ID; Result : out Table_Description; Status : out Outcome)
   with Post => (if Status /= Ok then Result = Table_Description'(others => <>));

   procedure Copy_Part
     (A : Authority; Table : Table_ID; Offset : Byte_Count;
      Into : out Byte_Array; Copied : out Byte_Count; Status : out Outcome)
   with Pre  => Into'Length <= Chunk_Max,
        Post => Copied <= Into'Length
                and then (if Status /= Ok then Copied = 0
                            and then (for all B of Into => B = 0));

private
   type Authority is limited record
      Handle : CuBit.Handles.Firmware_Snapshot := CuBit.Handles.No_Firmware_Snapshot;
   end record;
end CuBit.Firmware_Snapshot;
