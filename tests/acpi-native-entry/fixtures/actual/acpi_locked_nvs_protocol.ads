pragma Ada_2022;
with Interfaces;
with ACPI_FADT.Transactions;
with ACPI_Global_Lock_Protocol;
with ACPI_NVS_Protocol;
package ACPI_Locked_NVS_Protocol with Pure, SPARK_Mode is
   use Interfaces;
   package T renames ACPI_FADT.Transactions;
   subtype Packet is ACPI_Global_Lock_Protocol.Packet;
   subtype Lease_Ticket is ACPI_Global_Lock_Protocol.Ticket;
   type Record_ID is new Unsigned_64;
   type Descriptor_ID is new Unsigned_64;
   type Read_Sequence is new Unsigned_64;
   No_Record : constant Record_ID := 0;
   No_Descriptor : constant Descriptor_ID := 0;
   No_Read : constant Read_Sequence := 0;
   type Geometry is record
      Offset : Unsigned_64 := 0;
      Width : T.Access_Width := T.Byte_Access;
   end record;
   Empty_Geometry : constant Geometry := (0, T.Byte_Access);
   Width_Count : constant := T.Access_Width'Pos (T.Access_Width'Last) + 1;
   Maximum_Offset : constant Unsigned_64 := Unsigned_64'Last / Width_Count;
   function Valid (Item : Geometry) return Boolean;
   function Pack (Item : Geometry) return Unsigned_64 with Pre => Valid (Item);
   function Unpack (Item : Unsigned_64) return Geometry;
   type Operation is (Select_Datum, Read_Datum, Query_Read);
   -- Version one is fixed by these operation labels. No spare word carries
   -- a version; incompatible future layouts require new labels.
   First_Operation : constant Unsigned_32 := 16#2A0#;
   First_Reply : constant Unsigned_32 := 16#F2A0#;
   pragma Compile_Time_Error
     (Width_Count /= 4 or else First_Operation < 16#2A0# or else
      First_Operation + Operation'Pos (Operation'Last) > 16#2AF# or else
      First_Reply /= First_Operation + 16#F000#,
      "locked NVS protocol requires distinct labels in the reserved subrange");
   -- Cross-package check as well as each package's own subrange check.
   -- The unscoped NVS, Global Lock and scoped-data blocks cannot overlap.
   pragma Compile_Time_Error
     (ACPI_NVS_Protocol.Header_Operation >= ACPI_Global_Lock_Protocol.First_Operation or else
      ACPI_NVS_Protocol.Describe_Operation >= ACPI_Global_Lock_Protocol.First_Operation or else
      ACPI_NVS_Protocol.Read_Operation >= ACPI_Global_Lock_Protocol.First_Operation or else
      ACPI_Global_Lock_Protocol.First_Operation +
        ACPI_Global_Lock_Protocol.Operation'Pos (ACPI_Global_Lock_Protocol.Operation'Last) >= First_Operation or else
      ACPI_NVS_Protocol.Header_Reply >= ACPI_Global_Lock_Protocol.First_Reply or else
      ACPI_NVS_Protocol.Describe_Reply >= ACPI_Global_Lock_Protocol.First_Reply or else
      ACPI_NVS_Protocol.Read_Reply >= ACPI_Global_Lock_Protocol.First_Reply or else
      ACPI_Global_Lock_Protocol.First_Reply +
        ACPI_Global_Lock_Protocol.Operation'Pos (ACPI_Global_Lock_Protocol.Operation'Last) >= First_Reply,
      "ACPI NVS, lock and scoped-data label blocks must be pairwise distinct");
   function Owned_Label (Label : Unsigned_32) return Boolean;
   type Request is record
      Valid : Boolean := False;
      Kind : Operation := Select_Datum;
      Lease : Lease_Ticket := ACPI_Global_Lock_Protocol.No_Ticket;
      ID : Record_ID := No_Record;
      Datum : Geometry;
      Descriptor : Descriptor_ID := No_Descriptor;
      Sequence : Read_Sequence := No_Read;
   end record;
   function Select_Request (Lease : Lease_Ticket; ID : Record_ID; Datum : Geometry) return Packet
     with Pre => ACPI_Global_Lock_Protocol.Valid (Lease) and then ID /= No_Record and then Valid (Datum);
   function Read_Request (Kind : Operation; Lease : Lease_Ticket;
     Descriptor : Descriptor_ID; Sequence : Read_Sequence) return Packet
     with Pre => Kind /= Select_Datum and then ACPI_Global_Lock_Protocol.Valid (Lease)
       and then Descriptor /= No_Descriptor and then Sequence /= No_Read;
   function Decode_Request (Item : Packet) return Request;
   type Reply_Kind is (Prepared, Recorded, Cached, No_Result, Denied,
     Unavailable, Stale, Invalid_Request, Exhausted, Reentered, Malformed);
   type Read_Outcome is (No_Data, Done, Access_Refused, Read_Failed, Late_Completed);
   Outcome_Bits : constant := 3;
   Outcome_Base : constant := 2 ** Outcome_Bits;
   pragma Compile_Time_Error
     (Read_Outcome'Pos (Read_Outcome'Last) >= Outcome_Base,
      "locked NVS outcome no longer fits the version-one status layout");
   Maximum_Status : constant Unsigned_64 :=
     Reply_Kind'Pos (Reply_Kind'Last) * Outcome_Base + Read_Outcome'Pos (Read_Outcome'Last);
   type Reply is record
      Valid : Boolean := False;
      Kind : Reply_Kind := Malformed;
      Descriptor : Descriptor_ID := No_Descriptor;
      ID : Record_ID := No_Record;
      Datum : Geometry := Empty_Geometry;
      Sequence : Read_Sequence := No_Read;
      Outcome : Read_Outcome := No_Data;
      Value : Unsigned_64 := 0;
   end record;
   function Valid_Result (Kind : Operation; Item : Reply) return Boolean
     with Post => (if Valid_Result'Result and then Item.Kind = Prepared
       then Item.Descriptor /= No_Descriptor and then Valid (Item.Datum));
   function Encode_Reply (Kind : Operation; Item : Reply) return Packet
     with Pre => Valid_Result (Kind, Item);
   function Decode_Reply (Kind : Operation; Item : Packet;
     Width : T.Access_Width := T.Byte_Access) return Reply
     with Post => (if Decode_Reply'Result.Valid then Valid_Result (Kind, Decode_Reply'Result)
       and then (if Decode_Reply'Result.Kind = Prepared
         then Decode_Reply'Result.Descriptor /= No_Descriptor));
   -- Selection replies echo [status, descriptor, record, packed geometry].
   -- Read replies echo [packed kind/outcome, descriptor, sequence, value].
   -- Callers must match descriptor and sequence to their request; descriptor
   -- IDs never repeat in this backend incarnation, including across leases.
   function Matches_Selection (Item : Reply; ID : Record_ID; Datum : Geometry) return Boolean;
   function Matches_Read (Item : Reply; Descriptor : Descriptor_ID; Sequence : Read_Sequence) return Boolean;
   -- Recorded/Cached report the original attempt. Cached data is historical,
   -- not a live lease check. No_Result/Stale never establish nonexecution or
   -- authorize replay. A fresh effect requires a new sequence and lease check.
end ACPI_Locked_NVS_Protocol;
