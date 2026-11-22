pragma Ada_2022;
with Firmware_Tables;
with ACPI_Host_Bridge_Wire;
with CuBit.Host_Bridge_Windows;
-- Encode private copied data; decode only a private immutable copy of peer
-- bytes. This framing validates representation, not hardware truth/authority.
-- Input and output arrays must not alias.
package ACPI_Host_Bridge_Snapshot with SPARK_Mode, Pure is
   package W renames ACPI_Host_Bridge_Wire;
   package M renames CuBit.Host_Bridge_Windows;
   subtype Bytes is Firmware_Tables.Bytes;
   subtype Snapshot_State is M.Snapshot_State;
   type Coordinate is record
      Present : Boolean := False;
      Value : W.Word := 0;
   end record;
   type Header is record
      State : Snapshot_State := M.Failed;
      Root_ID, Sequence : W.Word := 0;
      Segment, Base_Bus : Coordinate;
      Required_Descriptors : W.Word := 0;
      Reason : M.Failure_Reason := M.No_Failure;
   end record;
   Word_Bytes : constant Positive := 8;
   Header_Words : constant Positive := 14;
   Header_Bytes : constant Positive := Header_Words * Word_Bytes;
   Row_Words : constant Positive := W.Column'Pos (W.Column'Last) + 1;
   Row_Bytes : constant Positive := Row_Words * Word_Bytes;
   Magic : constant W.Word := 16#3157_4248_4950_4341#; -- ACPIHBW1, little endian
   Version : constant W.Word := 1;
   type Outcome is (Done, Invalid_Header, Invalid_Row, Invalid_Length, Insufficient_Capacity);
   type Report is record
      Status : Outcome := Invalid_Length;
      Required_Rows, Required_Source_Bytes, Required_Packet_Bytes : Natural := 0;
      Used : Natural := 0;
   end record;
   function Valid_Header (Info : Header; Count, Source_Bytes : Natural) return Boolean;
   function Valid_Row (Data : W.Row; Number : Positive; Source_Bytes : Natural) return Boolean;
   -- Packet capacity is caller-selected. Error output is entirely zeroed.
   procedure Encode (Info : Header; Items : W.Rows; Source : Bytes;
      Packet : out Bytes; Result : out Report) with Global => null;
   -- Data is the exact published packet, without shared-region padding. Header
   -- and all arrays remain empty on refusal, including capacity refusal. Size
   -- requirements allow growth without accepting any partial descriptor list.
   procedure Decode (Data : Bytes; Info : out Header; Items : out W.Rows;
      Source : out Bytes; Result : out Report) with Global => null;
end ACPI_Host_Bridge_Snapshot;
