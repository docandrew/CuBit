pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
with ACPI_FADT.Transactions;
package ACPI_NVS_Protocol with Pure, SPARK_Mode is
   use Interfaces;
   package Wire renames ACPI_Region_Protocol;
   package T renames ACPI_FADT.Transactions;
   subtype Packet is Wire.Packet;
   type Operation is (Header, Describe, Read);
   Header_Operation : constant Unsigned_32 := 16#280#;
   Describe_Operation : constant Unsigned_32 := 16#281#;
   Read_Operation : constant Unsigned_32 := 16#282#;
   Header_Reply : constant Unsigned_32 := 16#F280#;
   Describe_Reply : constant Unsigned_32 := 16#F281#;
   Read_Reply : constant Unsigned_32 := 16#F282#;
   pragma Compile_Time_Error
     (Header_Operation < 16#0280# or else Header_Operation > 16#028F# or else
      Describe_Operation < 16#0280# or else Describe_Operation > 16#028F# or else
      Read_Operation < 16#0280# or else Read_Operation > 16#028F# or else
      Header_Operation = Describe_Operation or else Header_Operation = Read_Operation or else
      Describe_Operation = Read_Operation or else
      Header_Reply /= Header_Operation + 16#F000# or else
      Describe_Reply /= Describe_Operation + 16#F000# or else
      Read_Reply /= Read_Operation + 16#F000#,
      "NVS labels must be distinct within their reserved ACPI subrange");
   Version : constant Unsigned_64 := 1;
   Maximum_Records : constant Unsigned_64 := 16;
   type Outcome is (Denied, Malformed, Stale, Unavailable, Ready, Done, Fault);
   type Request is record
      Valid : Boolean := False;
      Kind : Operation := Header;
      Epoch, ID, Position : Unsigned_64 := 0;
      Width : T.Access_Width := T.Byte_Access;
   end record;
   function Decode_Request (Item : Packet) return Request;
   function Header_Request return Packet;
   function Describe_Request (Epoch, Ordinal : Unsigned_64) return Packet
     with Pre => Epoch /= 0 and then Ordinal /= 0;
   function Read_Request
     (Epoch, ID, Offset : Unsigned_64; Width : T.Access_Width) return Packet
     with Pre => Epoch /= 0 and then ID /= 0 and then Offset mod Unsigned_64 (T.Octets (Width)) = 0;
   function Error_Reply (Kind : Operation; Status : Outcome) return Packet
     with Pre => Status not in Ready | Done;
   function Header_Result (Epoch, Count : Unsigned_64) return Packet
     with Pre => Epoch /= 0 and then Count <= Maximum_Records;
   function Describe_Result (ID, Base, Length : Unsigned_64) return Packet
     with Pre => ID /= 0 and then Length /= 0 and then Length - 1 <= Unsigned_64'Last - Base;
   function Read_Result (Value : Unsigned_64) return Packet;
   type Reply is record
      Valid : Boolean := False;
      Status : Outcome := Malformed;
      Epoch, Count, ID, Base, Length, Value : Unsigned_64 := 0;
   end record;
   function Decode_Reply
     (Item : Packet; Kind : Operation; Width : T.Access_Width := T.Byte_Access) return Reply;
end ACPI_NVS_Protocol;
