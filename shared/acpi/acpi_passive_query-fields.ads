pragma Ada_2022;
with ACPI_Passive_Query.Pages;
-- Copied static observations. No method execution or namespace mutation.
package ACPI_Passive_Query.Fields with SPARK_Mode, Pure is
   subtype U32 is Interfaces.Unsigned_32;
   package Byte_IO renames ACPI_Passive_Query.Pages;
   type Field_Name is (Device_Path, Hardware_ID, Compatible_ID, Unique_ID, Address, Device_Status);
   type Value_Kind is (No_Value, Integer_Value, Text_Chunk, Identifier);
   type Byte_Span is record
      Offset, Length : U32 := 0;
   end record;
   type Field_Record is record
      Name : Field_Name := Device_Path;
      Status : Field_Status := Absent;
      Kind : Value_Kind := No_Value;
      Number : Word := 0;
      Text : Byte_Span;
      -- Offset/total in the complete path or UID, independent of page size.
      Logical_Offset, Total_Length : Word := 0;
      Ordinal : U32 := 0; -- Zero-based static CID index; zero for other fields.
      Defaulted : Boolean := False;
      ID : Device_ID := No_ID;
   end record;
   function Valid (Item : Field_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Boolean with Global => null;
   function Has_More (Item : Field_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Boolean with Global => null, Pre => Valid (Item, Limit, Payload_Bytes);
   -- Decode an AML compressed EISA integer, refusing high bits/reserved vendor
   -- letters instead of narrowing or turning a malformed ID into a name.
   function From_EISA (Encoded : Word) return ID_Result with Global => null;
   Record_Word_Count : constant := 16;
   Record_Byte_Count : constant := Record_Word_Count * Byte_IO.Bytes_Per_Word;
   subtype Record_Words is Word_Array (0 .. Record_Word_Count - 1);
   Record_Magic : constant Word := 16#3143_5344_4950_4341#; -- ACPIDSC1
   Record_Version : constant Word := 1;
   function Encode (Item : Field_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Record_Words with Global => null, Pre => Valid (Item, Limit, Payload_Bytes);
   type Field_Result (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Field_Record;
         when False => null;
      end case;
   end record;
   function Decode (Data : Record_Words; Limit : Node_Limit; Payload_Bytes : U32)
     return Field_Result with Global => null,
       Post => (if Decode'Result.Accepted then
         Valid (Decode'Result.Value, Limit, Payload_Bytes) and then
         Encode (Decode'Result.Value, Limit, Payload_Bytes) = Data);
   -- Payload is the stable private payload slice of an accepted Describe reply
   -- (excluding envelope/padding). Count comes from its checked header. This
   -- reader independently bounds count/position and refuses text inside record
   -- storage. Full page/field ordering and cursor semantics are handler work.
   function Field_At (Payload : Byte_IO.Byte_Array; Count, Position : U32; Limit : Node_Limit)
     return Field_Result with Global => null;
end ACPI_Passive_Query.Fields;
