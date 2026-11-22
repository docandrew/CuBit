pragma Ada_2022;
with ACPI_Passive_Query.Pages;
with ACPI_Passive_Query.Resource_Records;
-- An envelope is framing only. Consumers must validate each payload item with
-- its typed reader before publishing observations or selecting a driver.
package ACPI_Passive_Query.Replies with SPARK_Mode, Pure is
   subtype U32 is Interfaces.Unsigned_32;
   package Bytes_IO renames ACPI_Passive_Query.Pages;
   package Resources_IO renames ACPI_Passive_Query.Resource_Records;
   use type Bytes_IO.Byte;
   Header_Word_Count : constant := 32;
   Header_Byte_Count : constant := Header_Word_Count * Bytes_IO.Bytes_Per_Word;
   Node_Byte_Count : constant := Bytes_IO.Bytes_Per_Word;
   Resource_Byte_Count : constant := Resources_IO.Record_Word_Count * Bytes_IO.Bytes_Per_Word;
   subtype Header_Words is Word_Array (0 .. Header_Word_Count - 1);
   Header_Magic : constant Word := 16#3250_4552_4950_4341#; -- ACPIREP2
   Header_Version : constant Word := 2;
   type Header is record
      Status : Query_Status := Refused;
      -- Current result scope and optional NEXT cursor. On errors the original
      -- valid request may be echoed; an initial Find_By_ID may still be zero.
      Context : Request;
      Payload_Bytes, Record_Count : U32 := 0;
      -- Complete query/cursor identifier copies AFTER the typed payload.
      Filter_Bytes : U32 := 0;
      CIDs_Truncated : Boolean := False;
      -- Full required frame extent, meaningful only for Insufficient_Space.
      Required_Bytes : Word := 0;
   end record;
   function Extent (Item : Header) return Word is
     (Header_Byte_Count + Word (Item.Payload_Bytes) + Word (Item.Filter_Bytes));
   function Valid (Item : Header; Limit : Node_Limit) return Boolean;
   -- Correlate the actual query filter; the returned next cursor is allowed to
   -- advance. Only an initial wildcard Find_By_ID may adopt a new scope pair.
   function Matches_Request (Item : Header; Expected : Request; Limit : Node_Limit)
     return Boolean with Global => null;
   -- Complete-value correlation for copied variable-length query identifiers.
   function Matches_Request
     (Data, Request_Data : Bytes_IO.Byte_Array; Limit : Node_Limit) return Boolean
     with Global => null;
   function Encode_Header (Item : Header; Limit : Node_Limit) return Header_Words
     with Global => null, Pre => Valid (Item, Limit);
   type Header_Result (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Header;
         when False => null;
      end case;
   end record;
   function Decode_Header (Data : Header_Words; Limit : Node_Limit) return Header_Result
     with Global => null,
       Post => (if Decode_Header'Result.Accepted then
         Valid (Decode_Header'Result.Value, Limit) and then
         Encode_Header (Decode_Header'Result.Value, Limit) = Data);
   -- Zero the entire output on refusal; successful output zeroes all padding.
   -- Payload must already have been built/validated in private storage.
   procedure Encode_Envelope
     (Item : Header; Payload : Bytes_IO.Byte_Array; Data : out Bytes_IO.Byte_Array;
      Limit : Node_Limit; Written : out Word; Accepted : out Boolean;
      Filter : Bytes_IO.Byte_Array := [])
     with Global => null,
       Post => (if Accepted then Valid (Item, Limit) and then Written = Extent (Item)
         and then Written <= Word (Data'Length)
         else Written = 0 and then (for all B of Data => B = 0));
   -- Input must be a stable private copy. Checks framing/extent/padding, not
   -- Describe fields or the semantic validity of all payload records.
   function Read_Envelope (Data : Bytes_IO.Byte_Array; Limit : Node_Limit) return Header_Result
     with Global => null,
       Post => (if Read_Envelope'Result.Accepted then
         Valid (Read_Envelope'Result.Value, Limit) and then
         Extent (Read_Envelope'Result.Value) <= Word (Data'Length));
   type Node_Result (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Node_Ref;
         when False => null;
      end case;
   end record;
   -- These readers reread the header and check extent plus the selected item;
   -- no caller-fabricated header/span is trusted. Check Read_Envelope once
   -- before a batch to validate padding. All reads require a stable private
   -- copy. Position is zero-based. They do not establish node liveness.
   function Node_At (Data : Bytes_IO.Byte_Array; Position : U32; Limit : Node_Limit)
     return Node_Result with Global => null,
       Post => (if Node_At'Result.Accepted then
         Node_At'Result.Value.Instance /= 0 and then Node_At'Result.Value.Revision /= 0
         and then Node_At'Result.Value.Index in 1 .. Limit);
   function Resource_At (Data : Bytes_IO.Byte_Array; Position : U32; Limit : Node_Limit)
     return Resources_IO.Resource_Result with Global => null;
end ACPI_Passive_Query.Replies;
