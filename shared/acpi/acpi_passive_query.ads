pragma Ada_2022;
with Interfaces;
-- Pure wire types only. A decoded reference still requires a live-node check
-- against the service's current instance and Requests.Revision. No address,
-- process identity or resource fact in this protocol conveys authority.
package ACPI_Passive_Query with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   use type Interfaces.Unsigned_32;
   type Instance_ID is new Word;
   type Generation is new Word range 0 .. Word (Interfaces.Integer_64'Last);
   type Node_Index is new Interfaces.Unsigned_32 range 0 .. 2 ** 31 - 1;
   subtype Node_Limit is Node_Index range 1 .. Node_Index'Last;
   type Node_Ref is record
      Instance : Instance_ID := 0;
      Revision : Generation := 0;
      Index : Node_Index := 0;
   end record;
   type Query_Status is
     (OK, Not_Found, Stale_Generation, Bad_Cursor, Namespace_Not_Loaded,
      Refused, Insufficient_Space);
   type Field_Status is (Present, Absent, Method_Valued, Invalid, Unsupported);
   Default_STA : constant Word := 16#0F#;

   type Device_ID is private;
   No_ID : constant Device_ID;
   function Valid_ID (Item : Device_ID) return Boolean;
   type ID_Result (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Device_ID;
         when False => null;
      end case;
   end record;
   -- PNP/ACPI IDs have three/four vendor characters and four hex digits.
   -- Text input is canonicalized to uppercase; malformed IDs are refused.
   function From_Text (Text : String) return ID_Result with Global => null;
   function Image (Item : Device_ID) return String
     with Pre => Valid_ID (Item), Global => null;
   -- Query-only identifier: complete non-NUL bytes in the owning copied frame.
   -- Offset/length are checked again by the byte adapter, never native pointers.
   function External_ID (Offset, Length : Interfaces.Unsigned_32) return Device_ID;
   function Is_External (Item : Device_ID) return Boolean;
   function Text_Offset (Item : Device_ID) return Interfaces.Unsigned_32
     with Pre => Is_External (Item);
   function Text_Length (Item : Device_ID) return Interfaces.Unsigned_32
     with Pre => Is_External (Item);

   type Operation is (Find_By_ID, Find_By_Address, Describe, Resources);
   type Query_Key is record
      Kind : Operation := Find_By_ID;
      Instance : Instance_ID := 0;
      Revision : Generation := 0;
      Node : Node_Index := 0; -- Parent for ADR, target for Describe/Resources.
      Address : Word := 0;
      ID : Device_ID := No_ID;
   end record;
   function Valid_Key (Item : Query_Key; Limit : Node_Limit) return Boolean;
   -- Structural comparison only. External ID offsets may differ; the byte
   -- adapter must compare their complete values before accepting a cursor.
   function Same_Filter_Metadata (Left, Right : Query_Key) return Boolean;
   type Position_Kind is
     (Node_Scan, Description_Path, Description_CID, Description_UID,
      Resource_Subentry);
   type Cursor is record
      Filter : Query_Key;
      Part : Position_Kind := Node_Scan;
      Next_Node : Node_Index := 0;
      Offset, Subentry : Interfaces.Unsigned_32 := 0;
   end record;
   function Valid_Cursor (Item : Cursor; Limit : Node_Limit) return Boolean;
   type Cursor_Option (Present : Boolean := False) is record
      case Present is
         when True => Value : Cursor;
         when False => null;
      end case;
   end record;
   type Request is record
      Key : Query_Key;
      Continuation : Cursor_Option;
   end record;
   function Valid (Item : Request; Limit : Node_Limit) return Boolean;

   -- Version 2: 24 little-endian 64-bit words (192 bytes) in the loan page.
   -- Key occupies 2..8; cursor flag 9, full filter 10..16, position 17..20.
   -- ID length words use bit63 for external text, low32 for its length; the
   -- corresponding data word is its checked U32 offset. Other bits are zero.
   -- 21..23 are reserved zero. This codec validates metadata only; byte readers
   -- validate external text/cursor equality, canonical placement and padding.
   Frame_Word_Count : constant := 24;
   type Word_Array is array (Natural range <>) of Word;
   subtype Words is Word_Array (0 .. Frame_Word_Count - 1);
   Frame_Version : constant Word := 2;
   Frame_Magic : constant Word := 16#3259_5251_4950_4341#; -- ACPIQRY2
   function Encode (Item : Request; Limit : Node_Limit) return Words
     with Pre => Valid (Item, Limit), Global => null;
   type Decode_Status is (Decoded, Malformed, Unsupported_Version);
   type Decode_Result (Status : Decode_Status := Malformed) is record
      case Status is
         when Decoded => Value : Request;
         when others => null;
      end case;
   end record;
   function Decode (Data : Words; Limit : Node_Limit) return Decode_Result
     with Global => null,
       Post => (if Decode'Result.Status = Decoded then
         Valid (Decode'Result.Value, Limit) and then
         Encode (Decode'Result.Value, Limit) = Data);
private
   Maximum_ID_Length : constant := 8;
   type ID_Length is range 0 .. 2 ** 32 - 1;
   type ID_Storage is (Inline_ID, External_Text_ID);
   type Device_ID is record
      Length : ID_Length := 0;
      Data : Word := 0;
      Storage : ID_Storage := Inline_ID;
   end record;
   No_ID : constant Device_ID := (0, 0, Inline_ID);
end ACPI_Passive_Query;
