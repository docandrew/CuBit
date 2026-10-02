pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Protocols;

--  Versioned diagnostic records, not trusted audit records or authority.
package CuBit.Log_Records with Pure, SPARK_Mode is
   Maximum_Text_Bytes : constant := 512;
   Header_Bytes : constant := 32;
   --  Version 2 adds up to eight typed fields of 32 bytes each.
   Maximum_Fields : constant := 8;
   Field_Bytes : constant := 32;
   subtype Text_Count is Natural range 0 .. Maximum_Text_Bytes;
   subtype Wire_Count is Natural
     range 0 .. Header_Bytes + Maximum_Fields * Field_Bytes +
                 Maximum_Text_Bytes;
   type Wire_Buffer is array (Positive range 1 .. Wire_Count'Last)
     of Unsigned_8;
   type Severity is (Trace, Debug, Information, Warning, Error, Critical);
   type Timebase is (Unspecified, Monotonic_Milliseconds, Unix_Milliseconds);
   subtype Clock_Domain is Unsigned_64 range 1 .. Unsigned_64'Last;
   type Timestamp (Clock : Timebase := Unspecified) is record
      case Clock is
         when Unspecified => null;
         when Monotonic_Milliseconds =>
            Domain : Clock_Domain;
            Ticks : Unsigned_64;
         when Unix_Milliseconds =>
            Since_Epoch : Unsigned_64;
      end case;
   end record;
   --  Structured fields: a short identifier and one typed scalar. Kinds map
   --  onto CCL scalar values (integer, boolean); durations are integers in
   --  microseconds. Names use lowercase ASCII letters, digits, '.', '_', '-'.
   type Field_Kind is
     (Signed_Integer, Unsigned_Integer, Duration_Microseconds, Truth);
   for Field_Kind use
     (Signed_Integer => 1, Unsigned_Integer => 2, Duration_Microseconds => 3,
      Truth => 4);
   subtype Field_Count is Natural range 0 .. Maximum_Fields;
   subtype Field_Index is Field_Count range 1 .. Maximum_Fields;
   Maximum_Field_Name_Bytes : constant := 16;
   subtype Field_Name_Length is Natural range 0 .. Maximum_Field_Name_Bytes;
   subtype Field_Name_Index is Positive range 1 .. Maximum_Field_Name_Bytes;
   function Field_Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-');
   function Valid_Field_Name (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_Field_Name_Bytes and then
      (for all C of Name => Field_Name_Character (C)));
   type Field is private;
   function Name (Item : Field) return String;
   function Kind (Item : Field) return Field_Kind;
   --  Raw 64-bit value; Signed_Integer uses two's complement and Truth is
   --  zero or one.
   function Value (Item : Field) return Unsigned_64;
   function Signed_Value (Item : Field) return Integer_64;

   type Log_Record is private;
   Empty_Record : constant Log_Record;
   type Decoding_Error is
     (Too_Long, Invalid_Text, Truncated, Invalid_Header, Length_Mismatch,
      Invalid_Timestamp, Invalid_Field, Too_Many_Fields);
   type Decoded (Success : Boolean := False) is record
      case Success is
         when False => Reason : Decoding_Error := Invalid_Header;
         when True => Value : Log_Record;
      end case;
   end record;
   --  UTF-8 scalars; reject ASCII controls other than TAB, and reject DEL.
   --  Renderers must still escape text for HTML/terminal/markup contexts.
   function Valid_Text (Text : String) return Boolean;
   function Make
     (Text : String; Level : Severity := Information;
      Reported_Time : Timestamp := (Clock => Unspecified)) return Decoded;
   function Text (Item : Log_Record) return String;
   function Level (Item : Log_Record) return Severity;
   function Reported_Time (Item : Log_Record) return Timestamp;
   function Field_Total (Item : Log_Record) return Field_Count;
   function Field_At (Item : Log_Record; Index : Field_Index) return Field
     with Pre => Index <= Field_Total (Item);
   --  Appends one typed field. Fails with Too_Many_Fields, or Invalid_Field
   --  for a bad name or a Truth value other than zero or one.
   function With_Field
     (Item : Log_Record; Name : String; Kind : Field_Kind;
      Value : Unsigned_64) return Decoded;
   --  Records without fields encode as format version 1 (unchanged bytes);
   --  records with fields encode as version 2.
   procedure Encode
     (Item : Log_Record; Bytes : out Wire_Buffer; Used : out Wire_Count);
   --  Copy stable input bytes before decoding shared untrusted storage.
   --  On failure the discriminant exposes no partially decoded record.
   function Decode (Bytes : Wire_Buffer; Used : Wire_Count) return Decoded;

   Contract : constant CuBit.Protocols.Schema_Contract :=
     (Identity => 16#4355_424C_4F47_0001#, Version => 2,
      Sizing => CuBit.Protocols.Bounded_Size,
      Wire_Size => Unsigned_32 (Wire_Count'Last));
private
   type Field is record
      Label : String (Field_Name_Index) := [others => Character'Val (0)];
      Label_Length : Field_Name_Length := 0;
      Category : Field_Kind := Unsigned_Integer;
      Raw : Unsigned_64 := 0;
   end record;
   function Name (Item : Field) return String is
     (Item.Label (1 .. Item.Label_Length));
   function Kind (Item : Field) return Field_Kind is (Item.Category);
   function Value (Item : Field) return Unsigned_64 is (Item.Raw);
   type Field_Array is array (Field_Index) of Field;
   type Log_Record is record
      Content : String (1 .. Maximum_Text_Bytes) :=
        [others => Character'Val (0)];
      Length : Text_Count := 0;
      Importance : Severity := Information;
      Time : Timestamp := (Clock => Unspecified);
      Fields : Field_Array;
      Field_Used : Field_Count := 0;
   end record;
   function Field_Total (Item : Log_Record) return Field_Count is
     (Item.Field_Used);
   Empty_Record : constant Log_Record := (others => <>);
end CuBit.Log_Records;
