pragma Ada_2022;
with Interfaces; use Interfaces;

--  Typed metric records: fixed 64-byte slots of eight little-endian words,
--  batched into one 4 KiB page (one header slot plus up to 63 records).
--  No pointers, strings or variable-length data. Values are producer claims;
--  identity is supplied separately by the authenticated transport.
package CuBit.Metric_Records with Pure, SPARK_Mode is
   Words_Per_Slot : constant := 8;
   Bytes_Per_Word : constant := 8;
   Slot_Bytes : constant := Words_Per_Slot * Bytes_Per_Word;
   Slots_Per_Page : constant := 64;
   Page_Bytes : constant := Slot_Bytes * Slots_Per_Page;
   Maximum_Records : constant := Slots_Per_Page - 1;

   subtype Slot_Word_Index is Natural range 0 .. Words_Per_Slot - 1;
   type Slot_Words is array (Slot_Word_Index) of Unsigned_64;
   subtype Page_Word_Index is
     Natural range 0 .. Slots_Per_Page * Words_Per_Slot - 1;
   type Page_Words is array (Page_Word_Index) of Unsigned_64;

   subtype Record_Count is Natural range 0 .. Maximum_Records;
   subtype Record_Index is Positive range 1 .. Maximum_Records;
   subtype Batch_Record_Count is Record_Count range 1 .. Maximum_Records;

   --  Bytes a batch of Count records occupies: header slot plus records.
   function Batch_Bytes (Count : Batch_Record_Count) return Unsigned_64 is
     (Unsigned_64 (Count + 1) * Slot_Bytes);

   Maximum_Keys : constant := 32;
   subtype Key_Count is Natural range 0 .. Maximum_Keys;
   subtype Metric_Key is Key_Count range 1 .. Maximum_Keys;

   type Record_Kind is (Describe, Counter, Gauge, Latency, Span);
   for Record_Kind use
     (Describe => 1, Counter => 2, Gauge => 3, Latency => 4, Span => 5);
   subtype Metric_Kind is Record_Kind range Counter .. Span;

   type Unit is (Count, Microseconds, Nanoseconds, Bytes);
   for Unit use (Count => 1, Microseconds => 2, Nanoseconds => 3, Bytes => 4);

   --  Lowercase ASCII letters, digits, '.', '_' and '-'; zero padded.
   Maximum_Name_Bytes : constant := 32;
   Name_Words : constant := Maximum_Name_Bytes / Bytes_Per_Word;
   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Name_Index is Positive range 1 .. Maximum_Name_Bytes;
   type Name_Bytes is array (Name_Index) of Unsigned_8;
   type Metric_Name is record
      Bytes : Name_Bytes := [others => 0];
      Length : Name_Length := 0;
   end record;
   function Name_Byte_Allowed (Item : Unsigned_8) return Boolean is
     (Item in Character'Pos ('a') .. Character'Pos ('z')
        | Character'Pos ('0') .. Character'Pos ('9')
        | Character'Pos ('.') | Character'Pos ('_') | Character'Pos ('-'));
   function Valid_Name (Item : Metric_Name) return Boolean is
     (Item.Length > 0 and then
      (for all I in Name_Index =>
         (if I <= Item.Length then Name_Byte_Allowed (Item.Bytes (I))
          else Item.Bytes (I) = 0)));
   function Same_Name (Left, Right : Metric_Name) return Boolean is
     (Left.Length = Right.Length and then Left.Bytes = Right.Bytes);
   --  Builds a name from a String; invalid or overlong text yields Length 0.
   function To_Name (Text : String) return Metric_Name
     with Post => (if To_Name'Result.Length > 0
                   then Valid_Name (To_Name'Result));

   --  Producer clock domain for every timestamp in a batch.
   type Clock_Domain is (Monotonic_Microseconds);
   for Clock_Domain use (Monotonic_Microseconds => 1);

   type Metric_Record (Kind : Record_Kind := Counter) is record
      Key : Metric_Key := Metric_Key'First;
      case Kind is
         when Describe =>
            Declared : Metric_Kind := Counter;
            Measure : Unit := Count;
            Name : Metric_Name;
         when Counter .. Latency =>
            Time_Us : Unsigned_64 := 0;
            Value : Unsigned_64 := 0;
            Correlation : Unsigned_64 := 0;
         when Span =>
            Start_Us : Unsigned_64 := 0;
            End_Us : Unsigned_64 := 0;
            Span_Correlation : Unsigned_64 := 0;
      end case;
   end record;

   function Valid (Item : Metric_Record) return Boolean is
     (case Item.Kind is
         when Describe => Valid_Name (Item.Name),
         when Counter .. Latency => True,
         when Span => Item.Start_Us <= Item.End_Us);

   type Record_Error is
     (Unknown_Kind, Invalid_Key, Invalid_Declaration, Invalid_Name,
      Nonzero_Reserved, Reversed_Span);
   type Decoded_Record (Success : Boolean := False) is record
      case Success is
         when False => Reason : Record_Error := Unknown_Kind;
         when True => Value : Metric_Record;
      end case;
   end record;

   function Encode (Item : Metric_Record) return Slot_Words
     with Pre => Valid (Item);
   function Decode (Words : Slot_Words) return Decoded_Record
     with Post => (if Decode'Result.Success then Valid (Decode'Result.Value));

   --  Batch header (slot 0). Sequence numbers strictly increase per publisher
   --  and never wrap; the last value is reserved so exhaustion is explicit.
   Header_Magic : constant Unsigned_64 := 16#434D_4554_0000_0001#;
   subtype Batch_Sequence is Unsigned_64 range 1 .. Unsigned_64'Last - 1;
   type Batch_Header is record
      Records : Batch_Record_Count := 1;
      Sequence : Batch_Sequence := 1;
      Producer_Dropped : Unsigned_64 := 0;
      Clock : Clock_Domain := Monotonic_Microseconds;
   end record;
   type Header_Error is
     (Bad_Magic, Bad_Count, Bad_Sequence, Bad_Clock, Header_Reserved,
      Length_Mismatch);
   type Decoded_Header (Success : Boolean := False) is record
      case Success is
         when False => Reason : Header_Error := Bad_Magic;
         when True => Value : Batch_Header;
      end case;
   end record;

   function Encode_Header (Item : Batch_Header) return Slot_Words;
   --  Bytes is the transport length; it must equal Batch_Bytes (Records).
   function Decode_Header
     (Words : Slot_Words; Bytes : Unsigned_64) return Decoded_Header
     with Post =>
       (if Decode_Header'Result.Success then
          Bytes = Batch_Bytes (Decode_Header'Result.Value.Records));

   function Slot (Page : Page_Words; Index : Record_Count) return Slot_Words
     is ([for W in Slot_Word_Index => Page (Index * Words_Per_Slot + W)]);
   procedure Put_Slot
     (Page : in out Page_Words; Index : Record_Count; Words : Slot_Words)
     with Post =>
       Slot (Page, Index) = Words and
       (for all I in Page_Word_Index =>
          (if I / Words_Per_Slot /= Index then Page (I) = Page'Old (I)));
end CuBit.Metric_Records;
