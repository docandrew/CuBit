------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The payload of a GPIO bootstrap admission request (ACPI-POOL-001, the
--  fallback when devmgr could not read the Intel P2SB): what acpi.svc
--  observed of the identified GPIO controller before GPIO-consuming or
--  GPIO-touching initializers ran; ancestor/provider initializers may
--  already have run,
--  as one fixed-layout little-endian byte string, and its codec.
--
--  @description
--  acpi.svc evaluates the controller's _HID, _STA and _CRS once (one
--  bounded observation on the startup evaluator, no GPIO pad access),
--  copies the result here, lends devmgr the bytes as a read-only grant and
--  calls CuBit.GPIO_Bootstrap_Exchange.Admission_Request. devmgr copies
--  the bytes, decodes them (Decode: any defect is Malformed with the first
--  failing byte offset), then decides
--  (Platform_Quirks.GPIO_Bootstrap_Admission). This is the provenance the
--  admission logs: which device, from which table, in which phase.
--
--  Layout, version Format_Version (all integers little-endian):
--    offset  bytes  field
--         0      4  Format_Version
--         4      1  Span_Count, 1 .. Max_Spans
--         5      1  Path_Segments, 1 .. Max_Path_Segments
--         6      1  UID_Kind'Enum_Rep
--         7      1  STA_Source'Enum_Rep
--         8      8  HID: printable ASCII (21h .. 7Eh), at least one, then
--                   NUL padding (the _HID string, or the EISA ID decoded
--                   to its 7 characters, e.g. "INTC1057", "PNP0C02")
--        16      8  UID: the integer (Integer_UID), up to 8 ASCII bytes
--                   NUL-padded (String_UID), or zero (No_UID)
--        24      4  STA: the _STA value (Evaluated_STA) or 0Fh (Default_STA)
--        28      1  Phase'Enum_Rep (Before_GPIO_Consumer_Initialization only)
--        29      1  IRQ present: 0 or 1
--        30      1  Trigger'Enum_Rep      } zero when no IRQ
--        31      1  Polarity'Enum_Rep     }
--        32      1  Sharing'Enum_Rep      }
--        33      1  Wake'Enum_Rep         }
--        34      4  GSI                   }
--        38      4  defining table's signature (printable ASCII: "DSDT", "SSDT")
--        42      8  defining table's OEM Table ID (ASCII 20h .. 7Eh)
--        50      8  catalog epoch of the namespace observed
--        58     32  namespace path: Max_Path_Segments 4-byte NameSegs from
--                   the root ("_SB_", "GPI0"); unused slots zero
--        90      4  automatic initializer evaluator invocation count
--        94      8  ordered initializer-history fingerprint (diagnostic only)
--       102  18 x N  spans, in _CRS order: base (8), length (8),
--                   Descriptor'Enum_Rep (1), writable (1: 0 or 1)
--  Length = Header_Bytes + Span_Bytes * Span_Count exactly.
--
--  The IRQ is facts only: no interrupt route is granted by admission (the
--  GPIO driver stays polling-only until a parent-interrupt route exists).
--  Pure, SPARK-compatible codec. Verification scope is recorded in tests.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package GPIO_Bootstrap_Payload with Pure, SPARK_Mode is

   Format_Version : constant := 2;

   --  Named ceilings: the most memory descriptors one controller's _CRS
   --  may carry here, and the deepest namespace path.
   Max_Spans : constant := 16;
   Max_Path_Segments : constant := 8;

   subtype Span_Count is Natural range 0 .. Max_Spans;
   subtype Span_Index is Positive range 1 .. Max_Spans;
   subtype Path_Count is Natural range 0 .. Max_Path_Segments;
   subtype Path_Index is Positive range 1 .. Max_Path_Segments;

   --  Field offsets and sizes (the layout above).
   Version_Offset    : constant := 0;
   Span_Count_Offset : constant := 4;
   Path_Count_Offset : constant := 5;
   UID_Kind_Offset   : constant := 6;
   STA_Source_Offset : constant := 7;
   HID_Offset        : constant := 8;
   HID_Bytes         : constant := 8;
   UID_Offset        : constant := 16;
   UID_Bytes         : constant := 8;
   STA_Offset        : constant := 24;
   Phase_Offset      : constant := 28;
   IRQ_Present_Offset : constant := 29;
   Trigger_Offset    : constant := 30;
   Polarity_Offset   : constant := 31;
   Sharing_Offset    : constant := 32;
   Wake_Offset       : constant := 33;
   GSI_Offset        : constant := 34;
   Table_Signature_Offset : constant := 38;
   Table_Signature_Bytes  : constant := 4;
   Table_ID_Offset   : constant := 42;
   Table_ID_Bytes    : constant := 8;
   Epoch_Offset      : constant := 50;
   Path_Offset       : constant := 58;
   Name_Segment_Bytes : constant := 4;
   Initializer_Count_Offset : constant := Path_Offset + Max_Path_Segments * Name_Segment_Bytes;
   Initializer_Count_Bytes : constant := 4;
   Initializer_Digest_Offset : constant := Initializer_Count_Offset + Initializer_Count_Bytes;
   Initializer_Digest_Bytes : constant := 8;
   Header_Bytes : constant := Initializer_Digest_Offset + Initializer_Digest_Bytes; -- 102
   -- Canonical initializer-history encoding v1, FNV-1a64 empty prefix.
   -- Diagnostic provenance only; no digest value authorizes hardware access.
   Empty_Initializer_Digest : constant Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
   Span_Base_Offset  : constant := 0;
   Span_Length_Offset : constant := 8;
   Span_Descriptor_Offset : constant := 16;
   Span_Writable_Offset : constant := 17;
   Span_Bytes        : constant := 18;
   Max_Payload_Bytes : constant := Header_Bytes + Span_Bytes * Max_Spans;

   subtype Payload_Length is Natural range 0 .. Max_Payload_Bytes;

   function Encoded_Length (Count : Span_Count) return Natural is
     (Header_Bytes + Span_Bytes * Count)
   with Post => Encoded_Length'Result in Header_Bytes .. Max_Payload_Bytes;

   NUL : constant Character := Character'Val (0);

   --  _UID's type (ACPI 6.5 section 6.1.12: an integer or a string).
   type UID_Kind is (No_UID, Integer_UID, String_UID);
   for UID_Kind use (No_UID => 0, Integer_UID => 1, String_UID => 2);

   --  Whether _STA was evaluated, or absent (ACPI 6.5 section 6.3.7: then
   --  the device is assumed present, enabled, shown and functioning, 0Fh).
   type STA_Source is (Evaluated_STA, Default_STA);
   for STA_Source use (Evaluated_STA => 1, Default_STA => 2);
   Default_STA_Value : constant := 16#0F#;

   --  When the observation was made. Ancestor/provider initializers may already have run.
   type Phase is (Before_GPIO_Consumer_Initialization);
   for Phase use (Before_GPIO_Consumer_Initialization => 2);

   --  ACPI 6.5 section 6.4.3.6 Extended Interrupt Descriptor, byte 3
   --  ("Interrupt Vector Flags"): bit 1 mode, bit 2 polarity, bit 3
   --  sharing, bit 4 wake capability.
   type Trigger is (Level, Edge);
   for Trigger use (Level => 0, Edge => 1);
   type Polarity is (Active_High, Active_Low);
   for Polarity use (Active_High => 0, Active_Low => 1);
   type Sharing is (Exclusive, Shared);
   for Sharing use (Exclusive => 0, Shared => 1);
   type Wake is (Not_Wake_Capable, Wake_Capable);
   for Wake use (Not_Wake_Capable => 0, Wake_Capable => 1);

   --  Which resource descriptor a span came from (ACPI 6.5 sections
   --  6.4.3.4 32-Bit Fixed Memory Range, 6.4.3.3 32-Bit Memory Range,
   --  6.4.3.5.2 DWord / 6.4.3.5.1 QWord Address Space).
   type Descriptor is (Memory32_Fixed, Memory32, DWord_Address, QWord_Address);
   for Descriptor use
     (Memory32_Fixed => 1, Memory32 => 2, DWord_Address => 3, QWord_Address => 4);

   subtype HID_Text is String (1 .. HID_Bytes);
   subtype Name_Segment is String (1 .. Name_Segment_Bytes);
   subtype Table_Signature_Text is String (1 .. Table_Signature_Bytes);
   subtype Table_ID_Text is String (1 .. Table_ID_Bytes);

   type Name_Path is array (Path_Index) of Name_Segment;

   type Interrupt is record
      Present  : Boolean := False;
      Mode     : Trigger := Level;
      Level_Of : Polarity := Active_High;
      Share    : Sharing := Exclusive;
      Wakes    : Wake := Not_Wake_Capable;
      GSI      : Unsigned_32 := 0;
   end record;

   type Span is record
      Base, Length : Unsigned_64 := 0;
      From         : Descriptor := Memory32_Fixed;
      Writable     : Boolean := False;
   end record;
   type Span_Array is array (Span_Index) of Span;

   type Request is record
      Spans        : Span_Count := 0;
      Segments     : Path_Count := 0;
      UID_Type     : UID_Kind := No_UID;
      STA_From     : STA_Source := Evaluated_STA;
      HID          : HID_Text := [others => NUL];
      UID          : Unsigned_64 := 0;
      STA          : Unsigned_32 := 0;
      Observed     : Phase := Before_GPIO_Consumer_Initialization;
      Initializer_Invocations : Unsigned_32 := 0;
      Initializer_Ordered_Digest : Unsigned_64 := Empty_Initializer_Digest;
      IRQ          : Interrupt;
      Table        : Table_Signature_Text := [others => NUL];
      Table_ID     : Table_ID_Text := [others => NUL];
      Epoch        : Unsigned_64 := 0;
      Path         : Name_Path := [others => [others => NUL]];
      Items        : Span_Array := [others => (others => <>)];
   end record;

   type Byte_Array is array (Natural range <>) of Unsigned_8;

   ---------------------------------------------------------------------------
   --  Field rules (what Decode accepts, what Encode needs).
   ---------------------------------------------------------------------------
   function Printable (C : Character) return Boolean is
     (C in '!' .. '~');

   --  At least one printable character, then only NUL.
   function Valid_HID (H : HID_Text) return Boolean is
     (Printable (H (H'First))
      and then (for all I in H'First + 1 .. H'Last =>
                  (if H (I) = NUL then (for all J in I .. H'Last => H (J) = NUL)
                   else Printable (H (I)))));

   --  ACPI 6.5 section 20.2.2 NameSeg: LeadNameChar ('A'-'Z', '_') then
   --  three NameChars (also '0'-'9').
   function Lead_Name_Char (C : Character) return Boolean is
     (C in 'A' .. 'Z' | '_');
   function Name_Char (C : Character) return Boolean is
     (Lead_Name_Char (C) or else C in '0' .. '9');
   function Valid_Segment (S : Name_Segment) return Boolean is
     (Lead_Name_Char (S (S'First))
      and then (for all I in S'First + 1 .. S'Last => Name_Char (S (I))));
   function Empty_Segment (S : Name_Segment) return Boolean is
     (for all C of S => C = NUL);

   function Valid_Path (Count : Path_Count; P : Name_Path) return Boolean is
     (Count >= 1
      and then (for all I in Path_Index =>
                  (if I <= Count then Valid_Segment (P (I)) else Empty_Segment (P (I)))));

   function Valid_Interrupt (IRQ : Interrupt) return Boolean is
     (IRQ.Present
      or else (IRQ.Mode = Level and then IRQ.Level_Of = Active_High
               and then IRQ.Share = Exclusive and then IRQ.Wakes = Not_Wake_Capable
               and then IRQ.GSI = 0));

   function Valid_UID (Kind : UID_Kind; UID : Unsigned_64) return Boolean is
     (Kind /= No_UID or else UID = 0);

   function Valid_STA (From : STA_Source; STA : Unsigned_32) return Boolean is
     (From = Evaluated_STA or else STA = Default_STA_Value);

   function Valid_Table (Signature : Table_Signature_Text; ID : Table_ID_Text) return Boolean is
     ((for all C of Signature => Printable (C))
      and then (for all C of ID => C in ' ' .. '~'));

   --  Span fields past Spans are zero.
   function Unused_Spans_Empty (R : Request) return Boolean is
     (for all I in Span_Index =>
        (if I > R.Spans then R.Items (I) = (others => <>)));

   function Valid (R : Request) return Boolean is
     (R.Spans >= 1
      and then Valid_Path (R.Segments, R.Path)
      and then Valid_HID (R.HID)
      and then Valid_UID (R.UID_Type, R.UID)
      and then Valid_STA (R.STA_From, R.STA)
      and then Valid_Interrupt (R.IRQ)
      and then Valid_Table (R.Table, R.Table_ID)
      and then Unused_Spans_Empty (R));

   ---------------------------------------------------------------------------
   --  The codec.
   ---------------------------------------------------------------------------
   type Decode_Status is (Decoded, Malformed);

   type Decode_Result is record
      Status : Decode_Status := Malformed;
      Offset : Payload_Length := 0;   --  the first failing byte (Malformed)
   end record;

   --  Bytes as a request. Decoded only for a whole, valid payload.
   procedure Decode (Bytes : Byte_Array; R : out Request; Result : out Decode_Result)
   with Pre  => Bytes'First = 0 and then Bytes'Length <= Max_Payload_Bytes,
        Post => (if Result.Status = Decoded then
                   Valid (R) and then Bytes'Length = Encoded_Length (R.Spans));

   --  R as bytes (acpi.svc's side, and the tests' round trip).
   procedure Encode (R : Request; Bytes : out Byte_Array)
   with Pre => Valid (R) and then Bytes'First = 0
               and then Bytes'Last = Encoded_Length (R.Spans) - 1;

end GPIO_Bootstrap_Payload;
