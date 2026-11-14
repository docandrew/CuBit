pragma Ada_2022;
with Interfaces; use Interfaces;

--  The sample codecs of metrics.svc's chunks (MET-002, DESIGN-metrics-store.md):
--  pure, small and proved.
--
--  * Zigzag folds a signed difference (two's complement in a word) into an
--    unsigned one, small magnitudes to small numbers: 0, -1, 1, -2 ... ->
--    0, 1, 2, 3 ...
--  * Varint writes a word in 7-bit groups, low group first, the top bit of
--    each byte saying another follows: 1 byte below 128, at most 10.
--  * A chunk stores each sample's time as the zigzag varint of its delta of
--    delta (a steady rate costs one byte), and its value as the zigzag
--    varint of its difference from the previous value (a counter's
--    increments, a gauge's steps).
--
--  Proved (gnatprove level 2, tests/metric-store/run.sh --prove): the round
--  trips Unzigzag (Zigzag (X)) = X and, for bytes Put_Varint wrote, that
--  Get_Varint reads back the value and the length; and freedom from
--  run-time errors.
package Metric_Codecs with Pure, SPARK_Mode is

   Top_Bit : constant := 16#8000_0000_0000_0000#;
   function Zigzag (Value : Unsigned_64) return Unsigned_64 is
     (Shift_Left (Value, 1) xor (if (Value and Top_Bit) /= 0 then Unsigned_64'Last else 0));
   function Unzigzag (Value : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Value, 1) xor (if (Value and 1) /= 0 then Unsigned_64'Last else 0));

   procedure Lemma_Zigzag_Round_Trip (Value : Unsigned_64)
   with Ghost, Global => null, Post => Unzigzag (Zigzag (Value)) = Value;

   Group_Bits : constant := 7;
   Group_Mask : constant := 16#7F#;
   More_Bit : constant := 16#80#;
   Maximum_Varint_Bytes : constant := 10;
   subtype Varint_Length is Positive range 1 .. Maximum_Varint_Bytes;
   subtype Group_Index is Natural range 0 .. Maximum_Varint_Bytes - 1;

   --  The bytes Value takes.
   function Varint_Bytes (Value : Unsigned_64) return Varint_Length is
     (if Value < 2 ** 7 then 1 elsif Value < 2 ** 14 then 2 elsif Value < 2 ** 21 then 3
      elsif Value < 2 ** 28 then 4 elsif Value < 2 ** 35 then 5 elsif Value < 2 ** 42 then 6
      elsif Value < 2 ** 49 then 7 elsif Value < 2 ** 56 then 8 elsif Value < 2 ** 63 then 9 else 10);

   type Byte_Array is array (Natural range <>) of Unsigned_8;
   --  Whether another byte follows this one.
   function Continues (Item : Unsigned_8) return Boolean is ((Item and More_Bit) /= 0);

   --  Group I of Value's seven-bit groups, low first.
   function Bits_Of (Value : Unsigned_64; I : Group_Index) return Unsigned_8 is
     (Unsigned_8 (Shift_Right (Value, Group_Bits * I) and Group_Mask));
   --  Byte I as Put_Varint writes it: the group, with More_Bit on all but
   --  the last.
   function Group (Value : Unsigned_64; I : Group_Index) return Unsigned_8 is
     (Bits_Of (Value, I) or (if I + 1 < Varint_Bytes (Value) then More_Bit else 0));

   --  Value at Into (At_Byte ..), its Varint_Bytes bytes.
   procedure Put_Varint (Into : in out Byte_Array; At_Byte : Natural; Value : Unsigned_64)
   with Pre => At_Byte >= Into'First and then At_Byte <= Natural'Last - Maximum_Varint_Bytes
               and then At_Byte + Varint_Bytes (Value) - 1 <= Into'Last,
        Post => (for all I in 0 .. Varint_Bytes (Value) - 1 => Into (At_Byte + I) = Group (Value, I))
                and then (for all K in Into'Range =>
                            (if K < At_Byte or else K >= At_Byte + Varint_Bytes (Value)
                             then Into (K) = Into'Old (K)));

   --  Holds when From (At_Byte ..) begins with exactly the bytes
   --  Put_Varint writes for Value.
   function Written (From : Byte_Array; At_Byte : Natural; Value : Unsigned_64) return Boolean is
     (At_Byte >= From'First and then At_Byte <= Natural'Last - Maximum_Varint_Bytes
      and then At_Byte + Varint_Bytes (Value) - 1 <= From'Last
      and then (for all I in 0 .. Varint_Bytes (Value) - 1 => From (At_Byte + I) = Group (Value, I)));

   --  The value of the first Count bytes at From (At_Byte ..), as groups.
   function Value_Of (From : Byte_Array; At_Byte : Natural; Count : Natural) return Unsigned_64 is
     (if Count = 0 then 0
      else Value_Of (From, At_Byte, Count - 1)
           or Shift_Left (Unsigned_64 (From (At_Byte + Count - 1) and Group_Mask), Group_Bits * (Count - 1)))
   with Pre => Count <= Maximum_Varint_Bytes and then At_Byte >= From'First
               and then At_Byte <= Natural'Last - Maximum_Varint_Bytes
               and then At_Byte + Count - 1 <= From'Last,
        Subprogram_Variant => (Decreases => Count);

   --  The varint at From (At_Byte ..): Valid False when it runs past
   --  From'Last or past Maximum_Varint_Bytes. Otherwise Length is up to and
   --  including the first byte without More_Bit, and Value its groups.
   procedure Get_Varint
     (From : Byte_Array; At_Byte : Natural; Value : out Unsigned_64; Length : out Varint_Length;
      Valid : out Boolean)
   with Pre => At_Byte >= From'First and then At_Byte <= Natural'Last - Maximum_Varint_Bytes,
        Post => (if Valid then At_Byte + Length - 1 <= From'Last
                   and then Value = Value_Of (From, At_Byte, Length)
                   and then not Continues (From (At_Byte + Length - 1))
                   and then (for all K in At_Byte .. At_Byte + Length - 2 => Continues (From (K))));

   --  The round trip: the bytes Put_Varint writes for Value end where
   --  Varint_Bytes says (only the last lacks More_Bit) and hold Value.
   procedure Lemma_Varint_Round_Trip (From : Byte_Array; At_Byte : Natural; Value : Unsigned_64)
   with Ghost, Global => null,
        Pre => Written (From, At_Byte, Value),
        Post => Value_Of (From, At_Byte, Varint_Bytes (Value)) = Value
                and then not Continues (From (At_Byte + Varint_Bytes (Value) - 1))
                and then (for all K in At_Byte .. At_Byte + Varint_Bytes (Value) - 2 => Continues (From (K)));
end Metric_Codecs;
