with Interfaces;

--  The canonical encoding of every hashed value (docs/ccl-build.md,
--  "Canonical encoding"): deterministic CBOR (RFC 8949, section 4.2.1)
--  restricted to unsigned integers, byte strings, text strings and definite
--  arrays. Every head has its shortest form, so one value has exactly one
--  encoding. The encoder and the decoder are defined by the same Is_Head,
--  and the decoder refuses every other form.
package Storehouse.Canonical with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_8;

   --  RFC 8949 major types used here, by their wire codes.
   type Major_Type is (Unsigned_Integer, Byte_String, Text_String, Array_Of);
   for Major_Type use (Unsigned_Integer => 0, Byte_String => 2, Text_String => 3, Array_Of => 4);

   --  Additional-information values of the first byte.
   Immediate_Limit : constant := 24;   --  0 .. 23 are the value itself
   One_Byte_Follows : constant := 24;
   Two_Bytes_Follow : constant := 25;
   Four_Bytes_Follow : constant := 26;
   Eight_Bytes_Follow : constant := 27;
   Major_Shift : constant := 32;       --  major type in the top three bits

   subtype Head_Length is Positive range 1 .. 9;

   --  The shortest head for Value: RFC 8949 section 4.2.1.
   function Length_Of (Value : Interfaces.Unsigned_64) return Head_Length is
     (if Value < Immediate_Limit then 1
      elsif Value <= 16#FF# then 2
      elsif Value <= 16#FFFF# then 3
      elsif Value <= 16#FFFF_FFFF# then 5
      else 9);

   function Code (Major : Major_Type) return Byte is
     (case Major is
        when Unsigned_Integer => 0, when Byte_String => 2,
        when Text_String => 3, when Array_Of => 4);

   function Initial (Major : Major_Type; Value : Interfaces.Unsigned_64) return Byte is
     (Code (Major) * Major_Shift +
        (case Length_Of (Value) is
           when 1 => Byte (Value),
           when 2 => One_Byte_Follows,
           when 3 => Two_Bytes_Follow,
           when 5 => Four_Bytes_Follow,
           when others => Eight_Bytes_Follow));

   --  Byte N of Value, counting from the least significant (N = 0).
   function Byte_Of (Value : Interfaces.Unsigned_64; N : Natural) return Byte is
     (Byte (Interfaces.Shift_Right (Value, 8 * N) and 16#FF#))
   with Pre => N < 8;

   --  Data is exactly the canonical head of (Major, Value): the initial
   --  byte, then the argument big-endian in the bytes the length implies.
   function Is_Head
     (Data : Byte_Array; Major : Major_Type; Value : Interfaces.Unsigned_64) return Boolean
   is (Data'Length = Length_Of (Value)
       and then Data (Data'First) = Initial (Major, Value)
       and then (for all K in 1 .. Data'Length - 1 =>
                   Data (Data'First + K) = Byte_Of (Value, Data'Length - 1 - K)))
   with Pre => Data'Length >= 1 and then Data'Last < Positive'Last;

   function Head (Major : Major_Type; Value : Interfaces.Unsigned_64) return Byte_Array
   with Post => Head'Result'First = 1
                and then Head'Result'Length = Length_Of (Value)
                and then Is_Head (Head'Result, Major, Value);

   --  The head at Data (First ..). Valid exactly when the bytes there are
   --  what Head writes for some major type above and value (so Is_Head
   --  holds of them); Next is the index after it.
   procedure Read_Head
     (Data : Byte_Array; First : Positive;
      Major : out Major_Type; Value : out Interfaces.Unsigned_64;
      Next : out Positive; Valid : out Boolean)
   with Pre => Data'Last < Positive'Last - 9,
        Post => (if Valid then
                   First in Data'Range
                   and then Next = First + Length_Of (Value)
                   and then Next - 1 <= Data'Last
                   and then Data (First .. Next - 1) = Head (Major, Value));

end Storehouse.Canonical;
