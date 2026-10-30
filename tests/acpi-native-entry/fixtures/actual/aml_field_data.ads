pragma Ada_2022;
with AML_Decode;
-- Read-only bit extraction from owned bytes, never an OperationRegion handler.
package AML_Field_Data with SPARK_Mode, Pure is
   use type AML_Decode.Status;
   use type AML_Decode.Byte;
   subtype Shift_Count is Natural range 0 .. 8;
   subtype Scale_Value is Positive range 1 .. 256;
   function Scale (Bits : Shift_Count) return Scale_Value is
     (case Bits is when 0 => 1, when 1 => 2, when 2 => 4,
        when 3 => 8, when 4 => 16, when 5 => 32, when 6 => 64,
        when 7 => 128, when 8 => 256);
   Max_Bits : constant := AML_Decode.Max_Buffer_Length * 8;
   function Fits (Length, Offset, Count : Natural) return Boolean is
     (Offset <= Natural'Last - Count and then
      (Offset + Count) / 8 <= Length and then
      ((Offset + Count) mod 8 = 0 or else (Offset + Count) / 8 < Length));
   -- A little-endian window of at most one result byte. Arithmetic model is
   -- independent of host word size/endianness and never rounds source reads up.
   function Window
     (Data : AML_Decode.Bytes; Offset : Natural; Count : Positive)
      return AML_Decode.Byte
     with Pre => Count <= 8 and then Fits (Data'Length, Offset, Count),
       Post => Natural (Window'Result) =
         (Natural (Data (Data'First + Offset / 8)) / Scale (Offset mod 8)
          + (if Count + Offset mod 8 > 8 then
               Natural (Data (Data'First + Offset / 8 + 1)) * Scale (8 - Offset mod 8)
             else 0)) mod Scale (Count);
   type Read_Result (Status : AML_Decode.Status := AML_Decode.Truncated) is record
      case Status is
         when AML_Decode.Accepted =>
            Length : Natural range 0 .. AML_Decode.Max_Buffer_Length;
            Content : AML_Decode.Buffer_Storage;
         when others => null;
      end case;
   end record;
   -- Bits beyond Count, including the unused tail, are zero. Count=0 is an
   -- empty read, legal up to the source end. Oversized results fail explicitly.
   function Read_Bits
     (Data : AML_Decode.Bytes; Offset, Count : Natural) return Read_Result
     with Post =>
       (if Read_Bits'Result.Status = AML_Decode.Accepted then
          Count <= Max_Bits and then Fits (Data'Length, Offset, Count) and then
          Read_Bits'Result.Length = Count / 8 + (if Count mod 8 = 0 then 0 else 1)
          and then (for all I in 1 .. Read_Bits'Result.Length =>
            Read_Bits'Result.Content (I) = Window
              (Data, Offset + (I - 1) * 8, Positive'Min (8, Count - (I - 1) * 8)))
          and then (for all I in Read_Bits'Result.Length + 1 .. AML_Decode.Max_Buffer_Length =>
            Read_Bits'Result.Content (I) = 0));
end AML_Field_Data;
