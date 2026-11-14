pragma Ada_2022;
package body CuBit.Metric_Query_Pages with SPARK_Mode is
   Byte_Bits : constant := 8;
   Bytes_Per_Word : constant := 8;
   Byte_Mask : constant := 16#FF#;

   procedure Put_Text (Into : in out Page; At_Word : Word_Index; Text : String; Bytes : Positive) is
      Length : constant Natural := Natural'Min (Text'Length, Bytes);
   begin
      Into (At_Word) := Unsigned_64 (Length);
      for W in 1 .. Bytes / Bytes_Per_Word loop
         Into (At_Word + W) := 0;
      end loop;
      for I in 0 .. Length - 1 loop
         declare
            W : constant Word_Index := At_Word + 1 + I / Bytes_Per_Word;
         begin
            Into (W) := Into (W) or
              Shift_Left (Unsigned_64 (Character'Pos (Text (Text'First + I))), Byte_Bits * (I mod Bytes_Per_Word));
         end;
      end loop;
   end Put_Text;

   function Get_Text (From : Page; At_Word : Word_Index; Bytes : Positive) return String is
      Length : constant Natural := Natural (Unsigned_64'Min (From (At_Word), Unsigned_64 (Bytes)));
      Result : String (1 .. Length);
   begin
      for I in 0 .. Length - 1 loop
         Result (I + 1) := Character'Val
           (Natural (Shift_Right (From (At_Word + 1 + I / Bytes_Per_Word), Byte_Bits * (I mod Bytes_Per_Word))
                     and Byte_Mask));
      end loop;
      return Result;
   end Get_Text;
end CuBit.Metric_Query_Pages;
