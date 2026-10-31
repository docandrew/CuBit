pragma Ada_2022;
package body AML_Coercions.Strings with SPARK_Mode is
   function From_Integer
     (Value : AML_Decode.Integer_Value; Width : AML_Decode.Integer_Width)
      return AML_Decode.Bytes
   is
      Result : AML_Decode.Bytes (1 .. Hex_Length (Width)) := [others => 0];
   begin
      for I in Result'Range loop
         Result (I) := Hex (AML_Decode.Byte
           (Interfaces.Shift_Right (Value, (Hex_Length (Width) - I) * 4) and 15));
         pragma Loop_Invariant
           (for all J in 1 .. I => Result (J) = Hex (AML_Decode.Byte
              (Interfaces.Shift_Right (Value, (Hex_Length (Width) - J) * 4) and 15)));
      end loop;
      return Result;
   end From_Integer;
   function From_Buffer (Data : AML_Decode.Bytes) return AML_Decode.Bytes is
      Length : constant Natural := (if Data'Length = 0 then 0 else Data'Length * 5 - 1);
      Result : AML_Decode.Bytes (1 .. Length) := [others => Character'Pos (' ')];
   begin
      for I in 1 .. Data'Length loop
         Result ((I - 1) * 5 + 1) := Character'Pos ('0');
         Result ((I - 1) * 5 + 2) := Character'Pos ('x');
         Result ((I - 1) * 5 + 3) := Hex (Data (Data'First + (I - 1)) / 16);
         Result ((I - 1) * 5 + 4) := Hex (Data (Data'First + (I - 1)) mod 16);
         pragma Loop_Invariant
           (for all J in 1 .. I =>
             Result ((J - 1) * 5 + 1) = Character'Pos ('0') and then
             Result ((J - 1) * 5 + 2) = Character'Pos ('x') and then
             Result ((J - 1) * 5 + 3) = Hex (Data (Data'First + (J - 1)) / 16) and then
             Result ((J - 1) * 5 + 4) = Hex (Data (Data'First + (J - 1)) mod 16));
         pragma Loop_Invariant
           (for all J in 1 .. Data'Length - 1 => Result (J * 5) = Character'Pos (' '));
      end loop;
      return Result;
   end From_Buffer;
end AML_Coercions.Strings;
