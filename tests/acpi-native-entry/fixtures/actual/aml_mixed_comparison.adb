pragma Ada_2022;
with AML_Coercions.Strings;
with Interfaces;
package body AML_Mixed_Comparison with SPARK_Mode is
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Width;
   use type AML_Coercions.Conversion_Status;
   function Compare
     (Width : AML_Decode.Integer_Width;
      Left_Kind : Input_Kind; Left_Number : AML_Decode.Integer_Value;
      Left_Data : AML_Decode.Bytes;
      Right_Kind : Input_Kind; Right_Number : AML_Decode.Integer_Value;
      Right_Data : AML_Decode.Bytes) return Result
   is
      Integer_Bytes : constant Positive :=
        (if Width = AML_Decode.Bits_32 then 4 else 8);
      R_Length : View_Length := 0;
      Converted : AML_Coercions.Result;
      function Right_Byte (Offset : View_Length) return AML_Decode.Byte
        with Pre => Left_Kind /= Integer_Input
          and then Right_Data'Length <= Max_Input_Length
          and then R_Length =
            (if Left_Kind = Buffer_Input then
               (case Right_Kind is
                  when Integer_Input => (if Width = AML_Decode.Bits_32 then 4 else 8),
                  when String_Input => Right_Data'Length + 1,
                  when Buffer_Input => Right_Data'Length)
             else
               (case Right_Kind is
                  when Integer_Input => AML_Coercions.Strings.Hex_Length (Width),
                  when String_Input => Right_Data'Length,
                  when Buffer_Input => (if Right_Data'Length = 0 then 0
                                        else Right_Data'Length * 5 - 1)))
          and then Offset < R_Length
      is
         B : AML_Decode.Byte;
      begin
         if Left_Kind = Buffer_Input then
            case Right_Kind is
               when Integer_Input =>
                  return AML_Coercions.Octet (Right_Number, Offset);
               when String_Input =>
                  if Offset = Right_Data'Length then return 0; end if;
                  return Right_Data (Right_Data'First + Offset);
               when Buffer_Input =>
                  return Right_Data (Right_Data'First + Offset);
            end case;
         elsif Right_Kind = Integer_Input then
            return AML_Coercions.Strings.Hex (AML_Decode.Byte
              (Interfaces.Shift_Right (Right_Number,
               (R_Length - Offset - 1) * 4) and 15));
         elsif Right_Kind = String_Input then
            return Right_Data (Right_Data'First + Offset);
         else
            B := Right_Data (Right_Data'First + Offset / 5);
            case Offset mod 5 is
               when 0 => return Character'Pos ('0');
               when 1 => return Character'Pos ('x');
               when 2 => return AML_Coercions.Strings.Hex (B / 16);
               when 3 => return AML_Coercions.Strings.Hex (B mod 16);
               when others => return Character'Pos (' ');
            end case;
         end if;
      end Right_Byte;
      L, R : AML_Decode.Byte;
   begin
      if Left_Kind = Integer_Input then
         case Right_Kind is
            when Integer_Input => Converted := (AML_Coercions.Converted, Right_Number);
            when String_Input => Converted := AML_Coercions.From_String (Right_Data, Width);
            when Buffer_Input => Converted := AML_Coercions.From_Buffer (Right_Data, Width);
         end case;
         if Converted.Status = AML_Coercions.Empty_Buffer then
            return (Empty_Buffer, 0);
         end if;
         return (Compared, (if Left_Number < Converted.Value then -1
           elsif Left_Number > Converted.Value then 1 else 0));
      end if;
      if Left_Kind = Buffer_Input then
         R_Length := (case Right_Kind is
           when Integer_Input => Integer_Bytes,
           when String_Input => Right_Data'Length + 1,
           when Buffer_Input => Right_Data'Length);
      else
         R_Length := (case Right_Kind is
           when Integer_Input => AML_Coercions.Strings.Hex_Length (Width),
           when String_Input => Right_Data'Length,
           when Buffer_Input => (if Right_Data'Length = 0 then 0
                                  else Right_Data'Length * 5 - 1));
      end if;
      for I in 1 .. Natural'Min (Left_Data'Length, R_Length) loop
         L := Left_Data (Left_Data'First + (I - 1));
         R := Right_Byte (I - 1);
         if L < R then return (Compared, -1);
         elsif L > R then return (Compared, 1); end if;
      end loop;
      return (Compared, (if Left_Data'Length < R_Length then -1
        elsif Left_Data'Length > R_Length then 1 else 0));
   end Compare;
end AML_Mixed_Comparison;
