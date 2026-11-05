pragma Ada_2022;
with AML_Coercions;
with AML_Coercions.Strings;
package body AML_Concatenation with SPARK_Mode is
   use AML_Decode;
   use type AML_Coercions.Conversion_Status;
   Hex_Byte_Characters : constant Positive := 4;
   Hex_Byte_Stride : constant Positive := 5;
   function Build
     (Width : Integer_Width;
      Left_Kind : Input_Kind; Left_Number : Integer_Value;
      Left_Data : Bytes;
      Right_Kind : Input_Kind; Right_Number : Integer_Value;
      Right_Data : Bytes) return Result
   is
      Integer_Bytes : constant Positive := (if Width = Bits_32 then 4 else 8);
      Left_Length, Right_Length : Result_Length := 0;
      Converted_Right : Integer_Value := Right_Number;
      Conversion : AML_Coercions.Result;
      Kind : Output_Kind := Buffer_Output;
   begin
      case Left_Kind is
         when Integer_Input =>
            case Right_Kind is
               when Integer_Input => null;
               when String_Input =>
                  Conversion := AML_Coercions.From_String (Right_Data, Width);
                  Converted_Right := Conversion.Value;
               when Buffer_Input =>
                  Conversion := AML_Coercions.From_Buffer (Right_Data, Width);
                  if Conversion.Status = AML_Coercions.Empty_Buffer then
                     return (Status => Empty_Buffer);
                  end if;
                  Converted_Right := Conversion.Value;
            end case;
            if Integer_Bytes > Max_Result_Length / 2 then
               return (Status => Length_Limit);
            end if;
            Left_Length := Integer_Bytes;
            Right_Length := Integer_Bytes;
         when String_Input | Buffer_Input =>
            if Left_Data'Length > Max_Result_Length then
               return (Status => Length_Limit);
            end if;
            Left_Length := Left_Data'Length;
            if Left_Kind = String_Input then
               Kind := String_Output;
               case Right_Kind is
                  when Integer_Input =>
                     if AML_Coercions.Strings.Hex_Length (Width) > Max_Result_Length - Left_Length then
                        return (Status => Length_Limit);
                     end if;
                     Right_Length := AML_Coercions.Strings.Hex_Length (Width);
                  when String_Input =>
                     if Right_Data'Length > Max_Result_Length - Left_Length then
                        return (Status => Length_Limit);
                     end if;
                     Right_Length := Right_Data'Length;
                  when Buffer_Input =>
                     if Right_Data'Length > 0 then
                        -- Four characters for the first byte, then five each.
                        if Max_Result_Length - Left_Length < Hex_Byte_Characters
                          or else Right_Data'Length - 1 >
                            (Max_Result_Length - Left_Length - Hex_Byte_Characters) / Hex_Byte_Stride
                        then
                           return (Status => Length_Limit);
                        end if;
                        Right_Length := (Right_Data'Length - 1) * Hex_Byte_Stride + Hex_Byte_Characters;
                     end if;
               end case;
            else
               case Right_Kind is
                  when Integer_Input =>
                     if Integer_Bytes > Max_Result_Length - Left_Length then
                        return (Status => Length_Limit);
                     end if;
                     Right_Length := Integer_Bytes;
                  when String_Input =>
                     -- Include the terminating zero without overflowing Length+1.
                     if Right_Data'Length >= Max_Result_Length - Left_Length then
                        return (Status => Length_Limit);
                     end if;
                     Right_Length := Right_Data'Length + 1;
                  when Buffer_Input =>
                     if Right_Data'Length > Max_Result_Length - Left_Length then
                        return (Status => Length_Limit);
                     end if;
                     Right_Length := Right_Data'Length;
               end case;
            end if;
      end case;
      return Output : Result :=
        (Status => Built, Kind => Kind, Length => Left_Length + Right_Length,
         Data => [others => 0])
      do
         declare
            Position : Result_Length := 0;
            procedure Put (Value : Byte) with Pre => Position < Output.Length is
            begin
               Position := Position + 1;
               Output.Data (Position) := Value;
            end Put;
            procedure Put_Data (Data : Bytes; Stop_At_Zero : Boolean) is
            begin
               for I in 0 .. Data'Length - 1 loop
                  exit when Stop_At_Zero and then Data (Data'First + I) = 0;
                  Put (Data (Data'First + I));
               end loop;
            end Put_Data;
            procedure Put_Integer (Value : Integer_Value) is
            begin
               for I in 0 .. Integer_Bytes - 1 loop
                  Put (AML_Coercions.Octet (Value, I));
               end loop;
            end Put_Integer;
         begin
            case Left_Kind is
               when Integer_Input =>
                  Put_Integer (Left_Number);
                  Put_Integer (Converted_Right);
               when Buffer_Input =>
                  Put_Data (Left_Data, False);
                  case Right_Kind is
                     when Integer_Input => Put_Integer (Right_Number);
                     when String_Input => Put_Data (Right_Data, False); Put (0);
                     when Buffer_Input => Put_Data (Right_Data, False);
                  end case;
               when String_Input =>
                  Put_Data (Left_Data, True);
                  case Right_Kind is
                     when Integer_Input =>
                        Put_Data (AML_Coercions.Strings.From_Integer (Right_Number, Width), False);
                     when String_Input => Put_Data (Right_Data, True);
                     when Buffer_Input =>
                        for I in 0 .. Right_Data'Length - 1 loop
                           if I > 0 then Put (Character'Pos (' ')); end if;
                           Put (Character'Pos ('0')); Put (Character'Pos ('x'));
                           Put (AML_Coercions.Strings.Hex (Right_Data (Right_Data'First + I) / 16));
                           Put (AML_Coercions.Strings.Hex (Right_Data (Right_Data'First + I) mod 16));
                        end loop;
                  end case;
            end case;
         end;
      end return;
   end Build;
end AML_Concatenation;
