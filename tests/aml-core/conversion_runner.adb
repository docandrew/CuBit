pragma Ada_2022;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Coercions.Strings;
procedure Conversion_Runner is
   package S renames AML_Coercions.Strings;
   procedure Print (Data : Bytes) is
   begin
      for B of Data loop Ada.Text_IO.Put (Character'Val (B)); end loop;
      Ada.Text_IO.New_Line;
   end Print;
begin
   if Argument (1) = "integer" then
      Print (S.From_Integer (Integer_Value'Value (Argument (3)),
        (if Argument (2) = "32" then Bits_32 else Bits_64)));
   else
      declare
         Data : Bytes (1 .. Argument_Count - 1);
      begin
         for I in Data'Range loop Data (I) := Byte'Value (Argument (I + 1)); end loop;
         Print (S.From_Buffer (Data));
      end;
   end if;
end Conversion_Runner;
