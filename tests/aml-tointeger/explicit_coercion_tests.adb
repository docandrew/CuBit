with Ada.Text_IO;
with AML_Coercions;
with AML_Decode;
procedure Explicit_Coercion_Tests is
   use AML_Decode;
   use type Integer_Value;
   use type AML_Coercions.Conversion_Status;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Natural'Image (Checks); end if;
   end Check;
   procedure Test (Text : String; Width : Integer_Width; Expected : Integer_Value) is
      Data : Bytes (Natural'Last - Text'Length + 1 .. Natural'Last);
      R : AML_Coercions.Result;
   begin
      for I in Text'Range loop
         Data (Data'First + (I - Text'First)) := Character'Pos (Text (I));
      end loop;
      R := AML_Coercions.From_Explicit_String (Data, Width);
      Check (R.Status = AML_Coercions.Converted and then R.Value = Expected);
   end Test;
begin
   for Width in Integer_Width loop
      Test ("10", Width, 10);
      Test ("0x10", Width, 16);
      Test ("0X10", Width, 16);
      Test ("  00010", Width, 10);
      Test ("+10", Width, 0);
      Test ("-10", Width, 0);
      Test ("12xyz", Width, 12);
      Test ("0x", Width, 0);
      Test ("12" & Character'Val (0) & "34", Width, 12);
      Test ("4294967295", Width, 4_294_967_295);
      Test ("4294967296", Width, (if Width = Bits_32 then 429_496_729 else 4_294_967_296));
      Test ("18446744073709551616", Width,
        (if Width = Bits_32 then 1_844_674_407 else 1_844_674_407_370_955_161));
      Check (AML_Coercions.From_Explicit_String (Bytes'(1 .. 0 => 0), Width).Value = 0);
      Check (AML_Coercions.From_String (Bytes'(49,48), Width).Value = 16);
   end loop;
   Ada.Text_IO.Put_Line ("explicit coercion checks" & Natural'Image (Checks));
end Explicit_Coercion_Tests;
