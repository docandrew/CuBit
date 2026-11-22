with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Offscreen_Surface; use Intel_GPU_ADLN_Offscreen_Surface;
procedure Offscreen_Surface_Tests is
   function Decode_0 is new Ada.Unchecked_Conversion (Unsigned_32, DW0_Fields);
   function Decode_1 is new Ada.Unchecked_Conversion (Unsigned_32, DW1_Fields);
   function Decode_2 is new Ada.Unchecked_Conversion (Unsigned_32, DW2_Fields);
   function Decode_3 is new Ada.Unchecked_Conversion (Unsigned_32, DW3_Fields);
   function Decode_4 is new Ada.Unchecked_Conversion (Unsigned_32, DW4_Fields);
   function Decode_5 is new Ada.Unchecked_Conversion (Unsigned_32, DW5_Fields);
   function Decode_6 is new Ada.Unchecked_Conversion (Unsigned_32, DW6_Fields);
   function Decode_7 is new Ada.Unchecked_Conversion (Unsigned_32, DW7_Fields);
   function Decode_Binding is new Ada.Unchecked_Conversion
     (Unsigned_32, Binding_Entry);
   Expected : State_Words := [16#23014000#,16#82000010#,16#003F003F#,255,
     0,256,0,16#09770000#,16#00202000#,0,0,0,0,0,0,0];
   Result : Image;
   Word : Unsigned_32;
begin
   for I in 0 .. 31 loop
      Word := Shift_Left (1, I);
      pragma Assert (Encode (Decode_0 (Word)) = Word);
      pragma Assert (Encode (Decode_1 (Word)) = Word);
      pragma Assert (Encode (Decode_2 (Word)) = Word);
      pragma Assert (Encode (Decode_3 (Word)) = Word);
      pragma Assert (Encode (Decode_4 (Word)) = Word);
      pragma Assert (Encode (Decode_5 (Word)) = Word);
      pragma Assert (Encode (Decode_6 (Word)) = Word);
      pragma Assert (Encode (Decode_7 (Word)) = Word);
      pragma Assert (Encode_Binding (Decode_Binding (Word)) = Word);
   end loop;
   pragma Assert (Encode_Binding (Binding_Entry'(others => <>)) = 64);
   for MOCS in Unsigned_32 range 0 .. 255 loop
      Result := Build (MOCS);
      pragma Assert (Result.Valid = (MOCS > 0 and MOCS <= 126 and MOCS mod 2 = 0));
      if Result.Valid then
         Expected (1) := 16#80000010# or Shift_Left (MOCS, 24);
         pragma Assert (Result.Words = Expected);
      else
         pragma Assert (for all W of Result.Words => W = 0);
      end if;
   end loop;
   pragma Assert (not Build (Unsigned_32'Last).Valid);
   Ada.Text_IO.Put_Line ("Offscreen surface PASS: 288 record bits, binding offset, Mesa fixture, MOCS rejection; NOT submitted");
end Offscreen_Surface_Tests;
