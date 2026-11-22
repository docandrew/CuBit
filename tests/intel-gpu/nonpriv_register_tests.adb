with Interfaces; use Interfaces;
with Intel_GPU_Nonpriv_Registers; use Intel_GPU_Nonpriv_Registers;
procedure Nonpriv_Register_Tests is
   A : Register_Value := (Address_DWords => 16#2690# / 4, others => <>);
   B : Register_Value;
begin
   declare
      Expected : constant array (Documented_RCS_Slot) of Unsigned_32 :=
        [16#24D0#, 16#24D4#, 16#24D8#, 16#24DC#,
         16#24E0#, 16#24E4#, 16#24E8#, 16#24EC#,
         16#24F0#, 16#24F4#, 16#24F8#, 16#24FC#,
         16#2010#, 16#2014#, 16#2018#, 16#201C#,
         16#21E0#, 16#21E4#, 16#21E8#, 16#21EC#];
   begin
      for Slot in Documented_RCS_Slot loop
         pragma Assert (Documented_RCS_Offset (Slot) = Expected (Slot));
         for Other in Documented_RCS_Slot loop
            pragma Assert (Slot = Other or else
              Documented_RCS_Offset (Slot) /= Documented_RCS_Offset (Other));
         end loop;
      end loop;
   end;
   for I in 0 .. 31 loop
      declare
         Raw : constant Unsigned_32 := Shift_Left (Unsigned_32 (1), I);
      begin
         pragma Assert (Encode (Decode (Raw)) = Raw);
      end;
   end loop;
   pragma Assert (Encode (Decode (Unsigned_32'Last)) = Unsigned_32'Last);
   pragma Assert (Evaluate ([A], 16#2690#, Read_Register) = Allow);
   pragma Assert (Evaluate ([A], 16#2694#, Read_Register) = Unspecified);
   for Range_Code in Bits_2 loop
      A.Offset_Range := Range_Code;
      declare
         Bytes : constant Unsigned_32 := 4 * 4 ** Natural (Range_Code);
         First : constant Unsigned_32 := 16#2690# / Bytes * Bytes;
      begin
         for N in 0 .. Bytes / 4 - 1 loop
            pragma Assert (Evaluate ([A], First + 4 * N, Write_Register) = Allow);
         end loop;
         pragma Assert (Evaluate ([A], First - 4, Write_Register) = Unspecified);
         pragma Assert (Evaluate ([A], First + Bytes, Write_Register) = Unspecified);
      end;
   end loop;
   A.Offset_Range := 0;
   for Selection in Bits_2 range 0 .. 2 loop
      A.Access_Selection := Selection;
      pragma Assert (Evaluate ([A], 16#2690#, Read_Register) =
        (if Selection = 2 then Unspecified else Allow));
      pragma Assert (Evaluate ([A], 16#2690#, Write_Register) =
        (if Selection = 1 then Unspecified else Allow));
   end loop;
   A.Access_Selection := 0;
   B := A; B.Denylist := 1; B.Access_Selection := 2;
   pragma Assert (Evaluate ([A, B], 16#2690#, Write_Register) = Deny);
   pragma Assert (Evaluate ([B, A], 16#2690#, Write_Register) = Deny);
   pragma Assert (Evaluate ([A, B], 16#2690#, Read_Register) = Allow);
   B.Reserved := 1;
   pragma Assert (Evaluate ([A, B], 16#2690#, Write_Register) = Invalid);
   B.Reserved := 0; B.Access_Selection := 3;
   pragma Assert (Evaluate ([A, B], 16#2690#, Read_Register) = Invalid);
   B.Access_Selection := 0; B.Virtual_Function := 1;
   pragma Assert (Evaluate ([A, B], 16#2690#, Read_Register) = Invalid);
   pragma Assert (Evaluate ([A], 16#2691#, Read_Register) = Invalid);
   pragma Assert (Evaluate ([A], 2 ** 26, Read_Register) = Invalid);
   pragma Assert (Evaluate (Register_List'(1 .. 0 => A), 0, Read_Register) = Unspecified);
end Nonpriv_Register_Tests;
