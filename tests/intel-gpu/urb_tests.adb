with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_URB; use Intel_GPU_ADLN_URB;
procedure URB_Tests is
   function To_Record is new Ada.Unchecked_Conversion (Unsigned_32, Stage_Control);
   R : Image;
begin
   for Bit in 0 .. 31 loop
      declare V : constant Unsigned_32 := Shift_Left (1, Bit); begin
         pragma Assert (Encode (To_Record (V)) = V);
      end;
   end loop;
   for Capacity in 0 .. 1024 loop
      R := Build (Capacity);
      pragma Assert (R.Valid = (Capacity in 40 .. 512));
      if R.Valid then
         pragma Assert (R.VS_Entries mod 8 = 0);
         pragma Assert (32768 + R.VS_Entries * 64 <= Capacity * 1024);
         for Stage in Natural range 0 .. 3 loop
            pragma Assert (R.Data (Stage * 2) = 16#78300000# + Shift_Left (Unsigned_32 (Stage), 16));
            pragma Assert (R.Data (Stage * 2 + 1) =
              16#08000000# + (if Stage = 0 then Unsigned_32 (R.VS_Entries) else 0));
         end loop;
      else
         pragma Assert (R.Data = Words'(others => 0) and R.VS_Entries = 0);
      end if;
   end loop;
   R := Build (512);
   pragma Assert (R.Data = Words'
     [16#78300000#, 16#08000DF8#, 16#78310000#, 16#08000000#,
      16#78320000#, 16#08000000#, 16#78330000#, 16#08000000#]);
   R := Build (40);
   pragma Assert (R.VS_Entries = 128);
   R := Build (Natural'Last);
   pragma Assert (not R.Valid);
   Put_Line ("URB PASS: 32 representation bits, capacity sweep, fixed VS-only layout");
end URB_Tests;
