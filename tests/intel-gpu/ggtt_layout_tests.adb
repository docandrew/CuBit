with Ada.Text_IO; with Interfaces; with Intel_GPU_GGTT_Layout;
procedure GGTT_Layout_Tests is
   use Interfaces; use Intel_GPU_GGTT_Layout;
   Tables : constant array (1 .. 3) of Unsigned_64 := [2_097_152, 4_194_304, 8_388_608];
   -- Literal reference boundaries, not the implementation's formula.
   Splits : constant array (1 .. 3) of Unsigned_64 := [16#3EE00000#, 16#7EE00000#, 16#FEE00000#];
   Guards : constant array (1 .. 3) of Unsigned_64 := [16#3FFFF000#, 16#7FFFF000#, 16#FFFFF000#];
   R : Layout;
begin
   for I in Tables'Range loop
      for Bias in Unsigned_64 range 0 .. 8193 loop
         R := Plan (Tables (I), Bias);
         pragma Assert (R.Valid and R.Runtime_First >= Bias);
         pragma Assert (R.Runtime_First in 4096 | 8192 | 12288);
         pragma Assert (R.Runtime_First - Bias < 4096 or else Bias = 0);
         pragma Assert (R.Runtime_Limit = Splits (I) and R.Upload_First = Splits (I));
         pragma Assert (R.Upload_Limit = Guards (I) and R.Guard_First = Guards (I));
      end loop;
      R := Plan (Tables (I), Splits (I) - 4096);
      pragma Assert (R.Valid and R.Runtime_Limit - R.Runtime_First = 4096);
      for Delta_Byte in Unsigned_64 range 0 .. 4095 loop
         pragma Assert (not Plan (Tables (I), Splits (I) - Delta_Byte).Valid);
      end loop;
      pragma Assert (not Plan (Tables (I), Unsigned_64'Last).Valid);
   end loop;
   pragma Assert (not Plan (0, 0).Valid);
   pragma Assert (not Plan (8_388_609, 0).Valid);
   pragma Assert (not Plan (Unsigned_64'Last, 0).Valid);
   Ada.Text_IO.Put_Line ("GGTT placement PASS: three table sizes, bias rounding, top reservation and end guard");
end GGTT_Layout_Tests;
