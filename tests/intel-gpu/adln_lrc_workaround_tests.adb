with Interfaces; use Interfaces;
with Intel_GPU_ADLN_LRC_Workaround; use Intel_GPU_ADLN_LRC_Workaround;
with Ada.Text_IO;
procedure ADLN_LRC_Workaround_Tests is
   Expected : constant Batch_Words :=
     [16#14C80002#,16#600#,16#10108C#,0,
      16#150C0001#,16#600#,16#3A8#,16#150C0001#,16#600#,16#3A8#,
      16#14C80002#,16#600#,16#1012DC#,0,16#150C0001#,16#600#,16#84#,
      16#14C80002#,16#600#,16#1011D4#,0,
      16#11020001#,16#4208#,1,16#0E01C003#,0,16#4208#,0,0,
      16#11000001#,16#20D8#,16#00400040#];
   procedure Reject (Base, Capacity : Unsigned_64) is
      Result : constant Indirect_Batch := Build (Base, Capacity);
   begin
      pragma Assert (not Result.Valid);
      pragma Assert (for all Word of Result.Words => Word = 0);
   end Reject;
begin
   pragma Assert (Build (16#100000#, 65536).Words = Expected);
   pragma Assert (Build (16#FEDF0000#, 65536).Valid);
   for Offset in Unsigned_64 range 1 .. 4095 loop
      Reject (16#100000# + Offset, 65536);
   end loop;
   for Capacity in Unsigned_64 range 0 .. 65535 loop
      Reject (16#100000#, Capacity);
   end loop;
   Reject (0, 65536);
   Reject (16#FEDF1000#, 65536);
   Reject (Unsigned_64'Last, 65536);
   Ada.Text_IO.Put_Line ("ADL-N indirect context batch PASS (not hardware executed)");
end ADLN_LRC_Workaround_Tests;
