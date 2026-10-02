with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Settings;
procedure ADLN_Context_Settings_Tests is
   package Settings renames Intel_GPU_ADLN_Context_Settings;
   use type Settings.Segment_Words;
begin
   for Bit in Natural range 0 .. 31 loop
      declare
         Previous : constant Unsigned_32 := Shift_Left (Unsigned_32 (1), Bit);
         Value : constant Settings.Segment := Settings.Build (True, Previous);
      begin
         pragma Assert (Value.Valid and Value.Words =
           [16#1100000B#, 16#2580#, 16#00060002#,
            16#5584#, Previous or 16#20#, 16#6604#, 16#E0040000#,
            16#7018#, 16#20002000#, 16#7300#, 16#00400040#,
            16#7304#, 16#02000200#, 0]);
      end;
   end loop;
   for Read_Valid in Boolean loop
      declare Value : constant Settings.Segment := Settings.Build (Read_Valid, Unsigned_32'Last); begin
         pragma Assert (not Value.Valid and Value.Words = [0 .. 13 => 0]);
      end;
   end loop;
   declare
      Invalid : constant Settings.Segment := Settings.Build (False, 0);
      Valid : constant Settings.Segment := Settings.Build (True, 0);
   begin
      pragma Assert (not Invalid.Valid and Invalid.Words = [0 .. 13 => 0]);
      pragma Assert (Valid.Valid and Valid.Words (4) = 16#20#);
   end;
end ADLN_Context_Settings_Tests;
