with Interfaces; use Interfaces;
with Intel_GPU_Submission_Backing; use Intel_GPU_Submission_Backing;
with Ada.Text_IO;
procedure Submission_Backing_Tests is
   Cursor : Unsigned_64 := 16#8C000#;
begin
   pragma Assert (Valid_Layout);
   for R in Region loop
      pragma Assert (Offsets (R) = Cursor);
      Cursor := Cursor + Sizes (R);
   end loop;
   pragma Assert (Cursor = After_Last and Cursor <= 1024 * 1024);
   pragma Assert (Sizes (Context_Image) = 16 * 4096);
   pragma Assert (Sizes (Command_Ring) = 2 ** 14);
   pragma Assert (for all R in PML4 .. Engine_Status_Page => Sizes (R) = 4096);
   pragma Assert (Offsets (Engine_Status_Page) = 16#A6000#);
   pragma Assert (16#84000# + 32768 = First);
   Ada.Text_IO.Put_Line ("Submission backing layout PASS: contiguous, disjoint retained slices");
end Submission_Backing_Tests;
