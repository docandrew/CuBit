with Interfaces; use Interfaces;
with Intel_GPU_ADLN_LRC_Initial; use Intel_GPU_ADLN_LRC_Initial;
with Intel_GPU_ADLN_LRC_Template;
with Ada.Text_IO;
procedure ADLN_LRC_Initial_Tests is
   Template : constant Intel_GPU_ADLN_LRC_Template.Register_Page :=
     Intel_GPU_ADLN_LRC_Template.Build;
   procedure Rejected (Ring_GPU, Root_DMA : Unsigned_64;
                       Size : Ring_Size_Log2 := 12) is
      State : constant Initial_State := Build (Ring_GPU, Root_DMA, Size);
   begin
      pragma Assert (not State.Prepared);
      pragma Assert (for all Word of State.Registers => Word = 0);
   end Rejected;
begin
   for Size in Ring_Size_Log2 loop
      declare
         Base : constant Unsigned_64 := 16#FEE0_0000# - 2 ** Size;
         State : constant Initial_State := Build (Base, 16#FFFFF000#, Size);
      begin
         pragma Assert (State.Prepared);
         for I in State.Registers'Range loop
            pragma Assert (State.Registers (I) =
              (case I is
                 when 3 => 16#90009#, when 9 => Unsigned_32 (Base),
                 when 11 => Unsigned_32 (2 ** Size - 4096 + 1),
                 when 51 => 16#FFFFF000#, when 67 => 16#80041000#,
                 when 97 => 16#01000000#, when others => Template (I)));
         end loop;
         Rejected (Base + 4096, 4096, Size);
      end;
   end loop;
   for Offset in Unsigned_64 range 1 .. 4095 loop
      Rejected (4096 + Offset, 4096);
      Rejected (4096, 4096 + Offset);
   end loop;
   Rejected (0, 4096);
   Rejected (4096, 0);
   Rejected (4096, 2 ** 32);
   Rejected (Unsigned_64'Last, 4096);
   Rejected (4096, Unsigned_64'Last);
   Ada.Text_IO.Put_Line ("ADL-N initial register values PASS (workarounds pending)");
end ADLN_LRC_Initial_Tests;
