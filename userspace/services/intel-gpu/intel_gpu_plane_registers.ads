with Interfaces;
with Intel_GPU_Display_Topology;
package Intel_GPU_Plane_Registers with SPARK_Mode is
   use type Interfaces.Unsigned_32;
   -- ADL-N display version 13: primary plane and four sprite planes.
   -- Hardware-numbered 1..5, unlike Linux's zero-based enum plane_id.
   subtype Plane_Number is Positive range 1 .. 5;
   type Field is (Control, Stride, Size, Offset, Surface, Live_Surface);
   type Selection is record
      Valid : Boolean := False;
      Register_Offset : Interfaces.Unsigned_32 := 0;
   end record;
   -- Address selection is not authorization. Native readers require the
   -- corresponding pipe's retained power reference before touching MMIO.
   function Select_Register
     (Pipe : Intel_GPU_Display_Topology.Pipe; Plane : Plane_Number;
      Item : Field) return Selection
   with Post =>
     (if Select_Register'Result.Valid then
        Select_Register'Result.Register_Offset in 16#70180# .. 16#735AC#
      else Select_Register'Result.Register_Offset = 0);
end Intel_GPU_Plane_Registers;
