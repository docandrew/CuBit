package body Intel_GPU_Plane_Registers with SPARK_Mode is
   use Interfaces;
   use Intel_GPU_Display_Topology;
   function Select_Register
     (Pipe : Intel_GPU_Display_Topology.Pipe; Plane : Plane_Number;
      Item : Field) return Selection
   is
      Base : constant array (Field) of Unsigned_32 :=
        [16#70180#, 16#70188#, 16#70190#, 16#701A4#, 16#7019C#, 16#701AC#];
   begin
      return (True, Base (Item) + Unsigned_32 (Plane - 1) * 16#100# +
              Unsigned_32 (Intel_GPU_Display_Topology.Pipe'Pos (Pipe)) * 16#1000#);
   end Select_Register;
end Intel_GPU_Plane_Registers;
