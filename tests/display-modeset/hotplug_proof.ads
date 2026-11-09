--  SPARK stand-ins for the hot-plug registers, so gnatprove analyzes an
--  instance of Intel_GPU_Hotplug_IRQ (absence of run-time errors for any
--  register values) next to the pure debounce unit.
with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
with Intel_GPU_Hotplug_IRQ;
package Hotplug_Proof with SPARK_Mode is
   procedure Read (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : out Unsigned_32)
     with Import, Global => null, Always_Terminates;
   procedure Write (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : Unsigned_32)
     with Import, Global => null, Always_Terminates;
   package Instance is new Intel_GPU_Hotplug_IRQ (Read, Write);
end Hotplug_Proof;
