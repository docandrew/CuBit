--  SPARK stand-ins for the GMBUS registers and clock, so gnatprove analyzes
--  an instance of Intel_GPU_GMBUS (absence of run-time errors, termination
--  of every wait). The values are unconstrained inputs.
with Interfaces; use Interfaces;
with Intel_GPU_Display_Registers;
package GMBUS_Proof_IO with SPARK_Mode, Abstract_State => (Hardware with External => Async_Writers) is
   procedure Read (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : out Unsigned_32)
     with Global => (Input => Hardware);
   procedure Write
     (Offset : Intel_GPU_Display_Registers.Register_Offset; Value : Unsigned_32; Written : out Boolean)
     with Global => (Input => Hardware);
   procedure Now_Us (Value : out Unsigned_64) with Global => (Input => Hardware);
end GMBUS_Proof_IO;
