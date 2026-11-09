--  SPARK stand-ins for the detection's formals, so gnatprove analyzes an
--  instance of Intel_GPU_Port_Scan (the sequence's postcondition for any
--  register values and any DDC read outcome).
with Interfaces; use Interfaces;
with CuBit.Monitor_EDID.Catalog;
with Intel_GPU_Port_Detection;
package Detection_Proof_IO with SPARK_Mode is
   function Read_MMIO (Offset : Unsigned_32) return Unsigned_32 with Import, Global => null;
   function Is_Lit (Item : Intel_GPU_Port_Detection.Port) return Boolean with Import, Global => null;
   procedure Read_DDC_Base
     (Item : Intel_GPU_Port_Detection.Port; Base : out CuBit.Monitor_EDID.Catalog.Block;
      Outcome : out Intel_GPU_Port_Detection.EDID_Outcome)
     with Import, Global => null, Always_Terminates;
end Detection_Proof_IO;
