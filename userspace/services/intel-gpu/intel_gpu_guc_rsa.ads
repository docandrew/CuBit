with Interfaces;
with Intel_GPU_Firmware;
-- ADL-N's selected firmware uses the 256-byte MMIO signature path.
-- This supplies signature bytes to the GPU; it does NOT verify them.
generic
   -- Source must remain immutable throughout execution. Bounded/nonraising
   -- callbacks, with retained source ownership and exclusive forcewake/MMIO
   -- ownership established by caller. Failure may follow a posted write.
   with procedure Read_Byte
     (Offset : Interfaces.Unsigned_64; Value : out Interfaces.Unsigned_8;
      Success : out Boolean);
   with procedure Write32
     (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
package Intel_GPU_GuC_RSA is
   type Phase is (Fresh, Consumed, Quarantined, Supplied);
   type Attempt is limited private;
   function Current (Object : Attempt) return Phase;
   type Result is (Rejected, Source_Failed, Write_Failed, Complete);
   -- Header must describe the same stable blob used by Read_Byte. Only the
   -- pinned ADL-N format is admitted; identity/version checks are not trust.
   procedure Execute
     (Object : in out Attempt; Header : Intel_GPU_Firmware.CSS_Header;
      Blob_Bytes : Interfaces.Unsigned_64; Status : out Result);
private
   type Attempt is limited record
      Value : Phase := Fresh;
   end record;
end Intel_GPU_GuC_RSA;
