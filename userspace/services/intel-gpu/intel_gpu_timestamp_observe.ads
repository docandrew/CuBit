with Interfaces;
generic
   -- Caller enforces device identity, MMIO bounds, power and exclusive clock
   -- ownership. A successful load alone is not evidence of valid hardware.
   with procedure Read (Offset : Interfaces.Unsigned_32;
                        Value : out Interfaces.Unsigned_32;
                        Success : out Boolean);
package Intel_GPU_Timestamp_Observe with SPARK_Mode is
   type Outcome is (Ready, Read_Failed, Changing, Reserved_Selector);
   procedure Sample (Hz : out Interfaces.Unsigned_32; Status : out Outcome);
   -- Two bounded samples, selected fields only. No writes, retries, forcewake
   -- acquisition or claims that this frequency remains valid after release.
end Intel_GPU_Timestamp_Observe;
