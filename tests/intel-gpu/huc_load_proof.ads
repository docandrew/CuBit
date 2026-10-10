with Interfaces; use Interfaces;
with Intel_GPU_HuC_Registers; use Intel_GPU_HuC_Registers;
with Intel_GPU_HuC_Load;
-- Proof-only hardware boundary for Intel_GPU_HuC_Load. Imported callbacks
-- return arbitrary values: register contents, success flags, clock values
-- and GuC replies are NOT assumed. Global => null models callbacks that do
-- not touch the loader object; real MMIO side effects, DMA and GuC behaviour
-- are covered by the hosted mock tests, not by this model. Callback
-- termination (Always_Terminates) is an external assumption.
package HuC_Load_Proof with SPARK_Mode,
  Abstract_State => (Clock with External => Async_Writers)
is
   procedure Read32 (Offset : Register_Offset; Value : out Register_Word;
                     Success : out Boolean) with Import, Global => null, Always_Terminates;
   procedure Write32 (Offset : Register_Offset; Value : Register_Word;
                      Success : out Boolean) with Import, Global => null, Always_Terminates;
   -- Volatile: successive reads may differ (no determinism assumed).
   function Now return Unsigned_64
     with Import, Volatile_Function, Global => (Input => Clock);
   procedure Pause with Import, Global => null, Always_Terminates;
   procedure Request_Authentication (RSA_GGTT : Unsigned_32;
                                     Reply : out Auth_Reply)
     with Import, Global => null, Always_Terminates;
   package Loader is new Intel_GPU_HuC_Load
     (Read32, Write32, Now, Pause, Request_Authentication);
end HuC_Load_Proof;
