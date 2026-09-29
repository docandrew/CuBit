with Interfaces;
package Intel_GPU_GuC_Status with SPARK_Mode is
   type State is (Pending, Invalid_MMIO, Authentication_Failed,
                  Bootrom_Failed, Firmware_Failed, Ready);
   function Decode (Value : Interfaces.Unsigned_32) return State
     with Global => null;
   -- Ready requires firmware F0, authentication GOOD, and MIA out of reset.
   -- Unknown/transitional firmware codes never establish readiness. Raw evidence must
   -- be retained by the caller; these categories do not identify every error.
end Intel_GPU_GuC_Status;
