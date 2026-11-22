with Interfaces; use Interfaces;
package Intel_GPU_ADLN_LRC_Descriptor with SPARK_Mode is
   type EU_Priority is (Low, Normal, High);
   -- Numeric encoding only, for a driver-owned four-level PPGTT context.
   -- GPU_Base names the full context's HWSP, NOT its following register page.
   -- Caller must own/retain the complete GGTT mapping above the GuC pin bias,
   -- initialize the context and establish visibility before registration.
   -- Privileged descriptors MUST NOT expose arbitrary client command streams.
   function Admissible (GPU_Base, Image_Bytes : Unsigned_64) return Boolean is
     (GPU_Base /= 0 and then GPU_Base mod 4096 = 0 and then
      Image_Bytes /= 0 and then Image_Bytes mod 4096 = 0 and then
      GPU_Base < 16#FEE0_0000# and then
      Image_Bytes <= 16#FEE0_0000# - GPU_Base);
   function Encode (GPU_Base, Image_Bytes : Unsigned_64;
                    Priority : EU_Priority) return Unsigned_32
     with Post => (if not Admissible (GPU_Base, Image_Bytes)
                   then Encode'Result = 0);
end Intel_GPU_ADLN_LRC_Descriptor;
