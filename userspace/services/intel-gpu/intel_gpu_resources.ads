with Interfaces; use Interfaces;
with Intel_GPU_Probe;

-- Pure planning only: this neither mints authority nor maps physical memory.
-- Resource_Bytes must come from trusted resource discovery for this same BAR.
package Intel_GPU_Resources with SPARK_Mode is
   use type Intel_GPU_Probe.Platform;
   Page_Bytes : constant Unsigned_64 := 4096;
   type Admission_Status is
     (Admitted, Unknown_Platform, Memory_Decode_Disabled, Invalid_BAR,
      Wrong_Cache_Class, Unknown_Extent, Invalid_Alignment,
      Address_Overflow, Outside_Resource);
   type Mapping_Plan is record
      Status : Admission_Status := Invalid_BAR;
      Physical_Base : Unsigned_64 := 0;
      Bytes : Unsigned_64 := 0;
   end record;

   -- A non-prefetchable register BAR, not the framebuffer aperture. Never
   -- round an unaligned request outward into pages outside the resource.
   -- The future adapter must request read-only, uncached device mappings and
   -- independently check owner/lifetime and register power/access semantics.
   function Plan_Registers
     (Hardware : Intel_GPU_Probe.Platform;
      Command : Unsigned_16; BAR : Intel_GPU_Probe.BAR_Result;
      Resource_Bytes, Offset, Bytes : Unsigned_64) return Mapping_Plan
   with Global => null,
     Post =>
       (if Plan_Registers'Result.Status = Admitted then
          Plan_Registers'Result.Physical_Base /= 0
          and then Plan_Registers'Result.Physical_Base mod Page_Bytes = 0
          and then Plan_Registers'Result.Bytes = Bytes
          and then Bytes > 0 and then Bytes mod Page_Bytes = 0
          and then Offset <= Resource_Bytes
          and then Bytes <= Resource_Bytes - Offset
          and then Plan_Registers'Result.Physical_Base >= BAR.Base
          and then Plan_Registers'Result.Physical_Base - BAR.Base = Offset
          and then Bytes - 1 <= Unsigned_64'Last - Plan_Registers'Result.Physical_Base
        else Plan_Registers'Result.Physical_Base = 0
          and then Plan_Registers'Result.Bytes = 0);
   -- Alder Lake-N GTTMMADR: 16 MiB BAR, first 2 MiB registers, then
   -- 6 MiB reserved, then 8 MiB GGTT. This path cannot map GGTT as registers.
   -- Exact platform whitelist; Kaby Lake requires its own reference validation.
   function Plan_ADLN_Registers
     (Hardware : Intel_GPU_Probe.Platform; Command : Unsigned_16;
      Low, High : Unsigned_32; Offset, Bytes : Unsigned_64) return Mapping_Plan
   with Global => null,
     Post =>
       (if Plan_ADLN_Registers'Result.Status = Admitted then
          Hardware = Intel_GPU_Probe.Alder_Lake_N
          and then Offset < 16#20_0000#
          and then Bytes > 0 and then Bytes <= 16#20_0000# - Offset
          and then Plan_ADLN_Registers'Result.Bytes = Bytes
          and then Plan_ADLN_Registers'Result.Physical_Base mod Page_Bytes = 0
        else Plan_ADLN_Registers'Result.Physical_Base = 0
          and then Plan_ADLN_Registers'Result.Bytes = 0);
end Intel_GPU_Resources;
