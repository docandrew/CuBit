with Interfaces;
package Intel_GPU_GuC_Parameters with SPARK_Mode is
   use Interfaces;
   -- Runtime GuC resources cannot use the upper GGTT firmware-upload region.
   -- Exclusive ceiling from i915 intel_guc.h GUC_GGTT_TOP.
   Runtime_GGTT_Limit : constant Unsigned_64 := 16#FEE0_0000#;
   -- Distinct from CPU virtual/physical addresses. Construction checks numeric
   -- encoding only; it does NOT establish reservation, mapping or ownership.
   type GPU_Page_Address is private;
   function Address (GPU_Byte_Offset : Unsigned_64) return GPU_Page_Address;
   function Valid (Value : GPU_Page_Address) return Boolean;

   subtype Small_Count is Natural range 0 .. 3;
   subtype Debug_Count is Natural range 0 .. 15;
   type Log_Configuration is record
      -- Counts are ABI-encoded size fields, not allocation byte lengths.
      -- The caller must allocate/initialize matching firmware log sections.
      Base : GPU_Page_Address;
      -- Trusted mapped extent size, not the number of bytes currently used.
      -- This claim must be checked against retained GPU mapping ownership by
      -- the native caller; the pure encoder cannot consult that registry.
      Backing_Bytes : Unsigned_64;
      Notify_Half_Full, Capture_Megabyte_Units, Log_Megabyte_Units : Boolean;
      Crash_Count, Capture_Count : Small_Count;
      Debug_Pages_Count : Debug_Count;
   end record;
   function Required_Log_Bytes (Config : Log_Configuration) return Unsigned_64
   with Global => null,
     Post => Required_Log_Bytes'Result in 16_384 .. 25_169_920
       and then Required_Log_Bytes'Result mod 4096 = 0;
   type Configuration is record
      -- Trusted lower bound derived from the admitted WOPCM/platform layout,
      -- not guessed by this encoder. Zero/unaligned bounds are rejected.
      Pin_Bias : Unsigned_64;
      ADS : GPU_Page_Address;
      ADS_Backing_Bytes : Unsigned_64;
      Log : Log_Configuration;
      -- Actual PCI identity from the authenticated bootstrap. Encoding a
      -- family member does not authorize reset/submission on that device.
      Device : Unsigned_16;
      Revision : Unsigned_8;
      Scheduler_Enabled, SLPC_Enabled, PXP_Enabled : Boolean;
      Logging_Enabled : Boolean;
      Verbosity : Small_Count;
      -- These flags are supplied after platform/firmware workaround selection,
      -- not inferred from arbitrary PCI revision numbers by this encoder.
      Workarounds : Unsigned_32;
   end record;
   type Startup_Words is array (Natural range 0 .. 13) of Unsigned_32;
   type Parameter_Block is private;
   function Encode_ADLN (Config : Configuration) return Parameter_Block;
   function Valid (Value : Parameter_Block) return Boolean;
   function Words (Value : Parameter_Block) return Startup_Words;
private
   type GPU_Page_Address is record
      Present : Boolean := False;
      Page : Unsigned_32 := 0;
   end record;
   type Parameter_Block is record
      Present : Boolean := False;
      Data : Startup_Words := [others => 0];
   end record;
end Intel_GPU_GuC_Parameters;
