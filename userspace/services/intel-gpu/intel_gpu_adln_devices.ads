------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Alder Lake-N graphics family intel-gpu drives (HW-002).
--
--  @description
--  intel-gpu accepts the whole ADL-N/Twin Lake family its manifest declares
--  (drives => PCI_IDs 8086:46D0..46D4), not one machine's device: the N95
--  NUC is 46D2, the i3-N350 IdeaPad 46D4. Per-device facts are looked up
--  here, never pinned at a call site. The GT configuration follows Mesa's
--  PCI table (include/pci_ids/iris_pci_ids.h: 46D2 adl_gt05, the rest
--  adl_gt1). Topology (DSS and EU masks) is measured from the fuses at run
--  time, not taken from this table.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Intel_GPU_ADLN_Devices with SPARK_Mode, Pure is
   Intel_Vendor : constant Unsigned_16 := 16#8086#;
   First_Device : constant Unsigned_16 := 16#46D0#;
   Last_Device  : constant Unsigned_16 := 16#46D4#;
   subtype Device_ID is Unsigned_16 range First_Device .. Last_Device;

   --  Mesa's ADL GT configurations: GT0.5 (one half-populated slice) and GT1.
   type GT_Configuration is (GT0_5, GT1);

   --  Explicit text: the native runtime discards enumeration names.
   function GT_Name (GT : GT_Configuration) return String is
     (case GT is when GT0_5 => "GT0.5", when GT1 => "GT1");

   --  GuC firmware, per device. Linux i915 (v6.12) loads tgl_guc_70.bin on
   --  every ADL-N: gt/uc/intel_uc_fw.c lines 293-300, "ADL-N does not
   --  support HWConfig, so we should use the same binary as ADL-S, otherwise
   --  the GuC might attempt to fetch a config table that does not exist"
   --  (IS_ALDERLAKE_P_N covers INTEL_ADLN_IDS = 46D0..46D4,
   --  include/drm/intel/i915_pciids.h). adlp_guc_70.bin is for ADL-P only.
   type GuC_Firmware is (TGL_GuC_70);
   function GuC_File_Name (F : GuC_Firmware) return String is
     (case F is when TGL_GuC_70 => "tgl_guc_70.bin");
   --  Where devmgr's firmware read scope puts it (images/*.ccl "intel-guc").
   Firmware_Directory : constant String := "firmware/intel/";
   function GuC_Path (F : GuC_Firmware) return String is
     (Firmware_Directory & GuC_File_Name (F));

   type Device_Facts is record
      GT  : GT_Configuration;
      GuC : GuC_Firmware;
   end record;

   Facts : constant array (Device_ID) of Device_Facts :=
     [16#46D2# => (GT => GT0_5, GuC => TGL_GuC_70),
      others   => (GT => GT1,   GuC => TGL_GuC_70)];

   function Supported (Vendor, Device : Unsigned_16) return Boolean is
     (Vendor = Intel_Vendor and then Device in Device_ID);

   function Supported_Device (Device : Unsigned_16) return Boolean is
     (Device in Device_ID);

   --  PCI configuration dword 0: vendor in the low half, device in the high.
   function Supported_Identity (Word : Unsigned_32) return Boolean is
     (Supported (Unsigned_16 (Word and 16#FFFF#), Unsigned_16 (Shift_Right (Word, 16))));
end Intel_GPU_ADLN_Devices;
