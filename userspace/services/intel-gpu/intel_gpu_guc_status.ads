------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  GUC_STATUS (MMIO 0xC000): fields, codes and readiness.
--
--  @description
--  Layout and codes follow Linux i915 (v6.12): gt/uc/intel_guc_reg.h
--  (GS_RESET_SHIFT .. GS_AUTH_STATUS_GOOD), gt/uc/abi/guc_errors_abi.h
--  (enum intel_guc_load_status, enum intel_bootrom_load_status) and
--  gt/uc/intel_guc_fw.c guc_load_done(), which says which codes are fatal.
------------------------------------------------------------------------------
with Interfaces;
package Intel_GPU_GuC_Status with SPARK_Mode is
   use Interfaces;
   Register_Offset : constant Unsigned_32 := 16#C000#;

   type State is (Pending, Invalid_MMIO, Authentication_Failed,
                  Bootrom_Failed, Firmware_Failed, Ready);
   function Decode (Value : Unsigned_32) return State
     with Global => null;
   -- Ready requires firmware F0, authentication GOOD, and MIA out of reset.
   -- Unknown/transitional firmware codes never establish readiness. Raw evidence must
   -- be retained by the caller; these categories do not identify every error.

   --  GS_* field positions.
   Reset_Bit       : constant Unsigned_32 := 16#0000_0001#; --  GS_MIA_IN_RESET
   Bootrom_Shift   : constant := 1;
   Bootrom_Mask    : constant Unsigned_32 := 16#7F#;
   Ukernel_Shift   : constant := 8;
   Ukernel_Mask    : constant Unsigned_32 := 16#FF#;
   MIA_Shift       : constant := 16;
   MIA_Mask        : constant Unsigned_32 := 16#07#;
   Auth_Shift      : constant := 30;

   subtype Bootrom_Code is Unsigned_32 range 0 .. Bootrom_Mask;
   subtype Ukernel_Code is Unsigned_32 range 0 .. Ukernel_Mask;
   subtype MIA_Code is Unsigned_32 range 0 .. MIA_Mask;
   --  GS_AUTH_STATUS: 0 not yet checked, 1 BAD, 2 GOOD, 3 undefined.
   type Authentication is (Auth_Unchecked, Auth_Bad, Auth_Good, Auth_Undefined);

   --  enum intel_bootrom_load_status.
   Bootrom_No_Key_Found              : constant Bootrom_Code := 16#13#;
   Bootrom_AES_Prod_Key_Found        : constant Bootrom_Code := 16#1A#;
   Bootrom_Prod_Key_Check_Failure    : constant Bootrom_Code := 16#2B#;
   Bootrom_RSA_Failed                : constant Bootrom_Code := 16#50#;
   Bootrom_PAVPC_Failed              : constant Bootrom_Code := 16#73#;
   Bootrom_WOPCM_Failed              : constant Bootrom_Code := 16#74#;
   Bootrom_Loadloc_Failed            : constant Bootrom_Code := 16#75#;
   Bootrom_Jump_Passed               : constant Bootrom_Code := 16#76#;
   Bootrom_Jump_Failed               : constant Bootrom_Code := 16#77#;
   Bootrom_RC6_Context_Config_Failed : constant Bootrom_Code := 16#79#;
   Bootrom_MPU_Map_Incorrect         : constant Bootrom_Code := 16#7A#;
   Bootrom_Exception                 : constant Bootrom_Code := 16#7E#;

   --  enum intel_guc_load_status.
   Load_Default                       : constant Ukernel_Code := 16#00#;
   Load_Start                         : constant Ukernel_Code := 16#01#;
   Load_DevID_Build_Mismatch          : constant Ukernel_Code := 16#02#;
   Load_Preprod_Build_Mismatch        : constant Ukernel_Code := 16#03#;
   Load_DevID_Invalid_GuC_Type        : constant Ukernel_Code := 16#04#;
   Load_HWConfig_Start                : constant Ukernel_Code := 16#05#;
   Load_HWConfig_Done                 : constant Ukernel_Code := 16#06#;
   Load_HWConfig_Error                : constant Ukernel_Code := 16#07#;
   Load_GDT_Done                      : constant Ukernel_Code := 16#10#;
   Load_IDT_Done                      : constant Ukernel_Code := 16#20#;
   Load_LAPIC_Done                    : constant Ukernel_Code := 16#30#;
   Load_GuCINT_Done                   : constant Ukernel_Code := 16#40#;
   Load_DPC_Ready                     : constant Ukernel_Code := 16#50#;
   Load_DPC_Error                     : constant Ukernel_Code := 16#60#;
   Load_Exception                     : constant Ukernel_Code := 16#70#;
   Load_Init_Data_Invalid             : constant Ukernel_Code := 16#71#;
   Load_PXP_Teardown_Ctrl_Enabled     : constant Ukernel_Code := 16#72#;
   Load_MPU_Data_Invalid              : constant Ukernel_Code := 16#73#;
   Load_Init_MMIO_Save_Restore_Invalid : constant Ukernel_Code := 16#74#;
   Load_KLV_Workaround_Init_Error     : constant Ukernel_Code := 16#75#;
   Load_Ready                         : constant Ukernel_Code := 16#F0#;

   type Fields is record
      MIA_In_Reset : Boolean;
      Bootrom      : Bootrom_Code;
      Ukernel      : Ukernel_Code;
      MIA          : MIA_Code;
      Auth         : Authentication;
   end record;

   function Split (Value : Unsigned_32) return Fields is
     ((Value and Reset_Bit) /= 0,
      Shift_Right (Value, Bootrom_Shift) and Bootrom_Mask,
      Shift_Right (Value, Ukernel_Shift) and Ukernel_Mask,
      Shift_Right (Value, MIA_Shift) and MIA_Mask,
      Authentication'Val (Shift_Right (Value, Auth_Shift)))
   with Global => null;

   --  Names as i915 spells them, lower case; "unknown" for other codes.
   function Bootrom_Name (Code : Bootrom_Code) return String with Global => null;
   function Ukernel_Name (Code : Ukernel_Code) return String with Global => null;
   function Auth_Name (Auth : Authentication) return String is
     (case Auth is
        when Auth_Unchecked => "unchecked", when Auth_Bad => "bad",
        when Auth_Good => "good", when Auth_Undefined => "undefined");
   function State_Name (S : State) return String is
     (case S is
        when Pending => "pending", when Invalid_MMIO => "invalid-mmio",
        when Authentication_Failed => "authentication-failed",
        when Bootrom_Failed => "bootrom-failed",
        when Firmware_Failed => "firmware-failed", when Ready => "ready");

   --  "raw=0x... mia-reset=0 bootrom=0x76(jump-passed) ukernel=0xF0(ready)
   --  mia=0x0 auth=good state=ready" (all-ones: "raw=0xFFFFFFFF invalid-mmio").
   function Describe (Value : Unsigned_32) return String
     with Global => null, Post => Describe'Result'Length <= 160;
end Intel_GPU_GuC_Status;
