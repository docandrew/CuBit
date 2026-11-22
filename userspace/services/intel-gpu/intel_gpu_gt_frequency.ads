------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Gen12 render GT's frequency registers, decoded (observation only;
--  docs/gpu-async-submission.md). Register offsets, fields and units from
--  Linux v6.16 i915 (gt/intel_gt_regs.h, intel_mchbar_regs.h, gt/intel_rps.c):
--  - RP_STATE_CAP (MCHBAR mirror 0x145998): RP0 bits 7:0, RP1 15:8, RPn
--    23:16, in 50 MHz units (intel_rps.c multiplies them by GEN9_FREQ_SCALER
--    for Gen11+, to the 50/3 MHz units below).
--  - GEN6_RPNSWREQ (0xA008): the requested ratio in bits 31:23
--    (GEN9_FREQUENCY), 50/3 MHz units.
--  - GEN12_RPSTAT1 (0x1381B4): CAGF, the actual frequency, bits 19:11
--    (GEN12_CAGF_MASK), 50/3 MHz units.
--  - GEN6_RP_CONTROL (0xA024): bit 7 RP_ENABLE (host turbo on).
--  - GEN6_RC_CONTROL (0xA090) and GEN6_RC_STATE (0xA094): RC6 policy and
--    state, reported raw.
--  - GEN10_FREQ_INFO_REC (0x145EF0): RPe, the efficient frequency, bits
--    15:8, 50 MHz units.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Intel_GPU_GT_Frequency with Pure, SPARK_Mode is

   RP_State_Cap_Offset : constant Unsigned_32 := 16#14_5998#;
   RP_Request_Offset   : constant Unsigned_32 := 16#A008#;
   RP_Status_Offset    : constant Unsigned_32 := 16#13_81B4#;
   RP_Control_Offset   : constant Unsigned_32 := 16#A024#;
   RC_Control_Offset   : constant Unsigned_32 := 16#A090#;
   RC_State_Offset     : constant Unsigned_32 := 16#A094#;
   --  GEN10_FREQ_INFO_REC (MCHBAR mirror): RPe in bits 15:8, 50 MHz units
   --  (intel_rps.c __gen6_rps_get_freq_caps, Gen10+).
   Frequency_Info_Offset : constant Unsigned_32 := 16#14_5EF0#;
   --  GT0_PERF_LIMIT_REASONS (i915_reg.h; intel_gt_perf_limit_reasons_reg,
   --  Gen11+ render GT): why pcode holds the frequency below the request.
   --  Bits 15:0 are current, bits 31:16 the same reasons latched (log).
   Perf_Limit_Reasons_Offset : constant Unsigned_32 := 16#13_81A8#;

   --  GT_FREQUENCY_MULTIPLIER and GEN9_FREQ_SCALER (i915): a ratio of
   --  Multiplier / Scaler MHz, and capability fields in Multiplier MHz.
   Multiplier_MHz : constant := 50;
   Scaler         : constant := 3;

   Cap_Field_Mask     : constant Unsigned_32 := 16#FF#;
   RP1_Shift          : constant := 8;
   RPn_Shift          : constant := 16;
   Request_Shift      : constant := 23;
   Request_Mask       : constant Unsigned_32 := 16#1FF#;   --  bits 31:23
   CAGF_Shift         : constant := 11;
   CAGF_Mask          : constant Unsigned_32 := 16#1FF#;   --  bits 19:11
   RP_Enable_Bit      : constant Unsigned_32 := 16#80#;
   RPe_Shift          : constant := 8;

   subtype MHz is Unsigned_32 range 0 .. 16#1FF# * Multiplier_MHz;

   --  A 50/3 MHz ratio in MHz, rounded to nearest (DIV_ROUND_CLOSEST).
   function Ratio_MHz (Ratio : Unsigned_32) return MHz is
     ((Unsigned_32'Min (Ratio, Request_Mask) * Multiplier_MHz + Scaler / 2) / Scaler);
   --  A 50 MHz capability field in MHz.
   function Cap_MHz (Field : Unsigned_32) return MHz is
     (Unsigned_32'Min (Field, Cap_Field_Mask) * Multiplier_MHz);

   function RP0_MHz (Cap : Unsigned_32) return MHz is (Cap_MHz (Cap and Cap_Field_Mask));
   function RP1_MHz (Cap : Unsigned_32) return MHz is
     (Cap_MHz (Shift_Right (Cap, RP1_Shift) and Cap_Field_Mask));
   function RPn_MHz (Cap : Unsigned_32) return MHz is
     (Cap_MHz (Shift_Right (Cap, RPn_Shift) and Cap_Field_Mask));
   function RPe_MHz (Info : Unsigned_32) return MHz is
     (Cap_MHz (Shift_Right (Info, RPe_Shift) and Cap_Field_Mask));
   function Requested_MHz (Request : Unsigned_32) return MHz is
     (Ratio_MHz (Shift_Right (Request, Request_Shift) and Request_Mask));
   function Actual_MHz (Status : Unsigned_32) return MHz is
     (Ratio_MHz (Shift_Right (Status, CAGF_Shift) and CAGF_Mask));
   function Host_Turbo (Control : Unsigned_32) return Boolean is
     ((Control and RP_Enable_Bit) /= 0);

   type Limit_Reason is (Prochot, Thermal, RATL, VR_Thermalert, VR_TDC,
                         Power_Limit_4, Power_Limit_1, Power_Limit_2);
   --  PROCHOT_MASK .. POWER_LIMIT_2_MASK.
   function Reason_Bit (Item : Limit_Reason) return Unsigned_32 is
     (case Item is
         when Prochot => 2 ** 0, when Thermal => 2 ** 1, when RATL => 2 ** 5,
         when VR_Thermalert => 2 ** 6, when VR_TDC => 2 ** 7,
         when Power_Limit_4 => 2 ** 8, when Power_Limit_1 => 2 ** 10,
         when Power_Limit_2 => 2 ** 11);
   --  GT0_PERF_LIMIT_REASONS_MASK: exactly the reasons above.
   Reasons_Mask : constant Unsigned_32 := 16#0DE3#;
   Log_Shift : constant := 16;
   function Reason_Name (Item : Limit_Reason) return String is
     (case Item is
         when Prochot => "prochot", when Thermal => "thermal", when RATL => "ratl",
         when VR_Thermalert => "vr_thermalert", when VR_TDC => "vr_tdc",
         when Power_Limit_4 => "pl4", when Power_Limit_1 => "pl1",
         when Power_Limit_2 => "pl2");
   --  The reason holds now (Log False) or was latched (Log True).
   function Limited_By (Value : Unsigned_32; Item : Limit_Reason; Log : Boolean)
     return Boolean is
     ((Shift_Right (Value, (if Log then Log_Shift else 0)) and Reason_Bit (Item)) /= 0);
   function Any_Limit (Value : Unsigned_32; Log : Boolean) return Boolean is
     ((Shift_Right (Value, (if Log then Log_Shift else 0)) and Reasons_Mask) /= 0)
     with Post => Any_Limit'Result =
       (for some R in Limit_Reason => Limited_By (Value, R, Log));

   type Snapshot is record
      Valid : Boolean := False;
      Cap, Info, Request, Status, RP_Control, RC_Control, RC_State : Unsigned_32 := 0;
      Limits : Unsigned_32 := 0;
   end record;

end Intel_GPU_GT_Frequency;
