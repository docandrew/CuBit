with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_PHY_Registers with SPARK_Mode is
   -- Layout source: Intel IHD-OS-TGL-Vol 2c-12.21, printed pages below.
   -- Linux v6.16 intel_combo_phy.c cross-checks the ADLN fields we program.
   -- Numeric fields cover ALL 32 bits; conversion is of a copied MMIO word,
   -- never a live record overlay. Unspecified encodings remain representable.
   -- TGL labels Comp0[7:0] and Comp3[31:29] reserved/MBZ, but N95 samples
   -- disagree. Preserve these observations; do NOT apply TGL MBZ validation
   -- to ADLN or invent their semantics. Only documented owned fields govern
   -- restoration/readiness. No field accessor itself authorizes an MMIO write.
   type Bits_1 is mod 2 ** 1 with Size => 1;
   type Bits_2 is mod 2 ** 2 with Size => 2;
   type Bits_3 is mod 2 ** 3 with Size => 3;
   type Bits_4 is mod 2 ** 4 with Size => 4;
   type Bits_5 is mod 2 ** 5 with Size => 5;
   type Bits_6 is mod 2 ** 6 with Size => 6;
   type Bits_7 is mod 2 ** 7 with Size => 7;
   type Bits_8 is mod 2 ** 8 with Size => 8;
   type Bits_9 is mod 2 ** 9 with Size => 9;
   type Bits_12 is mod 2 ** 12 with Size => 12;
   type Bits_14 is mod 2 ** 14 with Size => 14;
   type Bits_20 is mod 2 ** 20 with Size => 20;

   -- PRM p664.
   type Misc_Register is record
      Reserved_Low : Bits_20 := 0;
      Spare_20 : Bits_1 := 0;
      Spare_21 : Bits_1 := 0;
      Spare_22 : Bits_1 := 0;
      Comp_Power_Down : Bits_1 := 0;
      IO_To_DE : Bits_4 := 0;
      DE_To_IO : Bits_4 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Misc_Register use record
      Reserved_Low at 0 range 0 .. 19;
      Spare_20 at 0 range 20 .. 20;
      Spare_21 at 0 range 21 .. 21;
      Spare_22 at 0 range 22 .. 22;
      Comp_Power_Down at 0 range 23 .. 23;
      IO_To_DE at 0 range 24 .. 27;
      DE_To_IO at 0 range 28 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Misc_Register);
   function Encode is new Ada.Unchecked_Conversion (Misc_Register, Interfaces.Unsigned_32);

   -- PRM p953.
   type TX_Register is record
      ODCC_Upper_Limit : Bits_5 := 0;
      IDCC_Therm_Low : Bits_3 := 0;
      IDCC_Code : Bits_5 := 0;
      IDCC_Therm_High : Bits_2 := 0;
      Reserved_15 : Bits_1 := 0;
      ODCC_Lower_Limit : Bits_5 := 0;
      Reserved_21 : Bits_1 := 0;
      ODCC_Fuse_Enable : Bits_1 := 0;
      ODCC_Override_Enable : Bits_1 := 0;
      ODCC_Override_Code : Bits_5 := 0;
      Clock_Divider : Bits_2 := 0;
      Clock_Select : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for TX_Register use record
      ODCC_Upper_Limit at 0 range 0 .. 4;
      IDCC_Therm_Low at 0 range 5 .. 7;
      IDCC_Code at 0 range 8 .. 12;
      IDCC_Therm_High at 0 range 13 .. 14;
      Reserved_15 at 0 range 15 .. 15;
      ODCC_Lower_Limit at 0 range 16 .. 20;
      Reserved_21 at 0 range 21 .. 21;
      ODCC_Fuse_Enable at 0 range 22 .. 22;
      ODCC_Override_Enable at 0 range 23 .. 23;
      ODCC_Override_Code at 0 range 24 .. 28;
      Clock_Divider at 0 range 29 .. 30;
      Clock_Select at 0 range 31 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, TX_Register);
   function Encode is new Ada.Unchecked_Conversion (TX_Register, Interfaces.Unsigned_32);

   -- PRM p906 and following page.
   type PCS_Register is record
      Soft_Reset_N : Bits_1 := 0;
      Soft_Reset_Enable : Bits_1 := 0;
      Latency_Optimize : Bits_2 := 0;
      Deemphasis : Bits_1 := 0;
      FIFO_Reset_Override : Bits_1 := 0;
      FIFO_Reset_Enable : Bits_1 := 0;
      TBC_Symbol_Clock : Bits_1 := 0;
      Clock_Request : Bits_2 := 0;
      Reserved_10 : Bits_2 := 0;
      TX_High : Bits_2 := 0;
      Reserved_14 : Bits_3 := 0;
      TX_Calibration_Enable : Bits_1 := 0;
      Calibration_Wake : Bits_1 := 0;
      Calibration_Bypass : Bits_1 := 0;
      DCC_Mode : Bits_2 := 0;
      Reserved_22 : Bits_2 := 0;
      Keeper_Bias : Bits_2 := 0;
      Keeper_Enable : Bits_1 := 0;
      Power_Down_Enable : Bits_1 := 0;
      Keeper_In_PG : Bits_1 := 0;
      Reserved_29 : Bits_3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for PCS_Register use record
      Soft_Reset_N at 0 range 0 .. 0;
      Soft_Reset_Enable at 0 range 1 .. 1;
      Latency_Optimize at 0 range 2 .. 3;
      Deemphasis at 0 range 4 .. 4;
      FIFO_Reset_Override at 0 range 5 .. 5;
      FIFO_Reset_Enable at 0 range 6 .. 6;
      TBC_Symbol_Clock at 0 range 7 .. 7;
      Clock_Request at 0 range 8 .. 9;
      Reserved_10 at 0 range 10 .. 11;
      TX_High at 0 range 12 .. 13;
      Reserved_14 at 0 range 14 .. 16;
      TX_Calibration_Enable at 0 range 17 .. 17;
      Calibration_Wake at 0 range 18 .. 18;
      Calibration_Bypass at 0 range 19 .. 19;
      DCC_Mode at 0 range 20 .. 21;
      Reserved_22 at 0 range 22 .. 23;
      Keeper_Bias at 0 range 24 .. 25;
      Keeper_Enable at 0 range 26 .. 26;
      Power_Down_Enable at 0 range 27 .. 27;
      Keeper_In_PG at 0 range 28 .. 28;
      Reserved_29 at 0 range 29 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, PCS_Register);
   function Encode is new Ada.Unchecked_Conversion (PCS_Register, Interfaces.Unsigned_32);

   -- PRM p896.
   type Comp0_Register is record
      Unspecified_ADLN_Low : Bits_8 := 0;
      Periodic_Counter : Bits_12 := 0;
      Reserved_20 : Bits_3 := 0;
      Procmon_Clock : Bits_1 := 0;
      Spare : Bits_2 := 0;
      Drive_Switch_Control : Bits_1 := 0;
      Drive_Switch_On : Bits_2 := 0;
      Slew_Control : Bits_2 := 0;
      Initialized : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Comp0_Register use record
      Unspecified_ADLN_Low at 0 range 0 .. 7;
      Periodic_Counter at 0 range 8 .. 19;
      Reserved_20 at 0 range 20 .. 22;
      Procmon_Clock at 0 range 23 .. 23;
      Spare at 0 range 24 .. 25;
      Drive_Switch_Control at 0 range 26 .. 26;
      Drive_Switch_On at 0 range 27 .. 28;
      Slew_Control at 0 range 29 .. 30;
      Initialized at 0 range 31 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Comp0_Register);
   function Encode is new Ada.Unchecked_Conversion (Comp0_Register, Interfaces.Unsigned_32);

   -- PRM p897 and following page.
   type Comp1_Register is record
      NLVT_Low : Bits_2 := 0;
      NLVT_High : Bits_2 := 0;
      PLVT_Low : Bits_2 := 0;
      PLVT_High : Bits_2 := 0;
      NHVT_Low : Bits_2 := 0;
      NHVT_High : Bits_2 := 0;
      PHVT_Low : Bits_2 := 0;
      PHVT_High : Bits_2 := 0;
      N_Low : Bits_2 := 0;
      N_High : Bits_2 := 0;
      P_Low : Bits_2 := 0;
      P_High : Bits_2 := 0;
      Rcomp_Enable : Bits_1 := 0;
      Fcomp_Polarity : Bits_1 := 0;
      Fcomp_Input : Bits_2 := 0;
      Fcomp_Bias : Bits_1 := 0;
      Fcomp_Cap_Ratio : Bits_1 := 0;
      Fcomp_Override : Bits_1 := 0;
      LDO_Bypass : Bits_1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Comp1_Register use record
      NLVT_Low at 0 range 0 .. 1;
      NLVT_High at 0 range 2 .. 3;
      PLVT_Low at 0 range 4 .. 5;
      PLVT_High at 0 range 6 .. 7;
      NHVT_Low at 0 range 8 .. 9;
      NHVT_High at 0 range 10 .. 11;
      PHVT_Low at 0 range 12 .. 13;
      PHVT_High at 0 range 14 .. 15;
      N_Low at 0 range 16 .. 17;
      N_High at 0 range 18 .. 19;
      P_Low at 0 range 20 .. 21;
      P_High at 0 range 22 .. 23;
      Rcomp_Enable at 0 range 24 .. 24;
      Fcomp_Polarity at 0 range 25 .. 25;
      Fcomp_Input at 0 range 26 .. 27;
      Fcomp_Bias at 0 range 28 .. 28;
      Fcomp_Cap_Ratio at 0 range 29 .. 29;
      Fcomp_Override at 0 range 30 .. 30;
      LDO_Bypass at 0 range 31 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Comp1_Register);
   function Encode is new Ada.Unchecked_Conversion (Comp1_Register, Interfaces.Unsigned_32);

   -- PRM p899 and following page.
   type Comp3_Register is record
      MIPI_Lpdn_Code : Bits_6 := 0;
      Lpdn_Min : Bits_1 := 0;
      Lpdn_Max : Bits_1 := 0;
      Icomp_Code : Bits_7 := 0;
      Reserved_15 : Bits_4 := 0;
      Icomp_Min : Bits_1 := 0;
      Icomp_Max : Bits_1 := 0;
      Procmon_Done : Bits_1 := 0;
      First_Comp_Done : Bits_1 := 0;
      PLL_Power_Ack : Bits_1 := 0;
      Voltage_Info : Bits_2 := 0;
      Process_Info : Bits_3 := 0;
      Unspecified_ADLN_High : Bits_3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Comp3_Register use record
      MIPI_Lpdn_Code at 0 range 0 .. 5;
      Lpdn_Min at 0 range 6 .. 6;
      Lpdn_Max at 0 range 7 .. 7;
      Icomp_Code at 0 range 8 .. 14;
      Reserved_15 at 0 range 15 .. 18;
      Icomp_Min at 0 range 19 .. 19;
      Icomp_Max at 0 range 20 .. 20;
      Procmon_Done at 0 range 21 .. 21;
      First_Comp_Done at 0 range 22 .. 22;
      PLL_Power_Ack at 0 range 23 .. 23;
      Voltage_Info at 0 range 24 .. 25;
      Process_Info at 0 range 26 .. 28;
      Unspecified_ADLN_High at 0 range 29 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Comp3_Register);
   function Encode is new Ada.Unchecked_Conversion (Comp3_Register, Interfaces.Unsigned_32);

   -- PRM p901.
   type Comp8_Register is record
      Reserved_Low : Bits_14 := 0;
      Disable_Periodic_Comp : Bits_1 := 0;
      Reserved_15 : Bits_9 := 0;
      Reference_Enable : Bits_1 := 0;
      Reserved_25 : Bits_7 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Comp8_Register use record
      Reserved_Low at 0 range 0 .. 13;
      Disable_Periodic_Comp at 0 range 14 .. 14;
      Reserved_15 at 0 range 15 .. 23;
      Reference_Enable at 0 range 24 .. 24;
      Reserved_25 at 0 range 25 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Comp8_Register);
   function Encode is new Ada.Unchecked_Conversion (Comp8_Register, Interfaces.Unsigned_32);

   -- PRM p902.
   type Comp9_Register is record
      P_High : Bits_8 := 0;
      P_Low : Bits_8 := 0;
      N_High : Bits_8 := 0;
      N_Low : Bits_8 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Comp9_Register use record
      P_High at 0 range 0 .. 7;
      P_Low at 0 range 8 .. 15;
      N_High at 0 range 16 .. 23;
      N_Low at 0 range 24 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Comp9_Register);
   function Encode is new Ada.Unchecked_Conversion (Comp9_Register, Interfaces.Unsigned_32);

   -- PRM p903.
   type Comp10_Register is record
      PLVT_High : Bits_8 := 0;
      PLVT_Low : Bits_8 := 0;
      NLVT_High : Bits_8 := 0;
      NLVT_Low : Bits_8 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Comp10_Register use record
      PLVT_High at 0 range 0 .. 7;
      PLVT_Low at 0 range 8 .. 15;
      NLVT_High at 0 range 16 .. 23;
      NLVT_Low at 0 range 24 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Comp10_Register);
   function Encode is new Ada.Unchecked_Conversion (Comp10_Register, Interfaces.Unsigned_32);

   -- PRM p885 and following page.
   type CL_Register is record
      Suspend_Clock : Bits_2 := 0;
      Power_Ack_Override : Bits_1 := 0;
      CRI_Clock_Select : Bits_1 := 0;
      Power_Down_Enable : Bits_1 := 0;
      Stagger_Disable : Bits_1 := 0;
      Port_Stagger : Bits_1 := 0;
      Reserved_7 : Bits_1 := 0;
      Broadcast_Enable : Bits_1 := 0;
      IOSF_Divider : Bits_3 := 0;
      Reserved_12 : Bits_1 := 0;
      IOSF_PD_Count : Bits_2 := 0;
      Reserved_15 : Bits_1 := 0;
      CRI_Max : Bits_4 := 0;
      Fuse_Repull : Bits_1 := 0;
      Fuse_Override : Bits_1 := 0;
      Fuse_Reset : Bits_1 := 0;
      Reserved_23 : Bits_1 := 0;
      Force_Control : Bits_8 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for CL_Register use record
      Suspend_Clock at 0 range 0 .. 1;
      Power_Ack_Override at 0 range 2 .. 2;
      CRI_Clock_Select at 0 range 3 .. 3;
      Power_Down_Enable at 0 range 4 .. 4;
      Stagger_Disable at 0 range 5 .. 5;
      Port_Stagger at 0 range 6 .. 6;
      Reserved_7 at 0 range 7 .. 7;
      Broadcast_Enable at 0 range 8 .. 8;
      IOSF_Divider at 0 range 9 .. 11;
      Reserved_12 at 0 range 12 .. 12;
      IOSF_PD_Count at 0 range 13 .. 14;
      Reserved_15 at 0 range 15 .. 15;
      CRI_Max at 0 range 16 .. 19;
      Fuse_Repull at 0 range 20 .. 20;
      Fuse_Override at 0 range 21 .. 21;
      Fuse_Reset at 0 range 22 .. 22;
      Reserved_23 at 0 range 23 .. 23;
      Force_Control at 0 range 24 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, CL_Register);
   function Encode is new Ada.Unchecked_Conversion (CL_Register, Interfaces.Unsigned_32);
end Intel_GPU_PHY_Registers;
