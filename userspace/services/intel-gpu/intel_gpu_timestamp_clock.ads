with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_Timestamp_Clock with SPARK_Mode, Pure is
   -- TGL PRM Vol 2c-12.21 pp243-244, CONFIG0 at 00D00h.
   -- Linux v6.16 intel_gt_clock_utils.c gen11_read_clock_frequency.
   -- Copied register words only; no MMIO access or clock writes.
   CONFIG0_Offset : constant Interfaces.Unsigned_32 := 16#D00#;
   -- Source/divider field layouts cross-checked with Linux v6.16
   -- gt/intel_gt_regs.h and i915_reg.h; local PRM confirmation pending.
   CTC_Mode_Offset : constant Interfaces.Unsigned_32 := 16#A26C#;
   Override_Offset : constant Interfaces.Unsigned_32 := 16#44074#;
   type Bit is mod 2 with Size => 1;
   type Bits_2 is mod 4 with Size => 2;
   type Bits_3 is mod 8 with Size => 3;
   type Bits_24 is mod 2 ** 24 with Size => 24;
   type Bits_4 is mod 16 with Size => 4;
   type Bits_10 is mod 2 ** 10 with Size => 10;
   type Bits_16 is mod 2 ** 16 with Size => 16;
   type Bits_29 is mod 2 ** 29 with Size => 29;
   type CTC_Mode is record
      Divide_Logic : Bit := 0;
      Legacy_Shift : Bits_2 := 0; -- Not the Gen11+ timestamp shift
      Other : Bits_29 := 0;      -- Uninterpreted, never written here
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for CTC_Mode use record
      Divide_Logic at 0 range 0 .. 0;
      Legacy_Shift at 0 range 1 .. 2;
      Other at 0 range 3 .. 31;
   end record;
   type Timestamp_Override is record
      Divider : Bits_10 := 0;
      Other_Low : Bits_2 := 0;
      Denominator : Bits_4 := 0;
      Other_High : Bits_16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Timestamp_Override use record
      Divider at 0 range 0 .. 9;
      Other_Low at 0 range 10 .. 11;
      Denominator at 0 range 12 .. 15;
      Other_High at 0 range 16 .. 31;
   end record;
   function Decode_Mode is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, CTC_Mode);
   function Decode_Override is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Timestamp_Override);
   type RPM_CONFIG0 is record
      Disable_TSC_Synchronization : Bit := 0;
      CTC_Shift : Bits_2 := 0;
      Crystal_Selector : Bits_3 := 0;
      Reserved_6 : Bit := 0;
      Placeholder : Bits_24 := 0;
      Locked : Bit := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for RPM_CONFIG0 use record
      Disable_TSC_Synchronization at 0 range 0 .. 0;
      CTC_Shift at 0 range 1 .. 2;
      Crystal_Selector at 0 range 3 .. 5;
      Reserved_6 at 0 range 6 .. 6;
      Placeholder at 0 range 7 .. 30;
      Locked at 0 range 31 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, RPM_CONFIG0);
   function Encode is new Ada.Unchecked_Conversion
     (RPM_CONFIG0, Interfaces.Unsigned_32);
   -- ONLY for a separately verified crystal-source selection in CTC_MODE.
   -- Zero rejects reserved crystal selectors. Not a complete clock snapshot;
   -- do not report this as the timestamp frequency before checking its source.
   function Crystal_Timestamp_Hz (Config : RPM_CONFIG0)
     return Interfaces.Unsigned_32;
   -- The three inputs must be observed under retained power/ownership. This
   -- pure decoder cannot establish MMIO validity or prevent later changes.
   function Timestamp_Hz (Mode : CTC_Mode; Config : RPM_CONFIG0;
                          Divider : Timestamp_Override)
     return Interfaces.Unsigned_32;
   -- Compare only fields determining frequency in the selected path, never
   -- demand equality of complete registers or unrelated live status fields.
   function Same_Clock
     (Mode_A, Mode_B : CTC_Mode; Config_A, Config_B : RPM_CONFIG0;
      Divider_A, Divider_B : Timestamp_Override) return Boolean;
end Intel_GPU_Timestamp_Clock;
