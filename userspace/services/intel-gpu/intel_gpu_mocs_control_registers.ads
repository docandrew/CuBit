with Ada.Unchecked_Conversion;
with Interfaces;
with System;
package Intel_GPU_MOCS_Control_Registers with SPARK_Mode, Pure is
   -- Field layout reference: Intel IHD-OS-ICLLP-Vol2c-1.20 pp816-819,
   -- GFX_MOCS_LECC_11_TC_01. Cross-check: Linux v6.16 gt/intel_mocs.c,
   -- LE_* fields and the Gen12 table selected for ADLN.
   -- The older PRM does NOT establish all ADLN field semantics: e.g. its
   -- SCF description says unused on ICL, while ADLN table entries set it.
   -- Preserve the ADLN table exactly. Do not transplant ICL policy/defaults
   -- or infer support for another device from this representation alone.
   type Bits_1 is mod 2 ** 1 with Size => 1;
   type Bits_2 is mod 2 ** 2 with Size => 2;
   type Bits_3 is mod 2 ** 3 with Size => 3;
   type Bits_13 is mod 2 ** 13 with Size => 13;
   type Control_Register is record
      Cacheability : Bits_2 := 0;
      Target_Cache : Bits_2 := 0;
      LRU_Management : Bits_2 := 0;
      Do_Not_Allocate_On_Miss : Bits_1 := 0;
      Reverse_Skip_Caching : Bits_1 := 0;
      Skip_Caching_Control : Bits_3 := 0;
      Page_Fault_Mode : Bits_3 := 0;
      Snoop_Control : Bits_1 := 0;
      Class_Of_Service : Bits_2 := 0;
      Self_Snoop : Bits_2 := 0;
      Reserved_High : Bits_13 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control_Register use record
      Cacheability at 0 range 0 .. 1;
      Target_Cache at 0 range 2 .. 3;
      LRU_Management at 0 range 4 .. 5;
      Do_Not_Allocate_On_Miss at 0 range 6 .. 6;
      Reverse_Skip_Caching at 0 range 7 .. 7;
      Skip_Caching_Control at 0 range 8 .. 10;
      Page_Fault_Mode at 0 range 11 .. 13;
      Snoop_Control at 0 range 14 .. 14;
      Class_Of_Service at 0 range 15 .. 16;
      Self_Snoop at 0 range 17 .. 18;
      Reserved_High at 0 range 19 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Control_Register);
   function Encode is new Ada.Unchecked_Conversion (Control_Register, Interfaces.Unsigned_32);
   function Matches (Raw, Expected : Interfaces.Unsigned_32) return Boolean;
end Intel_GPU_MOCS_Control_Registers;
