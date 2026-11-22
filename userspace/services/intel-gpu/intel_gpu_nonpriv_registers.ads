with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;
package Intel_GPU_Nonpriv_Registers with SPARK_Mode is
   -- TGL PRM Vol 2c-12.21, printed pp961-962 (PDF987-988).
   -- Documentary RCS register map, NOT an ADL-N presence/capability probe.
   -- i915 manages slots0..11; the additional PRM slots are NONCONTIGUOUS.
   -- Do not infer permission to access them from this address function.
   subtype Documented_RCS_Slot is Natural range 0 .. 19;
   function Documented_RCS_Offset (Slot : Documented_RCS_Slot)
     return Unsigned_32 is
       (if Slot < 12 then 16#24D0# + Unsigned_32 (Slot) * 4
        elsif Slot < 16 then 16#2010# + Unsigned_32 (Slot - 12) * 4
        else 16#21E0# + Unsigned_32 (Slot - 16) * 4);
   -- Intel TGL PRM Vol 2c-12.21, FORCE_TO_NONPRIV, printed pp988-989.
   -- Copied register values, not volatile MMIO overlays. All bit patterns are
   -- representable; reserved encodings must be checked before interpretation.
   type Bit is mod 2 with Size => 1;
   type Bits_2 is mod 4 with Size => 2;
   type Bits_24 is mod 2 ** 24 with Size => 24;
   type Register_Value is record
      Offset_Range : Bits_2 := 0;
      Address_DWords : Bits_24 := 0;
      Reserved : Bits_2 := 0;
      Access_Selection : Bits_2 := 0;
      Denylist : Bit := 0;
      Virtual_Function : Bit := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Register_Value use record
      Offset_Range at 0 range 0 .. 1;
      Address_DWords at 0 range 2 .. 25;
      Reserved at 0 range 26 .. 27;
      Access_Selection at 0 range 28 .. 29;
      Denylist at 0 range 30 .. 30;
      Virtual_Function at 0 range 31 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Unsigned_32, Register_Value);
   function Encode (Value : Register_Value) return Unsigned_32 is
     (Unsigned_32 (Value.Offset_Range) or
      Shift_Left (Unsigned_32 (Value.Address_DWords), 2) or
      Shift_Left (Unsigned_32 (Value.Reserved), 26) or
      Shift_Left (Unsigned_32 (Value.Access_Selection), 28) or
      Shift_Left (Unsigned_32 (Value.Denylist), 30) or
      Shift_Left (Unsigned_32 (Value.Virtual_Function), 31));
   type Operation is (Read_Register, Write_Register);
   type Decision is (Unspecified, Allow, Deny, Invalid);
   type Register_List is array (Positive range <>) of Register_Value;
   -- Evaluates ONLY these programmable overrides, NOT the built-in hardware
   -- permissions. Unspecified is not permission. Caller supplies a complete,
   -- authoritative snapshot for the engine; this does not determine slot count.
   -- VF mode is deliberately unsupported/Invalid, never interpreted as an
   -- ordinary allow entry. No hardware writes or application admission here.
   function Evaluate
     (Entries : Register_List; Offset : Unsigned_32; Access_Kind : Operation)
      return Decision;
end Intel_GPU_Nonpriv_Registers;
