with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;
package Intel_GPU_Arbitration_Command with SPARK_Mode is
   -- Intel TGL PRM vol2a printed957-958, MI_ARB_ON_OFF. This copied
   -- command word is not a volatile MMIO overlay. All encodings representable.
   type Bit is mod 2 with Size => 1;
   type Bits_21 is mod 2 ** 21 with Size => 21;
   type Bits_6 is mod 2 ** 6 with Size => 6;
   type Bits_3 is mod 2 ** 3 with Size => 3;
   type Command is record
      Arbitration_Enable : Bit := 1;
      Lite_Restore_Disable : Bit := 0;
      Reserved : Bits_21 := 0;
      Opcode : Bits_6 := 8;
      Command_Type : Bits_3 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Command use record
      Arbitration_Enable at 0 range 0 .. 0;
      Lite_Restore_Disable at 0 range 1 .. 1;
      Reserved at 0 range 2 .. 22;
      Opcode at 0 range 23 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Decode is new Ada.Unchecked_Conversion (Unsigned_32, Command);
   function Encode (Value : Command) return Unsigned_32 is
     (Unsigned_32 (Value.Arbitration_Enable) or
      Shift_Left (Unsigned_32 (Value.Lite_Restore_Disable), 1) or
      Shift_Left (Unsigned_32 (Value.Reserved), 2) or
      Shift_Left (Unsigned_32 (Value.Opcode), 23) or
      Shift_Left (Unsigned_32 (Value.Command_Type), 29));
   function Valid (Value : Command) return Boolean is
     (Value.Reserved = 0 and Value.Opcode = 8 and Value.Command_Type = 0);
   Enable : constant Unsigned_32 := Encode ((others => <>));
   Disable : constant Unsigned_32 :=
     Encode ((Arbitration_Enable => 0, others => <>));
   -- Off must be balanced by On in the same dispatch before ring exhaustion.
   -- MI_ARB_CHECK/pre-parser control is a different opcode, not a substitute.
end Intel_GPU_Arbitration_Command;
