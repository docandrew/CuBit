with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_Arbitration_Command;
procedure Arbitration_Command_Tests is
   package Arb renames Intel_GPU_Arbitration_Command;
   use type Arb.Bit;
   use type Arb.Bits_21;
   use type Arb.Bits_6;
   use type Arb.Bits_3;
   function Raw is new Ada.Unchecked_Conversion (Arb.Command, Unsigned_32);
begin
   pragma Assert (Arb.Enable = 16#04000001# and Arb.Disable = 16#04000000#);
   for Index in 0 .. 31 loop
      declare
         Word : constant Unsigned_32 := Shift_Left (1, Index);
         Value : constant Arb.Command := Arb.Decode (Word);
      begin
         pragma Assert (Raw (Value) = Word and Arb.Encode (Value) = Word);
         pragma Assert (Value.Arbitration_Enable = (if Index = 0 then 1 else 0));
         pragma Assert (Value.Lite_Restore_Disable = (if Index = 1 then 1 else 0));
         pragma Assert (Value.Reserved = Arb.Bits_21 (Shift_Right (Word, 2) and 16#1FFFFF#));
         pragma Assert (Value.Opcode = Arb.Bits_6 (Shift_Right (Word, 23) and 63));
         pragma Assert (Value.Command_Type = Arb.Bits_3 (Shift_Right (Word, 29)));
         -- Only the two control bits may vary without invalidating the opcode.
         pragma Assert (Arb.Valid (Arb.Decode (Arb.Enable xor Word)) = (Index < 2));
      end;
   end loop;
   for Enabled in Arb.Bit loop
      for Lite_Disabled in Arb.Bit loop
         declare
            Value : constant Arb.Command :=
              (Arbitration_Enable => Enabled, Lite_Restore_Disable => Lite_Disabled,
               others => <>);
         begin
            pragma Assert (Arb.Valid (Value));
            pragma Assert (Raw (Value) = Arb.Encode (Value));
            pragma Assert (Arb.Encode (Value) =
              16#04000000# + Unsigned_32 (Enabled) + 2 * Unsigned_32 (Lite_Disabled));
         end;
      end loop;
   end loop;
   pragma Assert (not Arb.Valid (Arb.Decode (16#02800100#)));
   pragma Assert (Raw (Arb.Decode (Unsigned_32'Last)) = Unsigned_32'Last);
   Ada.Text_IO.Put_Line ("Arbitration command PASS: every field bit, encodings, pre-parser distinction");
end Arbitration_Command_Tests;
