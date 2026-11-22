with Ada.Text_IO;
with Interfaces; use Interfaces;
with USB_Keyboards; use USB_Keyboards;
procedure Keyboard_Tests is
   S : State;
   D : Changes;
   R : Decode_Result;
   Data : Report := [others => 0];
begin
   for Key in Unsigned_8 range 4 .. 16#DF# loop
      Data := [0, 0, Key, Key, 0, 0, 0, 0];
      Update (S, Data, D, R);
      pragma Assert (R = Decoded and then D.Pressed (Key));
      Update (S, Data, D, R);
      pragma Assert (for all K in Key_Set'Range => not D.Pressed (K) and not D.Released (K));
      Data (3) := 1;
      Update (S, Data, D, R);
      pragma Assert (R = Rollover);
      pragma Assert (for all K in Key_Set'Range => not D.Pressed (K) and not D.Released (K));
      Release_All (S, D);
      pragma Assert (D.Released (Key));
   end loop;
   for Mods in Unsigned_8 loop
      Data := [Mods, 0, 0, 0, 0, 0, 0, 0];
      Update (S, Data, D, R);
      pragma Assert (R = Decoded);
      for Bit in 0 .. 7 loop
         pragma Assert (D.Pressed (16#E0# + Unsigned_8 (Bit)) =
           ((Mods and Shift_Left (Unsigned_8'(1), Bit)) /= 0));
      end loop;
      Release_All (S, D);
   end loop;
   Data := [0, 1, 4, 0, 0, 0, 0, 0];
   Update (S, Data, D, R);
   pragma Assert (R = Malformed);
   Data := [2, 0, 4, 5, 0, 0, 0, 0];
   Update (S, Data, D, R);
   pragma Assert (R = Decoded and D.Pressed (4) and D.Pressed (5) and D.Pressed (16#E1#));
   Data := [2, 0, 5, 4, 0, 0, 0, 0];
   Update (S, Data, D, R);
   pragma Assert (for all K in Key_Set'Range => not D.Pressed (K) and not D.Released (K));
   Data := [0, 0, 5, 6, 0, 0, 0, 0];
   Update (S, Data, D, R);
   pragma Assert (D.Released (4) and D.Released (16#E1#) and D.Pressed (6));
   pragma Assert (not D.Pressed (5) and not D.Released (5));
   Release_All (S, D);
   pragma Assert (D.Released (5) and D.Released (6));
   Release_All (S, D);
   pragma Assert (for all K in Key_Set'Range => not D.Pressed (K) and not D.Released (K));
   Data := [0, 0, 16#E0#, 0, 0, 0, 0, 0];
   Update (S, Data, D, R);
   pragma Assert (R = Malformed);
   Ada.Text_IO.Put_Line ("PASS: boot keyboard transitions, modifiers and rollover");
end Keyboard_Tests;
