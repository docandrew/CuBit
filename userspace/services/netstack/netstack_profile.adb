------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with System.Machine_Code;
with CuBit.Messages;

package body Netstack_Profile is

   Totals  : array (Stage) of Unsigned_64 := [others => 0];
   Packets : Unsigned_64 := 0;
   Report_Interval : constant := 2 ** 16;

   function Now return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      if not Enabled then
         return 0;
      end if;
      System.Machine_Code.Asm
        ("rdtsc",
         Outputs => [Unsigned_32'Asm_Output ("=a", Low),
                     Unsigned_32'Asm_Output ("=d", High)],
         Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end Now;

   procedure Charge (S : Stage; Since : Unsigned_64) is
   begin
      if Enabled then
         Totals (S) := Totals (S) + (Now - Since);
      end if;
   end Charge;

   procedure Packet is
   begin
      if not Enabled then
         return;
      end if;
      Packets := Packets + 1;
      if Packets mod Report_Interval = 0 then
         CuBit.Messages.debugPrint ("netstack-profile: cycles/packet");
         for S in Stage loop
            CuBit.Messages.debugPrint
              (" " & Stage'Image (S) & "=" &
               Unsigned_64'Image (Totals (S) / Packets));
         end loop;
         CuBit.Messages.debugPrint ("" & ASCII.LF);
      end if;
   end Packet;

end Netstack_Profile;
