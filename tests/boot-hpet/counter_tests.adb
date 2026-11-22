with Interfaces; use Interfaces;
with HPET_Counter; use HPET_Counter;
with Ada.Text_IO;
procedure Counter_Tests is
begin
   for N in Unsigned_32 range 0 .. 31 loop
      pragma Assert (Timer_Count (Shift_Left (N, 8)) = Natural (N) + 1);
   end loop;
   for C in Unsigned_32 range 0 .. 65_535 loop
      pragma Assert ((Quiet_Timer (C) and 16#4004#) = 0);
      pragma Assert ((Quiet_Timer (C) and not Unsigned_32'(16#4004#)) =
                     (C and not Unsigned_32'(16#4004#)));
      pragma Assert ((Counter_Only (C) and 3) = 1);
      pragma Assert ((Counter_Only (C) and not Unsigned_32'(3)) = (C and not Unsigned_32'(3)));
   end loop;
   pragma Assert (Admitted (16#2001#, 100_000_000));
   pragma Assert (not Admitted (16#2001#, 100_000_001));
   pragma Assert (not Admitted (16#2001#, 0));
   pragma Assert (not Admitted (1, 100));
   pragma Assert (not Admitted (16#2000#, 100));
   pragma Assert (not Admitted (Unsigned_32'Last, 100));
   for P in Tick_Period range 1 .. 100_000 loop
      pragma Assert (Microseconds (1_000_000_000, P) = P);
      pragma Assert (Microseconds (999_999_999, P) = P - 1);
      pragma Assert (Microseconds (1_000_000_001, P) = P);
   end loop;
   pragma Assert (Microseconds (Unsigned_64'Last, 100_000_000) = Unsigned_64'Last / 10);
   pragma Assert (Microseconds (Unsigned_64'Last, 1) = Unsigned_64'Last / 1_000_000_000);
   Ada.Text_IO.Put_Line ("PASS: HPET counter policy and overflow-safe conversion boundaries");
end Counter_Tests;
