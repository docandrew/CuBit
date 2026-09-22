with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Graphics_Metrics; use CuBit.Graphics_Metrics;
procedure Graphics_Counter_Test is
   C : Counter;
begin
   Add (C, 0);
   pragma Assert (C = (0, 0, False));
   for I in 1 .. 1_000 loop
      Add (C, 4);
      pragma Assert (C = (Unsigned_64 (I) * 4, Unsigned_64 (I), False));
   end loop;
   C := (Unsigned_64'Last - 1, 5, False);
   Add (C, 1);
   pragma Assert (C = (Unsigned_64'Last, 6, False));
   Add (C, 1);
   pragma Assert (C = (Unsigned_64'Last, 6, True));
   Add (C, 123);
   pragma Assert (C = (Unsigned_64'Last, 6, True));
   C := (7, Unsigned_64'Last, False);
   Add (C, 1);
   pragma Assert (C = (7, Unsigned_64'Last, True));
   Put_Line ("PASS graphics counters");
end Graphics_Counter_Test;
