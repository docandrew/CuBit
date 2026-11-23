--  Hosted test of Timekeeping_Turns: who may keep time, msTicks never
--  backward from any keeper's reading, CPU 0's saturating elapsed path, and
--  the silence report's threshold, against plain references.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Timekeeping_Turns; use Timekeeping_Turns;

procedure Main is
   Checks : Natural := 0;
   type U64_List is array (Positive range <>) of Unsigned_64;
   Values : constant U64_List :=
     [0, 1, 2, 99, 100, 101, 1_000, 11_805, 2 ** 32, 2 ** 62,
      Unsigned_64'Last - 100, Unsigned_64'Last - 1, Unsigned_64'Last];
begin
   --  CPU 0 always; another CPU only with one clock.
   for CPU in 0 .. 7 loop
      for Shared in Boolean loop
         pragma Assert (May_Keep (CPU, Shared) = (CPU = 0 or else Shared));
         Checks := Checks + 1;
      end loop;
   end loop;

   for P of Values loop
      for R of Values loop
         --  A keeper's reading: the later of msTicks and the reading.
         pragma Assert (Next (P, R, True) = (if R > P then R else P));
         pragma Assert (Next (P, R, False) = P);
         pragma Assert (Next (P, R, True) >= P);
         --  CPU 0's own ticks: added, saturating at the top.
         pragma Assert (Advanced (P, R) =
           (if R > Unsigned_64'Last - P then Unsigned_64'Last else P + R));
         --  Silence: how far Now is past the last run, never negative.
         pragma Assert (Silence (P, R) = (if R > P then R - P else 0));
         pragma Assert (Silent (P, R) = (R > P and then R - P >= Silence_Report_Ms));
         Checks := Checks + 6;
      end loop;
   end loop;

   --  Two keepers reading out of order never move msTicks back.
   declare
      Ms : Unsigned_64 := 0;
   begin
      for Reading of U64_List'[5, 9, 7, 9, 12, 3, 12, 40] loop
         declare
            Before : constant Unsigned_64 := Ms;
         begin
            Ms := Next (Ms, Reading, True);
            pragma Assert (Ms >= Before and then Ms >= Reading);
            Checks := Checks + 1;
         end;
      end loop;
      pragma Assert (Ms = 40);
   end;

   --  The report threshold: one millisecond short is not silent.
   pragma Assert (not Silent (1_000, 1_000 + Silence_Report_Ms - 1));
   pragma Assert (Silent (1_000, 1_000 + Silence_Report_Ms));
   Checks := Checks + 2;

   Put_Line ("timekeeping-turns:" & Natural'Image (Checks) & " checks PASS");
end Main;
