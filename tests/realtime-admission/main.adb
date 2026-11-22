--  Hosted tests of Realtime_Admission (docs/scheduler.md).
with Ada.Text_IO; use Ada.Text_IO;
with Realtime_Admission; use Realtime_Admission;

procedure Main is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then Put_Line ("PASS " & Name);
      else Put_Line ("FAIL " & Name); Failures := Failures + 1; end if;
   end Check;
   A : Total := 0;
   G : Boolean;
begin
   Check (Utilization_Of (500, 5_000) = 100_000, "0.5 ms of 5 ms is 10%");
   Check (Utilization_Of (1, 3) = 333_334, "rounds up, never understates");
   Check (Utilization_Of (7, 7) = Parts_Per_Million, "whole CPU");
   Check (Covers (1_000, 5_000, 500, 2_500), "same share covers");
   Check (not Covers (1_000, 5_000, 600, 2_500), "larger share not covered");

   --  One CPU: 70% total.
   Admit (A, 400_000, 1, G); Check (G and A = 400_000, "first reservation");
   Admit (A, 300_000, 1, G); Check (G and A = 700_000, "up to the share");
   Admit (A, 1, 1, G);       Check (not G and A = 700_000, "past the share refused");
   Release (A, 300_000);     Check (A = 400_000, "release returns exactly");
   --  Four CPUs: 280% total, but no single reservation above 70%.
   Admit (A, 800_000, 4, G); Check (not G, "one reservation above the share refused");
   Admit (A, 700_000, 4, G); Check (G and A = 1_100_000, "admitted across CPUs");

   if Failures = 0 then Put_Line ("realtime-admission: all tests passed");
   else Put_Line ("realtime-admission:" & Failures'Image & " failures"); end if;
end Main;
