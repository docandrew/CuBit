pragma Ada_2022;
--  CAP-001 NUC diagnostics: what the Desktop bootstrap screen keeps and how
--  intel-gpu reports a refused mapping.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Log_Records; use CuBit.Log_Records;
with Desktop_Bootstrap_Notes; use Desktop_Bootstrap_Notes;
with Intel_GPU_Map_Report;
procedure Diagnostics_Tests is
   Checks, Failures : Natural := 0;
   procedure Expect (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Label);
      end if;
   end Expect;
   N : Notes;
   Long : constant String (1 .. 400) := [others => 'x'];
begin
   Note (N, False, "desktop: information only");
   Expect (N.Recent_Used = 0 and then N.Map_Used = 0, "information not kept");
   Note (N, True, "kernel: device-memory denied intel-gpu MAP_DEVICE 0x6000000000");
   Expect (N.Recent_Used = 1 and then
           Text_Of (N.Recent (1)) = "kernel: device-memory denied intel-gpu MAP_DEVICE 0x6000000000",
           "warning kept whole");
   for I in 1 .. Recent_Capacity + 3 loop
      Note (N, True, "w" & Integer'Image (I));
   end loop;
   Expect (N.Recent_Used = Recent_Capacity, "recent bounded");
   Expect (Text_Of (N.Recent (Recent_Capacity)) = "w" & Integer'Image (Recent_Capacity + 3),
           "latest last");
   Expect (Text_Of (N.Recent (1)) = "w" & Integer'Image (4), "oldest gives way");
   Expect (N.Shown_Total = Recent_Capacity + 4, "seen counted");
   Note (N, False, "kernel: physical map: 12 spans");
   Note (N, True, "kernel: physical map root 1: 0xC0000000-0xFEBFFFFF");
   Expect (N.Map_Used = 2 and then N.Recent_Used = Recent_Capacity
           and then Text_Of (N.Recent (Recent_Capacity)) = "w" & Integer'Image (Recent_Capacity + 3),
           "map records go to the map, whatever their level");
   for I in 1 .. Map_Capacity + 5 loop
      Note (N, False, "kernel: physical map" & Integer'Image (I) & ": spans");
   end loop;
   Expect (N.Map_Used = Map_Capacity, "map bounded, first kept");
   Expect (Text_Of (N.Map (1)) = "kernel: physical map: 12 spans", "map first line kept");
   Note (N, True, Long);
   Expect (N.Recent (Recent_Capacity).Length = Line_Bytes, "long text cut");
   Note (N, True, "");
   Expect (N.Recent (Recent_Capacity).Length = 0, "empty text");
   Expect (Is_Map_Record ("kernel: physical map") and then not Is_Map_Record ("kernel: physical"),
           "map prefix exact");
   --  Kernel record numbers.
   Expect (Denial_Number ("kernel: device-memory denied #12 intel-gpu.drv MAP_DEVICE") = 12, "number");
   Expect (Denial_Number ("kernel: device-memory denied #") = 0, "no digits");
   Expect (Denial_Number ("kernel: device-memory denied records lost: 3") = 0, "lost line");
   Expect (Denial_Number ("kernel: device-memory denied #99999999999999") = Number_Limit, "saturates");
   Expect (Denial_Number ("intel-gpu: map-denied #4") = 0, "other text");
   Note (N, True, "kernel: device-memory denied #7 x");
   Note (N, True, "kernel: device-memory denied #5 y");
   Expect (N.Highest_Denial = 7, "highest kept");
   --  Kernel kill records: numbered, kept apart, latest Kill_Capacity.
   Expect (Kill_Number ("kernel: process killed #3 name=intel-gpu.drv pid=0x1 kind=not-present") = 3,
           "kill number");
   Expect (Kill_Number ("kernel: device-memory denied #3 x") = 0, "a denial is not a kill");
   Expect (Denial_Number ("kernel: process killed #3 x") = 0, "a kill is not a denial");
   for I in 1 .. Kill_Capacity + 2 loop
      Note (N, True, "kernel: process killed #" & Integer'Image (I) (2 .. Integer'Image (I)'Last) & " k");
   end loop;
   Expect (N.Highest_Kill = Kill_Capacity + 2, "highest kill");
   Expect (N.Kills_Used = Kill_Capacity, "kills bounded");
   Expect (Text_Of (N.Kills (Kill_Capacity)) =
           "kernel: process killed #" & Integer'Image (Kill_Capacity + 2) (2 .. Integer'Image (Kill_Capacity + 2)'Last) & " k",
           "latest kill last");
   Note (N, True, "kernel: process killed #1 late");
   Expect (N.Highest_Kill = Kill_Capacity + 2, "highest kill never decreases");
   --  intel-gpu
   Expect (Intel_GPU_Map_Report.Denied (16#60_0000_0000#, 4096) = "map-denied 0x6000000000 +0x1000",
           "denied text names the range");
   Expect (Intel_GPU_Map_Report.Denied (0, 0) = "map-denied 0x0 +0x0", "zero range");
   Expect (Intel_GPU_Map_Report.Denied (Unsigned_64'Last, Unsigned_64'Last) =
           "map-denied 0xFFFFFFFFFFFFFFFF +0xFFFFFFFFFFFFFFFF", "widest range");
   Expect (Intel_GPU_Map_Report.Escalate ("intel-gpu: GGTT write mapping map-denied 0x1", Information) = Warning,
           "map-denied escalates");
   Expect (Intel_GPU_Map_Report.Escalate ("intel-gpu: reset pages grant-denied", Debug) = Warning,
           "grant-denied escalates");
   Expect (Intel_GPU_Map_Report.Escalate ("intel-gpu: ready", Information) = Information,
           "plain text keeps its level");
   Expect (Intel_GPU_Map_Report.Escalate ("denied", Error) = Error, "never lowers");
   Expect (Intel_GPU_Map_Report.Escalate ("denie", Information) = Information, "short text");
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("PASS: device-memory diagnostics" & Natural'Image (Checks) & " checks");
   else
      Ada.Text_IO.Put_Line ("FAILED:" & Natural'Image (Failures) & " of" & Natural'Image (Checks));
      raise Program_Error;
   end if;
end Diagnostics_Tests;
