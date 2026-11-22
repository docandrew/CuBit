--  Hosted tests of Icon_Edge_Choice: the exported edge drawn 1:1 at each
--  density, and its placement (run.sh).
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Icon_Edge_Choice;

procedure Icon_Edge_Choice_Tests is
   package C renames Icon_Edge_Choice;
   Desktop : constant C.Edge_List := [16, 24, 32, 48, 64];
   Toolkit : constant C.Edge_List := [16, 32];
   Checks, Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Edge (List : C.Edge_List; Design : C.Edge; N, D : C.Scale_Part) return Positive is
     (List (C.Nearest (List, Design, N, D)));
begin
   --  1x: the design edge itself.
   Check (Edge (Desktop, 24, 1, 1) = 24 and Edge (Desktop, 16, 1, 1) = 16, "1x draws the design edge");
   --  125%: 24 covers 30 physical pixels; 32 (25.6 logical) is nearer
   --  than 24 (19.2). The 16-pixel title tile covers 20: 16 is nearer.
   Check (Edge (Desktop, 24, 5, 4) = 32, "125%: 24 -> 32");
   Check (Edge (Desktop, 16, 5, 4) = 16, "125%: 16 -> 16");
   Check (Edge (Toolkit, 16, 5, 4) = 16, "125% toolkit: 16 -> 16");
   --  150%.
   Check (Edge (Desktop, 24, 3, 2) = 32, "150%: 24 covers 36; 32 is 4 off, 48 is 12");
   Check (Edge (Desktop, 16, 3, 2) = 24, "150%: 16 -> 24");
   Check (Edge (Toolkit, 16, 3, 2) = 16, "150% toolkit: 24 is 8 from 16 and 8 from 32: the smaller wins");
   --  175%: 42 -> 48 (6 off) over 32 (10 off).
   Check (Edge (Desktop, 24, 7, 4) = 48, "175%: 24 -> 48");
   --  200%: double exports.
   Check (Edge (Desktop, 24, 2, 1) = 48 and Edge (Desktop, 16, 2, 1) = 32, "200% doubles");
   Check (Edge (Toolkit, 16, 2, 1) = 32, "200% toolkit: 32");
   --  Extremes clamp to the ends of the list.
   Check (Edge (Desktop, 24, 1, 16) = 16 and Edge (Desktop, 24, 16, 1) = 64, "extremes");
   --  The footprint error is exact: 32 at 5/4 is 32*4 - 24*5 = 8.
   Check (C.Distance (32, 24, 5, 4) = 8 and C.Distance (24, 24, 5, 4) = 24, "distance");
   --  Placement: centred, an odd remainder to the left; larger squares
   --  start before the span.
   Check (C.Centered (10, 40, 32) = 9, "30-pixel span, 32 square starts one before");
   Check (C.Centered (0, 30, 24) = 3, "30-pixel span, 24 square inset 3");
   Check (C.Centered (5, 5, 16) = -3, "empty span");
   Put_Line ("icon edge choice:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Icon_Edge_Choice_Tests;
