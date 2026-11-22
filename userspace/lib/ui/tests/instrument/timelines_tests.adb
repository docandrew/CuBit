------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  CuBit.UI.Timelines: a disabled strip takes no scrubbing and says so; an
--  enabled one maps positions and keys; markers and the playhead draw.
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Timelines; use CuBit.UI.Timelines;
with CuBit.UI.Instrument;

procedure Timelines_Tests is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Put_Line ((if Condition then "PASS " else "FAIL ") & What);
      if not Condition then Failures := Failures + 1; end if;
   end Check;
   P : CuBit.UI.Instrument.Palette renames CuBit.UI.Instrument.Instrument;
   W : constant := 1_000;
   H : constant := 80;
   type Pixels is array (0 .. W * H - 1) of Color;
   type Pixels_Access is access Pixels;
   Buffer : constant Pixels_Access := new Pixels'(others => 0);
   C : constant Canvas := (addr => Buffer.all'Address, width => W, height => H, pitch => W * 4, others => <>);
   Area : constant Rect := (0, 0, W, H);
   S : Strip;
   Position : Permille;
   Inside : Boolean;
   KEY_J : constant := 16#24#;
   KEY_PERIOD : constant := 16#34#;
   function Has (Col : Color) return Boolean is (for some X of Buffer.all => X = Col);
begin
   Set_Title (S, "REPLAY", "coming");
   Set_Notice (S, "Not recorded yet.", "J K L");
   S.Lanes (1) := Lane_Of ("main");
   S.Lane_Total := 1;
   Check (Scrub_Of (S, KEY_J) = No_Scrub, "a disabled strip takes no scrubbing");
   Position_At (S, Area, 600, Position, Inside);
   Check (not Inside and then Position = Permille'Last, "nor clicks; its playhead is at the end");
   S.Enabled := True;
   Check (Scrub_Of (S, KEY_J) = Reverse_Play and then Scrub_Of (S, KEY_PERIOD) = Step_Forward, "J and . scrub");
   Position_At (S, Area, LABEL_WIDTH, Position, Inside);
   Check (Inside and then Position = 0, "the track starts after the labels");
   S.Markers (1) := (Lane => 1, At_Position => 500, Kind => Fault_Mark);
   S.Marker_Total := 1;
   S.Playhead := 250;
   Draw (C, Area, S, P);
   Check (Has (P.Danger) and then Has (P.Accent), "markers and the playhead are drawn");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
   Put_Line ("timelines:" & (if Failures = 0 then " all checks passed" else Natural'Image (Failures) & " FAILED"));
end Timelines_Tests;
