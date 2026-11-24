------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted: list the styles a range of playlist seeds draw (stage 1
--  and the later stages' biases), and check the space's spread: every
--  region and mode is reached, tempos cover their range, and seeds rarely
--  repeat a style's main traits. Not a CuBit result.
--
--  Usage: neon_styles_host FIRST LAST [--list]
------------------------------------------------------------------------------
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Style; use Neon_Style;

procedure Neon_Styles_Host is
   First : constant Unsigned_64 := Unsigned_64'Value (Argument (1));
   Last : constant Unsigned_64 := Unsigned_64'Value (Argument (2));
   Listing : constant Boolean := Argument_Count >= 3 and then Argument (3) = "--list";
   Regions : array (Region_Kind) of Natural := [others => 0];
   Modes : array (Mode_Kind) of Natural := [others => 0];
   Slowest, Fastest : Natural := 0;
   Stage_Floor : array (1 .. 4) of Natural := [others => Natural'Last];
   Count : Natural := 0;
   --  Two seeds share a fingerprint: region, tempo, mode, root, hats, bass, fill.
   Fingerprint_Buckets : constant := 65_536;
   Seen : array (0 .. Fingerprint_Buckets - 1) of Boolean := [others => False];
   Repeats : Natural := 0;
   Failed : Boolean := False;

   function Mask_Image (M : Steps_Mask) return String is
      Text : String (1 .. 16);
   begin
      for I in M'Range loop
         Text (I + 1) := (if M (I) then 'x' else '.');
      end loop;
      return Text;
   end Mask_Image;
begin
   for Seed in First .. Last loop
      declare
         S : constant Style := Stage_Style (Seed, 1);
         Beats : constant Positive := BPM (S.Step);
         Print : constant Unsigned_64 :=
           (Unsigned_64 (Region_Kind'Pos (S.Region)) * 1_000_003 + Unsigned_64 (S.Step) * 7_919
            + Unsigned_64 (Mode_Kind'Pos (S.Mode)) * 104_729 + Unsigned_64 (S.Root) * 31
            + Unsigned_64 (Hat_Kind'Pos (S.Hats)) * 17 + Unsigned_64 (Bass_Kind'Pos (S.Bass)) * 13
            + Unsigned_64 (Fill_Kind'Pos (S.Fill)) * 3) mod Fingerprint_Buckets;
      begin
         Count := Count + 1;
         Regions (S.Region) := Regions (S.Region) + 1;
         Modes (S.Mode) := Modes (S.Mode) + 1;
         if Slowest = 0 or else Beats < Slowest then
            Slowest := Beats;
         end if;
         Fastest := Natural'Max (Fastest, Beats);
         if Seen (Natural (Print)) then
            Repeats := Repeats + 1;
         end if;
         Seen (Natural (Print)) := True;
         for Stage in 2 .. 4 loop
            Stage_Floor (Stage) := Natural'Min (Stage_Floor (Stage), BPM (Stage_Style (Seed, Stage).Step));
         end loop;
         if Listing then
            Put_Line (Unsigned_64'Image (Seed) & "  " & Region_Name (S.Region) & "  " & Name (S)
                      & "  " & Positive'Image (Beats) & " BPM"
                      & (if S.Drop_Step /= S.Step then " (drop" & Positive'Image (BPM (S.Drop_Step)) & ")"
                         else "")
                      & "  " & Key_Name (S.Root) & " " & Mode_Name (S.Mode)
                      & "  kick " & Mask_Image (S.Kick_Main) & "  bass " & Bass_Kind'Image (S.Bass)
                      & "  hats " & Hat_Kind'Image (S.Hats) & "  fill " & Fill_Kind'Image (S.Fill)
                      & "  swing" & Natural'Image (Natural (100.0 * S.Swing)) & "%"
                      & "  answer " & Answer_Voice'Image (S.Answer)
                      & (if S.Drop_Double then "  double-drop" else ""));
         end if;
      end;
   end loop;
   Put_Line ("NEON-STYLES: seeds" & Natural'Image (Count) & " BPM" & Natural'Image (Slowest) & " .."
             & Natural'Image (Fastest) & " repeats" & Natural'Image (Repeats));
   for R in Region_Kind loop
      Put_Line ("NEON-STYLES:   " & Region_Name (R) & Natural'Image (Regions (R)));
      Failed := Failed or else Regions (R) = 0;
   end loop;
   for M in Mode_Kind loop
      Put_Line ("NEON-STYLES:   " & Mode_Name (M) & Natural'Image (Modes (M)));
      Failed := Failed or else Modes (M) = 0;
   end loop;
   for Stage in 2 .. 4 loop
      Put_Line ("NEON-STYLES:   stage" & Natural'Image (Stage) & " slowest" & Natural'Image (Stage_Floor (Stage))
                & " BPM");
   end loop;
   --  The named presets still land in their regions.
   for R in Named_Region loop
      if Stage_Style (Presets (R), 1).Region /= R then
         Put_Line ("NEON-STYLES: preset " & Region_Name (R) & " seed" & Unsigned_64'Image (Presets (R))
                   & " is now " & Region_Name (Stage_Style (Presets (R), 1).Region));
         Failed := True;
      end if;
   end loop;
   --  The hot stages keep their floors.
   Failed := Failed or else Stage_Floor (3) < 104 or else Stage_Floor (4) < 124;
   --  At most one seed in twenty repeats another's main traits.
   Failed := Failed or else Repeats * 20 > Count;
   Put_Line ("NEON-STYLES: " & (if Failed then "FAIL" else "PASS"));
   if Failed then
      Set_Exit_Status (Failure);
   end if;
end Neon_Styles_Host;
