------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted Neon Blast replay: the seed and the autopilot drive the
--  game in lockstep (audio write head a fixed gap ahead of the simulation),
--  the software renderer draws every 60 Hz frame, and the run reports frame
--  and audio hashes and render timings. Writes PPM screenshots at chosen
--  frames and the soundtrack as a WAV. Not a CuBit result.
--
--  Usage: neon_replay_host OUT_DIR SECONDS SEED WIDTH HEIGHT [SHOT_EVERY_FRAMES]
------------------------------------------------------------------------------
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Calendar;
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Game;
with Neon_Synth;
with Neon_Composer;
with Neon_Replay;
with Wave_Files;

procedure Neon_Replay_Host is
   Out_Dir : constant String := Argument (1);
   Seconds_Run : constant Natural := Natural'Value (Argument (2));
   Seed : constant Unsigned_64 := Unsigned_64'Value (Argument (3));
   Width : constant Positive := Positive'Value (Argument (4));
   Height : constant Positive := Positive'Value (Argument (5));
   Shot_Every : constant Natural := (if Argument_Count >= 6 then Natural'Value (Argument (6)) else 0);

   type Pixels_Type is array (0 .. Width * Height - 1) of Unsigned_32;
   type Pixels_Access is access Pixels_Type;
   Pixels : constant Pixels_Access := new Pixels_Type;
   type Replay_Access is access Neon_Replay.Replay;
   R : constant Replay_Access := new Neon_Replay.Replay;
   Wave : Wave_Files.Writer;
   Frames : constant Natural := Seconds_Run * Neon_Replay.Frames_Per_Second;
   Render_Total, Render_Worst : Duration := 0.0;
   use type Ada.Calendar.Time;
   Kicks : Ada.Text_IO.File_Type;
   Last_Kick : Samples := 0;

   procedure Write_PPM (Name : String) is
      package SIO renames Ada.Streams.Stream_IO;
      File : SIO.File_Type;
      Row : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Width * 3));
      P : Unsigned_32;
   begin
      SIO.Create (File, SIO.Out_File, Out_Dir & "/" & Name & ".ppm");
      String'Write (SIO.Stream (File), "P6" & ASCII.LF & Positive'Image (Width) & Positive'Image (Height)
                    & ASCII.LF & "255" & ASCII.LF);
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            P := Pixels (Y * Width + X);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 1)) := Ada.Streams.Stream_Element (Shift_Right (P, 16) and 255);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 2)) := Ada.Streams.Stream_Element (Shift_Right (P, 8) and 255);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 3)) := Ada.Streams.Stream_Element (P and 255);
         end loop;
         SIO.Write (File, Row);
      end loop;
      SIO.Close (File);
   end Write_PPM;

   function Image (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;
begin
   Neon_Replay.Start (R.all, Seed);
   Wave_Files.Create (Wave, Out_Dir & "/replay.wav");
   --  Every kick the composer placed, by song sample (the sync check).
   Ada.Text_IO.Create (Kicks, Ada.Text_IO.Out_File, Out_Dir & "/kicks.txt");
   for Frame in 1 .. Frames loop
      declare
         T0 : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         T1 : Ada.Calendar.Time;
      begin
         Neon_Replay.Step (R.all, Pixels.all'Address, Width, Height);
         T1 := Ada.Calendar.Clock;
         Render_Total := Render_Total + (T1 - T0);
         if T1 - T0 > Render_Worst then
            Render_Worst := T1 - T0;
         end if;
      end;
      Wave_Files.Write (Wave, R.Left, R.Right, Neon_Replay.Frame_Samples);
      declare
         At_Time : Samples;
         Found : Boolean;
      begin
         Neon_Composer.Last_Kick (R.Game.Composer, Neon_Game.Write_Head (R.Game), At_Time, Found);
         if Found and then At_Time > Last_Kick then
            Last_Kick := At_Time;
            Put_Line (Kicks, Samples'Image (At_Time));
         end if;
      end;
      if Shot_Every > 0 and then Frame mod Shot_Every = 0 then
         Write_PPM ("frame-" & Image (Frame));
      end if;
   end loop;
   Wave_Files.Close (Wave);
   Ada.Text_IO.Close (Kicks);
   declare
      O : constant Neon_Replay.Result := Neon_Replay.Outcome (R.all);
   begin
      Put_Line ("REPLAY frames=" & Image (O.Frames) & " seed=" & Unsigned_64'Image (Seed)
                & " size=" & Image (Width) & "x" & Image (Height));
      Put_Line ("REPLAY frame_hash=" & Unsigned_64'Image (O.Frame_Hash));
      Put_Line ("REPLAY audio_hash=" & Unsigned_64'Image (O.Audio_Hash));
      Put_Line ("REPLAY score=" & Image (O.Score) & " wave=" & Image (O.Wave) & " kills="
                & Image (O.Kills) & " on_beat=" & Image (O.On_Beat)
                & " dropped_notes=" & Image (Neon_Synth.Dropped_Notes (R.Game.Synth)));
      Put_Line ("REPLAY step_ms_avg=" & Duration'Image (Render_Total / Frames * 1000)
                & " step_ms_worst=" & Duration'Image (Render_Worst * 1000));
   end;
end Neon_Replay_Host;
