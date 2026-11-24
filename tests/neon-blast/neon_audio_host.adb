------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted: render the composer and synthesizer as the game would
--  drive them, to a 16-bit stereo WAV, for listening and the checks. Not a
--  CuBit result.
--
--  @description
--  The mood script: the title for Title_Bars bars, then a run from the
--  playlist seed's first stage (or Start_Stage), each stage's boss
--  "defeated" after Boss_Bars bars of its peak, so the piece goes on
--  through outros into the next stages, as a game that wins would hear it.
--  Writes OUTPUT.wav, its kick times (OUTPUT.wav.kicks.txt: the
--  beat-to-sample check) and its sections (OUTPUT.wav.sections.txt: song
--  sample, stage, section, BPM, style; the flow check). Prints the
--  synthesis cost per block (the release build's numbers are the ones
--  that matter).
--
--  Usage: neon_audio_host OUTPUT.wav [SECONDS] [SEED] [BLOCK_FRAMES]
--                         [PLAYLIST] [START_STAGE] [TITLE_BARS] [BOSS_BARS]
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Composer;
with Neon_Notes;
with Neon_Style;
with Neon_Synth;
with Wave_Files;

procedure Neon_Audio_Host is
   package C renames Neon_Composer;
   use type C.Section_Kind;
   use type C.Scene_Kind;
   function Argument_Or (Index : Positive; Default : Natural) return Natural is
     (if Ada.Command_Line.Argument_Count >= Index
      then Natural'Value (Ada.Command_Line.Argument (Index)) else Default);
   Seconds : constant Natural := Argument_Or (2, 30);
   Seed : constant Unsigned_64 := Unsigned_64 (Argument_Or (3, 7));
   Block_Size : constant Positive := Positive'Max (1, Argument_Or (4, Neon_Synth.Maximum_Block));
   Playlist : constant Neon_Style.Playlist_Seed :=
     Neon_Style.Playlist_Seed (Natural'Max (1, Argument_Or (5, Natural (Seed))));
   Start_Stage : constant C.Stage_Number := C.Stage_Number (Natural'Max (1, Argument_Or (6, 1)));
   Title_Bars : constant Natural := Argument_Or (7, 2);
   Boss_Bars : constant Natural := Argument_Or (8, 8);
   Total : constant Natural := Seconds * Sample_Rate;
   type Synth_Access is access Neon_Synth.Synth;
   Comp : C.Composer;
   Synth : constant Synth_Access := new Neon_Synth.Synth;
   Batch : C.Note_Batch;
   Info : C.Step_Record;
   Mix : C.Mix_Targets;
   Left, Right : Neon_Synth.Block;
   Done : Natural := 0;
   Frames : Natural;
   Wave : Wave_Files.Writer;
   Kicks, Sections : File_Type;
   Mood : C.Mood := (Scene => C.Attract, Playlist => Playlist, Start_Stage => Start_Stage, others => <>);
   Bars_Seen : Natural := 0;
   Peak_Bars : Natural := 0;
   Last_Section : C.Section_Kind := C.Metronome;
   Last_Stage : Natural := Natural'Last;
   Spent, Worst : Duration := 0.0;
   Blocks : Natural := 0;
   --  Every kick note sits exactly on its step's first sample, and every
   --  step the composer marks as a kick has one (the grid is the clock).
   Kick_Steps, Off_Grid : Natural := 0;
begin
   Create (Kicks, Out_File, Ada.Command_Line.Argument (1) & ".kicks.txt");
   Create (Sections, Out_File, Ada.Command_Line.Argument (1) & ".sections.txt");
   C.Start (Comp, Seed);
   Neon_Synth.Start (Synth.all, Seed);
   Wave_Files.Create (Wave, Ada.Command_Line.Argument (1));
   while Done < Total loop
      Frames := Natural'Min (Block_Size, Total - Done);
      while C.Next_Start (Comp) < Neon_Synth.Position (Synth.all) + Samples (Frames) loop
         Batch.Count := 0;
         C.Compose_Next (Comp, Mood, Batch, Info, Mix);
         if Info.Kick then
            Put_Line (Kicks, Samples'Image (Info.Start));
         end if;
         declare
            use type Neon_Notes.Instrument;
            Found : Boolean := False;
         begin
            for I in 1 .. Batch.Count loop
               if Batch.Items (I).Voice = Neon_Notes.Kick then
                  Found := True;
                  if Batch.Items (I).At_Time /= Info.Start or else not Info.Kick then
                     Off_Grid := Off_Grid + 1;
                  end if;
               end if;
            end loop;
            if Info.Kick and then Info.Section /= C.Metronome then
               Kick_Steps := Kick_Steps + 1;
               if not Found then
                  Off_Grid := Off_Grid + 1;
               end if;
            end if;
         end;
         if Info.Section /= Last_Section or else Info.Stage /= Last_Stage then
            Put_Line (Sections, Samples'Image (Info.Start) & Natural'Image (Info.Stage) & " "
                      & C.Section_Kind'Image (Info.Section) & Natural'Image (BPM (Info.Length)) & " "
                      & C.Style_Name (Info) & " / " & Neon_Style.Region_Name (Info.Region));
            Last_Section := Info.Section;
            Last_Stage := Info.Stage;
         end if;
         for I in 1 .. Batch.Count loop
            Neon_Synth.Schedule (Synth.all, Batch.Items (I));
         end loop;
         Neon_Synth.Set_Targets
           (Synth.all, Info.Start,
            (Pad_Level => Mix.Pad_Level, Cutoff => Mix.Cutoff, Delay_Samples => Mix.Delay_Samples,
             Echo_Feedback => Mix.Echo_Feedback, Wobble => Mix.Wobble, Reverb_Wet => Mix.Reverb_Wet,
             Reverb_Room => Mix.Reverb_Room, Room_Level => Mix.Room_Level));
         --  The script, a bar at a time (the composer reads moods on bars).
         if Info.Step = Step_Index'Last then
            Bars_Seen := Bars_Seen + 1;
            if Bars_Seen >= Title_Bars and then Mood.Scene = C.Attract then
               Mood.Scene := C.Play;
               Mood.Run := 1;
            end if;
            if Info.Section = C.Peak then
               Peak_Bars := Peak_Bars + 1;
               if Peak_Bars >= Boss_Bars then
                  Mood.Defeated_Stage := Info.Stage;
               end if;
            else
               Peak_Bars := 0;
            end if;
         end if;
      end loop;
      declare
         Began : constant Time := Clock;
         Took : Duration;
      begin
         Neon_Synth.Render (Synth.all, Left, Right, Frames);
         Took := Clock - Began;
         Spent := Spent + Took;
         if Frames = Block_Size and then Took > Worst then
            Worst := Took;
         end if;
         Blocks := Blocks + 1;
      end;
      Wave_Files.Write (Wave, Left, Right, Frames);
      Done := Done + Frames;
   end loop;
   Wave_Files.Close (Wave);
   Close (Kicks);
   Close (Sections);
   Put_Line ("rendered" & Natural'Image (Seconds) & " s, playlist" & Unsigned_64'Image (Playlist)
             & ", dropped notes" & Natural'Image (Neon_Synth.Dropped_Notes (Synth.all))
             & ", composer refusals" & Natural'Image (C.Dropped (Comp)) & ", hash "
             & Wave_Files.Hash (Wave)'Image);
   Put_Line ("synthesis: blocks" & Natural'Image (Blocks) & " of" & Positive'Image (Block_Size)
             & " frames, us_per_block_avg="
             & Natural'Image (Natural (Float (Spent) * 1.0e6 / Float (Natural'Max (1, Blocks))))
             & " us_per_block_max=" & Natural'Image (Natural (Float (Worst) * 1.0e6))
             & " load_pct=" & Natural'Image (Natural (100.0 * Float (Spent) / Float (Seconds))));
   Put_Line ("NEON-KICKS: kick steps" & Natural'Image (Kick_Steps) & ", off the grid"
             & Natural'Image (Off_Grid) & (if Off_Grid = 0 then " PASS" else " FAIL"));
   if Off_Grid /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Neon_Audio_Host;
