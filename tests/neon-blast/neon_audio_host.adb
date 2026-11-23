------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted: render the composer and synthesizer through a fixed mood
--  script to a 16-bit stereo WAV (and its kick times, OUTPUT.wav.kicks.txt),
--  for listening and the beat-to-sample check. Not a CuBit result.
--  Usage: neon_audio_host OUTPUT.wav [SECONDS] [SEED] [BLOCK_FRAMES]
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Composer;
with Neon_Synth;
with Wave_Files;

procedure Neon_Audio_Host is
   package C renames Neon_Composer;
   Seconds : constant Natural :=
     (if Ada.Command_Line.Argument_Count >= 2 then Natural'Value (Ada.Command_Line.Argument (2)) else 30);
   Seed : constant Unsigned_64 :=
     (if Ada.Command_Line.Argument_Count >= 3 then Unsigned_64'Value (Ada.Command_Line.Argument (3)) else 7);
   Block_Size : constant Positive :=
     (if Ada.Command_Line.Argument_Count >= 4 then Positive'Value (Ada.Command_Line.Argument (4))
      else Neon_Synth.Maximum_Block);
   Total : constant Natural := Seconds * Sample_Rate;
   Comp : C.Composer;
   Synth : Neon_Synth.Synth;
   Batch : C.Note_Batch;
   Info : C.Step_Record;
   Left, Right : Neon_Synth.Block;
   Done : Natural := 0;
   Frames : Natural;
   Out_File : Wave_Files.Writer;

   function Mood_At (Bar : Natural) return C.Mood is
   begin
      if Bar < 2 then
         return (Section => C.Title_Theme, Level => 0, Speed => Lounge, others => False);
      elsif Bar < 4 then
         return (Section => C.Groove, Level => 1, Speed => Lounge, others => False);
      elsif Bar < 6 then
         return (Section => C.Groove, Level => 2, Speed => Cruise, Arp_On => False, Lead_On => False);
      elsif Bar < 8 then
         return (Section => C.Build, Level => 3, Speed => Cruise, others => False);
      elsif Bar < 12 then
         return (Section => C.Peak, Level => 4, Speed => Drive, Arp_On => True, Lead_On => True);
      else
         return (Section => C.Breakdown, Level => 1, Speed => Drive, others => False);
      end if;
   end Mood_At;

   Bar : Natural := 0;
   Kicks : File_Type;
begin
   --  Every kick the composer placed (check_sync.py compares the audio).
   Create (Kicks, Ada.Text_IO.Out_File, Ada.Command_Line.Argument (1) & ".kicks.txt");
   C.Start (Comp, Seed);
   Neon_Synth.Start (Synth, Seed);
   Wave_Files.Create (Out_File, Ada.Command_Line.Argument (1));
   while Done < Total loop
      Frames := Natural'Min (Block_Size, Total - Done);
      while C.Next_Start (Comp) < Neon_Synth.Position (Synth) + Samples (Frames) loop
         Batch.Count := 0;
         C.Compose_Next (Comp, Mood_At (Bar), Batch, Info);
         Bar := Info.Bar;
         if Info.Kick then
            Put_Line (Kicks, Samples'Image (Info.Start));
         end if;
         for I in 1 .. Batch.Count loop
            Neon_Synth.Schedule (Synth, Batch.Items (I));
         end loop;
         Neon_Synth.Set_Targets (Synth, Info.Start, Info.Pad_Level, Info.Cutoff, 3 * Info.Length, 0.55);
      end loop;
      Neon_Synth.Render (Synth, Left, Right, Frames);
      Wave_Files.Write (Out_File, Left, Right, Frames);
      Done := Done + Frames;
   end loop;
   Wave_Files.Close (Out_File);
   Close (Kicks);
   Put_Line ("rendered" & Natural'Image (Seconds) & " s, dropped notes" &
             Natural'Image (Neon_Synth.Dropped_Notes (Synth)) & ", hash " &
             Wave_Files.Hash (Out_File)'Image);
end Neon_Audio_Host;
