------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's engine: the simulation, the composer and the synthesizer on
--  one song clock.
--
--  @description
--  Two song positions move forward:
--  - the write head, where the synthesizer renders next (ahead, by the
--    audio queue's depth);
--  - the simulation's time, which follows what is heard now.
--  The composer stays ahead of both. Sounds the simulation asks for start
--  at the write head (a shot's latency is the queue's), except the musical
--  ones: an explosion's arpeggio waits for its next sixteenth, and its
--  flash is scheduled for that same sample, so the eye and the ear agree.
--
--  The native app drives the write head from the audio ring's free space
--  and the simulation from the measured playback position; the hosted
--  replay drives both in lockstep with a fixed gap, so a seed and an input
--  stream reproduce every frame and every sample.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Sim;
with Neon_Composer;
with Neon_Synth;

package Neon_Game is

   type Game is limited record
      Sim : Neon_Sim.World;
      Composer : Neon_Composer.Composer;
      Synth : Neon_Synth.Synth;
      Next_Tick : Samples := 0;       --  song time of the next simulation step
      Current : Neon_Composer.Step_Record;
      Sounds : Neon_Sim.Sound_List;
   end record;

   procedure Start (G : out Game; Seed : Unsigned_64);

   --  Step the simulation until its time reaches Target. Edge controls
   --  (Dash, Confirm, Calibrate, Tap, Press_Judged and the title's
   --  pickers) apply to the first step only; held ones to every step.
   procedure Advance (G : in out Game; Input : Neon_Sim.Controls; Target : Samples);

   --  Render the next Frames of audio at the write head.
   procedure Render_Audio (G : in out Game; Left, Right : out Neon_Synth.Block;
                           Frames : Neon_Synth.Block_Frames);

   function Write_Head (G : Game) return Samples is (Neon_Synth.Position (G.Synth));
   function Sim_Time (G : Game) return Samples is (G.Next_Tick);

   --  Signed distance of song time Time from the nearest composed beat, in
   --  milliseconds (Found False when no beat is known there).
   procedure Beat_Error_Ms (G : Game; Time : Samples; Error : out Float; Found : out Boolean);

end Neon_Game;
