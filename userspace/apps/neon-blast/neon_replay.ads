------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's replay benchmark: a fixed seed and the autopilot, driven in
--  lockstep (60 frames a song second, the audio a fixed gap ahead of the
--  simulation), rendered by the software path at a fixed size. It digests
--  every frame's pixels and every audio sample, so the same build gives the
--  same two hashes on CuBit and on a Linux host (tests/neon-blast), and a
--  change to the simulation, the music or the renderer shows as a new hash.
------------------------------------------------------------------------------
with System;
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Game;
with Neon_Scene;
with Neon_Soft;
with Neon_Synth;

package Neon_Replay is

   Frames_Per_Second : constant := 60;
   Frame_Samples : constant := Sample_Rate / Frames_Per_Second;
   --  The simulation follows the write head by this much (33 ms).
   Audio_Gap : constant := 1_600;

   type Result is record
      Frames : Natural := 0;
      Frame_Hash, Audio_Hash : Unsigned_64 := 0;
      Score, Stage, Kills, On_Beat : Natural := 0;
   end record;

   --  The audio sample as 16-bit PCM, as written to the mixer and the WAV.
   function PCM (Sample : Float) return Integer_16;
   --  FNV-1a 64 step over one 16-bit sample (little-endian bytes).
   function Digest_Sample (Digest : Unsigned_64; Value : Integer_16) return Unsigned_64;
   Digest_Start : constant Unsigned_64 := 16#CBF2_9CE4_8422_2325#;

   type Replay is limited record
      Game : Neon_Game.Game;
      Frame : Neon_Scene.Frame;
      Renderer : Neon_Soft.Renderer;
      Left, Right : Neon_Synth.Block;
      Result_So_Far : Result;
   end record;

   procedure Start (R : in out Replay; Seed : Unsigned_64);
   --  One frame: a frame's audio, the simulation to the heard time, the
   --  software render into Target (Width x Height, packed rows).
   procedure Step (R : in out Replay; Target : System.Address; Width, Height : Positive);
   function Outcome (R : Replay) return Result;

end Neon_Replay;
