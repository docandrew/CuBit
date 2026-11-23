------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Autopilot;
with Neon_Math;

package body Neon_Replay is

   FNV_Prime : constant Unsigned_64 := 16#100_0000_01B3#;
   PCM_Scale : constant := 32_767.0;

   function PCM (Sample : Float) return Integer_16 is
     (Integer_16 (Neon_Math.Nearest (Neon_Math.Clamp (Sample, -1.0, 1.0) * PCM_Scale)));

   function Digest_Sample (Digest : Unsigned_64; Value : Integer_16) return Unsigned_64 is
      Bits : constant Unsigned_16 := Unsigned_16 (Integer (Value) mod 2**16);
      D : Unsigned_64 := Digest;
   begin
      D := (D xor Unsigned_64 (Bits and 16#FF#)) * FNV_Prime;
      return (D xor Unsigned_64 (Shift_Right (Bits, 8))) * FNV_Prime;
   end Digest_Sample;

   procedure Start (R : in out Replay; Seed : Unsigned_64) is
   begin
      Neon_Game.Start (R.Game, Seed);
      R.Result_So_Far := (Frame_Hash => Digest_Start, Audio_Hash => Digest_Start, others => 0);
   end Start;

   procedure Step (R : in out Replay; Target : System.Address; Width, Height : Positive) is
      Head : Samples;
   begin
      Neon_Game.Render_Audio (R.Game, R.Left, R.Right, Frame_Samples);
      for I in 1 .. Frame_Samples loop
         R.Result_So_Far.Audio_Hash := Digest_Sample (R.Result_So_Far.Audio_Hash, PCM (R.Left (I)));
         R.Result_So_Far.Audio_Hash := Digest_Sample (R.Result_So_Far.Audio_Hash, PCM (R.Right (I)));
      end loop;
      Head := Neon_Game.Write_Head (R.Game);
      while Head > Audio_Gap and then Neon_Game.Sim_Time (R.Game) < Head - Audio_Gap loop
         Neon_Game.Advance (R.Game, Neon_Autopilot.Drive (R.Game.Sim, Neon_Game.Sim_Time (R.Game)),
                            Neon_Game.Sim_Time (R.Game) + Tick_Samples);
      end loop;
      Neon_Scene.Build (R.Frame, R.Game, Width, Height);
      Neon_Soft.Render (R.Renderer, R.Frame, Target, Width, Height, Width);
      R.Result_So_Far.Frame_Hash :=
        (R.Result_So_Far.Frame_Hash xor Neon_Soft.Frame_Hash (Target, Width, Height, Width)) * FNV_Prime;
      R.Result_So_Far.Frames := R.Result_So_Far.Frames + 1;
   end Step;

   function Outcome (R : Replay) return Result is
     ((R.Result_So_Far with delta
         Score => R.Game.Sim.Score, Wave => R.Game.Sim.Wave, Kills => R.Game.Sim.Kills,
         On_Beat => R.Game.Sim.On_Beat_Kills));

end Neon_Replay;
