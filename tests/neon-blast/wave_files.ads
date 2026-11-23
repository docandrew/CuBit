------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted test helper: 48 kHz 16-bit stereo WAV output, with an
--  FNV-1a 64 hash of the PCM bytes written.
------------------------------------------------------------------------------
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with Neon_Synth;

package Wave_Files is
   type Writer is limited private;
   procedure Create (W : in out Writer; Path : String);
   procedure Write (W : in out Writer; Left, Right : Neon_Synth.Block; Frames : Natural);
   procedure Close (W : in out Writer);
   function Hash (W : Writer) return Unsigned_64;
   --  Float sample to 16-bit, as the game converts for the mixer.
   function To_PCM (Sample : Float) return Integer_16;
private
   type Writer is limited record
      File : Ada.Streams.Stream_IO.File_Type;
      Frames : Natural := 0;
      Digest : Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
   end record;
end Wave_Files;
