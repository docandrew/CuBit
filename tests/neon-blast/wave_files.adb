------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Replay;
package body Wave_Files is
   package SIO renames Ada.Streams.Stream_IO;
   Header_Bytes : constant := 44;

   function To_PCM (Sample : Float) return Integer_16 renames Neon_Replay.PCM;

   procedure Put_32 (W : in out Writer; Value : Unsigned_32) is
   begin
      Unsigned_32'Write (SIO.Stream (W.File), Value);
   end Put_32;
   procedure Put_16 (W : in out Writer; Value : Unsigned_16) is
   begin
      Unsigned_16'Write (SIO.Stream (W.File), Value);
   end Put_16;

   procedure Header (W : in out Writer) is
      Data : constant Unsigned_32 := Unsigned_32 (W.Frames) * 4;
   begin
      SIO.Set_Index (W.File, 1);
      String'Write (SIO.Stream (W.File), "RIFF");
      Put_32 (W, 36 + Data);
      String'Write (SIO.Stream (W.File), "WAVEfmt ");
      Put_32 (W, 16);
      Put_16 (W, 1);         --  PCM
      Put_16 (W, 2);         --  stereo
      Put_32 (W, 48_000);
      Put_32 (W, 48_000 * 4);
      Put_16 (W, 4);
      Put_16 (W, 16);
      String'Write (SIO.Stream (W.File), "data");
      Put_32 (W, Data);
   end Header;

   procedure Create (W : in out Writer; Path : String) is
   begin
      SIO.Create (W.File, SIO.Out_File, Path);
      Header (W);
   end Create;

   procedure Feed (W : in out Writer; Value : Integer_16) is
   begin
      W.Digest := Neon_Replay.Digest_Sample (W.Digest, Value);
      Integer_16'Write (SIO.Stream (W.File), Value);
   end Feed;

   procedure Write (W : in out Writer; Left, Right : Neon_Synth.Block; Frames : Natural) is
   begin
      for I in 1 .. Frames loop
         Feed (W, To_PCM (Left (I)));
         Feed (W, To_PCM (Right (I)));
      end loop;
      W.Frames := W.Frames + Frames;
   end Write;

   procedure Close (W : in out Writer) is
   begin
      Header (W);
      SIO.Close (W.File);
   end Close;

   function Hash (W : Writer) return Unsigned_64 is (W.Digest);
   pragma Unreferenced (Header_Bytes);
end Wave_Files;
