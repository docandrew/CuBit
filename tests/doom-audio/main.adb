with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Audio;
with CuBit.Doom_Sound;
procedure Main is
   package Sink renames CuBit.Audio;
   package Sound renames CuBit.Doom_Sound;
   use type Sink.Sample_Array;
   type Source_Array is array (Natural range 0 .. 4999) of Unsigned_8;
   Source : aliased Source_Array;
   Reference : Sink.Sample_Array;
   Expected_Frames : Natural;

   procedure Begin_Sound (Length : Unsigned_32; Rate : Unsigned_32 := 48_000) is
      Result : Integer;
   begin
      Sink.Reset;
      Result := Sound.sndInit;
      pragma Assert (Result = 1);
      Result := Sound.sndStartChannel
        (Source'Address, Length, Rate, 0, 100, 128);
      pragma Assert (Result = 0);
   end Begin_Sound;

   procedure Run_Case (Length, Rate : Unsigned_32) is
      Limits : constant array (Positive range 1 .. 8) of Natural :=
        [0, 1, 17, 0, 511, 3, 1535, 79];
      Previous_Calls : Natural;
   begin
      Begin_Sound (Length, Rate);
      for I in 1 .. 100 loop Sound.sndUpdate; end loop;
      pragma Assert (Sound.sndIsPlaying (0) = 0);
      Expected_Frames := Sink.Frame_Count;
      Reference := Sink.Captured;
      pragma Assert (Expected_Frames > 0);
      Sound.sndShutdown;

      Begin_Sound (Length, Rate);
      for I in 1 .. 1000 loop
         Sink.Limit := Limits ((I - 1) mod Limits'Length + 1);
         Previous_Calls := Sink.Calls;
         Sound.sndUpdate;
         -- An update must never spin waiting for the ring to gain space.
         pragma Assert (Sink.Calls - Previous_Calls <= 2);
      end loop;
      pragma Assert (Sound.sndIsPlaying (0) = 0);
      pragma Assert (Sink.Frame_Count = Expected_Frames);
      pragma Assert (Sink.Captured = Reference);
      Sound.sndShutdown;
   end Run_Case;
begin
   for I in Source'Range loop
      Source (I) := Unsigned_8 (1 + ((I * 73 + 19) mod 253));
   end loop;
   -- Short effect ends during the very first batch: pending PCM must still
   -- drain even after the channel has become inactive.
   Run_Case (73, 48_000);
   Run_Case (1536, 48_000);
   Run_Case (5000, 48_000);
   Run_Case (2000, 11_025);

   -- Explicitly leave the final frame pending, then drain after inactivity.
   Begin_Sound (73);
   Sink.Limit := 1535;
   Sound.sndUpdate;
   pragma Assert (Sink.Frame_Count = 1535 and Sound.sndIsPlaying (0) = 0);
   Sink.Limit := 1;
   Sound.sndUpdate;
   pragma Assert (Sink.Frame_Count = 1536);
   Sound.sndShutdown;

   -- A full sink must not consume CPU or overwrite the retained batch.
   Begin_Sound (73);
   Sink.Limit := 0;
   for I in 1 .. 100 loop Sound.sndUpdate; end loop;
   pragma Assert (Sink.Frame_Count = 0);
   Sink.Limit := Natural'Last;
   Sound.sndUpdate;
   pragma Assert (Sink.Frame_Count = 1536);
   Sound.sndShutdown;

   -- Channel reuse while old PCM is pending must not overwrite that tail.
   declare
      Result : Integer;
   begin
      Begin_Sound (73);
      Sound.sndUpdate;
      Result := Sound.sndStartChannel (Source (100)'Address, 41, 48_000, 0, 90, 20);
      pragma Assert (Result = 0);
      Sound.sndUpdate;
      Reference := Sink.Captured;
      pragma Assert (Sink.Frame_Count = 3072);
      Sound.sndShutdown;
      Begin_Sound (73);
      Sink.Limit := 0;
      Sound.sndUpdate;
      Result := Sound.sndStartChannel (Source (100)'Address, 41, 48_000, 0, 90, 20);
      pragma Assert (Result = 0);
      Sink.Limit := 79;
      for I in 1 .. 100 loop Sound.sndUpdate; end loop;
      pragma Assert (Sink.Frame_Count = 3072 and Sink.Captured = Reference);
      Sound.sndShutdown;
   end;

   -- Shutdown discards pending output and active channels; reinitialization
   -- cannot replay either of them.
   Begin_Sound (5000);
   Sink.Limit := 0;
   Sound.sndUpdate;
   pragma Assert (Sound.sndIsPlaying (0) = 1);
   Sound.sndShutdown;
   Sink.Reset;
   pragma Assert (Sound.sndInit = 1);
   Sound.sndUpdate;
   pragma Assert (Sink.Frame_Count = 0);
   Sound.sndShutdown;
   Ada.Text_IO.Put_Line
     ("DOOM-AUDIO: PASS (short/zero writes, exact PCM order, final tail, reuse, reset)");
end Main;
