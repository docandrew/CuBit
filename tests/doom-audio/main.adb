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
   Reopened : Boolean;

   procedure Begin_Sound (Length : Natural; Rate : Positive := 48_000) is
      Opened : Boolean;
   begin
      Sink.Reset;
      Opened := Sound.Init;
      pragma Assert (Opened);
      Sound.Start (0, Source'Address, Length, Rate, 100, 128);
   end Begin_Sound;

   procedure Run_Case (Length : Natural; Rate : Positive) is
      Limits : constant array (Positive range 1 .. 8) of Natural :=
        [0, 1, 17, 0, 511, 3, 1535, 79];
      Previous_Calls : Natural;
   begin
      Begin_Sound (Length, Rate);
      for I in 1 .. 100 loop Sound.Update; end loop;
      pragma Assert (not Sound.Is_Playing (0));
      Expected_Frames := Sink.Frame_Count;
      Reference := Sink.Captured;
      pragma Assert (Expected_Frames > 0);
      Sound.Shutdown;

      Begin_Sound (Length, Rate);
      for I in 1 .. 1000 loop
         Sink.Limit := Limits ((I - 1) mod Limits'Length + 1);
         Previous_Calls := Sink.Calls;
         Sound.Update;
         -- An update must never spin waiting for the ring to gain space.
         pragma Assert (Sink.Calls - Previous_Calls <= 2);
      end loop;
      pragma Assert (not Sound.Is_Playing (0));
      pragma Assert (Sink.Frame_Count = Expected_Frames);
      pragma Assert (Sink.Captured = Reference);
      Sound.Shutdown;
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
   Sound.Update;
   pragma Assert (Sink.Frame_Count = 1535 and not Sound.Is_Playing (0));
   Sink.Limit := 1;
   Sound.Update;
   pragma Assert (Sink.Frame_Count = 1536);
   Sound.Shutdown;

   -- A full sink must not consume CPU or overwrite the retained batch.
   Begin_Sound (73);
   Sink.Limit := 0;
   for I in 1 .. 100 loop Sound.Update; end loop;
   pragma Assert (Sink.Frame_Count = 0);
   Sink.Limit := Natural'Last;
   Sound.Update;
   pragma Assert (Sink.Frame_Count = 1536);
   Sound.Shutdown;

   -- Channel reuse while old PCM is pending must not overwrite that tail.
   begin
      Begin_Sound (73);
      Sound.Update;
      Sound.Start (0, Source (100)'Address, 41, 48_000, 90, 20);
      Sound.Update;
      Reference := Sink.Captured;
      pragma Assert (Sink.Frame_Count = 3072);
      Sound.Shutdown;
      Begin_Sound (73);
      Sink.Limit := 0;
      Sound.Update;
      Sound.Start (0, Source (100)'Address, 41, 48_000, 90, 20);
      Sink.Limit := 79;
      for I in 1 .. 100 loop Sound.Update; end loop;
      pragma Assert (Sink.Frame_Count = 3072 and Sink.Captured = Reference);
      Sound.Shutdown;
   end;

   -- Shutdown discards pending output and active channels; reinitialization
   -- cannot replay either of them.
   Begin_Sound (5000);
   Sink.Limit := 0;
   Sound.Update;
   pragma Assert (Sound.Is_Playing (0));
   Sound.Shutdown;
   Sink.Reset;
   Reopened := Sound.Init;
   pragma Assert (Reopened);
   Sound.Update;
   pragma Assert (Sink.Frame_Count = 0);
   Sound.Shutdown;
   Ada.Text_IO.Put_Line
     ("DOOM-AUDIO: PASS (short/zero writes, exact PCM order, final tail, reuse, reset)");
end Main;
