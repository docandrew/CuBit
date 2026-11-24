------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's one clock: song time in samples at 48 kHz.
--
--  @description
--  The soundtrack's tempo grid is the game's clock (APP-008, "One clock").
--  Every tempo the composer uses has a whole number of samples per
--  sixteenth note, so beats and bars land on exact samples and a replay
--  reproduces them bit for bit. The simulation steps 240 times a second of
--  song time (Tick_Samples), whatever the display's refresh.
------------------------------------------------------------------------------
package Neon_Time with SPARK_Mode, Pure is

   Sample_Rate : constant := 48_000;
   --  Song position in samples since the song started.
   type Samples is range 0 .. 2**60;
   --  A signed distance between two song positions.
   type Sample_Offset is range -(2**60) .. 2**60;

   --  The simulation step: 200 samples, 1/240 s.
   Tick_Samples : constant := 200;
   Ticks_Per_Second : constant := Sample_Rate / Tick_Samples;
   Tick_Seconds : constant := 1.0 / Float (Ticks_Per_Second);

   Steps_Per_Beat : constant := 4;
   Beats_Per_Bar : constant := 4;
   Steps_Per_Bar : constant := Steps_Per_Beat * Beats_Per_Bar;
   subtype Step_Index is Natural range 0 .. Steps_Per_Bar - 1;

   --  A tempo is its sixteenth note's length in samples: any whole number
   --  in this range (80 .. 160 BPM), so every style's beats and bars land
   --  on exact samples, whatever tempo its seed draws.
   Slowest_Step : constant := 9_000;    --  80 BPM
   Fastest_Step : constant := 4_500;    --  160 BPM
   subtype Step_Samples is Positive range Fastest_Step .. Slowest_Step;
   --  Samples per minute over sixteenths per beat: BPM = this / step.
   Sixteenth_Minute : constant := 60 * Sample_Rate / Steps_Per_Beat;
   function BPM (Step : Step_Samples) return Positive is (Sixteenth_Minute / Step);
   function Step_For_BPM (Beats : Positive) return Step_Samples is
     (Step_Samples (Sixteenth_Minute / Beats))
     with Pre => Beats in Sixteenth_Minute / Slowest_Step .. Sixteenth_Minute / Fastest_Step;
   --  The calibration metronome's and the first composed step's tempo.
   Default_Step : constant Step_Samples := 8_000;   --  90 BPM

   function Seconds (Count : Sample_Offset) return Float is (Float (Count) / Float (Sample_Rate));
   function Milliseconds (Count : Sample_Offset) return Float is
     (Float (Count) * 1000.0 / Float (Sample_Rate));
   function To_Samples (Seconds : Float) return Sample_Offset is
     (Sample_Offset (Seconds * Float (Sample_Rate)))
     with Pre => abs Seconds < 1.0e6;

end Neon_Time;
