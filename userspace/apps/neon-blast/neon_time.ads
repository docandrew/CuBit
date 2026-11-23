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

   --  The tempos the soundtrack moves between, slow to fast.
   type Tempo is (Lounge, Cruise, Drive, Rush);
   --  Samples per sixteenth note: 720_000 / BPM, a whole number for each.
   type Step_Lengths is array (Tempo) of Positive;
   Step_Length : constant Step_Lengths :=
     [Lounge => 8_000,   --  90 BPM
      Cruise => 7_500,   --  96 BPM
      Drive => 7_200,    --  100 BPM
      Rush => 6_000];    --  120 BPM
   type Tempo_BPM is array (Tempo) of Positive;
   BPM : constant Tempo_BPM := [Lounge => 90, Cruise => 96, Drive => 100, Rush => 120];

   function Seconds (Count : Sample_Offset) return Float is (Float (Count) / Float (Sample_Rate));
   function Milliseconds (Count : Sample_Offset) return Float is
     (Float (Count) * 1000.0 / Float (Sample_Rate));
   function To_Samples (Seconds : Float) return Sample_Offset is
     (Sample_Offset (Seconds * Float (Sample_Rate)))
     with Pre => abs Seconds < 1.0e6;

end Neon_Time;
