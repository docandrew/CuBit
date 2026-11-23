------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast: the notes the composer and the game ask the synthesizer to
--  play, each at an exact song sample.
------------------------------------------------------------------------------
with Neon_Time; use Neon_Time;

package Neon_Notes with SPARK_Mode, Pure is

   type Instrument is
     (Pad,        --  detuned saw pad through the chorus
      Piano,      --  two-operator FM electric piano
      Bass,       --  wobbly saw-and-sub bass
      Lead,       --  vibrato lead for the melody
      Arp,        --  plucked arpeggio layer (rhythm combos unlock it)
      Kick, Snare, Hat, Open_Hat, Clap,
      Blip,       --  the player's shot, pitched to the chord
      Sparkle,    --  explosion arpeggio note
      Boom,       --  explosion body (noise and thump)
      Whoosh,     --  dash
      Chime,      --  combo and wave chimes
      Hurt,       --  the player is hit
      Rise,       --  spawn warning and build-up riser
      Click);     --  calibration metronome
   subtype Drum is Instrument range Kick .. Clap;
   subtype Effect is Instrument range Blip .. Click;

   --  Pan: -1.0 left .. 1.0 right. Pitch: MIDI note number (60 = middle C).
   type Note is record
      At_Time : Samples := 0;
      Voice : Instrument := Pad;
      Pitch : Float := 60.0;
      Velocity : Float := 1.0;
      Length : Positive := 1;
      Pan : Float := 0.0;
   end record;

end Neon_Notes;
