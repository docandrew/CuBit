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
      Bass,       --  saw-and-sub bass; slides by Glide
      Lead,       --  vibrato pulse lead: the call of a phrase
      Arp,        --  plucked arpeggiator voice
      Bell,       --  FM bell: the response to the lead's call
      Kick, Snare, Hat, Open_Hat, Clap,
      Tom,        --  pitched tom for fills
      Crash,      --  crash cymbal at section starts
      Blip,       --  the player's shot, pitched to the chord
      Sparkle,    --  explosion arpeggio note
      Boom,       --  explosion body (noise and thump)
      Whoosh,     --  dash
      Chime,      --  combo and wave chimes
      Hurt,       --  the player is hit
      Rise,       --  spawn warning and build-up riser
      Impact,     --  sub drop at a drop's downbeat and a boss's arrival
      Zap,        --  enemy fire and warps: a pitched laser sweep (by Glide)
      Growl,      --  a boss's FM growl
      Vinyl,      --  lo-fi crackle bed (one note a bar)
      Click);     --  calibration metronome
   subtype Melodic is Instrument range Pad .. Bell;
   subtype Drum is Instrument range Kick .. Crash;
   subtype Effect is Instrument range Blip .. Click;

   --  A note's timbre controls, 0 .. 1 (dark .. bright, the instrument
   --  decides what that means: a kick's click, a snare's noise colour, a
   --  pad's filter), and a multiplier on its envelope's decay times.
   subtype Unit_Float is Float range 0.0 .. 1.0;
   subtype Decay_Scale is Float range 0.25 .. 4.0;
   --  Semitones the pitch slides from (a bass slide, a laser sweep).
   subtype Glide_Semitones is Float range -24.0 .. 24.0;

   --  Pan: -1.0 left .. 1.0 right. Pitch: MIDI note number (60 = middle C).
   type Note is record
      At_Time : Samples := 0;
      Voice : Instrument := Pad;
      Pitch : Float := 60.0;
      Velocity : Float := 1.0;
      Length : Positive := 1;
      Pan : Float := 0.0;
      Tone : Unit_Float := 0.5;
      Decay : Decay_Scale := 1.0;
      Glide : Glide_Semitones := 0.0;
   end record;

end Neon_Notes;
