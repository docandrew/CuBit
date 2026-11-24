------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's synthesizer: the band's voices and sound effects, the mix
--  buses and the master chain, rendered sample by sample at 48 kHz.
--
--  @description
--  Notes wait in a time-ordered queue and start on their exact sample.
--  Voices (no samples: oscillators, FM pairs and filtered noise) feed four
--  buses: dry, the pad's chorus, the reverb send and the ping-pong delay
--  send; the snare and clap also feed a short gated room, the 80s gated
--  snare. The kick drives the sidechain that pumps the pad, piano, bass and
--  reverb. The master then runs through the "underwater" low-pass sweep,
--  tape wow and flutter, and the limiter.
--
--  Each note carries its timbre (Tone), a scale on its decay and a glide:
--  the style's kit and sounds are notes' values, not this unit's state, so
--  a style change is exact to the sample like everything else. The mix
--  targets (Set_Targets) carry the style's effects.
--
--  Underruns are not this unit's business: it renders exactly the frames
--  asked for, and the caller counts where they could not be delivered.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Notes;
with Neon_DSP;

package Neon_Synth with SPARK_Mode is

   Maximum_Block : constant := 1024;
   subtype Block_Frames is Natural range 0 .. Maximum_Block;
   type Block is array (1 .. Maximum_Block) of Float;

   --  The queue holds notes not yet started; a full queue refuses a note
   --  and counts it (Dropped_Notes).
   Queue_Ceiling : constant := 512;
   --  Voices sounding at once; a new note steals the quietest when full.
   Voice_Ceiling : constant := 64;

   type Synth is limited private;

   procedure Start (S : out Synth; Seed : Unsigned_64);
   --  The song position of the next rendered frame (the write head).
   function Position (S : Synth) return Samples;
   --  Queue a note. One starting before the write head starts at once.
   procedure Schedule (S : in out Synth; Item : Neon_Notes.Note);
   --  From song time At_Time on, the mix glides to these targets: pad
   --  loudness, master low-pass opening (0 underwater .. 1 open), the delay
   --  time in samples and its feedback, the tape wobble depth, the reverb's
   --  return and room size and the gated room's return (each 0 .. 1).
   --  Timed like notes, so the rendered samples do not depend on how the
   --  caller cuts blocks.
   type Targets is record
      Pad_Level, Cutoff : Float := 0.5;
      Delay_Samples : Positive := 24_000;
      Echo_Feedback : Float := 0.42;
      Wobble : Float := 0.6;
      Reverb_Wet : Float := 1.0;
      Reverb_Room : Float := 0.92;
      Room_Level : Float := 1.0;
   end record;
   procedure Set_Targets (S : in out Synth; At_Time : Samples; Wanted : Targets);
   --  Render the next Frames frames.
   procedure Render (S : in out Synth; Left, Right : out Block; Frames : Block_Frames);

   --  Samples between a note's start and its sound leaving the master
   --  chain (the tape stage's fixed delay): what is heard lags the grid by
   --  this much.
   Output_Latency : constant := Neon_DSP.Tape_Delay_Samples;

   --  Diagnostics.
   function Dropped_Notes (S : Synth) return Natural;
   function Voices_Sounding (S : Synth) return Natural;
   function Peak_Level (S : Synth) return Float;    --  since the last call to Reset_Peak
   --  The limiter's deepest gain reduction since Reset_Peak (0 = none, 0.5 = -6 dB).
   function Limiting (S : Synth) return Float;
   procedure Reset_Peak (S : in out Synth);
   --  The sidechain envelope now (0 .. 1), for visuals that pump with it.
   function Duck (S : Synth) return Float;

private
   use Neon_Notes;

   type Queue_Array is array (1 .. Queue_Ceiling) of Note;
   subtype Queue_Count is Natural range 0 .. Queue_Ceiling;

   type Voice is record
      Active : Boolean := False;
      Kind : Instrument := Pad;
      Frequency : Float := 440.0;
      Glide_From : Float := 440.0;
      Velocity : Float := 1.0;
      Pan : Float := 0.0;
      Age : Natural := 0;           --  samples since its start
      Tone : Float := 0.5;          --  the note's timbre, 0 .. 1
      Glide : Float := 0.0;         --  semitones the pitch slides from (decays by Aux_2)
      Length : Positive := 1;       --  samples until release
      Releasing : Boolean := False;
      Envelope : Float := 0.0;
      Phase_1, Phase_2, Phase_3, Phase_4, Phase_5 : Float := 0.0;
      --  Two per-voice modulation envelopes (pitch drops, filter and FM
      --  index sweeps) and their per-sample decay factors.
      Aux_1, Aux_2 : Float := 0.0;
      Aux_Decay_1, Aux_Decay_2 : Float := 0.0;
      --  ADSR per-sample factors, fixed at the note's start.
      Attack_Step, Decay_Factor, Sustain, Release_Factor : Float := 0.0;
      Gain_Left, Gain_Right : Float := 0.0;
      Filter : Neon_DSP.State_Variable;
      Noise : Unsigned_32 := 1;
      Last_Output : Float := 0.0;
   end record;
   type Voice_Array is array (1 .. Voice_Ceiling) of Voice;

   type Target_Change is record
      At_Time : Samples := 0;
      Pad_Level, Cutoff, Delay_Time, Wobble, Echo_Feedback, Reverb_Wet, Reverb_Room,
      Room_Level : Float := 0.0;
   end record;
   Change_Ceiling : constant := 64;
   type Change_Array is array (1 .. Change_Ceiling) of Target_Change;

   type Synth is limited record
      Head : Samples := 0;
      Queue : Queue_Array;
      Queued : Queue_Count := 0;
      Dropped : Natural := 0;
      Voices : Voice_Array;
      Last_Lead : Float := 440.0;
      Pad_Level, Pad_Target : Float := 0.6;
      Cutoff, Cutoff_Target : Float := 0.5;
      Delay_Time, Delay_Target : Float := 24_000.0;
      Wobble : Float := 0.6;
      Echo_Feedback, Echo_Target : Float := 0.42;
      Reverb_Wet, Wet_Target : Float := 1.0;
      Room_Size, Room_Target : Float := 0.92;
      Room_Level, Room_Level_Target : Float := 1.0;
      Changes : Change_Array;
      Changes_Queued : Natural range 0 .. Change_Ceiling := 0;
      Duck_Level : Float := 0.0;
      Gate_Level : Float := 0.0;
      Gate_Open : Natural := 0;     --  samples the snare gate stays open
      Reverb : Neon_DSP.Reverb;
      Room : Neon_DSP.Reverb;       --  the gated snare room
      Echo : Neon_DSP.Ping_Pong;
      Pad_Chorus : Neon_DSP.Chorus;
      Wow : Neon_DSP.Tape;
      Master_Left, Master_Right : Neon_DSP.State_Variable;
      Master_Tuned : Natural := 0;
      Limit : Neon_DSP.Limiter;
      Peak : Float := 0.0;
      Rumble_Left, Rumble_Right : Neon_DSP.One_Pole;   --  DC and sub-rumble removal
   end record;
end Neon_Synth;
