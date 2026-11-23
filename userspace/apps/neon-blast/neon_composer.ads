------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's composer: a seeded generative vaporwave soundtrack on an
--  exact sixteenth-note grid, and that grid as the game's clock.
--
--  @description
--  Each sixteenth note is composed once, just before the synthesizer reaches
--  it: the drums, bass, electric piano, pad, melody and arpeggio notes that
--  start in it, from the bar's chord (slow major-seventh and minor-ninth
--  progressions, one chord a bar) and the mood the game asks for. The mood
--  is read at bar starts only, so tempo and section changes fall on bars.
--
--  The composed steps stay in a short history the game reads by song time:
--  the simulation spawns on them, the renderer pulses the grid on their
--  kicks and swells the sun on their chord changes, and shots take their
--  pitch from their chord.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Notes;
with Neon_Random;

package Neon_Composer with SPARK_Mode is

   type Section_Kind is
     (Title_Theme,   --  attract screen: pad and piano, soft hats
      Groove,        --  a wave in play
      Build,         --  harder waves coming: risers, snare rolls
      Peak,          --  everything on
      Breakdown,     --  between waves: filtered, lo-fi, no drums
      Finale,        --  game over: slow, filtered
      Metronome);    --  calibration: clicks on every beat over the pad

   --  How much of the band plays, 0 (pad alone) .. 5 (everything).
   subtype Level_Range is Natural range 0 .. 5;

   type Mood is record
      Section : Section_Kind := Title_Theme;
      Level : Level_Range := 0;
      Speed : Tempo := Lounge;
      Arp_On : Boolean := False;    --  rhythm combo unlock
      Lead_On : Boolean := False;   --  rhythm combo unlock
   end record;

   type Chord_Quality is
     (Major_7, Major_9, Minor_7, Minor_9, Dominant_9, Dominant_13, Suspended_9, Minor_6,
      Major_7_Sharp_11, Add_9);

   Maximum_Tones : constant := 5;
   subtype Tone_Count is Natural range 0 .. Maximum_Tones;
   subtype Tone_Index is Positive range 1 .. Maximum_Tones;
   type Tone_Array is array (Tone_Index) of Float;

   type Chord_Value is record
      Root : Natural := 60;                --  MIDI note of the root
      Quality : Chord_Quality := Major_7;
      Count : Tone_Count := 0;
      Tones : Tone_Array := [others => 60.0];  --  voiced pad notes, ascending
      Bass : Float := 36.0;
   end record;

   type Step_Record is record
      Serial : Natural := 0;
      Start : Samples := 0;
      Length : Positive := Step_Length (Lounge);
      Bar : Natural := 0;
      Step : Step_Index := 0;
      Section : Section_Kind := Title_Theme;
      Level : Level_Range := 0;
      Speed : Tempo := Lounge;
      Chord : Chord_Value;
      Chord_Change : Boolean := False;
      Kick, Snare : Boolean := False;
      --  0 .. 1 targets the synthesizer glides to: pad loudness and the
      --  master low-pass opening (0 = underwater).
      Pad_Level : Float := 0.5;
      Cutoff : Float := 1.0;
   end record;

   function Is_Beat (S : Step_Record) return Boolean is (S.Step mod Steps_Per_Beat = 0);
   function Beat_In_Bar (S : Step_Record) return Natural is (S.Step / Steps_Per_Beat);
   --  The pitch of a shot: the chord's tones walked upward by Degree, an
   --  octave above the pad.
   function Chord_Pitch (C : Chord_Value; Degree : Natural) return Float;

   Maximum_Batch : constant := 48;
   subtype Batch_Count is Natural range 0 .. Maximum_Batch;
   type Note_List is array (1 .. Maximum_Batch) of Neon_Notes.Note;
   type Note_Batch is record
      Count : Batch_Count := 0;
      Items : Note_List;
   end record;

   type Composer is private;

   procedure Start (C : out Composer; Seed : Unsigned_64);
   --  Where the next uncomposed step begins.
   function Next_Start (C : Composer) return Samples;
   --  Compose the next step: its notes are added to Into (as many as fit).
   procedure Compose_Next (C : in out Composer; Wanted : Mood; Into : in out Note_Batch;
                           Info : out Step_Record);

   --  The composed step playing at Time. Found is False before the first
   --  step or when Time is older than the history.
   procedure Find (C : Composer; Time : Samples; Info : out Step_Record; Found : out Boolean);
   --  The most recently composed step at or before Time with a kick
   --  (Found False: none in the history).
   procedure Last_Kick (C : Composer; Time : Samples; At_Time : out Samples; Found : out Boolean);
   procedure Last_Chord_Change (C : Composer; Time : Samples; At_Time : out Samples;
                                Found : out Boolean);
   --  The first step boundary at or after Time (Time itself when composed
   --  steps do not reach that far: the caller is ahead of the composer).
   function Next_Boundary (C : Composer; Time : Samples) return Samples;
   --  The nearest beat to Time among composed steps, as a signed distance
   --  Time - beat (Found False when no beat is known).
   procedure Beat_Error (C : Composer; Time : Samples; Error : out Sample_Offset;
                         Found : out Boolean);
   --  The serial of the newest composed step (0 before any).
   function Newest_Serial (C : Composer) return Natural;
   --  The composed step with this serial, when still in the history.
   procedure By_Serial (C : Composer; Serial : Natural; Info : out Step_Record; Found : out Boolean);

private
   History_Length : constant := 64;
   type History_Index is mod History_Length;
   type History_Array is array (History_Index) of Step_Record;

   subtype Progression_Index is Natural range 0 .. 3;
   type Phrase_Notes is array (0 .. 15) of Integer;  --  scale steps, -100 = rest

   type Composer is record
      Random : Neon_Random.Generator;
      Key_Root : Natural := 53;          --  F3
      Progression : Natural := 0;
      Next : Samples := 0;
      Serial : Natural := 0;
      Bar : Natural := 0;
      Step : Step_Index := 0;
      Current : Step_Record;
      Previous_Tones : Tone_Array := [others => 60.0];
      History : History_Array;
      Composed : Natural := 0;           --  steps in History, saturating
      Phrase : Phrase_Notes := [others => -100];
      Arp_Direction_Up : Boolean := True;
      Arp_Position : Natural := 0;
      Bar_Mood : Mood;
   end record;
end Neon_Composer;
