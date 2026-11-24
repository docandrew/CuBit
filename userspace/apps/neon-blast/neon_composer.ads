------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's composer: one continuous, evolving piece on an exact
--  sixteenth-note grid, and that grid as the game's clock.
--
--  @description
--  The piece never stops or fades between waves. A run is a playlist of
--  stages; each stage plays one style (Neon_Style, drawn from a seed)
--  through a fixed form: intro, groove, build, drop, a half-time
--  breakdown, a second build, the peak (the boss; it keeps going, in
--  four-bar phrases that alternate with a double-time chase, until the
--  boss is down) and a two-bar outro that pivots into the next stage's key,
--  tempo and look. Sections hand over on bars, by fills, risers, impacts,
--  filter sweeps and a lift of the key at the drop. The title plays the
--  selected seed's style as a lo-fi half-time theme, so the title runs
--  into the first stage without a seam.
--
--  Each sixteenth note is composed once, just before the synthesizer
--  reaches it: drums from the style's grammar (kick patterns and their
--  variations, ghost snares, hats, claps, fills, swing and humanize; kicks
--  are never moved off the grid, they are the clock), the bass's line and
--  slides, the electric piano's comping, the pad, the arpeggiator, and a
--  melody in call-and-response: a two-bar call on the lead, answered by
--  the style's answer voice. Harmony comes from the style's mode and its
--  two progressions, voiced from stacked thirds.
--
--  The game asks (Mood) at bar starts only, so every change lands on a
--  bar. The composed steps stay in a short history the game reads by song
--  time: the simulation spawns and moves on them, the renderer pulses on
--  their kicks, swells the sun on their chord changes and paints their
--  stage's look, and shots take their pitch from their chord.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Neon_Time; use Neon_Time;
with Neon_Notes;
with Neon_Random;
with Neon_Style;

package Neon_Composer with SPARK_Mode is

   type Section_Kind is
     (Title_Theme,   --  attract screen: the style's lo-fi, half-time theme
      Intro,         --  a stage begins: the groove assembling
      Groove,        --  the verse: the style's main groove
      Build,         --  rising: risers, a snare roll, the filter opening
      Drop,          --  the chorus: everything, a key lift, an impact
      Breakdown,     --  half-time, filtered, melodic: a breather, never silent
      Peak,          --  the climax: the boss, chase phrases until it falls
      Outro,         --  two bars that pivot into the next stage
      Finale,        --  game over: the style keeps playing, slow and dark
      Metronome);    --  calibration: clicks on every beat over the pad
   --  How the drums sit on the grid this bar.
   type Feel_Kind is (Straight, Half_Time, Double_Time);
   --  How much of the band plays, 0 (pad alone) .. 5 (everything).
   subtype Energy_Range is Natural range 0 .. 5;
   subtype Stage_Number is Positive range 1 .. 999;
   Stages_Per_Run : constant := 4;   --  then the playlist keeps going

   --  What the game is doing, read at bar starts.
   type Scene_Kind is (Attract, Play, Over, Calibrate);
   type Mood is record
      Scene : Scene_Kind := Attract;
      Playlist : Neon_Style.Playlist_Seed := 1;  --  the title's seed
      Run : Natural := 0;                       --  a new number starts a run
      Start_Stage : Stage_Number := 1;
      Boss_Practice : Boolean := False;         --  start at the stage's boss
      Defeated_Stage : Natural := 0;            --  the newest stage whose boss is down
      Arp_On : Boolean := False;                --  rhythm-combo unlocks
      Lead_On : Boolean := False;
   end record;

   Maximum_Tones : constant := 5;
   subtype Tone_Count is Natural range 0 .. Maximum_Tones;
   subtype Tone_Index is Positive range 1 .. Maximum_Tones;
   type Tone_Array is array (Tone_Index) of Float;

   type Chord_Value is record
      Root : Natural := 60;                --  MIDI note of the root
      Degree : Neon_Style.Scale_Degree := 0;
      Count : Tone_Count := 0;
      Tones : Tone_Array := [others => 60.0];  --  voiced pad notes, ascending
      Bass : Float := 36.0;
   end record;

   type Step_Record is record
      Serial : Natural := 0;
      Start : Samples := 0;
      Length : Step_Samples := Default_Step;
      Bar : Natural := 0;
      Step : Step_Index := 0;
      Section : Section_Kind := Title_Theme;
      Section_Bar : Natural := 0;          --  bars since the section began
      Section_Bars : Natural := 0;         --  its length (the peak: its current phrase)
      Feel : Feel_Kind := Straight;
      Energy : Energy_Range := 0;
      Stage : Natural := 0;                --  0 on the title
      Run : Natural := 0;
      Chord : Chord_Value;
      Chord_Change : Boolean := False;
      Kick, Snare : Boolean := False;
      Impact : Boolean := False;           --  a drop's or peak's downbeat hit
      --  0 .. 1 targets the synthesizer glides to: pad loudness and the
      --  master low-pass opening (0 = underwater).
      Pad_Level : Float := 0.5;
      Cutoff : Float := 1.0;
      --  The style playing, for the title and the HUD, and its look.
      Style_Seed : Unsigned_64 := 0;
      Region : Neon_Style.Region_Kind := Neon_Style.Drift;
      Name : Neon_Style.Name_Text := [others => ' '];
      Name_Last : Neon_Style.Name_Length := 0;
      Key : Natural := 53;
      Mode : Neon_Style.Mode_Kind := Neon_Style.Ionian;
      Look : Neon_Style.Look;
   end record;

   function Is_Beat (S : Step_Record) return Boolean is (S.Step mod Steps_Per_Beat = 0);
   function Beat_In_Bar (S : Step_Record) return Natural is (S.Step / Steps_Per_Beat);
   function Style_Name (S : Step_Record) return String is (S.Name (1 .. S.Name_Last));
   --  The pitch of a shot: the chord's tones walked upward by Degree, an
   --  octave above the pad.
   function Chord_Pitch (C : Chord_Value; Degree : Natural) return Float;

   Maximum_Batch : constant := 64;
   subtype Batch_Count is Natural range 0 .. Maximum_Batch;
   type Note_List is array (1 .. Maximum_Batch) of Neon_Notes.Note;
   type Note_Batch is record
      Count : Batch_Count := 0;
      Items : Note_List;
   end record;

   --  The mix the synthesizer glides to from a step on.
   type Mix_Targets is record
      Pad_Level, Cutoff : Float := 0.5;
      Delay_Samples : Positive := 24_000;
      Echo_Feedback : Float := 0.4;
      Wobble : Float := 0.5;
      Reverb_Wet : Float := 0.6;
      Reverb_Room : Float := 0.92;   --  Freeverb room size
      Room_Level : Float := 0.5;     --  the gated snare room's return
   end record;

   type Composer is private;

   procedure Start (C : out Composer; Seed : Unsigned_64);
   --  Where the next uncomposed step begins.
   function Next_Start (C : Composer) return Samples;
   --  Compose the next step: its notes are added to Into (as many as fit),
   --  and the mix to glide to from its start.
   procedure Compose_Next (C : in out Composer; Wanted : Mood; Into : in out Note_Batch;
                           Info : out Step_Record; Mix : out Mix_Targets);
   --  Notes refused because a step's batch was full (a design ceiling).
   function Dropped (C : Composer) return Natural;

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

   --  A stage's form: the plan's sections in order (the peak repeats).
   Plan_Length : constant := 8;
   subtype Plan_Index is Natural range 0 .. Plan_Length - 1;

   --  A melody: two bars of scale positions (Rest: none), the call; its
   --  answer is derived from it.
   Phrase_Steps : constant := 2 * Steps_Per_Bar;
   subtype Phrase_Index is Natural range 0 .. Phrase_Steps - 1;
   Rest : constant := -100;
   Lowest_Position : constant := -7;
   Highest_Position : constant := 14;
   subtype Melody_Position is Integer range Rest .. Highest_Position;
   type Phrase_Notes is array (Phrase_Index) of Melody_Position;
   type Answer_Shape is (Inverted, Sequenced, Echoed);
   Look_Fade_Steps : constant := Steps_Per_Bar;

   type Composer is record
      Random : Neon_Random.Generator;
      Style, Next_Style : Neon_Style.Style;
      Playlist : Unsigned_64 := 1;
      Start_Stage : Natural := 1;
      Run : Natural := 0;
      Stage : Natural := 0;
      Plan : Plan_Index := 0;
      Section : Section_Kind := Title_Theme;
      Section_Bar : Natural := 0;
      Section_Bars : Natural := 0;
      Peak_Phrases : Natural := 0;
      Feel : Feel_Kind := Straight;
      Look_From : Neon_Style.Look;       --  a new title style fades in from this
      Fade_Done : Natural := 0;
      Next : Samples := 0;
      Serial : Natural := 0;
      Bar : Natural := 0;
      Step : Step_Index := 0;
      Current : Step_Record;
      History : History_Array;
      Composed : Natural := 0;           --  steps in History, saturating
      Call : Phrase_Notes := [others => Rest];
      Answer : Answer_Shape := Inverted;
      Arp_Position : Natural := 0;
      Arp_Up : Boolean := True;
      Last_Bass : Float := 36.0;
      Bar_Mood : Mood;
      Alt_Bar : Boolean := False;        --  this bar takes the variation kick
      Dropped_Notes : Natural := 0;
   end record;
end Neon_Composer;
