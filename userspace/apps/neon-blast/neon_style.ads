------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's style space: a soundtrack style (and its stage's look) as a
--  value drawn from a seed, so every seed is a new style and the playlist
--  never ends.
--
--  @description
--  A style fixes everything the composer does not decide bar by bar: the
--  tempo, the swing and humanize amounts, the drum grammar (kick, ghost
--  and hat patterns, the fill), the kit's timbre, the mode, the chord
--  colours and two progressions, the bass, arpeggiator, comping and
--  answer-voice choices, the arrangement's density, the effects (reverb,
--  echo, tape wobble, brightness) and the stage's palette and grid.
--
--  Each value is drawn inside the bounds of a region of the space: a lo-fi
--  chill, a late-night slow jam, an upbeat mall-funk, a driving outrun, or
--  Drift, anywhere inside the global musically safe bounds. The regions are
--  ranges, not fixed songs; a named preset is just one seed inside a
--  region. The bounds are the subtypes below, so the SPARK proof
--  (tests/neon-blast/prove.sh) shows every seed's style lies inside them,
--  whatever the region tables hold (they are data the proof does not
--  trust: every drawn value is clamped into its subtype).
--
--  A run's stages are a playlist: stage 1 plays the title's seed (what the
--  title auditions is what the first stage plays); later stages draw from
--  seeds derived from it, biased hotter and faster stage by stage.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Neon_Math; use Neon_Math;
with Neon_Time; use Neon_Time;
with Neon_Notes; use Neon_Notes;

package Neon_Style with SPARK_Mode is

   type Region_Kind is (Lo_Fi, Slow_Jam, Mall_Funk, Outrun, Drift);
   subtype Named_Region is Region_Kind range Lo_Fi .. Outrun;

   --  How hot a stage's draw is: Any (the title and stage 1) .. Blazing.
   type Energy_Bias is (Any, Warm, Hot, Blazing);

   --  The seeds a player sees and picks on the title (six digits).
   subtype Playlist_Seed is Unsigned_64 range 1 .. 999_999;

   ---------------------------------------------------------------------------
   --  Harmony
   ---------------------------------------------------------------------------
   type Mode_Kind is (Ionian, Dorian, Aeolian, Lydian, Mixolydian);
   Scale_Length : constant := 7;
   subtype Scale_Degree is Natural range 0 .. Scale_Length - 1;
   subtype Semitone is Natural range 0 .. 11;
   type Scale_Table is array (Mode_Kind, Scale_Degree) of Semitone;
   Scales : constant Scale_Table :=
     [Ionian     => [0, 2, 4, 5, 7, 9, 11],
      Dorian     => [0, 2, 3, 5, 7, 9, 10],
      Aeolian    => [0, 2, 3, 5, 7, 8, 10],
      Lydian     => [0, 2, 4, 6, 7, 9, 11],
      Mixolydian => [0, 2, 4, 5, 7, 9, 10]];
   Octave : constant := 12;
   Diminished_Fifth : constant := 6;
   --  Semitones from degree From up Steps scale steps (Steps up to two octaves).
   subtype Scale_Steps is Natural range 0 .. 2 * Scale_Length;
   function Interval (Mode : Mode_Kind; From : Scale_Degree; Steps : Scale_Steps) return Integer is
     (Scales (Mode, (From + Steps) mod Scale_Length) + Octave * ((From + Steps) / Scale_Length)
      - Scales (Mode, From));
   --  The stacked fifth on this degree is diminished (the composer never
   --  builds a chord there).
   function Diminished (Mode : Mode_Kind; Degree : Scale_Degree) return Boolean is
     (Interval (Mode, Degree, 4) = Diminished_Fifth);

   --  What a chord adds to its triad.
   type Chord_Color is (Seventh, Ninth, Suspended, Sixth, Eleventh, Thirteenth);
   Progression_Length : constant := 4;
   subtype Chord_Slot is Natural range 0 .. Progression_Length - 1;
   type Degree_Array is array (Chord_Slot) of Scale_Degree;
   type Color_Array is array (Chord_Slot) of Chord_Color;
   type Progression is record
      Degrees : Degree_Array := [0, 3, 4, 0];
      Colors : Color_Array := [others => Seventh];
   end record;

   subtype Key_Root is Natural range 48 .. 59;          --  C3 .. B3
   subtype Lift_Semitones is Natural range 0 .. 2;      --  modulation up at a drop
   subtype Bars_Per_Chord is Positive range 1 .. 2;

   ---------------------------------------------------------------------------
   --  Rhythm
   ---------------------------------------------------------------------------
   type Steps_Mask is array (Step_Index) of Boolean;
   No_Steps : constant Steps_Mask := [others => False];
   type Hat_Kind is
     (Eighths,       --  closed eighths, soft sixteenth ghosts
      Sixteenths,    --  driving sixteenths, accented on the beat
      Offbeat_Open,  --  closed sixteenths, an open hat on every off-beat
      Gallop);       --  eighth plus the last sixteenth of each beat
   type Fill_Kind is (Snare_Roll, Tom_Run, Stutter, Drop_Out);
   type Bass_Kind is
     (Pulse,         --  straight eighths on the root (outrun)
      Funk,          --  syncopated roots, octaves and fifths, slides
      Walking,       --  quarter notes walking chord tones, an approach note
      Sustained);    --  long roots under the chord
   type Arp_Kind is (Rising, Falling, Bounce, Pinball);
   type Answer_Voice is (Bell_Answer, Piano_Answer, Arp_Answer);

   --  Fraction of a sixteenth the off-sixteenths land late.
   subtype Swing_Amount is Float range 0.0 .. 0.3;
   --  Velocity spread, and the scale of the small timing jitter of hats,
   --  ghost notes and keys (kicks stay exact: they are the clock).
   subtype Humanize_Amount is Float range 0.0 .. 0.35;
   subtype Kick_Pitch is Float range 26.0 .. 34.0;     --  MIDI: the body settles at 37 .. 58 Hz
   subtype Snare_Pitch is Float range 50.0 .. 58.0;    --  MIDI: the body at 147 .. 233 Hz
   subtype Kit_Decay is Decay_Scale range 0.6 .. 1.6;
   subtype Arp_Rate is Positive range 1 .. 2;           --  sixteenths per arpeggio note
   subtype Arp_Octaves is Positive range 1 .. 2;

   ---------------------------------------------------------------------------
   --  Effects
   ---------------------------------------------------------------------------
   subtype Wet_Level is Float range 0.2 .. 1.0;         --  reverb return
   subtype Room_Size is Float range 0.6 .. 0.95;        --  Freeverb's room size (comb feedback)
   subtype Echo_Steps is Positive range 2 .. 6;         --  ping-pong delay in sixteenths
   subtype Echo_Feedback is Float range 0.15 .. 0.55;
   subtype Brightness is Float range 0.5 .. 1.0;        --  master filter opening, at most

   ---------------------------------------------------------------------------
   --  Look: the stage's palette and grid
   ---------------------------------------------------------------------------
   subtype Cell_Size is Float range 1.6 .. 3.2;         --  floor grid cell, world units
   subtype Band_Count is Float range 4.0 .. 10.0;       --  sun stripes
   subtype Ridge_Scale is Float range 0.4 .. 1.6;       --  mountain height
   type Look is record
      Sky_Top, Sky_Mid, Sky_Low, Horizon, Floor_Base, Grid_Inner, Grid_Outer,
      Sun_Top, Sun_Bottom, Mountain_Fill, Mountain_Edge : RGB;
      Grid_Cell : Cell_Size := 2.0;
      Sun_Bands : Band_Count := 7.0;
      Ridge_Height : Ridge_Scale := 1.0;
   end record;
   --  A colour with every channel in 0 .. 1.
   function Is_Unit (C : RGB) return Boolean is
     (C.R in 0.0 .. 1.0 and then C.G in 0.0 .. 1.0 and then C.B in 0.0 .. 1.0);
   function Is_Unit (L : Look) return Boolean is
     (Is_Unit (L.Sky_Top) and then Is_Unit (L.Sky_Mid) and then Is_Unit (L.Sky_Low)
      and then Is_Unit (L.Horizon) and then Is_Unit (L.Floor_Base) and then Is_Unit (L.Grid_Inner)
      and then Is_Unit (L.Grid_Outer) and then Is_Unit (L.Sun_Top) and then Is_Unit (L.Sun_Bottom)
      and then Is_Unit (L.Mountain_Fill) and then Is_Unit (L.Mountain_Edge));
   --  The look T of the way from A to B (stage transitions).
   function Blend (A, B : Look; T : Float) return Look
     with Pre => Is_Unit (A) and then Is_Unit (B),
          Post => Is_Unit (Blend'Result);
   --  Round one's classic palette (the calibration screen and the default).
   function Classic return Look
     with Post => Is_Unit (Classic'Result);

   ---------------------------------------------------------------------------
   Name_Capacity : constant := 24;
   subtype Name_Length is Natural range 0 .. Name_Capacity;
   subtype Name_Text is String (1 .. Name_Capacity);

   type Style is record
      Seed : Unsigned_64 := 0;
      Region : Region_Kind := Drift;
      Name : Name_Text := [others => ' '];
      Name_Last : Name_Length := 0;
      --  Tempo: the sixteenth's length; from a drop on, Drop_Step (the
      --  same or a little faster: a lift).
      Step, Drop_Step : Step_Samples := Default_Step;
      Root : Key_Root := 53;
      Mode : Mode_Kind := Ionian;
      Lift : Lift_Semitones := 0;
      Verse, Chorus : Progression;
      Chord_Bars : Bars_Per_Chord := 1;
      Colour_Chance : Unit_Float := 0.5;   --  how often a chord takes an extension

      Swing : Swing_Amount := 0.0;
      Humanize : Humanize_Amount := 0.0;
      Kick_Main, Kick_Alt : Steps_Mask := No_Steps;
      Ghosts : Steps_Mask := No_Steps;     --  where ghost snares may fall
      Ghost_Chance : Unit_Float := 0.0;
      Hats : Hat_Kind := Eighths;
      Open_Chance : Unit_Float := 0.0;
      Clap_Layer : Boolean := False;
      Drop_Double : Boolean := False;      --  the drop runs double-time
      Chase : Boolean := True;             --  boss phrases alternate double-time
      Fill : Fill_Kind := Snare_Roll;

      Kick_Body : Kick_Pitch := 30.0;
      Kick_Punch : Unit_Float := 0.5;
      Kick_Length : Kit_Decay := 1.0;
      Snare_Body : Snare_Pitch := 54.0;
      Snare_Tone : Unit_Float := 0.5;
      Snare_Room : Unit_Float := 0.5;
      Snare_Length : Kit_Decay := 1.0;
      Hat_Tone : Unit_Float := 0.5;

      Bass : Bass_Kind := Sustained;
      Bass_Mask : Steps_Mask := No_Steps;
      Slide_Chance : Unit_Float := 0.0;
      Bass_Tone : Unit_Float := 0.5;
      Arp : Arp_Kind := Rising;
      Arp_Every : Arp_Rate := 1;
      Arp_Span : Arp_Octaves := 1;
      Arp_Tone : Unit_Float := 0.5;
      Comp_Mask : Steps_Mask := No_Steps;  --  electric piano stabs
      Piano_Tone : Unit_Float := 0.5;
      Pad_Tone : Unit_Float := 0.5;
      Lead_Tone : Unit_Float := 0.5;
      Answer : Answer_Voice := Bell_Answer;
      Density : Unit_Float := 0.5;         --  how many layers the grooves carry
      Crackle : Unit_Float := 0.0;         --  vinyl bed

      Reverb_Wet : Wet_Level := 0.6;
      Reverb_Room : Room_Size := 0.92;
      Echo : Echo_Steps := 3;
      Echo_Amount : Echo_Feedback := 0.4;
      Wobble : Unit_Float := 0.5;
      Ceiling : Brightness := 1.0;

      Colors : Look;
   end record;

   --  The style a seed draws, at a stage's bias.
   function Generate (Seed : Unsigned_64; Bias : Energy_Bias) return Style
     with Post => Is_Unit (Generate'Result.Colors);
   --  Named presets: playlist seeds whose first stage lands in each region
   --  (tests/neon-blast's neon_styles_host checks they still do). The title
   --  starts on the lo-fi one.
   type Preset_Seeds is array (Named_Region) of Playlist_Seed;
   Presets : constant Preset_Seeds :=
     [Lo_Fi => 19, Slow_Jam => 8, Mall_Funk => 21, Outrun => 36];
   function Is_Preset (Seed : Unsigned_64) return Boolean is
     (for some R in Named_Region => Presets (R) = Seed);

   --  A run's playlist: stage 1 plays the playlist seed itself (at Any);
   --  later stages seeds derived from it, hotter as they go.
   function Stage_Seed (Playlist : Unsigned_64; Stage : Positive) return Unsigned_64;
   function Stage_Bias (Stage : Positive) return Energy_Bias is
     (case Stage is
        when 1 => Any, when 2 => Warm, when 3 => Hot, when 4 => Blazing, when others => Any);
   function Stage_Style (Playlist : Unsigned_64; Stage : Positive) return Style is
     (Generate (Stage_Seed (Playlist, Stage), Stage_Bias (Stage)))
     with Post => Is_Unit (Stage_Style'Result.Colors);

   function Region_Name (Region : Region_Kind) return String is
     (case Region is
        when Lo_Fi => "LO-FI CHILL", when Slow_Jam => "SLOW JAM", when Mall_Funk => "MALL FUNK",
        when Outrun => "OUTRUN", when Drift => "DRIFT");
   function Mode_Name (Mode : Mode_Kind) return String is
     (case Mode is
        when Ionian => "MAJOR", when Dorian => "DORIAN", when Aeolian => "MINOR",
        when Lydian => "LYDIAN", when Mixolydian => "MIXOLYDIAN");
   function Key_Name (Root : Natural) return String is
     (case Root mod Octave is
        when 0 => "C", when 1 => "C#", when 2 => "D", when 3 => "EB", when 4 => "E",
        when 5 => "F", when 6 => "F#", when 7 => "G", when 8 => "AB", when 9 => "A",
        when 10 => "BB", when others => "B");
   function Name (S : Style) return String is (S.Name (1 .. S.Name_Last));

end Neon_Style;
