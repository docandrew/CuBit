------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Random; use Neon_Random;

package body Neon_Style with SPARK_Mode is

   --  The style generator's own stream of the seed (the composer and the
   --  game draw from others).
   Style_Stream : constant := 16#57_1E#;
   Stage_Mix_A : constant Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;   --  golden-ratio odd constant
   Stage_Mix_B : constant Unsigned_64 := 16#BF58_476D_1CE4_E5B9#;   --  SplitMix64's first multiplier
   Stage_Mix_Shift : constant := 31;
   Percent : constant := 100;

   ---------------------------------------------------------------------------
   --  Draws
   ---------------------------------------------------------------------------

   type Span is record
      Low, High : Float;
   end record;
   --  The widest span any draw takes (the tables are not trusted: every
   --  value is clamped again into its subtype after the draw).
   Span_Limit : constant := 1.0e3;

   procedure Draw (G : in out Generator; S : Span; Value : out Float)
     with Post => Value in -Span_Limit .. Span_Limit
   is
      U : Float;
      Low : constant Float := Clamp (S.Low, -Span_Limit, Span_Limit);
      High : constant Float := Clamp (S.High, Low, Span_Limit);
   begin
      Uniform (G, U);
      Value := Clamp (Low + (High - Low) * U, Low, High);
   end Draw;

   procedure Chance_Of (G : in out Generator; Probability : Float; Hit : out Boolean)
     with Global => null
   is
      U : Float;
   begin
      Uniform (G, U);
      Hit := U < Probability;
   end Chance_Of;

   function Unit (X : Float) return Unit_Float is (Clamp (X, 0.0, 1.0));

   --  A percentage draw: 0 .. 99.
   procedure Roll (G : in out Generator; Value : out Natural) with Post => Value < Percent is
   begin
      Pick (G, Percent, Value);
   end Roll;

   ---------------------------------------------------------------------------
   --  Region bounds
   ---------------------------------------------------------------------------

   type Region_Spans is record
      Slow_BPM, Fast_BPM : Positive;
      Swing, Humanize, Ghost, Open, Kick_Punch, Kick_Length, Kick_Body, Snare_Body, Snare_Tone,
      Snare_Room, Snare_Length, Hat_Tone, Slide, Bass_Tone, Arp_Tone, Piano_Tone, Pad_Tone,
      Lead_Tone, Density, Crackle, Reverb_Wet, Reverb_Room, Echo_Amount, Wobble, Ceiling,
      Colour, Hue, Ridge : Span;
   end record;
   --  Each region's bounds. Hue: the turns a base palette is rotated by.
   --  Drift's spans are the whole safe space. The table is data, kept out
   --  of the proof: what it holds cannot take a style out of its bounds.
   function Bounds (Region : Region_Kind) return Region_Spans;
   function Bounds (Region : Region_Kind) return Region_Spans with SPARK_Mode => Off is
      type Span_Table is array (Region_Kind) of Region_Spans;
      Spans : constant Span_Table :=
        [Lo_Fi =>
           (80, 90, Swing => (0.16, 0.28), Humanize => (0.15, 0.3), Ghost => (0.3, 0.6),
            Open => (0.0, 0.15), Kick_Punch => (0.15, 0.4), Kick_Length => (0.7, 0.95),
            Kick_Body => (29.0, 33.0), Snare_Body => (53.0, 57.0), Snare_Tone => (0.2, 0.45),
            Snare_Room => (0.0, 0.25), Snare_Length => (0.7, 1.0), Hat_Tone => (0.2, 0.45),
            Slide => (0.0, 0.15), Bass_Tone => (0.1, 0.35), Arp_Tone => (0.2, 0.45),
            Piano_Tone => (0.2, 0.5), Pad_Tone => (0.2, 0.45), Lead_Tone => (0.2, 0.45),
            Density => (0.3, 0.6), Crackle => (0.5, 1.0), Reverb_Wet => (0.4, 0.7),
            Reverb_Room => (0.75, 0.88), Echo_Amount => (0.2, 0.4), Wobble => (0.6, 1.0),
            Ceiling => (0.55, 0.75), Colour => (0.5, 0.9), Hue => (-0.04, 0.04), Ridge => (0.6, 1.2)),
         Slow_Jam =>
           (80, 94, Swing => (0.08, 0.2), Humanize => (0.1, 0.25), Ghost => (0.2, 0.45),
            Open => (0.05, 0.25), Kick_Punch => (0.3, 0.6), Kick_Length => (1.0, 1.4),
            Kick_Body => (27.0, 31.0), Snare_Body => (51.0, 55.0), Snare_Tone => (0.4, 0.7),
            Snare_Room => (0.4, 0.7), Snare_Length => (1.0, 1.4), Hat_Tone => (0.35, 0.6),
            Slide => (0.15, 0.4), Bass_Tone => (0.2, 0.5), Arp_Tone => (0.3, 0.6),
            Piano_Tone => (0.3, 0.6), Pad_Tone => (0.4, 0.7), Lead_Tone => (0.3, 0.6),
            Density => (0.4, 0.7), Crackle => (0.0, 0.25), Reverb_Wet => (0.6, 0.9),
            Reverb_Room => (0.88, 0.95), Echo_Amount => (0.3, 0.5), Wobble => (0.4, 0.8),
            Ceiling => (0.8, 0.95), Colour => (0.6, 0.95), Hue => (-0.04, 0.04), Ridge => (0.8, 1.5)),
         Mall_Funk =>
           (100, 118, Swing => (0.04, 0.14), Humanize => (0.08, 0.2), Ghost => (0.35, 0.7),
            Open => (0.3, 0.7), Kick_Punch => (0.5, 0.8), Kick_Length => (0.7, 1.0),
            Kick_Body => (29.0, 33.0), Snare_Body => (54.0, 58.0), Snare_Tone => (0.6, 0.9),
            Snare_Room => (0.2, 0.45), Snare_Length => (0.7, 1.0), Hat_Tone => (0.55, 0.85),
            Slide => (0.2, 0.5), Bass_Tone => (0.5, 0.85), Arp_Tone => (0.5, 0.8),
            Piano_Tone => (0.5, 0.85), Pad_Tone => (0.5, 0.8), Lead_Tone => (0.5, 0.8),
            Density => (0.6, 0.9), Crackle => (0.0, 0.1), Reverb_Wet => (0.35, 0.6),
            Reverb_Room => (0.7, 0.85), Echo_Amount => (0.25, 0.45), Wobble => (0.2, 0.5),
            Ceiling => (0.9, 1.0), Colour => (0.6, 1.0), Hue => (-0.04, 0.04), Ridge => (0.4, 0.9)),
         Outrun =>
           (116, 140, Swing => (0.0, 0.04), Humanize => (0.03, 0.12), Ghost => (0.05, 0.25),
            Open => (0.2, 0.6), Kick_Punch => (0.75, 1.0), Kick_Length => (1.0, 1.4),
            Kick_Body => (26.0, 30.0), Snare_Body => (50.0, 54.0), Snare_Tone => (0.55, 0.85),
            Snare_Room => (0.7, 1.0), Snare_Length => (1.1, 1.5), Hat_Tone => (0.6, 0.9),
            Slide => (0.0, 0.2), Bass_Tone => (0.55, 0.9), Arp_Tone => (0.6, 0.95),
            Piano_Tone => (0.4, 0.7), Pad_Tone => (0.6, 0.9), Lead_Tone => (0.6, 0.95),
            Density => (0.7, 1.0), Crackle => (0.0, 0.0), Reverb_Wet => (0.5, 0.8),
            Reverb_Room => (0.85, 0.95), Echo_Amount => (0.35, 0.55), Wobble => (0.05, 0.3),
            Ceiling => (0.92, 1.0), Colour => (0.3, 0.7), Hue => (-0.04, 0.04), Ridge => (0.9, 1.6)),
         Drift =>
           (82, 136, Swing => (0.0, 0.25), Humanize => (0.0, 0.3), Ghost => (0.0, 0.6),
            Open => (0.0, 0.6), Kick_Punch => (0.2, 1.0), Kick_Length => (0.7, 1.4),
            Kick_Body => (27.0, 33.0), Snare_Body => (51.0, 57.0), Snare_Tone => (0.3, 0.9),
            Snare_Room => (0.0, 0.9), Snare_Length => (0.8, 1.4), Hat_Tone => (0.3, 0.85),
            Slide => (0.0, 0.45), Bass_Tone => (0.2, 0.85), Arp_Tone => (0.3, 0.9),
            Piano_Tone => (0.3, 0.8), Pad_Tone => (0.3, 0.85), Lead_Tone => (0.3, 0.9),
            Density => (0.4, 0.95), Crackle => (0.0, 0.5), Reverb_Wet => (0.35, 0.85),
            Reverb_Room => (0.7, 0.95), Echo_Amount => (0.2, 0.5), Wobble => (0.1, 0.9),
            Ceiling => (0.65, 1.0), Colour => (0.3, 1.0), Hue => (-0.15, 0.15), Ridge => (0.4, 1.6))];
   begin
      return Spans (Region);
   end Bounds;

   Slowest_BPM : constant := Sixteenth_Minute / Slowest_Step;
   Fastest_BPM : constant := Sixteenth_Minute / Fastest_Step;
   --  The tempo floors of the hot biases (BPM), whatever the region.
   Hot_Floor_BPM : constant := 104;
   Blazing_Floor_BPM : constant := 124;
   --  A drop's tempo lift: this many percent faster, at most.
   Lift_Percent_Low : constant := 2;
   Lift_Percent_Span : constant := 4;

   ---------------------------------------------------------------------------
   --  Looks
   ---------------------------------------------------------------------------

   function Unit (C : RGB) return RGB is ((Unit (C.R), Unit (C.G), Unit (C.B)))
     with Post => Is_Unit (Unit'Result);

   function Blend (A, B : Look; T : Float) return Look is
      U : constant Float := Saturate (T);
      function M (X, Y : RGB) return RGB is (Unit (Mix (X, Y, U)))
        with Pre => Is_Unit (X) and then Is_Unit (Y), Post => Is_Unit (M'Result);
   begin
      return (Sky_Top => M (A.Sky_Top, B.Sky_Top), Sky_Mid => M (A.Sky_Mid, B.Sky_Mid),
              Sky_Low => M (A.Sky_Low, B.Sky_Low), Horizon => M (A.Horizon, B.Horizon),
              Floor_Base => M (A.Floor_Base, B.Floor_Base),
              Grid_Inner => M (A.Grid_Inner, B.Grid_Inner),
              Grid_Outer => M (A.Grid_Outer, B.Grid_Outer), Sun_Top => M (A.Sun_Top, B.Sun_Top),
              Sun_Bottom => M (A.Sun_Bottom, B.Sun_Bottom),
              Mountain_Fill => M (A.Mountain_Fill, B.Mountain_Fill),
              Mountain_Edge => M (A.Mountain_Edge, B.Mountain_Edge),
              Grid_Cell => (if U < 0.5 then A.Grid_Cell else B.Grid_Cell),
              Sun_Bands => (if U < 0.5 then A.Sun_Bands else B.Sun_Bands),
              Ridge_Height => Ridge_Scale (Clamp (Lerp (A.Ridge_Height, B.Ridge_Height, U),
                                                  Ridge_Scale'First, Ridge_Scale'Last)));
   end Blend;

   --  Each region's palette before its seed turns the hue a little (data,
   --  kept out of the proof like the bounds: Sanitized clamps it).
   function Base_Palette (Region : Named_Region) return Look;
   function Base_Palette (Region : Named_Region) return Look with SPARK_Mode => Off is
      type Base_Palettes is array (Named_Region) of Look;
      Bases : constant Base_Palettes :=
        [Outrun =>      --  round one's sunset strip
           (Sky_Top => From_Hex (16#0B0420#), Sky_Mid => From_Hex (16#3A0E5C#),
            Sky_Low => From_Hex (16#B0206E#), Horizon => From_Hex (16#FF6A5E#),
            Floor_Base => From_Hex (16#0E0418#), Grid_Inner => From_Hex (16#FF2BD6#),
            Grid_Outer => From_Hex (16#7A3CFF#), Sun_Top => From_Hex (16#FFE45C#),
            Sun_Bottom => From_Hex (16#FF2B8A#), Mountain_Fill => From_Hex (16#14072A#),
            Mountain_Edge => From_Hex (16#18F0FF#), Grid_Cell => 2.0, Sun_Bands => 7.0,
            Ridge_Height => 1.0),
         Slow_Jam =>    --  midnight: deep blue, a violet-pink moon of a sun
           (Sky_Top => From_Hex (16#02030F#), Sky_Mid => From_Hex (16#0E1640#),
            Sky_Low => From_Hex (16#3B1E78#), Horizon => From_Hex (16#C04BD8#),
            Floor_Base => From_Hex (16#050816#), Grid_Inner => From_Hex (16#4B7BFF#),
            Grid_Outer => From_Hex (16#8A3CFF#), Sun_Top => From_Hex (16#FFB8F0#),
            Sun_Bottom => From_Hex (16#B23BFF#), Mountain_Fill => From_Hex (16#070A20#),
            Mountain_Edge => From_Hex (16#FF4FD8#), Grid_Cell => 3.2, Sun_Bands => 5.0,
            Ridge_Height => 1.3),
         Mall_Funk =>   --  pastel plaza: teal floor, pink and cream sun
           (Sky_Top => From_Hex (16#0C1A30#), Sky_Mid => From_Hex (16#1A4A66#),
            Sky_Low => From_Hex (16#2F8F9A#), Horizon => From_Hex (16#FF9AC4#),
            Floor_Base => From_Hex (16#06161C#), Grid_Inner => From_Hex (16#3FF2E0#),
            Grid_Outer => From_Hex (16#FF7AC8#), Sun_Top => From_Hex (16#FFF2A8#),
            Sun_Bottom => From_Hex (16#FF6FB5#), Mountain_Fill => From_Hex (16#0A2430#),
            Mountain_Edge => From_Hex (16#FFD0F0#), Grid_Cell => 1.6, Sun_Bands => 9.0,
            Ridge_Height => 0.7),
         Lo_Fi =>       --  dusk tape: muted lavender and peach
           (Sky_Top => From_Hex (16#140A1C#), Sky_Mid => From_Hex (16#3A2440#),
            Sky_Low => From_Hex (16#7A4A6A#), Horizon => From_Hex (16#E8A07A#),
            Floor_Base => From_Hex (16#120A14#), Grid_Inner => From_Hex (16#E88AB8#),
            Grid_Outer => From_Hex (16#8A6AC8#), Sun_Top => From_Hex (16#FFD8A0#),
            Sun_Bottom => From_Hex (16#E86A8A#), Mountain_Fill => From_Hex (16#1C1024#),
            Mountain_Edge => From_Hex (16#9AD8C8#), Grid_Cell => 2.0, Sun_Bands => 5.0,
            Ridge_Height => 0.9)];
   begin
      return Bases (Region);
   end Base_Palette;

   function Sanitized (L : Look) return Look is
     ((Sky_Top => Unit (L.Sky_Top), Sky_Mid => Unit (L.Sky_Mid), Sky_Low => Unit (L.Sky_Low),
       Horizon => Unit (L.Horizon), Floor_Base => Unit (L.Floor_Base),
       Grid_Inner => Unit (L.Grid_Inner), Grid_Outer => Unit (L.Grid_Outer),
       Sun_Top => Unit (L.Sun_Top), Sun_Bottom => Unit (L.Sun_Bottom),
       Mountain_Fill => Unit (L.Mountain_Fill), Mountain_Edge => Unit (L.Mountain_Edge),
       Grid_Cell => L.Grid_Cell, Sun_Bands => L.Sun_Bands, Ridge_Height => L.Ridge_Height))
     with Post => Is_Unit (Sanitized'Result);

   function Classic return Look is (Sanitized (Base_Palette (Outrun)));

   --  C turned about the grey axis by Turns (a hue rotation), kept in range.
   --  One weighted channel: bounded factors, so the sums cannot overflow.
   function Term (Channel, Weight : Float) return Float is (Channel * Weight)
     with Pre => Channel in 0.0 .. 1.0 and then Weight in -2.0 .. 2.0,
          Post => Term'Result in -2.0 .. 2.0;

   function Turn_Hue (C : RGB; Turns : Float) return RGB
     with Pre => Is_Unit (C) and then Turns in -1.0 .. 1.0, Post => Is_Unit (Turn_Hue'Result)
   is
      Third : constant := 1.0 / 3.0;
      Root_Third : constant := 0.577_350_27;   --  1 / sqrt 3
      Cosine : constant Float := Cos_Turns (Turns);
      Sine : constant Float := Sin_Turns (Turns);
      K : constant Float := Clamp ((1.0 - Cosine) * Third, 0.0, 1.0);
      S : constant Float := Clamp (Sine * Root_Third, -1.0, 1.0);
      Same : constant Float := Clamp (Cosine + K, -2.0, 2.0);
      Ahead : constant Float := Clamp (K + S, -2.0, 2.0);
      Behind : constant Float := Clamp (K - S, -2.0, 2.0);
   begin
      return Unit ((Term (C.R, Same) + Term (C.G, Behind) + Term (C.B, Ahead),
                    Term (C.R, Ahead) + Term (C.G, Same) + Term (C.B, Behind),
                    Term (C.R, Behind) + Term (C.G, Ahead) + Term (C.B, Same)));
   end Turn_Hue;

   procedure Make_Look (G : in out Generator; Region : Region_Kind; Into : out Look)
     with Post => Is_Unit (Into)
   is
      Base_Region : Named_Region;
      Pick_Value : Natural;
      Sky_Turn, Grid_Turn, Ridge, Bands : Float;
      Base : Look;
      Cells : constant array (0 .. 2) of Cell_Size := [1.6, 2.0, 3.2];
   begin
      if Region = Drift then
         Pick (G, 4, Pick_Value);
         Base_Region := Named_Region'Val (Pick_Value);
      else
         Base_Region := Region;
      end if;
      Base := Sanitized (Base_Palette (Base_Region));
      Draw (G, Bounds (Region).Hue, Sky_Turn);
      Draw (G, Bounds (Region).Hue, Grid_Turn);
      Draw (G, Bounds (Region).Ridge, Ridge);
      Sky_Turn := Clamp (Sky_Turn, -1.0, 1.0);
      Grid_Turn := Clamp (Grid_Turn, -1.0, 1.0);
      Draw (G, (-1.0, 1.0), Bands);
      Into := (Sky_Top => Turn_Hue (Base.Sky_Top, Sky_Turn),
               Sky_Mid => Turn_Hue (Base.Sky_Mid, Sky_Turn),
               Sky_Low => Turn_Hue (Base.Sky_Low, Sky_Turn),
               Horizon => Turn_Hue (Base.Horizon, Sky_Turn),
               Floor_Base => Turn_Hue (Base.Floor_Base, Sky_Turn),
               Grid_Inner => Turn_Hue (Base.Grid_Inner, Grid_Turn),
               Grid_Outer => Turn_Hue (Base.Grid_Outer, Grid_Turn),
               Sun_Top => Turn_Hue (Base.Sun_Top, Sky_Turn),
               Sun_Bottom => Turn_Hue (Base.Sun_Bottom, Sky_Turn),
               Mountain_Fill => Turn_Hue (Base.Mountain_Fill, Sky_Turn),
               Mountain_Edge => Turn_Hue (Base.Mountain_Edge, Grid_Turn),
               Grid_Cell => Base.Grid_Cell,
               Sun_Bands => Band_Count (Clamp (Base.Sun_Bands + 2.0 * Bands, Band_Count'First,
                                               Band_Count'Last)),
               Ridge_Height => Ridge_Scale (Clamp (Ridge, Ridge_Scale'First, Ridge_Scale'Last)));
      if Region = Drift then
         Pick (G, Cells'Length, Pick_Value);
         Into.Grid_Cell := Cells (Pick_Value);
      end if;
   end Make_Look;

   ---------------------------------------------------------------------------
   --  Names
   ---------------------------------------------------------------------------

   Word_Length : constant := 10;
   subtype Word is String (1 .. Word_Length);
   subtype Word_Index is Natural range 0 .. 15;
   Word_Count : constant := 16;
   type Word_List is array (Word_Index) of Word;
   function Adjective (I : Word_Index) return Word;
   function Noun (I : Word_Index) return Word;
   --  The name words (data, out of the proof: any word fits the name).
   function Adjective (I : Word_Index) return Word with SPARK_Mode => Off is
      Adjectives : constant Word_List :=
        ["MIDNIGHT  ", "PASTEL    ", "CHROME    ", "VELVET    ", "LUNAR     ", "CRYSTAL   ",
         "SATIN     ", "VAPOR     ", "PRISM     ", "AQUA      ", "CORAL     ", "NEON      ",
         "SILK      ", "HOLO      ", "COBALT    ", "AMBER     "];
   begin
      return Adjectives (I);
   end Adjective;
   function Noun (I : Word_Index) return Word with SPARK_Mode => Off is
      Nouns : constant Word_List :=
        ["DRIVE     ", "PLAZA     ", "LOUNGE    ", "ARCADE    ", "HIGHWAY   ", "POOL      ",
         "SKYLINE   ", "CASSETTE  ", "PARADISE  ", "ATRIUM    ", "FOUNTAIN  ", "RIVIERA   ",
         "TERMINAL  ", "GARDEN    ", "MOTEL     ", "ESCALATOR "];
   begin
      return Nouns (I);
   end Noun;

   procedure Append (Into : in out Name_Text; Last : in out Name_Length; Item : Word)
     with Pre => Last <= Name_Capacity - Word_Length - 1, Post => Last <= Last'Old + Word_Length + 1
   is
   begin
      if Last > 0 then
         Last := Last + 1;
         Into (Last) := ' ';
      end if;
      declare
         Start : constant Name_Length := Last;
      begin
         for I in Item'Range loop
            pragma Loop_Invariant (Last <= Start + I - 1);
            exit when Item (I) = ' ';
            Last := Last + 1;
            Into (Last) := Item (I);
         end loop;
      end;
   end Append;

   ---------------------------------------------------------------------------
   --  Rhythm grammars
   ---------------------------------------------------------------------------

   type Kick_Family is (Four_Floor, Broken, Boom_Bap);

   procedure Make_Kicks (G : in out Generator; Region : Region_Kind; Main, Alt : out Steps_Mask)
     with Global => null
   is
      Roll_Value, Pick_Value : Natural;
      Family : Kick_Family;
      Hit : Boolean;
      Syncopes : constant array (0 .. 5) of Step_Index := [3, 6, 7, 11, 13, 14];
      Pickups : constant array (0 .. 2) of Step_Index := [7, 14, 15];
      Count : Natural := 0;
      Most_Syncopes : constant := 3;
   begin
      Roll (G, Roll_Value);
      Family :=
        (case Region is
           when Lo_Fi => Boom_Bap,
           when Slow_Jam => Broken,
           when Mall_Funk => (if Roll_Value < 50 then Four_Floor else Broken),
           when Outrun => (if Roll_Value < 85 then Four_Floor else Broken),
           when Drift => (if Roll_Value < 34 then Four_Floor elsif Roll_Value < 67 then Broken
                          else Boom_Bap));
      Main := No_Steps;
      Main (0) := True;
      case Family is
         when Four_Floor =>
            Main (4) := True;
            Main (8) := True;
            Main (12) := True;
         when Broken =>
            Chance_Of (G, 0.5, Hit);
            Main ((if Hit then 8 else 10)) := True;
            for S of Syncopes loop
               Chance_Of (G, 0.3, Hit);
               if Hit and then Count < Most_Syncopes and then not Main (S) then
                  Main (S) := True;
                  Count := Count + 1;
               end if;
            end loop;
         when Boom_Bap =>
            Main (10) := True;
            Chance_Of (G, 0.5, Hit);
            Main (7) := Hit;
            Chance_Of (G, 0.25, Hit);
            Main (3) := Hit;
      end case;
      Alt := Main;
      Pick (G, Pickups'Length, Pick_Value);
      Alt (Pickups (Pick_Value)) := True;
   end Make_Kicks;

   procedure Make_Ghosts (G : in out Generator; Kicks : Steps_Mask; Into : out Steps_Mask)
     with Global => null
   is
      Candidates : constant array (0 .. 8) of Step_Index := [2, 3, 6, 7, 9, 10, 11, 14, 15];
      Hit : Boolean;
      Count : Natural := 0;
      Fewest : constant := 2;
   begin
      Into := No_Steps;
      for S of Candidates loop
         Chance_Of (G, 0.35, Hit);
         if Hit and then not Kicks (S) then
            Into (S) := True;
            Count := Count + 1;
         end if;
      end loop;
      if Count < Fewest then
         Into (7) := not Kicks (7);
         Into (15) := not Kicks (15);
      end if;
   end Make_Ghosts;

   procedure Make_Bass (G : in out Generator; Kind : Bass_Kind; Into : out Steps_Mask)
     with Global => null
   is
      Candidates : constant array (0 .. 6) of Step_Index := [3, 6, 7, 8, 10, 11, 14];
      Hit : Boolean;
      Count : Natural := 1;
      Fewest : constant := 3;
   begin
      Into := No_Steps;
      Into (0) := True;
      case Kind is
         when Pulse =>
            for S in Step_Index loop
               Into (S) := S mod 2 = 0;
            end loop;
         when Funk =>
            for S of Candidates loop
               Chance_Of (G, 0.45, Hit);
               if Hit then
                  Into (S) := True;
                  Count := Count + 1;
               end if;
            end loop;
            if Count < Fewest then
               Into (10) := True;
               Into (14) := True;
            end if;
         when Walking =>
            Into (4) := True;
            Into (8) := True;
            Into (12) := True;
            Chance_Of (G, 0.5, Hit);
            Into (14) := Hit;
         when Sustained =>
            Chance_Of (G, 0.35, Hit);
            Into (8) := Hit;
            Chance_Of (G, 0.3, Hit);
            Into (10) := Hit and then not Into (8);
      end case;
   end Make_Bass;

   procedure Make_Comp (G : in out Generator; Region : Region_Kind; Into : out Steps_Mask)
     with Global => null
   is
      Candidates : constant array (0 .. 4) of Step_Index := [3, 6, 10, 11, 14];
      Hit : Boolean;
      Count : Natural := 0;
      Fewest : constant := 2;
   begin
      Into := No_Steps;
      Into (0) := True;
      case Region is
         when Lo_Fi =>
            Chance_Of (G, 0.5, Hit);
            Into ((if Hit then 10 else 6)) := True;
         when Slow_Jam =>
            Chance_Of (G, 0.5, Hit);
            Into (10) := Hit;
         when Mall_Funk | Drift =>
            Chance_Of (G, 0.4, Hit);
            Into (0) := Hit;
            for S of Candidates loop
               Chance_Of (G, 0.45, Hit);
               if Hit then
                  Into (S) := True;
                  Count := Count + 1;
               end if;
            end loop;
            if Count < Fewest then
               Into (6) := True;
               Into (14) := True;
            end if;
         when Outrun =>
            Chance_Of (G, 0.4, Hit);
            Into (8) := Hit;
      end case;
   end Make_Comp;

   ---------------------------------------------------------------------------
   --  Harmony grammar
   ---------------------------------------------------------------------------

   --  Root motion in scale steps up (mod 7), weighted: falling fifths most,
   --  falling thirds and steps next; a repeat never.
   type Motion_Weights is array (Scale_Degree) of Natural;
   Motions : constant Motion_Weights := [0, 2, 1, 5, 2, 3, 2];
   Strong_Cadence : constant := 2;   --  the loop back to its first chord at least this weight
   Progression_Tries : constant := 12;

   procedure Next_Degree (G : in out Generator; Mode : Mode_Kind; From : Scale_Degree;
                          Into : out Scale_Degree)
     with Global => null
   is
      Total : Natural := 0;
      Draw_Value : Natural;
      Weight : Natural;
   begin
      for M in Scale_Degree loop
         if not Diminished (Mode, (From + M) mod Scale_Length) then
            Total := Total + Motions (M);
         end if;
         pragma Loop_Invariant (Total <= 5 * (M + 1));
      end loop;
      Into := (From + 3) mod Scale_Length;
      if Total = 0 then
         return;
      end if;
      Pick (G, Total, Draw_Value);
      for M in Scale_Degree loop
         Weight := (if Diminished (Mode, (From + M) mod Scale_Length) then 0 else Motions (M));
         if Draw_Value < Weight then
            Into := (From + M) mod Scale_Length;
            return;
         end if;
         Draw_Value := Draw_Value - Weight;
      end loop;
   end Next_Degree;

   procedure Make_Colors (G : in out Generator; Region : Region_Kind; Colour : Float;
                          Into : out Color_Array)
     with Global => null
   is
      Hit : Boolean;
      Pick_Value : Natural;
   begin
      for Slot in Chord_Slot loop
         Chance_Of (G, Colour, Hit);
         if not Hit then
            Into (Slot) := Seventh;
         else
            Pick (G, 3, Pick_Value);
            Into (Slot) :=
              (case Region is
                 when Lo_Fi => (case Pick_Value is when 0 => Ninth, when 1 => Sixth, when others => Ninth),
                 when Slow_Jam => (case Pick_Value is when 0 => Ninth, when 1 => Eleventh,
                                   when others => Thirteenth),
                 when Mall_Funk => (case Pick_Value is when 0 => Ninth, when 1 => Thirteenth,
                                    when others => Eleventh),
                 when Outrun => (case Pick_Value is when 0 => Suspended, when 1 => Ninth,
                                 when others => Seventh),
                 when Drift => (case Pick_Value is when 0 => Ninth, when 1 => Suspended,
                                when others => Sixth));
         end if;
      end loop;
   end Make_Colors;

   procedure Make_Progression (G : in out Generator; Mode : Mode_Kind; Region : Region_Kind;
                               Colour : Float; Into : out Progression)
     with Global => null
   is
      Openers : constant array (0 .. 3) of Scale_Degree := [0, 3, 5, 1];
      Pick_Value : Natural;
      Back : Natural;
   begin
      Into := (others => <>);
      for Try in 1 .. Progression_Tries loop
         Pick (G, Openers'Length, Pick_Value);
         Into.Degrees (0) := Openers (Pick_Value);
         if Diminished (Mode, Into.Degrees (0)) then
            Into.Degrees (0) := 0;
         end if;
         for Slot in 1 .. Chord_Slot'Last loop
            Next_Degree (G, Mode, Into.Degrees (Slot - 1), Into.Degrees (Slot));
         end loop;
         Back := (Into.Degrees (0) + Scale_Length - Into.Degrees (Chord_Slot'Last)) mod Scale_Length;
         exit when Motions (Back) >= Strong_Cadence;
      end loop;
      Make_Colors (G, Region, Colour, Into.Colors);
   end Make_Progression;

   ---------------------------------------------------------------------------

   procedure Region_For (G : in out Generator; Bias : Energy_Bias; Region : out Region_Kind)
     with Global => null
   is
      Value : Natural;
   begin
      Roll (G, Value);
      Region :=
        (case Bias is
           when Any =>
             (if Value < 20 then Lo_Fi elsif Value < 40 then Slow_Jam
              elsif Value < 60 then Mall_Funk elsif Value < 75 then Outrun else Drift),
           when Warm => (if Value < 30 then Slow_Jam elsif Value < 70 then Mall_Funk else Drift),
           when Hot => (if Value < 35 then Mall_Funk elsif Value < 75 then Outrun else Drift),
           when Blazing => (if Value < 70 then Outrun else Drift));
   end Region_For;

   --  The style is filled a group at a time (small proofs, one each).
   procedure Fill_Name (G : in out Generator; S : in out Style)
     with Global => null, Pre => S.Name_Last = 0
   is
      Pick_Value : Natural;
   begin
      --  Name.
      Pick (G, Word_Count, Pick_Value);
      Append (S.Name, S.Name_Last, Adjective (Pick_Value));
      Pick (G, Word_Count, Pick_Value);
      Append (S.Name, S.Name_Last, Noun (Pick_Value));
   end Fill_Name;

   procedure Fill_Tempo (G : in out Generator; Region : Region_Kind; Bias : Energy_Bias; S : in out Style)
     with Global => null
   is
      T : constant Region_Spans := Bounds (Region);
      Value : Natural;
      Hit : Boolean;
      Slowest : Step_Samples;
      Fastest : Step_Samples;
   begin
      --  Tempo: any whole sixteenth between the region's BPM bounds.
      Slowest := Step_For_BPM (Positive'Max (Slowest_BPM, Positive'Min (Fastest_BPM, T.Slow_BPM)));
      Fastest := Step_For_BPM (Positive'Max (Slowest_BPM, Positive'Min (Fastest_BPM, T.Fast_BPM)));
      if Bias = Blazing then
         Slowest := Step_Samples'Min (Slowest, Step_For_BPM (Blazing_Floor_BPM));
      elsif Bias = Hot then
         Slowest := Step_Samples'Min (Slowest, Step_For_BPM (Hot_Floor_BPM));
      end if;
      if Fastest > Slowest then
         Fastest := Slowest;
      end if;
      Pick (G, Slowest - Fastest + 1, Value);
      S.Step := Fastest + Value;
      S.Drop_Step := S.Step;
      Chance_Of (G, (if Region in Outrun | Mall_Funk | Drift then 0.4 else 0.0), Hit);
      Pick (G, Lift_Percent_Span, Value);
      if Hit then
         S.Drop_Step := Step_Samples'Max
           (Fastest_Step, S.Step - S.Step * (Lift_Percent_Low + Value) / Percent);
      end if;
   end Fill_Tempo;

   procedure Fill_Harmony (G : in out Generator; Region : Region_Kind; S : in out Style)
     with Global => null
   is
      T : constant Region_Spans := Bounds (Region);
      V : Float;
      Value : Natural;
      Hit : Boolean;
   begin
      --  Key and mode.
      Pick (G, Key_Root'Last - Key_Root'First + 1, Value);
      S.Root := Key_Root'First + Value;
      Roll (G, Value);
      S.Mode :=
        (case Region is
           when Lo_Fi => (if Value < 40 then Dorian elsif Value < 70 then Ionian else Aeolian),
           when Slow_Jam => (if Value < 40 then Ionian elsif Value < 70 then Lydian else Dorian),
           when Mall_Funk => (if Value < 40 then Ionian elsif Value < 70 then Dorian else Mixolydian),
           when Outrun => (if Value < 60 then Aeolian else Dorian),
           when Drift => Mode_Kind'Val (Value mod 5));
      Pick (G, (if Region in Outrun | Mall_Funk then 3 else 2), Value);
      S.Lift := Value;
      Draw (G, T.Colour, V);
      S.Colour_Chance := Unit (V);
      Make_Progression (G, S.Mode, Region, S.Colour_Chance, S.Verse);
      Chance_Of (G, 0.5, Hit);
      if Hit then
         --  The chorus answers the verse: its first half, a new cadence.
         S.Chorus := S.Verse;
         Next_Degree (G, S.Mode, S.Chorus.Degrees (1), S.Chorus.Degrees (2));
         Next_Degree (G, S.Mode, S.Chorus.Degrees (2), S.Chorus.Degrees (3));
         Make_Colors (G, Region, S.Colour_Chance, S.Chorus.Colors);
      else
         Make_Progression (G, S.Mode, Region, S.Colour_Chance, S.Chorus);
      end if;
      Chance_Of (G, (case Region is when Lo_Fi | Slow_Jam => 0.55, when Outrun => 0.25,
                       when Mall_Funk => 0.0, when Drift => 0.3), Hit);
      S.Chord_Bars := (if Hit then 2 else 1);
   end Fill_Harmony;

   procedure Fill_Groove (G : in out Generator; Region : Region_Kind; S : in out Style)
     with Global => null
   is
      T : constant Region_Spans := Bounds (Region);
      V : Float;
      Value : Natural;
      Hit : Boolean;
   begin
      --  Groove.
      Draw (G, T.Swing, V);
      S.Swing := Clamp (V, Swing_Amount'First, Swing_Amount'Last);
      Draw (G, T.Humanize, V);
      S.Humanize := Clamp (V, Humanize_Amount'First, Humanize_Amount'Last);
      Make_Kicks (G, Region, S.Kick_Main, S.Kick_Alt);
      Make_Ghosts (G, S.Kick_Main, S.Ghosts);
      Draw (G, T.Ghost, V);
      S.Ghost_Chance := Unit (V);
      Roll (G, Value);
      S.Hats :=
        (case Region is
           when Lo_Fi => (if Value < 60 then Eighths else Gallop),
           when Slow_Jam => (if Value < 50 then Eighths elsif Value < 80 then Gallop else Sixteenths),
           when Mall_Funk | Outrun => (if Value < 50 then Offbeat_Open else Sixteenths),
           when Drift => Hat_Kind'Val (Value mod 4));
      Draw (G, T.Open, V);
      S.Open_Chance := Unit (V);
      Chance_Of (G, (case Region is when Mall_Funk => 0.7, when Lo_Fi => 0.2, when others => 0.5), Hit);
      S.Clap_Layer := Hit;
      Chance_Of (G, (case Region is when Outrun => 0.35, when Mall_Funk => 0.25, when Drift => 0.3,
                       when others => 0.1), Hit);
      S.Drop_Double := Hit;
      Chance_Of (G, (if Region = Lo_Fi then 0.5 else 0.85), Hit);
      S.Chase := Hit;
      Roll (G, Value);
      S.Fill :=
        (case Region is
           when Lo_Fi => (if Value < 40 then Drop_Out elsif Value < 70 then Stutter else Snare_Roll),
           when Slow_Jam => (if Value < 50 then Tom_Run else Snare_Roll),
           when Mall_Funk => (if Value < 40 then Snare_Roll elsif Value < 70 then Stutter else Tom_Run),
           when Outrun => (if Value < 55 then Tom_Run else Snare_Roll),
           when Drift => Fill_Kind'Val (Value mod 4));
   end Fill_Groove;

   procedure Fill_Kit (G : in out Generator; Region : Region_Kind; S : in out Style)
     with Global => null
   is
      T : constant Region_Spans := Bounds (Region);
      V : Float;
   begin
      --  Kit.
      Draw (G, T.Kick_Body, V);
      S.Kick_Body := Clamp (V, Kick_Pitch'First, Kick_Pitch'Last);
      Draw (G, T.Kick_Punch, V);
      S.Kick_Punch := Unit (V);
      Draw (G, T.Kick_Length, V);
      S.Kick_Length := Clamp (V, Kit_Decay'First, Kit_Decay'Last);
      Draw (G, T.Snare_Body, V);
      S.Snare_Body := Clamp (V, Snare_Pitch'First, Snare_Pitch'Last);
      Draw (G, T.Snare_Tone, V);
      S.Snare_Tone := Unit (V);
      Draw (G, T.Snare_Room, V);
      S.Snare_Room := Unit (V);
      Draw (G, T.Snare_Length, V);
      S.Snare_Length := Clamp (V, Kit_Decay'First, Kit_Decay'Last);
      Draw (G, T.Hat_Tone, V);
      S.Hat_Tone := Unit (V);
   end Fill_Kit;

   procedure Fill_Arrangement (G : in out Generator; Region : Region_Kind; S : in out Style)
     with Global => null
   is
      T : constant Region_Spans := Bounds (Region);
      V : Float;
      Value : Natural;
      Hit : Boolean;
   begin
      --  Arrangement.
      Roll (G, Value);
      S.Bass :=
        (case Region is
           when Lo_Fi => (if Value < 50 then Sustained else Walking),
           when Slow_Jam => (if Value < 50 then Walking elsif Value < 75 then Sustained else Funk),
           when Mall_Funk => (if Value < 80 then Funk else Walking),
           when Outrun => (if Value < 80 then Pulse else Funk),
           when Drift => Bass_Kind'Val (Value mod 4));
      Make_Bass (G, S.Bass, S.Bass_Mask);
      Draw (G, T.Slide, V);
      S.Slide_Chance := Unit (V);
      Draw (G, T.Bass_Tone, V);
      S.Bass_Tone := Unit (V);
      Pick (G, 4, Value);
      S.Arp := Arp_Kind'Val (Value);
      Chance_Of (G, (case Region is when Outrun => 0.75, when Lo_Fi => 0.1, when others => 0.45), Hit);
      S.Arp_Every := (if Hit then 1 else 2);
      Pick (G, 2, Value);
      S.Arp_Span := 1 + Value;
      Draw (G, T.Arp_Tone, V);
      S.Arp_Tone := Unit (V);
      Make_Comp (G, Region, S.Comp_Mask);
      Draw (G, T.Piano_Tone, V);
      S.Piano_Tone := Unit (V);
      Draw (G, T.Pad_Tone, V);
      S.Pad_Tone := Unit (V);
      Draw (G, T.Lead_Tone, V);
      S.Lead_Tone := Unit (V);
      Roll (G, Value);
      S.Answer :=
        (case Region is
           when Lo_Fi => (if Value < 60 then Piano_Answer else Bell_Answer),
           when Slow_Jam => (if Value < 60 then Bell_Answer else Piano_Answer),
           when Mall_Funk => (if Value < 40 then Piano_Answer elsif Value < 70 then Arp_Answer
                              else Bell_Answer),
           when Outrun => (if Value < 60 then Arp_Answer else Bell_Answer),
           when Drift => Answer_Voice'Val (Value mod 3));
      Draw (G, T.Density, V);
      S.Density := Unit (V);
      Draw (G, T.Crackle, V);
      S.Crackle := Unit (V);
   end Fill_Arrangement;

   procedure Fill_Effects (G : in out Generator; Region : Region_Kind; S : in out Style)
     with Global => null
   is
      T : constant Region_Spans := Bounds (Region);
      V : Float;
      Value : Natural;
      Echo_Choices : constant array (0 .. 3) of Echo_Steps := [2, 3, 4, 6];
   begin
      --  Effects.
      Draw (G, T.Reverb_Wet, V);
      S.Reverb_Wet := Clamp (V, Wet_Level'First, Wet_Level'Last);
      Draw (G, T.Reverb_Room, V);
      S.Reverb_Room := Clamp (V, Room_Size'First, Room_Size'Last);
      Pick (G, Echo_Choices'Length, Value);
      S.Echo := Echo_Choices (Value);
      Draw (G, T.Echo_Amount, V);
      S.Echo_Amount := Clamp (V, Echo_Feedback'First, Echo_Feedback'Last);
      Draw (G, T.Wobble, V);
      S.Wobble := Unit (V);
      Draw (G, T.Ceiling, V);
      S.Ceiling := Clamp (V, Brightness'First, Brightness'Last);
   end Fill_Effects;

   procedure Fill_Style (G : in out Generator; Seed : Unsigned_64; Region : Region_Kind;
                         Bias : Energy_Bias; S : out Style)
     with Post => Is_Unit (S.Colors) and then S.Region = Region
   is
   begin
      S := (Seed => Seed, Region => Region, others => <>);
      Fill_Name (G, S);
      Fill_Tempo (G, Region, Bias, S);
      Fill_Harmony (G, Region, S);
      Fill_Groove (G, Region, S);
      Fill_Kit (G, Region, S);
      Fill_Arrangement (G, Region, S);
      Fill_Effects (G, Region, S);
      Make_Look (G, Region, S.Colors);
      S.Seed := Seed;
      S.Region := Region;
   end Fill_Style;

   function Generate (Seed : Unsigned_64; Bias : Energy_Bias) return Style is
      G : Generator := Seeded (Seed, Style_Stream);
      Region : Region_Kind;
      Result : Style;
   begin
      Region_For (G, Bias, Region);
      Fill_Style (G, Seed, Region, Bias, Result);
      return Result;
   end Generate;

   function Stage_Seed (Playlist : Unsigned_64; Stage : Positive) return Unsigned_64 is
      X : Unsigned_64;
   begin
      if Stage = 1 then
         return Playlist;
      end if;
      X := Playlist * Stage_Mix_A + Unsigned_64 (Stage) * Stage_Mix_B;
      return X xor Shift_Right (X, Stage_Mix_Shift);
   end Stage_Seed;

end Neon_Style;
