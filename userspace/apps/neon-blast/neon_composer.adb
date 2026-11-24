------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Math; use Neon_Math;

--  Not proved (regression-tested by tests/neon-blast): the draws are
--  functions with side effects on the composer's generator.
package body Neon_Composer with SPARK_Mode => Off is
   use Neon_Notes;
   use Neon_Style;

   ---------------------------------------------------------------------------
   --  The stage's form
   ---------------------------------------------------------------------------

   type Plan_Entry is record
      Kind : Section_Kind;
      Bars : Positive;
   end record;
   type Plan_Table is array (Plan_Index) of Plan_Entry;
   Stage_Plan : constant Plan_Table :=
     [(Intro, 2), (Groove, 8), (Build, 4), (Drop, 8), (Breakdown, 4), (Build, 4), (Peak, 4),
      (Outro, 2)];
   Boss_Build : constant Plan_Index := 5;    --  boss practice starts here
   Peak_Entry : constant Plan_Index := 6;
   Outro_Entry : constant Plan_Index := 7;
   Phrase_Bars : constant := 4;              --  call (two bars) and answer (two)
   Motif_Bars : constant := 8;               --  a new call this often
   Title_Chord_Bars : constant := 2;

   type Section_Energy is array (Section_Kind) of Energy_Range;
   Energies : constant Section_Energy :=
     [Title_Theme => 1, Intro => 2, Groove => 3, Build => 4, Drop => 5, Breakdown => 2, Peak => 5,
      Outro => 3, Finale => 1, Metronome => 0];
   type Section_Float is array (Section_Kind) of Float;
   Pad_Levels : constant Section_Float :=
     [Title_Theme => 0.75, Intro => 0.6, Groove => 0.55, Build => 0.5, Drop => 0.5,
      Breakdown => 0.85, Peak => 0.5, Outro => 0.6, Finale => 0.85, Metronome => 0.45];
   --  The master filter's opening at the section's start and end (ramps).
   Cutoff_From : constant Section_Float :=
     [Title_Theme => 0.55, Intro => 0.55, Groove => 0.88, Build => 0.45, Drop => 1.0,
      Breakdown => 0.42, Peak => 1.0, Outro => 0.9, Finale => 0.32, Metronome => 0.55];
   Cutoff_To : constant Section_Float :=
     [Title_Theme => 0.55, Intro => 0.9, Groove => 0.88, Build => 1.0, Drop => 1.0,
      Breakdown => 0.5, Peak => 1.0, Outro => 0.7, Finale => 0.32, Metronome => 0.55];
   --  Sections the tape and the room thicken in.
   function Hazy (S : Section_Kind) return Boolean is (S in Title_Theme | Breakdown | Finale);
   Haze_Wobble : constant := 0.25;
   Haze_Wet : constant := 0.15;

   ---------------------------------------------------------------------------
   --  Harmony
   ---------------------------------------------------------------------------

   --  The pad's register: chord tones fold into Pad_Low .. Pad_Low + 11,
   --  extensions an octave up; the bass below Bass_Low + 11.
   Pad_Low : constant := 55;
   Bass_Low : constant := 33;
   Melody_Lift : constant := Octave;         --  the melody sits an octave above the key
   Minor_Ninth : constant := 13;
   Minor_Thirteenth : constant := 20;
   Major_Third : constant := 4;
   Natural_Eleventh : constant := 17;
   Fifth_Steps : constant := 4;

   function Chord_Pitch (C : Chord_Value; Degree : Natural) return Float is
      Count : constant Positive := (if C.Count = 0 then 1 else C.Count);
      Index : constant Tone_Index := Tone_Index (Degree mod Count + 1);
      Lift : constant Natural := Natural'Min (Degree / Count, 3);
   begin
      return C.Tones (Index) + Float (Octave * (1 + Lift));
   end Chord_Pitch;

   type Interval_List is array (Tone_Index) of Natural;

   procedure Voice (Root : Natural; Degree : Scale_Degree; Count : Tone_Count;
                    Steps : Interval_List; Into : out Chord_Value) is
      Pitch : Integer;
      Swap : Float;
   begin
      Into := (Root => Root, Degree => Degree, Count => Count, Tones => [others => Float (Pad_Low)],
               Bass => Float (Bass_Low + (Root - Bass_Low) mod Octave));
      for I in 1 .. Count loop
         Pitch := Pad_Low + (Root + Steps (I) - Pad_Low) mod Octave;
         if Steps (I) >= Octave then
            Pitch := Pitch + Octave;
         end if;
         Into.Tones (I) := Float (Pitch);
      end loop;
      --  Ascending order (insertion sort of at most five).
      for I in 2 .. Count loop
         for J in reverse 2 .. I loop
            if Into.Tones (J) < Into.Tones (J - 1) then
               Swap := Into.Tones (J);
               Into.Tones (J) := Into.Tones (J - 1);
               Into.Tones (J - 1) := Swap;
            end if;
         end loop;
      end loop;
   end Voice;

   --  The chord on Degree of Tonic's Mode, coloured: stacked thirds from
   --  the scale, avoiding the clashing extensions (a minor ninth, a minor
   --  thirteenth, an eleventh over a major third).
   procedure Build_Chord (Tonic : Natural; Mode : Mode_Kind; Degree : Scale_Degree;
                          Color : Chord_Color; Into : out Chord_Value) is
      Third : constant Natural := Natural (Interval (Mode, Degree, 2));
      Fifth : constant Natural := Natural (Interval (Mode, Degree, Fifth_Steps));
      Seventh_Tone : constant Natural := Natural (Interval (Mode, Degree, 6));
      Ninth_Raw : constant Natural := Natural (Interval (Mode, Degree, 8));
      Fourth : constant Natural := Natural (Interval (Mode, Degree, 3));
      Sixth_Tone : constant Natural := Natural (Interval (Mode, Degree, 5));
      Eleventh_Raw : constant Natural := Natural (Interval (Mode, Degree, 10));
      Thirteenth_Raw : constant Natural := Natural (Interval (Mode, Degree, 12));
      Ninth_Tone : constant Natural := (if Ninth_Raw = Minor_Ninth then Fifth else Ninth_Raw);
      Steps : Interval_List := [others => 0];
      Count : Tone_Count := 4;
   begin
      case Color is
         when Seventh =>
            Steps := [0, Third, Fifth, Seventh_Tone, 0];
         when Ninth =>
            Steps := [0, Third, Fifth, Seventh_Tone, Ninth_Tone];
            Count := 5;
         when Suspended =>
            Steps := [0, Fourth, Fifth, Seventh_Tone, Ninth_Tone];
            Count := 5;
         when Sixth =>
            Steps := [0, Third, Fifth, Sixth_Tone, Ninth_Tone];
            Count := 5;
         when Eleventh =>
            Steps := [0, Third, Seventh_Tone, Ninth_Tone,
                      (if Third = Major_Third and then Eleventh_Raw = Natural_Eleventh
                       then Thirteenth_Raw else Eleventh_Raw)];
            Count := 5;
         when Thirteenth =>
            Steps := [0, Third, Seventh_Tone, Ninth_Tone,
                      (if Thirteenth_Raw = Minor_Thirteenth then Fifth else Thirteenth_Raw)];
            Count := 5;
      end case;
      Voice (Tonic + Scales (Mode, Degree), Degree, Count, Steps, Into);
   end Build_Chord;

   --  The pivot into the next stage: a suspended dominant of its key.
   Pivot_Steps : constant Interval_List := [0, 5, 7, 10, 14];
   Dominant : constant := 7;

   --  A melody position (scale steps above the key) as a MIDI pitch.
   Position_Offset : constant := 3 * Scale_Length;   --  keeps the arithmetic non-negative
   function Melody_Pitch (Tonic : Natural; Mode : Mode_Kind; Position : Integer) return Float is
      Shifted : constant Natural := Natural (Position + Position_Offset);
   begin
      return Float (Tonic + Melody_Lift + Scales (Mode, Shifted mod Scale_Length)
                    + Octave * (Shifted / Scale_Length)) - Float (3 * Octave);
   end Melody_Pitch;

   ---------------------------------------------------------------------------
   --  Small helpers
   ---------------------------------------------------------------------------

   function Draw (C : in out Composer) return Float is
      Value : Float;
   begin
      Neon_Random.Uniform (C.Random, Value);
      return Value;
   end Draw;

   function Choose (C : in out Composer; Count : Positive) return Natural is
      Value : Natural;
   begin
      Neon_Random.Pick (C.Random, Count, Value);
      return Value;
   end Choose;

   procedure Add (C : in out Composer; Into : in out Note_Batch; Item : Neon_Notes.Note) is
   begin
      if Into.Count < Maximum_Batch then
         Into.Count := Into.Count + 1;
         Into.Items (Into.Count) := Item;
      else
         C.Dropped_Notes := C.Dropped_Notes + 1;
      end if;
   end Add;

   function Open_Ended (S : Section_Kind) return Boolean is
     (S in Title_Theme | Finale | Metronome);

   procedure New_Call (C : in out Composer) is
      Position : Integer;
      Moves : constant array (0 .. 6) of Integer := [-2, -1, -1, 0, 1, 1, 2];
      Starts : constant array (0 .. 2) of Integer := [0, 2, 4];
      Breath : constant := 28;    --  the call's last beat rests
      Density : constant Float := C.Style.Density;
      Chance : Float;
      Lowest_Walk : constant := -3;
      Highest_Walk : constant := 9;
      Beat_Chance : constant := 0.55;
      Eighth_Chance : constant := 0.4;
      Sixteenth_Chance : constant := 0.12;
   begin
      Position := Starts (Choose (C, Starts'Length));
      for I in Phrase_Index loop
         Chance := (if I mod 4 = 0 then Beat_Chance elsif I mod 2 = 0 then Eighth_Chance
                    else Sixteenth_Chance) * (0.6 + 0.6 * Density);
         if I >= Breath or else Draw (C) >= Chance then
            C.Call (I) := Rest;
         else
            Position := Integer'Max (Lowest_Walk, Integer'Min (Highest_Walk,
                                     Position + Moves (Choose (C, Moves'Length))));
            C.Call (I) := Position;
         end if;
      end loop;
   end New_Call;

   function Section_Step_Length (C : Composer; S : Section_Kind) return Step_Samples is
     (case S is
        when Drop | Peak => C.Style.Drop_Step,
        when Metronome => Default_Step,
        when others => C.Style.Step);

   function Feel_For (C : Composer; S : Section_Kind) return Feel_Kind is
     (case S is
        when Title_Theme | Finale | Breakdown => Half_Time,
        when Drop => (if C.Style.Drop_Double then Double_Time else Straight),
        when Peak => (if C.Style.Chase and then C.Peak_Phrases mod 2 = 1 then Double_Time
                      else Straight),
        when others => Straight);

   procedure Enter (C : in out Composer; Kind : Section_Kind; Bars : Natural) is
   begin
      C.Section := Kind;
      C.Section_Bar := 0;
      C.Section_Bars := Bars;
      if Kind /= Peak then
         C.Peak_Phrases := 0;
      end if;
      C.Feel := Feel_For (C, Kind);
      if Kind in Groove | Drop | Breakdown | Peak | Title_Theme then
         New_Call (C);
      end if;
      C.Arp_Position := 0;
      C.Arp_Up := True;
   end Enter;

   procedure Enter_Plan (C : in out Composer; Index : Plan_Index) is
   begin
      C.Plan := Index;
      Enter (C, Stage_Plan (Index).Kind, Stage_Plan (Index).Bars);
   end Enter_Plan;

   procedure Fade_Look (C : in out Composer; From : Look) is
   begin
      C.Look_From := From;
      C.Fade_Done := 0;
   end Fade_Look;

   --  The bar-start decisions: where the piece goes next.
   procedure Next_Bar (C : in out Composer; M : Mood) is
      Old_Look : constant Look := C.Current.Look;
   begin
      C.Bar_Mood := M;
      case M.Scene is
         when Calibrate =>
            if C.Section /= Metronome then
               Enter (C, Metronome, 0);
            else
               C.Section_Bar := C.Section_Bar + 1;
            end if;
         when Attract =>
            if C.Section /= Title_Theme or else C.Playlist /= M.Playlist
              or else C.Start_Stage /= M.Start_Stage
            then
               if C.Playlist /= M.Playlist or else C.Start_Stage /= M.Start_Stage
                 or else C.Stage /= 0
               then
                  C.Style := Stage_Style (M.Playlist, M.Start_Stage);
                  Fade_Look (C, Old_Look);
               end if;
               C.Playlist := M.Playlist;
               C.Start_Stage := M.Start_Stage;
               C.Stage := 0;
               Enter (C, Title_Theme, 0);
            else
               C.Section_Bar := C.Section_Bar + 1;
            end if;
         when Over =>
            if C.Section /= Finale then
               Enter (C, Finale, 0);
            else
               C.Section_Bar := C.Section_Bar + 1;
            end if;
         when Play =>
            if M.Run /= C.Run or else C.Stage = 0 or else Open_Ended (C.Section) then
               --  A run begins: the title's style is its first stage's.
               C.Run := M.Run;
               C.Stage := M.Start_Stage;
               if C.Playlist /= M.Playlist or else C.Start_Stage /= M.Start_Stage then
                  C.Style := Stage_Style (M.Playlist, M.Start_Stage);
                  Fade_Look (C, Old_Look);
               end if;
               C.Playlist := M.Playlist;
               C.Start_Stage := M.Start_Stage;
               Enter_Plan (C, (if M.Boss_Practice then Boss_Build else 0));
            else
               C.Section_Bar := C.Section_Bar + 1;
               if C.Section = Peak then
                  if M.Defeated_Stage >= C.Stage then
                     C.Next_Style := Stage_Style (C.Playlist, C.Stage + 1);
                     Enter_Plan (C, Outro_Entry);
                  elsif C.Section_Bar >= C.Section_Bars then
                     C.Peak_Phrases := C.Peak_Phrases + 1;
                     Enter (C, Peak, Stage_Plan (Peak_Entry).Bars);
                  end if;
               elsif C.Section_Bar >= C.Section_Bars then
                  if C.Section = Outro then
                     C.Stage := C.Stage + 1;
                     C.Style := C.Next_Style;
                     Enter_Plan (C, 0);
                  elsif C.Plan < Peak_Entry then
                     Enter_Plan (C, C.Plan + 1);
                  end if;
               end if;
            end if;
      end case;
      if C.Section_Bar mod Motif_Bars = 0 and then C.Section_Bar > 0 then
         New_Call (C);
      end if;
      if C.Section_Bar mod Phrase_Bars = 0 then
         C.Answer := Answer_Shape'Val (Choose (C, 3));
      end if;
      C.Alt_Bar := C.Section_Bar mod Phrase_Bars = Phrase_Bars - 1
        or else (Energies (C.Section) = Energy_Range'Last and then Draw (C) < 0.25);
   end Next_Bar;

   ---------------------------------------------------------------------------

   procedure Start (C : out Composer; Seed : Unsigned_64) is
   begin
      C := (Random => Neon_Random.Seeded (Seed, 16#C0DE#), others => <>);
      C.Style := Stage_Style (C.Playlist, 1);
      C.Next_Style := C.Style;
      C.Look_From := C.Style.Colors;
      C.Current.Look := C.Style.Colors;
      C.Fade_Done := Look_Fade_Steps;
      New_Call (C);
   end Start;

   function Next_Start (C : Composer) return Samples is (C.Next);
   function Newest_Serial (C : Composer) return Natural is (C.Serial);
   function Dropped (C : Composer) return Natural is (C.Dropped_Notes);

   ---------------------------------------------------------------------------
   --  One step
   ---------------------------------------------------------------------------

   procedure Compose_Next (C : in out Composer; Wanted : Mood; Into : in out Note_Batch;
                           Info : out Step_Record; Mix : out Mix_Targets) is
      Step : constant Step_Index := C.Step;
   begin
      if Step = 0 then
         Next_Bar (C, Wanted);
      end if;
      declare
         S : constant Style := C.Style;
         M : constant Mood := C.Bar_Mood;
         Section : constant Section_Kind := C.Section;
         Feel : constant Feel_Kind := C.Feel;
         Energy : constant Energy_Range := Energies (Section);
         Length : constant Step_Samples := Section_Step_Length (C, Section);
         Lifted : constant Boolean := Section in Drop | Peak;
         Tonic : constant Natural := S.Root + (if Lifted then S.Lift else 0);
         Chords : constant Progression := (if Lifted then S.Chorus else S.Verse);
         Chord_Bars : constant Positive :=
           (if Open_Ended (Section) then Title_Chord_Bars else S.Chord_Bars);
         Slot : constant Chord_Slot := (C.Section_Bar / Chord_Bars) mod Progression_Length;
         Phrase_Bar : constant Natural := C.Section_Bar mod Phrase_Bars;
         Last_Bar : constant Boolean :=
           C.Section_Bars > 0 and then C.Section_Bar + 1 = C.Section_Bars;
         Start_Time : constant Samples := C.Next;
         Swing : constant Natural :=
           (if Step mod 2 = 1 then Natural (S.Swing * Float (Length)) else 0);
         Jitter_Span : constant Float := S.Humanize * Float (Length) / 10.0;
         Next_Is_Big : constant Boolean :=
           Last_Bar and then Section in Groove | Breakdown | Build | Intro;
         Fill_From : constant Step_Index := (if Next_Is_Big then 8 else 12);
         In_Fill : constant Boolean :=
           Step >= Fill_From
           and then (Last_Bar or else (Section in Drop | Peak and then Phrase_Bar = Phrase_Bars - 1))
           and then Section not in Title_Theme | Finale | Metronome;
         Change : Boolean := False;
         Kick_Hit, Snare_Hit, Impact_Hit : Boolean := False;
         Kick_Velocity, Snare_Velocity : Float := 0.0;
         Ch : Chord_Value;

         --  Humanized notes get a velocity spread and a small late jitter,
         --  drawn here in a fixed order (one draw a statement: the order of
         --  the draws is part of the replay).
         procedure Play (Voice_Of : Instrument; Pitch, Velocity : Float; Steps : Positive;
                         Pan : Float := 0.0; Late : Natural := 0; Tone : Unit_Float := 0.5;
                         Decay : Decay_Scale := 1.0; Glide : Glide_Semitones := 0.0;
                         Humanized : Boolean := False) is
            Spread : Float := 1.0;
            Jitter : Natural := 0;
         begin
            if Humanized then
               Spread := 1.0 + S.Humanize * (2.0 * Draw (C) - 1.0);
               Jitter := Natural (Jitter_Span * Draw (C));
            end if;
            Add (C, Into, (At_Time => Start_Time + Samples (Late + Jitter), Voice => Voice_Of,
                           Pitch => Pitch, Velocity => Velocity * Spread, Length => Steps * Length,
                           Pan => Pan, Tone => Tone, Decay => Decay, Glide => Glide));
         end Play;
      begin
         --  The chord: on the bar it changes, or a pivot into the next stage.
         if Section = Outro and then Last_Bar then
            Voice (C.Next_Style.Root + Dominant, 4, 5, Pivot_Steps, Ch);
         elsif Section = Metronome then
            Build_Chord (Tonic, S.Mode, S.Verse.Degrees (0), Seventh, Ch);
         else
            Build_Chord (Tonic, S.Mode, Chords.Degrees (Slot), Chords.Colors (Slot), Ch);
         end if;
         if Step = 0 then
            Change := C.Serial = 0 or else Ch /= C.Current.Chord or else C.Section_Bar = 0;
            if Change then
               C.Arp_Position := 0;
               C.Arp_Up := True;
            end if;
         else
            Ch := C.Current.Chord;
         end if;

         --  Pad: the chord, held across the chord's span.
         if Change then
            for I in 1 .. Ch.Count loop
               Play (Pad, Ch.Tones (I), Pad_Levels (Section), Chord_Bars * Steps_Per_Bar,
                     Pan => (Float (I) - 3.0) * 0.25,
                     Tone => (if Hazy (Section) then 0.5 * S.Pad_Tone else S.Pad_Tone));
            end loop;
         end if;

         --  Vinyl crackle under everything but the metronome.
         if Step = 0 and then S.Crackle > 0.05 and then Section /= Metronome then
            Play (Vinyl, 60.0, 0.6 * S.Crackle * (if Hazy (Section) then 1.3 else 1.0),
                  Steps_Per_Bar);
         end if;

         ---------------------------------------------------------------------
         --  Drums
         ---------------------------------------------------------------------
         if Section /= Metronome and then Energy >= 1 then
            declare
               Intro_Sparse : constant Boolean := Section = Intro and then C.Section_Bar = 0;
               Pattern : constant Steps_Mask := (if C.Alt_Bar then S.Kick_Alt else S.Kick_Main);
               Soft : constant Float :=
                 (case Section is
                    when Title_Theme => 0.6, when Finale => 0.5, when Breakdown => 0.8,
                    when others => 1.0);
               Rolling : constant Boolean := Section = Build;
               Roll_Every : constant Positive :=
                 (if C.Section_Bar = 0 then 4 elsif C.Section_Bar = 1 then 2 else 1);
               Progress : constant Float :=
                 (if C.Section_Bars = 0 then 0.0
                  else Float (C.Section_Bar * Steps_Per_Bar + Step)
                       / Float (C.Section_Bars * Steps_Per_Bar));
               Silent : constant Boolean := In_Fill and then S.Fill = Drop_Out and then Step >= 12;
            begin
               case Feel is
                  when Straight =>
                     Kick_Hit := Pattern (Step);
                     Snare_Hit := Step in 4 | 12;
                  when Half_Time =>
                     Kick_Hit := Step = 0 or else (Step in 10 | 11 | 14 and then S.Kick_Main (Step));
                     Snare_Hit := Step = 8;
                  when Double_Time =>
                     Kick_Hit := Step mod 4 = 0;
                     Snare_Hit := Step mod 4 = 2;
               end case;
               if Rolling then
                  Kick_Hit := Step mod Steps_Per_Beat = 0;
                  Snare_Hit := Step mod Roll_Every = 0 and then (C.Section_Bar > 0 or else Step in 4 | 12);
               end if;
               if Intro_Sparse then
                  Kick_Hit := Step in 0 | 8;
                  Snare_Hit := False;
               end if;
               Kick_Velocity := Soft;
               Snare_Velocity :=
                 (if Rolling then 0.3 + 0.65 * Progress
                  elsif Feel = Double_Time then (if Step mod 8 = 6 then 0.95 else 0.75)
                  else 0.85) * Soft;

               --  Fills into the next section.
               if In_Fill and then not Rolling then
                  case S.Fill is
                     when Snare_Roll =>
                        Snare_Hit := True;
                        Snare_Velocity := 0.3 + 0.6 * Float (Step - Fill_From) / 8.0;
                     when Tom_Run =>
                        Snare_Hit := Step = 15;
                        Play (Tom, 50.0 - 1.5 * Float (Step - Fill_From), 0.75, 2,
                              Pan => 0.4 - 0.1 * Float (Step - Fill_From), Late => Swing, Humanized => True);
                     when Stutter =>
                        Kick_Hit := Step mod 2 = 0;
                        Snare_Hit := Step mod 2 = 1;
                        Snare_Velocity := 0.7;
                     when Drop_Out =>
                        null;
                  end case;
               elsif In_Fill and then Last_Bar and then Rolling and then S.Fill = Tom_Run then
                  Play (Tom, 52.0 - 1.5 * Float (Step - Fill_From), 0.8, 2,
                        Pan => 0.4 - 0.1 * Float (Step - Fill_From), Humanized => True);
               end if;
               if Silent then
                  Kick_Hit := False;
                  Snare_Hit := False;
               end if;

               --  Ghost snares between the hits.
               if not Silent and then not Rolling and then not In_Fill and then not Intro_Sparse
                 and then S.Ghosts (Step) and then not Kick_Hit and then not Snare_Hit
                 and then Draw (C) < S.Ghost_Chance
               then
                  Play (Snare, S.Snare_Body, 0.18 * Soft, 1, Pan => 0.08,
                        Late => Swing, Humanized => True, Tone => 0.6 * S.Snare_Tone, Decay => 0.6);
               end if;

               --  Hats.
               if not Silent and then not (In_Fill and then S.Fill = Stutter) then
                  declare
                     Hat_Here : Boolean := False;
                     Open_Here : Boolean := False;
                     Level : Float := 0.0;
                  begin
                     if Feel = Double_Time then
                        Hat_Here := True;
                        Open_Here := Step mod 4 = 2;
                        Level := (if Step mod 4 = 0 then 0.5 else 0.38);
                     elsif Feel = Half_Time or else Energy <= 1 then
                        Hat_Here := Step mod 2 = 0;
                        Open_Here := Step = 14 and then Draw (C) < S.Open_Chance;
                        Level := (if Step mod 4 = 0 then 0.3 else 0.4);
                     else
                        case S.Hats is
                           when Eighths =>
                              Hat_Here := Step mod 2 = 0 or else Draw (C) < 0.3 * S.Density;
                              Level := (if Step mod 4 = 0 then 0.42 elsif Step mod 2 = 0 then 0.55
                                        else 0.18);
                           when Sixteenths =>
                              Hat_Here := True;
                              Level := (if Step mod 4 = 0 then 0.5 elsif Step mod 2 = 0 then 0.38
                                        else 0.26);
                           when Offbeat_Open =>
                              Hat_Here := True;
                              Open_Here := Step mod 4 = 2;
                              Level := (if Step mod 4 = 2 then 0.48 else 0.26);
                           when Gallop =>
                              Hat_Here := Step mod 2 = 0 or else Step mod 4 = 3;
                              Level := (if Step mod 4 = 3 then 0.3 else 0.45);
                        end case;
                        if Step = 14 and then not Open_Here and then Draw (C) < S.Open_Chance then
                           Open_Here := True;
                        end if;
                     end if;
                     if Intro_Sparse and then Step mod 2 = 1 then
                        Hat_Here := False;
                     end if;
                     if Hat_Here then
                        Play ((if Open_Here then Open_Hat else Hat), 80.0, Level * Soft,
                              (if Open_Here then 2 else 1), Pan => 0.3, Late => Swing,
                              Tone => S.Hat_Tone, Humanized => True);
                     end if;
                  end;
               end if;

               --  Crashes and impacts at the section heads.
               if Step = 0 and then C.Section_Bar = 0
                 and then Section in Groove | Drop | Peak | Intro
                 and then not (Section = Intro and then C.Stage <= C.Start_Stage
                               and then C.Plan = 0 and then C.Stage > 0 and then C.Bar = 0)
               then
                  Play (Crash, 70.0, (if Section in Drop | Peak then 0.8 else 0.5), 8, Pan => -0.25,
                        Tone => S.Hat_Tone);
               end if;
               if Step = 0 and then C.Section_Bar = 0
                 and then ((Section = Drop) or else (Section = Peak and then C.Peak_Phrases = 0))
               then
                  Impact_Hit := True;
                  Play (Impact, 28.0, 1.0, 16);
               end if;
            end;

            if Kick_Hit then
               Play (Kick, S.Kick_Body, Kick_Velocity, 2, Tone => S.Kick_Punch, Decay => S.Kick_Length);
            end if;
            if Snare_Hit then
               Play (Snare, S.Snare_Body, Snare_Velocity, 2, Pan => 0.05,
                     Late => (if Step mod 2 = 1 then Swing else 0), Tone => S.Snare_Tone,
                     Decay => S.Snare_Length, Humanized => True);
               if (S.Clap_Layer or else Energy = Energy_Range'Last) and then Snare_Velocity >= 0.6 then
                  Play (Clap, 60.0, 0.55, 2, Pan => -0.15, Late => 0, Humanized => True);
               end if;
            end if;
         elsif Section = Metronome and then Step mod Steps_Per_Beat = 0 then
            Play (Click, (if Step = 0 then 96.0 else 89.0), 0.9, 1);
            Kick_Hit := True;   --  the beat the visuals pulse on
         end if;

         --  Risers into the drops and the next stage.
         if Step = 0 and then C.Section_Bar = 0 and then Section in Build | Outro then
            Play (Rise, 48.0, (if Section = Build then 0.55 else 0.35),
                  Natural'Max (1, C.Section_Bars) * Steps_Per_Bar);
         end if;

         ---------------------------------------------------------------------
         --  Bass
         ---------------------------------------------------------------------
         if Section not in Metronome and then not (Section = Intro and then C.Section_Bar = 0) then
            declare
               Hit : Boolean;
               Pitch : Float := Ch.Bass;
               Steps : Positive := 2;
               Glide : Glide_Semitones := 0.0;
               Draw_Value : Float;
               Next_Root : Float;
            begin
               case Feel is
                  when Half_Time =>
                     Hit := Step = 0 or else (Step = 10 and then S.Bass_Mask (10));
                  when Double_Time =>
                     Hit := Step mod 2 = 0;
                  when Straight =>
                     Hit := (if Section = Build then Step mod 2 = 0 else S.Bass_Mask (Step));
               end case;
               if Energy <= 1 then
                  Hit := Step = 0;
               end if;
               if Hit then
                  Draw_Value := Draw (C);
                  case S.Bass is
                     when Pulse =>
                        if Step mod 8 = 6 and then Draw_Value < 0.3 then
                           Pitch := Pitch + Float (Octave);
                        end if;
                        Steps := 1;
                     when Funk =>
                        if Step /= 0 then
                           Pitch := Pitch + (if Draw_Value < 0.35 then Float (Octave)
                                             elsif Draw_Value < 0.55 then Float (Interval (S.Mode, Ch.Degree, Fifth_Steps))
                                             elsif Draw_Value < 0.65 then Float (Interval (S.Mode, Ch.Degree, 6))
                                             else 0.0);
                        end if;
                        Steps := (if Step mod 4 = 0 then 2 else 1);
                     when Walking =>
                        Next_Root := Ch.Bass;
                        Pitch := Pitch + (case Step is
                                            when 4 => Float (Interval (S.Mode, Ch.Degree, 2)),
                                            when 8 => Float (Interval (S.Mode, Ch.Degree, Fifth_Steps)),
                                            when 12 => Float (Interval (S.Mode, Ch.Degree, 6)) - Float (Octave),
                                            when 14 => (if Draw_Value < 0.5 then -1.0 else 2.0),
                                            when others => 0.0);
                        Steps := (if Step = 14 then 2 else 4);
                        Pitch := Clamp (Pitch, Next_Root - Float (Octave), Next_Root + Float (Octave));
                     when Sustained =>
                        Steps := (if Step = 0 and then not S.Bass_Mask (8) and then not S.Bass_Mask (10)
                                  then Steps_Per_Bar else 8);
                  end case;
                  if Feel = Half_Time then
                     Steps := (if Step = 0 then 10 else 6);
                  end if;
                  if Draw (C) < S.Slide_Chance and then abs (C.Last_Bass - Pitch) in 1.0 .. 12.0 then
                     Glide := Clamp (C.Last_Bass - Pitch, Glide_Semitones'First, Glide_Semitones'Last);
                  end if;
                  Play (Bass, Pitch, 0.85, Steps, Late => (if Step mod 2 = 1 then Swing else 0),
                        Tone => (if Lifted then Saturate (S.Bass_Tone + 0.2) else S.Bass_Tone),
                        Glide => Glide, Humanized => True);
                  C.Last_Bass := Pitch;
               end if;
            end;
         end if;

         ---------------------------------------------------------------------
         --  Electric piano comping
         ---------------------------------------------------------------------
         if Section /= Build and then Section /= Metronome then
            if Feel = Half_Time or else Energy <= 1 then
               if Step = 0 or else (Step = 10 and then S.Comp_Mask (10)) then
                  for I in 1 .. Ch.Count loop
                     Play (Piano, Ch.Tones (I), 0.38, (if Step = 0 then 8 else 4), Pan => 0.15,
                           Late => (I - 1) * Length / 10, Tone => S.Piano_Tone, Humanized => True);
                  end loop;
               end if;
            elsif S.Comp_Mask (Step) and then not (Section = Intro and then C.Section_Bar = 0) then
               for I in 1 .. Ch.Count loop
                  Play (Piano, Ch.Tones (I), (if Step = 0 then 0.48 else 0.38),
                        (if Step = 0 then 4 else 2), Pan => -0.1, Late => Swing,
                        Tone => S.Piano_Tone, Humanized => True);
               end loop;
            end if;
         end if;

         ---------------------------------------------------------------------
         --  Arpeggiator
         ---------------------------------------------------------------------
         declare
            Arp_Here : constant Boolean :=
              Section in Build | Drop | Peak | Outro
              or else (Section = Groove
                       and then (M.Arp_On or else S.Density >= 0.6 or else C.Section_Bar >= 4));
            Every : constant Positive :=
              (if Feel = Double_Time or else (Section = Build and then C.Section_Bar >= 2) then 1
               else S.Arp_Every);
            Span : constant Positive := Natural'Max (1, Ch.Count) * S.Arp_Span;
            Index : Natural;
         begin
            if Arp_Here and then Ch.Count > 0 and then Step mod Every = 0 then
               Index :=
                 (case S.Arp is
                    when Pinball =>
                      (if C.Arp_Position mod 2 = 0 then 0 else 1 + (C.Arp_Position / 2) mod Natural'Max (1, Span - 1)),
                    when others => C.Arp_Position mod Span);
               Play (Arp, Chord_Pitch (Ch, Index), (if Step mod 4 = 0 then 0.44 else 0.36), 1,
                     Pan => (if (Step / Every) mod 2 = 0 then -0.45 else 0.45), Late => Swing,
                     Tone => S.Arp_Tone, Decay => 0.8, Humanized => True);
               case S.Arp is
                  when Rising =>
                     C.Arp_Position := (C.Arp_Position + 1) mod Span;
                  when Falling =>
                     C.Arp_Position := (C.Arp_Position + Span - 1) mod Span;
                  when Pinball =>
                     C.Arp_Position := (C.Arp_Position + 1) mod (2 * Span);
                  when Bounce =>
                     if C.Arp_Up then
                        if C.Arp_Position + 1 >= Span then
                           C.Arp_Up := False;
                        else
                           C.Arp_Position := C.Arp_Position + 1;
                        end if;
                     elsif C.Arp_Position = 0 then
                        C.Arp_Up := True;
                     else
                        C.Arp_Position := C.Arp_Position - 1;
                     end if;
               end case;
            end if;
         end;

         ---------------------------------------------------------------------
         --  Melody: a call on the lead, the answer on the answer voice
         ---------------------------------------------------------------------
         declare
            Calling : constant Boolean :=
              Section in Drop | Peak | Breakdown
              or else (Section = Groove
                       and then (M.Lead_On or else S.Density >= 0.75 or else C.Section_Bar >= 4))
              or else (Open_Ended (Section) and then Section /= Metronome
                       and then C.Section_Bar mod Motif_Bars < Phrase_Bars);
            Answering : constant Boolean :=
              Calling or else Section = Outro or else (Section = Groove and then S.Density >= 0.45);
            In_Answer : constant Boolean := Phrase_Bar >= 2;
            Index : constant Phrase_Index := (Phrase_Bar mod 2) * Steps_Per_Bar + Step;
            Position : Integer := C.Call (Index);
            First : Integer := 0;
            Next_Gap : Positive := 1;
         begin
            if Position /= Rest and then ((In_Answer and then Answering) or else (not In_Answer and then Calling)) then
               for I in Phrase_Index loop
                  if C.Call (I) /= Rest then
                     First := C.Call (I);
                     exit;
                  end if;
               end loop;
               if In_Answer then
                  Position :=
                    (case C.Answer is
                       when Inverted => 2 * First - Position,
                       when Sequenced => Position - 2,
                       when Echoed => Position + Scale_Length);
                  --  The answer's last note comes home to the key.
                  declare
                     Later : Boolean := False;
                  begin
                     for I in Index + 1 .. Phrase_Index'Last loop
                        if C.Call (I) /= Rest then
                           Later := True;
                        end if;
                     end loop;
                     if not Later and then Phrase_Bar = Phrase_Bars - 1 then
                        Position := Scale_Length * ((Position + Position_Offset + Scale_Length / 2)
                                                    / Scale_Length) - Position_Offset;
                     end if;
                  end;
               end if;
               --  On the beat, a chord tone.
               if Step mod Steps_Per_Beat = 0
                 and then (Position + Position_Offset - Ch.Degree) mod 2 = 1
               then
                  Position := Position + 1;
               end if;
               Position := Integer'Max (Lowest_Position, Integer'Min (Highest_Position, Position));
               for I in Index + 1 .. Phrase_Index'Last loop
                  exit when C.Call (I) /= Rest or else Next_Gap >= 4;
                  Next_Gap := Next_Gap + 1;
               end loop;
               if not In_Answer then
                  Play (Lead, Melody_Pitch (Tonic, S.Mode, Position),
                        (if Open_Ended (Section) then 0.3 else 0.45), Next_Gap, Pan => 0.2,
                        Late => Swing, Tone => S.Lead_Tone, Humanized => True);
               else
                  case S.Answer is
                     when Bell_Answer =>
                        Play (Bell, Melody_Pitch (Tonic, S.Mode, Position), 0.36, Next_Gap + 2,
                              Pan => -0.3, Late => Swing, Tone => S.Lead_Tone, Humanized => True);
                     when Piano_Answer =>
                        Play (Piano, Melody_Pitch (Tonic, S.Mode, Position), 0.42, Next_Gap + 1,
                              Pan => -0.25, Late => Swing, Tone => S.Piano_Tone, Humanized => True);
                     when Arp_Answer =>
                        Play (Arp, Melody_Pitch (Tonic, S.Mode, Position), 0.4, Next_Gap,
                              Pan => -0.3, Late => Swing, Tone => S.Arp_Tone, Decay => 1.6, Humanized => True);
                  end case;
               end if;
            end if;
         end;

         ---------------------------------------------------------------------
         --  The step's record and mix
         ---------------------------------------------------------------------
         declare
            Progress : constant Float :=
              (if C.Section_Bars = 0 then 0.0
               else Float (C.Section_Bar * Steps_Per_Bar + Step) / Float (C.Section_Bars * Steps_Per_Bar));
            Opening : constant Float :=
              Lerp (Cutoff_From (Section), Cutoff_To (Section), Progress)
              * (if Lifted then 0.5 + 0.5 * S.Ceiling else S.Ceiling);
            Shown : Look;
         begin
            if Section = Outro and then C.Section_Bars > 0 then
               Shown := Blend (S.Colors, C.Next_Style.Colors,
                               Float (C.Section_Bar * Steps_Per_Bar + Step + 1)
                               / Float (C.Section_Bars * Steps_Per_Bar));
            elsif C.Fade_Done < Look_Fade_Steps then
               C.Fade_Done := C.Fade_Done + 1;
               Shown := Blend (C.Look_From, S.Colors, Float (C.Fade_Done) / Float (Look_Fade_Steps));
            else
               Shown := S.Colors;
            end if;
            Mix := (Pad_Level => Pad_Levels (Section), Cutoff => Saturate (Opening),
                    Delay_Samples => S.Echo * Length, Echo_Feedback => S.Echo_Amount,
                    Wobble => Saturate (S.Wobble + (if Hazy (Section) then Haze_Wobble else 0.0)),
                    Reverb_Wet => Saturate (S.Reverb_Wet + (if Hazy (Section) then Haze_Wet else 0.0)),
                    Reverb_Room => S.Reverb_Room, Room_Level => S.Snare_Room);
            C.Serial := C.Serial + 1;
            Info := (Serial => C.Serial, Start => Start_Time, Length => Length, Bar => C.Bar,
                     Step => Step, Section => Section, Section_Bar => C.Section_Bar,
                     Section_Bars => C.Section_Bars, Feel => Feel, Energy => Energy,
                     Stage => (if Section in Title_Theme | Metronome then 0 else C.Stage),
                     Run => C.Run, Chord => Ch, Chord_Change => Change, Kick => Kick_Hit,
                     Snare => Snare_Hit, Impact => Impact_Hit, Pad_Level => Pad_Levels (Section),
                     Cutoff => Mix.Cutoff, Style_Seed => S.Seed, Region => S.Region, Name => S.Name,
                     Name_Last => S.Name_Last, Key => Tonic, Mode => S.Mode, Look => Shown);
            C.Next := C.Next + Samples (Length);
         end;
      end;
      C.Current := Info;
      C.History (History_Index (C.Serial mod History_Length)) := Info;
      if C.Composed < History_Length then
         C.Composed := C.Composed + 1;
      end if;
      if Step = Step_Index'Last then
         C.Step := 0;
         C.Bar := C.Bar + 1;
      else
         C.Step := Step + 1;
      end if;
   end Compose_Next;

   ---------------------------------------------------------------------------
   --  The history
   ---------------------------------------------------------------------------

   procedure By_Serial (C : Composer; Serial : Natural; Info : out Step_Record; Found : out Boolean) is
   begin
      Found := Serial >= 1 and then Serial <= C.Serial
        and then C.Serial - Serial < C.Composed;
      Info := (if Found then C.History (History_Index (Serial mod History_Length)) else C.Current);
   end By_Serial;

   procedure Find (C : Composer; Time : Samples; Info : out Step_Record; Found : out Boolean) is
      Candidate : Step_Record;
   begin
      Found := False;
      Info := C.Current;
      for Back in 0 .. C.Composed - 1 loop
         Candidate := C.History (History_Index ((C.Serial - Back) mod History_Length));
         if Candidate.Start <= Time then
            Info := Candidate;
            Found := Time < Candidate.Start + Samples (Candidate.Length);
            return;
         end if;
      end loop;
   end Find;

   procedure Last_Kick (C : Composer; Time : Samples; At_Time : out Samples; Found : out Boolean) is
   begin
      At_Time := 0;
      Found := False;
      for Back in 0 .. C.Composed - 1 loop
         declare
            Candidate : Step_Record renames
              C.History (History_Index ((C.Serial - Back) mod History_Length));
         begin
            if Candidate.Start <= Time and then Candidate.Kick then
               At_Time := Candidate.Start;
               Found := True;
               return;
            end if;
         end;
      end loop;
   end Last_Kick;

   procedure Last_Chord_Change (C : Composer; Time : Samples; At_Time : out Samples;
                                Found : out Boolean) is
   begin
      At_Time := 0;
      Found := False;
      for Back in 0 .. C.Composed - 1 loop
         declare
            Candidate : Step_Record renames
              C.History (History_Index ((C.Serial - Back) mod History_Length));
         begin
            if Candidate.Start <= Time and then Candidate.Chord_Change then
               At_Time := Candidate.Start;
               Found := True;
               return;
            end if;
         end;
      end loop;
   end Last_Chord_Change;

   function Next_Boundary (C : Composer; Time : Samples) return Samples is
      Info : Step_Record;
      Found : Boolean;
   begin
      Find (C, Time, Info, Found);
      if not Found then
         return (if Time < C.Next then C.Next else Time);
      elsif Info.Start = Time then
         return Time;
      else
         return Info.Start + Samples (Info.Length);
      end if;
   end Next_Boundary;

   procedure Beat_Error (C : Composer; Time : Samples; Error : out Sample_Offset;
                         Found : out Boolean) is
      Info : Step_Record;
      Beat_Start : Sample_Offset;
      Beat_Length : Sample_Offset;
   begin
      Find (C, Time, Info, Found);
      Error := 0;
      if not Found then
         return;
      end if;
      Beat_Length := Sample_Offset (Info.Length) * Steps_Per_Beat;
      Beat_Start := Sample_Offset (Info.Start)
        - Sample_Offset (Info.Step mod Steps_Per_Beat) * Sample_Offset (Info.Length);
      Error := Sample_Offset (Time) - Beat_Start;
      if Error > Beat_Length / 2 then
         Error := Error - Beat_Length;
      end if;
   end Beat_Error;

end Neon_Composer;
