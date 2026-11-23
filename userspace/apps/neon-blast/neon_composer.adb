------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Math;

package body Neon_Composer with SPARK_Mode is
   use Neon_Notes;

   ---------------------------------------------------------------------------
   --  Harmony
   ---------------------------------------------------------------------------

   --  Semitones above the root, in voicing order; Count of them are used.
   type Interval_Array is array (Tone_Index) of Natural;
   type Quality_Shape is record
      Count : Tone_Count;
      Steps : Interval_Array;
   end record;
   type Shape_Table is array (Chord_Quality) of Quality_Shape;
   Shapes : constant Shape_Table :=
     [Major_7 => (4, [0, 4, 7, 11, 0]),
      Major_9 => (5, [0, 4, 7, 11, 14]),
      Minor_7 => (4, [0, 3, 7, 10, 0]),
      Minor_9 => (5, [0, 3, 7, 10, 14]),
      Dominant_9 => (5, [0, 4, 7, 10, 14]),
      Dominant_13 => (5, [0, 4, 10, 14, 21]),
      Suspended_9 => (5, [0, 5, 7, 10, 14]),
      Minor_6 => (4, [0, 3, 7, 9, 0]),
      Major_7_Sharp_11 => (5, [0, 4, 7, 11, 18]),
      Add_9 => (4, [0, 4, 7, 14, 0])];

   --  Four chords a progression: (semitones above the key, quality).
   type Chord_Step is record
      Degree : Natural;
      Quality : Chord_Quality;
   end record;
   type Progression is array (0 .. 3) of Chord_Step;
   type Progression_Table is array (Progression_Index) of Progression;
   --  Major-scale degrees: I = 0, ii = 2, iii = 4, IV = 5, V = 7, vi = 9.
   Progressions : constant Progression_Table :=
     [[(5, Major_7_Sharp_11), (4, Minor_7), (2, Minor_9), (0, Major_9)],   --  IV iii ii I
      [(0, Major_9), (9, Minor_9), (2, Minor_9), (7, Suspended_9)],        --  I vi ii V
      [(9, Minor_9), (5, Major_7), (0, Major_9), (7, Add_9)],              --  vi IV I V
      [(2, Minor_9), (7, Dominant_13), (0, Major_9), (5, Minor_6)]];       --  ii V I iv

   --  The keys a seed may choose (MIDI roots near the pad's register).
   type Key_Table is array (0 .. 5) of Natural;
   Keys : constant Key_Table := [53, 50, 57, 51, 48, 56];   --  F D A Eb C Ab

   --  The pad's register: chord tones fold into Pad_Low .. Pad_Low + 11,
   --  extensions an octave up.
   Pad_Low : constant := 55;
   Bass_Low : constant := 33;
   Octave : constant := 12;
   Pentatonic : constant array (0 .. 4) of Natural := [0, 2, 4, 7, 9];
   Rest : constant := -100;

   function Chord_Pitch (C : Chord_Value; Degree : Natural) return Float is
      Count : constant Positive := (if C.Count = 0 then 1 else C.Count);
      Index : constant Tone_Index := Tone_Index (Degree mod Count + 1);
      Lift : constant Natural := Natural'Min (Degree / Count, 3);
   begin
      return C.Tones (Index) + Float (Octave * (1 + Lift));
   end Chord_Pitch;

   procedure Voice (Root : Natural; Quality : Chord_Quality; Into : out Chord_Value) is
      Shape : constant Quality_Shape := Shapes (Quality);
      Pitch : Integer;
      Swap : Float;
   begin
      Into := (Root => Root, Quality => Quality, Count => Shape.Count,
               Tones => [others => Float (Pad_Low)],
               Bass => Float (Bass_Low + (Root - Bass_Low) mod Octave));
      for I in 1 .. Shape.Count loop
         Pitch := Pad_Low + (Root + Shape.Steps (I) - Pad_Low) mod Octave;
         if Shape.Steps (I) >= Octave then
            Pitch := Pitch + Octave;
         end if;
         Into.Tones (I) := Float (Pitch);
      end loop;
      --  Ascending order (insertion sort of at most five).
      for I in 2 .. Shape.Count loop
         for J in reverse 2 .. I loop
            if Into.Tones (J) < Into.Tones (J - 1) then
               Swap := Into.Tones (J);
               Into.Tones (J) := Into.Tones (J - 1);
               Into.Tones (J - 1) := Swap;
            end if;
         end loop;
      end loop;
   end Voice;

   ---------------------------------------------------------------------------
   --  Mood tables
   ---------------------------------------------------------------------------

   type Section_Float is array (Section_Kind) of Float;
   Pad_Levels : constant Section_Float :=
     [Title_Theme => 0.75, Groove => 0.55, Build => 0.5, Peak => 0.5, Breakdown => 0.9,
      Finale => 0.85, Metronome => 0.45];
   Cutoffs : constant Section_Float :=
     [Title_Theme => 0.5, Groove => 0.85, Build => 0.8, Peak => 1.0, Breakdown => 0.16,
      Finale => 0.28, Metronome => 0.55];

   function Slow_Harmony (S : Section_Kind) return Boolean is
     (S in Title_Theme | Breakdown | Finale | Metronome);

   ---------------------------------------------------------------------------

   procedure Start (C : out Composer; Seed : Unsigned_64) is
      Pick : Natural;
   begin
      C := (Random => Neon_Random.Seeded (Seed, 16#C0DE#), others => <>);
      Neon_Random.Pick (C.Random, Keys'Length, Pick);
      C.Key_Root := Keys (Pick);
      Neon_Random.Pick (C.Random, Progressions'Length, Pick);
      C.Progression := Pick;
   end Start;

   function Next_Start (C : Composer) return Samples is (C.Next);

   function Newest_Serial (C : Composer) return Natural is (C.Serial);

   procedure Add (Into : in out Note_Batch; Item : Neon_Notes.Note) is
   begin
      if Into.Count < Maximum_Batch then
         Into.Count := Into.Count + 1;
         Into.Items (Into.Count) := Item;
      end if;
   end Add;

   procedure New_Phrase (C : in out Composer) is
      Position : Integer := 0;
      Draw : Float;
      Pick : Natural;
   begin
      for I in C.Phrase'Range loop
         Neon_Random.Uniform (C.Random, Draw);
         if I mod 8 = 7 or else Draw < 0.4 then
            C.Phrase (I) := Rest;
         else
            Neon_Random.Pick (C.Random, 5, Pick);
            Position := Integer (Neon_Math.Clamp (Float (Position + Pick - 2), -3.0, 6.0));
            C.Phrase (I) := Position;
         end if;
      end loop;
   end New_Phrase;

   function Scale_Pitch (C : Composer; Position : Integer) return Float is
      Shifted : constant Natural := Natural (Position + 10);   --  Position >= -10
      Degree : constant Natural := Shifted mod 5;
      Octaves : constant Natural := Shifted / 5;
   begin
      return Float (C.Key_Root + Pentatonic (Degree) + Octave * Octaves - 2 * Octave + Octave);
   end Scale_Pitch;

   procedure Compose_Next (C : in out Composer; Wanted : Mood; Into : in out Note_Batch;
                           Info : out Step_Record) is
      Step : constant Step_Index := C.Step;
      M : Mood;
      Length : Positive;
      Chord_Number : Natural;
      Change : Boolean := False;
      Swing : Natural;
      Jitter : Float;
      Start_Time : Samples;
      Kick_Hit, Snare_Hit : Boolean := False;
      Phrase_Bar : constant Natural := C.Bar mod 4;

      procedure Play (Voice_Of : Instrument; Pitch, Velocity : Float; Steps : Positive;
                      Pan : Float := 0.0; Late : Natural := 0) is
      begin
         Add (Into, (At_Time => Start_Time + Samples (Late), Voice => Voice_Of, Pitch => Pitch,
                     Velocity => Velocity * Jitter, Length => Steps * Length, Pan => Pan));
      end Play;
   begin
      if Step = 0 then
         C.Bar_Mood := Wanted;
         if C.Bar mod 8 = 0 and then C.Bar > 0 then
            declare
               Draw : Natural;
            begin
               Neon_Random.Pick (C.Random, Progressions'Length, Draw);
               C.Progression := Draw;
            end;
         end if;
         if Phrase_Bar = 0 then
            New_Phrase (C);
         end if;
      end if;
      M := C.Bar_Mood;
      Length := Step_Length (M.Speed);
      Start_Time := C.Next;
      Swing := (if Step mod 2 = 1 then Length / 8 else 0);
      Neon_Random.Between (C.Random, 0.9, 1.05, Jitter);

      --  The bar's chord: one a bar, one every two in the slow sections.
      Chord_Number := (if Slow_Harmony (M.Section) then (C.Bar / 2) mod 4 else C.Bar mod 4);
      if Step = 0 then
         declare
            Next_Chord : Chord_Value;
            Item : constant Chord_Step := Progressions (C.Progression) (Chord_Number);
         begin
            Voice (C.Key_Root + Item.Degree, Item.Quality, Next_Chord);
            Change := C.Serial = 0 or else Next_Chord /= C.Current.Chord;
            C.Current.Chord := Next_Chord;
         end;
      end if;

      declare
         Ch : constant Chord_Value := C.Current.Chord;
         Level : constant Level_Range := M.Level;
         Drive_Drums : constant Boolean := M.Section = Peak or else Level >= 3;
         Fill_Bar : constant Boolean := Phrase_Bar = 3 and then Level >= 2;
      begin
         --  Pad: the chord, held across the chord's span.
         if Change then
            for I in 1 .. Ch.Count loop
               Play (Pad, Ch.Tones (I), Pad_Levels (M.Section),
                     (if Slow_Harmony (M.Section) then 2 * Steps_Per_Bar else Steps_Per_Bar),
                     Pan => (Float (I) - 3.0) * 0.25);
            end loop;
         end if;

         --  Electric piano stabs.
         case M.Section is
            when Title_Theme | Finale | Metronome | Breakdown =>
               if Step = 0 or else (Step = 10 and then C.Bar mod 2 = 0) then
                  for I in 1 .. Ch.Count loop
                     Play (Piano, Ch.Tones (I), 0.42, (if Step = 0 then 8 else 4),
                           Pan => 0.15, Late => (I - 1) * Length / 10);
                  end loop;
               end if;
            when Groove | Build | Peak =>
               if Step in 0 | 6 | 10 or else (Level >= 3 and then Step = 3) then
                  for I in 1 .. Ch.Count loop
                     Play (Piano, Ch.Tones (I), (if Step = 0 then 0.5 else 0.36),
                           (if Step = 0 then 4 else 2), Pan => -0.1);
                  end loop;
               end if;
         end case;

         --  Bass.
         if M.Section in Groove | Build | Peak and then Level >= 1 then
            if Level >= 3 or else M.Section = Peak then
               if Step mod 2 = 0 then
                  Play (Bass, Ch.Bass + (if Step mod 4 = 2 then Float (Octave) else 0.0),
                        0.8, 2);
               end if;
            elsif Level = 2 then
               if Step in 0 | 3 | 6 | 8 | 10 | 14 then
                  Play (Bass, Ch.Bass + (if Step = 14 then Float (Octave) else 0.0), 0.85,
                        (if Step in 0 | 8 then 3 else 2));
               end if;
            elsif Step in 0 | 6 | 10 then
               Play (Bass, Ch.Bass, 0.85, (if Step = 0 then 6 else 3));
            end if;
         elsif M.Section = Breakdown and then Step = 0 then
            Play (Bass, Ch.Bass, 0.6, Steps_Per_Bar);
         end if;

         --  Drums.
         case M.Section is
            when Title_Theme =>
               Kick_Hit := Step in 0 | 10;
               Snare_Hit := Step = 8;
               if Step mod 4 = 2 then
                  Play (Hat, 80.0, 0.32, 1, Pan => 0.3, Late => Swing);
               end if;
            when Groove | Peak | Build =>
               if M.Section = Build then
                  Kick_Hit := Step mod 4 = 0;
                  Snare_Hit := (C.Bar mod 2 = 1 and then Step >= 8)
                    or else Step in 4 | 12;
               elsif Drive_Drums then
                  Kick_Hit := Step mod 4 = 0;
                  Snare_Hit := Step in 4 | 12 or else (Fill_Bar and then Step in 13 | 14 | 15);
               elsif Level >= 2 then
                  Kick_Hit := Step in 0 | 6 | 10;
                  Snare_Hit := Step in 4 | 12 or else (Fill_Bar and then Step in 14 | 15);
               else
                  Kick_Hit := Step in 0 | 10;
                  Snare_Hit := Step = 8;
               end if;
               if Drive_Drums and then Step mod 4 = 2 then
                  Play (Open_Hat, 80.0, 0.45, 2, Pan => 0.25);
               elsif Step mod 2 = 0 then
                  Play (Hat, 80.0, (if Step mod 4 = 0 then 0.42 else 0.55), 1, Pan => 0.3);
               elsif Level >= 2 then
                  Play (Hat, 80.0, 0.22, 1, Pan => 0.35, Late => Swing);
               end if;
               if Snare_Hit and then (M.Section = Peak or else Level >= 4) then
                  Play (Clap, 60.0, 0.6, 2, Pan => -0.15);
               end if;
               if M.Section = Build and then Step = 0 and then C.Bar mod 2 = 0 then
                  Play (Rise, 48.0, 0.5, 2 * Steps_Per_Bar);
               end if;
            when Breakdown =>
               Kick_Hit := Step = 0;
            when Finale | Metronome =>
               null;
         end case;
         if Kick_Hit then
            Play (Kick, 36.0, (if M.Section = Breakdown then 0.55 else 1.0), 2);
         end if;
         if Snare_Hit then
            Play (Snare, 50.0,
                  (if M.Section = Build and then C.Bar mod 2 = 1 and then Step >= 8
                   then 0.25 + 0.05 * Float (Step - 8) else 0.85), 2, Pan => 0.05);
         end if;
         if M.Section = Metronome and then Step mod Steps_Per_Beat = 0 then
            Play (Click, (if Step = 0 then 96.0 else 89.0), 0.9, 1);
            Kick_Hit := True;   --  the beat the visuals pulse on
         end if;

         --  Melody: two bars of the phrase, every four bars.
         if (M.Lead_On or else Level >= 4 or else M.Section = Title_Theme)
           and then Phrase_Bar in 1 | 2 and then Step mod 2 = 0
         then
            declare
               Index : constant Natural := (Phrase_Bar - 1) * 8 + Step / 2;
               Position : constant Integer := C.Phrase (Index);
            begin
               if Position /= Rest then
                  Play (Lead, Scale_Pitch (C, Position),
                        (if M.Section = Title_Theme then 0.38 else 0.5), 2, Pan => 0.2,
                        Late => Swing);
               end if;
            end;
         end if;

         --  Arpeggio layer (unlocked by rhythm combos).
         if M.Arp_On and then M.Section in Groove | Build | Peak and then Ch.Count > 0 then
            declare
               Span : constant Positive := 2 * Ch.Count;
            begin
               Play (Arp, Chord_Pitch (Ch, C.Arp_Position), 0.4, 1,
                     Pan => (if Step mod 2 = 0 then -0.45 else 0.45), Late => Swing);
               if C.Arp_Direction_Up then
                  if C.Arp_Position + 1 >= Span then
                     C.Arp_Direction_Up := False;
                  else
                     C.Arp_Position := C.Arp_Position + 1;
                  end if;
               else
                  if C.Arp_Position = 0 then
                     C.Arp_Direction_Up := True;
                  else
                     C.Arp_Position := C.Arp_Position - 1;
                  end if;
               end if;
            end;
         end if;
      end;

      C.Serial := C.Serial + 1;
      Info := (Serial => C.Serial, Start => Start_Time, Length => Length, Bar => C.Bar,
               Step => Step, Section => M.Section, Level => M.Level, Speed => M.Speed,
               Chord => C.Current.Chord, Chord_Change => Change, Kick => Kick_Hit, Snare => Snare_Hit,
               Pad_Level => Pad_Levels (M.Section), Cutoff => Cutoffs (M.Section));
      C.Current := Info;
      C.History (History_Index (C.Serial mod History_Length)) := Info;
      if C.Composed < History_Length then
         C.Composed := C.Composed + 1;
      end if;
      C.Next := C.Next + Samples (Length);
      if Step = Step_Index'Last then
         C.Step := 0;
         C.Bar := C.Bar + 1;
      else
         C.Step := Step + 1;
      end if;
   end Compose_Next;

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
      Candidate : Step_Record;
   begin
      At_Time := 0;
      Found := False;
      for Back in 0 .. C.Composed - 1 loop
         Candidate := C.History (History_Index ((C.Serial - Back) mod History_Length));
         if Candidate.Start <= Time and then Candidate.Kick then
            At_Time := Candidate.Start;
            Found := True;
            return;
         end if;
      end loop;
   end Last_Kick;

   procedure Last_Chord_Change (C : Composer; Time : Samples; At_Time : out Samples;
                                Found : out Boolean) is
      Candidate : Step_Record;
   begin
      At_Time := 0;
      Found := False;
      for Back in 0 .. C.Composed - 1 loop
         Candidate := C.History (History_Index ((C.Serial - Back) mod History_Length));
         if Candidate.Start <= Time and then Candidate.Chord_Change then
            At_Time := Candidate.Start;
            Found := True;
            return;
         end if;
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
