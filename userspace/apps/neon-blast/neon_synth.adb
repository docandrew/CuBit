------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Math; use Neon_Math;

package body Neon_Synth with SPARK_Mode is
   use Neon_DSP;

   Rate : constant := Neon_DSP.Sample_Rate;

   ---------------------------------------------------------------------------
   --  Each instrument's envelope, level and routing.
   ---------------------------------------------------------------------------
   type Shape is record
      Attack, Decay, Sustain, Release : Float;   --  seconds, seconds, level, seconds
      Gain : Float;
      Dry, Chorus_Send, Reverb_Send, Delay_Send, Room_Send : Float;
      Ducked : Float;                            --  how much the kick ducks it
      Longest : Float;                           --  seconds; a hard stop
   end record;
   type Shape_Table is array (Instrument) of Shape;
   Shapes : constant Shape_Table :=
     [Pad      => (0.9, 1.2, 0.8, 2.2, 0.17, 0.0, 1.0, 0.55, 0.0, 0.0, 0.7, 12.0),
      Piano    => (0.002, 1.5, 0.0, 0.3, 0.3, 1.0, 0.0, 0.35, 0.18, 0.0, 0.55, 5.0),
      Bass     => (0.004, 0.5, 0.7, 0.09, 0.42, 1.0, 0.0, 0.04, 0.0, 0.0, 0.6, 6.0),
      Lead     => (0.012, 0.35, 0.75, 0.25, 0.15, 0.8, 0.0, 0.4, 0.42, 0.0, 0.25, 4.0),
      Arp      => (0.001, 0.22, 0.0, 0.1, 0.17, 0.8, 0.0, 0.3, 0.45, 0.0, 0.3, 1.0),
      Bell     => (0.001, 1.4, 0.0, 0.4, 0.13, 0.8, 0.0, 0.5, 0.35, 0.0, 0.3, 4.0),
      Kick     => (0.0005, 0.36, 0.0, 0.05, 0.95, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.5),
      Snare    => (0.0005, 0.16, 0.0, 0.05, 0.55, 0.75, 0.0, 0.15, 0.0, 1.0, 0.0, 1.5),
      Hat      => (0.0005, 0.035, 0.0, 0.02, 0.2, 1.0, 0.0, 0.05, 0.0, 0.0, 0.0, 0.4),
      Open_Hat => (0.0005, 0.2, 0.0, 0.05, 0.16, 1.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0.8),
      Clap     => (0.0005, 0.14, 0.0, 0.05, 0.45, 0.6, 0.0, 0.2, 0.0, 0.9, 0.0, 1.0),
      Tom      => (0.0005, 0.3, 0.0, 0.08, 0.5, 1.0, 0.0, 0.2, 0.0, 0.3, 0.0, 1.2),
      Crash    => (0.0005, 1.3, 0.0, 0.3, 0.16, 1.0, 0.0, 0.3, 0.0, 0.0, 0.2, 4.0),
      Blip     => (0.001, 0.12, 0.0, 0.05, 0.2, 0.9, 0.0, 0.15, 0.22, 0.0, 0.0, 0.5),
      Sparkle  => (0.001, 0.6, 0.0, 0.1, 0.15, 0.8, 0.0, 0.4, 0.35, 0.0, 0.0, 2.0),
      Boom     => (0.001, 0.55, 0.0, 0.1, 0.5, 1.0, 0.0, 0.3, 0.0, 0.0, 0.0, 2.0),
      Whoosh   => (0.04, 0.2, 0.0, 0.08, 0.35, 1.0, 0.0, 0.25, 0.0, 0.0, 0.0, 1.0),
      Chime    => (0.001, 1.2, 0.0, 0.2, 0.16, 0.8, 0.0, 0.5, 0.3, 0.0, 0.0, 4.0),
      Hurt     => (0.002, 0.45, 0.0, 0.1, 0.35, 1.0, 0.0, 0.3, 0.0, 0.0, 0.0, 1.5),
      Rise     => (0.002, 10.0, 1.0, 0.05, 0.28, 0.8, 0.0, 0.5, 0.0, 0.0, 0.0, 12.0),
      Impact   => (0.001, 1.1, 0.0, 0.3, 0.75, 1.0, 0.0, 0.35, 0.0, 0.0, 0.0, 3.0),
      Zap      => (0.001, 0.18, 0.0, 0.05, 0.16, 0.9, 0.0, 0.2, 0.25, 0.0, 0.0, 0.6),
      Growl    => (0.03, 1.0, 0.6, 0.4, 0.3, 1.0, 0.0, 0.3, 0.0, 0.0, 0.0, 3.0),
      Vinyl    => (0.01, 10.0, 1.0, 0.05, 0.12, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 12.0),
      Click    => (0.0005, 0.02, 0.0, 0.01, 0.4, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2)];

   --  Decay factor per sample for a time constant in seconds.
   function Per_Sample (Seconds : Float) return Float is
     (if Seconds <= 0.0 then 0.0 else Exp (-1.0 / (Clamp (Seconds, 1.0e-4, 1.0e3) * Rate)));

   --  Detune ratios: cents / 1200 as powers of two.
   Detune_Down : constant := 0.994_816;   --  -9 cents
   Detune_Up : constant := 1.004_630;     --  +8 cents
   Detune_Slight : constant := 1.004_050; --  +7 cents

   Stop_Level : constant := 1.0e-4;
   Slide_Seconds : constant := 0.05;      --  a bass slide's time constant
   Slide_Floor : constant := 1.0e-3;      --  a slide this far gone has landed
   Crash_Hertz : constant := 3_170.0;
   Growl_Wobble_Hertz : constant := 6.0;
   Crackle_Threshold : constant := 0.9985;
   Hiss_Level : constant := 0.03;
   Control_Period : constant := 32;       --  samples between filter retunes

   Wet_Scale : constant := 3.0;           --  Freeverb's wet gain
   Room_Return : constant := 2.2;         --  the gated snare room at full
   Echo_Feedback_Ceiling : constant := 0.6;
   --  Freeverb's room-size mapping (Neon_DSP.Configure): feedback 0.7 .. 0.98.
   function Room_Feedback (Room : Float) return Float is (0.7 + 0.28 * Saturate (Room));
   Duck_Depth : constant := 0.55;
   Gate_Seconds : constant := 0.22;
   Master_Pre : constant := 0.5;

   --  Smoothing coefficients, set once at Start.
   Pad_Glide, Cutoff_Glide, Delay_Glide, Duck_Release, Gate_Glide : Float := 0.0;

   ---------------------------------------------------------------------------
   --  Oscillators
   ---------------------------------------------------------------------------

   --  Valimaki's polynomial band-limited step residual.
   function Blep (T, Dt : Float) return Float is
   begin
      if T < Dt then
         declare
            X : constant Float := T / Dt;
         begin
            return X + X - X * X - 1.0;
         end;
      elsif T > 1.0 - Dt then
         declare
            X : constant Float := (T - 1.0) / Dt;
         begin
            return X * X + X + X + 1.0;
         end;
      else
         return 0.0;
      end if;
   end Blep;

   function Saw (Phase, Dt : Float) return Float is (2.0 * Phase - 1.0 - Blep (Phase, Dt));
   function Pulse (Phase, Width, Dt : Float) return Float is
     (Saw (Phase, Dt) - Saw (Fraction (Phase + Width), Dt));

   procedure Advance (Phase : in out Float; Hertz : Float) is
   begin
      Phase := Fraction (Phase + Hertz / Rate);
   end Advance;

   procedure Noise (V : in out Voice; Value : out Float) is
      X : Unsigned_32 := V.Noise;
   begin
      X := X xor Shift_Left (X, 13);
      X := X xor Shift_Right (X, 17);
      X := X xor Shift_Left (X, 5);
      V.Noise := X;
      Value := Float (X) / 2_147_483_648.0 - 1.0;
   end Noise;

   ---------------------------------------------------------------------------

   procedure Start (S : out Synth; Seed : Unsigned_64) is
   begin
      Pad_Glide := Smoothing (0.5);
      Cutoff_Glide := Smoothing (0.9);
      Delay_Glide := Smoothing (0.05);
      Duck_Release := Per_Sample (0.17);
      Gate_Glide := Smoothing (0.002);
      S.Head := 0;
      S.Queued := 0;
      S.Dropped := 0;
      S.Voices := [others => (others => <>)];
      for I in S.Voices'Range loop
         S.Voices (I).Noise := Unsigned_32 (Seed mod 2**31) + Unsigned_32 (I) * 7919 + 1;
      end loop;
      S.Last_Lead := 440.0;
      S.Pad_Level := 0.6;
      S.Pad_Target := 0.6;
      S.Cutoff := 0.5;
      S.Cutoff_Target := 0.5;
      S.Delay_Time := 24_000.0;
      S.Delay_Target := 24_000.0;
      S.Wobble := 0.6;
      S.Echo_Feedback := 0.42;
      S.Echo_Target := 0.42;
      S.Reverb_Wet := 1.0;
      S.Wet_Target := 1.0;
      S.Room_Size := 0.92;
      S.Room_Target := 0.92;
      S.Room_Level := 1.0;
      S.Room_Level_Target := 1.0;
      S.Changes_Queued := 0;
      S.Duck_Level := 0.0;
      S.Gate_Level := 0.0;
      S.Gate_Open := 0;
      Configure (S.Reverb, Room => 0.92, Damping => 0.32);
      Configure (S.Room, Room => 0.55, Damping => 0.15, Comb_Scale => 0.3);
      S.Echo := (others => <>);
      S.Pad_Chorus := (others => <>);
      S.Wow := (others => <>);
      S.Master_Left := (others => <>);
      S.Master_Right := (others => <>);
      S.Master_Tuned := 0;
      S.Limit := (others => <>);
      S.Peak := 0.0;
      S.Rumble_Left := (others => <>);
      S.Rumble_Right := (others => <>);
   end Start;

   function Position (S : Synth) return Samples is (S.Head);
   function Dropped_Notes (S : Synth) return Natural is (S.Dropped);
   function Peak_Level (S : Synth) return Float is (S.Peak);
   function Duck (S : Synth) return Float is (S.Duck_Level);

   function Limiting (S : Synth) return Float is (S.Limit.Reduction_Peak);

   procedure Reset_Peak (S : in out Synth) is
   begin
      S.Peak := 0.0;
      S.Limit.Reduction_Peak := 0.0;
   end Reset_Peak;

   function Voices_Sounding (S : Synth) return Natural is
      Count : Natural := 0;
   begin
      for V of S.Voices loop
         if V.Active then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Voices_Sounding;

   procedure Schedule (S : in out Synth; Item : Note) is
      Place : Positive;
   begin
      if S.Queued = Queue_Ceiling then
         S.Dropped := S.Dropped + 1;
         return;
      end if;
      --  Keep the queue ordered by time; equal times stay in arrival order.
      Place := S.Queued + 1;
      while Place > 1 and then S.Queue (Place - 1).At_Time > Item.At_Time loop
         S.Queue (Place) := S.Queue (Place - 1);
         Place := Place - 1;
      end loop;
      S.Queue (Place) := Item;
      S.Queued := S.Queued + 1;
   end Schedule;

   procedure Apply (S : in out Synth; Change : Target_Change) is
   begin
      S.Pad_Target := Change.Pad_Level;
      S.Cutoff_Target := Change.Cutoff;
      S.Delay_Target := Change.Delay_Time;
      S.Wobble := Change.Wobble;
      S.Echo_Target := Change.Echo_Feedback;
      S.Wet_Target := Change.Reverb_Wet;
      S.Room_Target := Change.Reverb_Room;
      S.Room_Level_Target := Change.Room_Level;
   end Apply;

   procedure Set_Targets (S : in out Synth; At_Time : Samples; Wanted : Targets) is
      Change : constant Target_Change :=
        (At_Time => At_Time, Pad_Level => Saturate (Wanted.Pad_Level),
         Cutoff => Saturate (Wanted.Cutoff),
         Delay_Time => Clamp (Float (Wanted.Delay_Samples), 64.0, Float (Line_Size - 4)),
         Wobble => Saturate (Wanted.Wobble),
         Echo_Feedback => Clamp (Wanted.Echo_Feedback, 0.0, Echo_Feedback_Ceiling),
         Reverb_Wet => Saturate (Wanted.Reverb_Wet), Reverb_Room => Saturate (Wanted.Reverb_Room),
         Room_Level => Saturate (Wanted.Room_Level));
   begin
      if S.Changes_Queued = Change_Ceiling then
         --  Full: the oldest pending change takes effect now.
         Apply (S, S.Changes (1));
         S.Changes (1 .. Change_Ceiling - 1) := S.Changes (2 .. Change_Ceiling);
         S.Changes_Queued := S.Changes_Queued - 1;
      end if;
      S.Changes_Queued := S.Changes_Queued + 1;
      S.Changes (S.Changes_Queued) := Change;
   end Set_Targets;

   ---------------------------------------------------------------------------
   --  Starting a note
   ---------------------------------------------------------------------------

   procedure Begin_Voice (S : in out Synth; Item : Note) is
      Chosen : Positive := 1;
      Quietest : Float := Float'Last;
      Sh : constant Shape := Shapes (Item.Voice);
      Pan : constant Float := Clamp (Item.Pan, -1.0, 1.0);
      Tone : constant Float := Saturate (Item.Tone);
      Loudness : Float;
   begin
      for I in S.Voices'Range loop
         if not S.Voices (I).Active then
            Chosen := I;
            Quietest := -1.0;
            exit;
         end if;
         Loudness := S.Voices (I).Envelope * S.Voices (I).Velocity
           * (if S.Voices (I).Releasing then 0.25 else 1.0);
         if Loudness < Quietest then
            Quietest := Loudness;
            Chosen := I;
         end if;
      end loop;
      declare
         V : Voice renames S.Voices (Chosen);
         Seed : constant Unsigned_32 := V.Noise;
      begin
         V := (Active => True, Kind => Item.Voice,
               Frequency => Note_Hertz (Clamp (Item.Pitch, 0.0, 127.0)),
               Glide_From => S.Last_Lead,
               Velocity => Clamp (Item.Velocity, 0.0, 2.0), Pan => Pan, Age => 0,
               Tone => Tone, Glide => Clamp (Item.Glide, -24.0, 24.0),
               Length => Item.Length, Releasing => False, Envelope => 0.0,
               Noise => (if Seed = 0 then 1 else Seed),
               Attack_Step => 1.0 / Max (Sh.Attack * Rate, 1.0),
               Decay_Factor => Per_Sample (Sh.Decay * Item.Decay), Sustain => Sh.Sustain,
               Release_Factor => Per_Sample (Sh.Release * Item.Decay),
               Gain_Left => Sqrt (0.5 * (1.0 - Pan)), Gain_Right => Sqrt (0.5 * (1.0 + Pan)),
               others => <>);
         case Item.Voice is
            when Pad =>
               V.Phase_2 := 0.31;
               V.Phase_3 := 0.67;
               Tune (V.Filter, 1700.0, 0.1);
            when Piano =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.45);
               V.Aux_2 := 1.0;
               V.Aux_Decay_2 := Per_Sample (0.06);
            when Bass =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.18);
               V.Aux_2 := 1.0;
               V.Aux_Decay_2 := Per_Sample (Slide_Seconds);
               Tune (V.Filter, 900.0, 0.35);
            when Lead =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.05);
               S.Last_Lead := V.Frequency;
               Tune (V.Filter, 1200.0 + 3000.0 * Tone, 0.2);
            when Arp =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.09);
               Tune (V.Filter, 4000.0, 0.45);
            when Bell =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.6);
               V.Aux_2 := 1.0;
               V.Aux_Decay_2 := Per_Sample (0.08);
            when Kick =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.012);
               V.Aux_2 := 1.0;
               V.Aux_Decay_2 := Per_Sample (0.05);
               S.Duck_Level := Max (S.Duck_Level, Clamp (Item.Velocity, 0.0, 1.0));
            when Snare | Clap =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.045 * Item.Decay);
               V.Aux_2 := 1.0;
               V.Aux_Decay_2 := Per_Sample (0.004);
               if Item.Voice = Snare then
                  Tune (V.Filter, 1200.0 + 4000.0 * Tone, 0.25);
               else
                  Tune (V.Filter, 1400.0, 0.5);
               end if;
               S.Gate_Open := Natural (Gate_Seconds * Rate);
            when Hat | Open_Hat =>
               Tune (V.Filter, 5000.0 + 5000.0 * Tone, 0.1);
            when Tom =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.05);
            when Crash =>
               Tune (V.Filter, 5000.0 + 3000.0 * Tone, 0.15);
            when Blip =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.012);
               Tune (V.Filter, 4800.0, 0.15);
            when Sparkle =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.25);
            when Boom =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.12);
               V.Aux_2 := 1.0;
               V.Aux_Decay_2 := Per_Sample (0.06);
               Tune (V.Filter, 3000.0, 0.3);
            when Whoosh =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.12);
               Tune (V.Filter, 300.0, 0.6);
            when Chime =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.5);
            when Hurt =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.15);
               Tune (V.Filter, 2000.0, 0.3);
            when Rise =>
               Tune (V.Filter, 200.0, 0.55);
            when Impact =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.09);
               V.Aux_2 := 1.0;
               V.Aux_Decay_2 := Per_Sample (0.03);
               Tune (V.Filter, 400.0, 0.3);
               S.Duck_Level := 1.0;
            when Zap =>
               V.Aux_1 := 1.0;
               V.Aux_Decay_1 := Per_Sample (0.1);
               V.Aux_2 := 1.0;
               V.Aux_Decay_2 := Per_Sample (0.06);
               Tune (V.Filter, 5000.0, 0.4);
            when Growl =>
               Tune (V.Filter, 900.0, 0.45);
            when Vinyl =>
               Tune (V.Filter, 2500.0, 0.3);
            when Click =>
               null;
         end case;
      end;
   end Begin_Voice;

   ---------------------------------------------------------------------------
   --  One sample of one voice (mono, before the envelope's gain is applied).
   ---------------------------------------------------------------------------

   --  The pitch ratio of a voice still sliding (Glide semitones, fading by Aux_2).
   function Slide (V : Voice) return Float is
     (if V.Glide = 0.0 or else V.Aux_2 < Slide_Floor then 1.0
      else Exp2 (V.Glide * V.Aux_2 / 12.0));

   function Square (Phase : Float) return Float is (if Phase < 0.5 then 1.0 else -1.0);

   procedure Sound (S : Synth; V : in out Voice; Retune : Boolean; Output : out Float) is
      F : constant Float := V.Frequency;
      Dt : constant Float := Min (F / Rate, 0.5);
      Low, Band, High, N, X : Float;
      T : constant Float := Float (V.Age) / Rate;
   begin
      case V.Kind is
         when Pad =>
            if Retune then
               Tune (V.Filter, 500.0 + 1800.0 * V.Tone + 1500.0 * S.Pad_Level
                       + 500.0 * Sin_Turns (T * 0.15 + V.Pan), 0.12);
            end if;
            X := Saw (V.Phase_1, Dt) + Saw (V.Phase_2, Dt * Detune_Down)
              + Saw (V.Phase_3, Dt * Detune_Up) + 0.5 * Saw (V.Phase_4, Dt * 0.5);
            Advance (V.Phase_1, F);
            Advance (V.Phase_2, F * Detune_Down);
            Advance (V.Phase_3, F * Detune_Up);
            Advance (V.Phase_4, F * 0.5);
            Process (V.Filter, X, Low, Band, High);
            Output := Low * S.Pad_Level;
         when Piano =>
            X := Sin_Turns (V.Phase_1 + (0.6 + 0.8 * V.Tone) * (1.6 * V.Aux_1 + 0.22)
                              * Sin_Turns (V.Phase_2) / Two_Pi
                            + 0.5 * V.Aux_2 * Sin_Turns (V.Phase_3) / Two_Pi);
            Advance (V.Phase_1, F);
            Advance (V.Phase_2, F);
            Advance (V.Phase_3, Min (F * 14.0, 0.45 * Rate));
            Output := X;
         when Bass =>
            declare
               Hz : constant Float := F * Slide (V);
            begin
               if Retune then
                  Tune (V.Filter, 120.0 + (500.0 + 1600.0 * V.Tone) * V.Aux_1, 0.35);
               end if;
               X := Saw (V.Phase_1, Min (Hz / Rate, 0.5)) + 0.9 * Sin_Turns (V.Phase_2);
               Advance (V.Phase_1, Hz);
               Advance (V.Phase_2, Hz);
            end;
            Process (V.Filter, X, Low, Band, High);
            Output := Low;
         when Lead =>
            declare
               Vibrato : constant Float :=
                 (if T > 0.18 then 0.017 * Sin_Turns (T * 5.5) else 0.0);
               Current : constant Float :=
                 (F + (V.Glide_From - F) * V.Aux_1) * (1.0 + Vibrato);
               Lead_Dt : constant Float := Min (Current / Rate, 0.5);
            begin
               X := Pulse (V.Phase_1, 0.42, Lead_Dt)
                 + 0.6 * Pulse (V.Phase_2, 0.5, Lead_Dt * Detune_Slight);
               Advance (V.Phase_1, Current);
               Advance (V.Phase_2, Current * Detune_Slight);
            end;
            Process (V.Filter, X, Low, Band, High);
            Output := Low;
         when Arp =>
            if Retune then
               Tune (V.Filter, 300.0 + (1500.0 + 4000.0 * V.Tone) * V.Aux_1, 0.45);
            end if;
            X := Saw (V.Phase_1, Dt) + Saw (V.Phase_2, Dt * Detune_Slight);
            Advance (V.Phase_1, F);
            Advance (V.Phase_2, F * Detune_Slight);
            Process (V.Filter, X, Low, Band, High);
            Output := Low;
         when Bell =>
            X := Sin_Turns (V.Phase_1 + (0.4 + 1.4 * V.Aux_1) * Sin_Turns (V.Phase_2) / Two_Pi)
              + 0.2 * V.Aux_2 * Sin_Turns (V.Phase_3);
            Advance (V.Phase_1, F);
            Advance (V.Phase_2, Min (F * 2.0, 0.45 * Rate));
            Advance (V.Phase_3, Min (F * 4.07, 0.45 * Rate));
            Output := X;
         when Kick =>
            --  A punchy body: a fast snap (Aux_1, by Tone) and a slower
            --  sweep (Aux_2) down to the body pitch, a click, saturation.
            Noise (V, N);
            Advance (V.Phase_1, F * (1.0 + 1.8 * V.Aux_2 + 3.5 * V.Tone * V.Aux_1));
            Output := 0.95 * Soft_Clip ((1.5 + V.Tone) * Sin_Turns (V.Phase_1))
              + (0.15 + 0.35 * V.Tone) * N * V.Aux_1;
         when Snare =>
            --  Two body modes, a snap of noise, and the wires: noise bright
            --  or dark by Tone.
            Noise (V, N);
            Process (V.Filter, N, Low, Band, High);
            Advance (V.Phase_1, F * (1.0 + 0.3 * V.Aux_1));
            Advance (V.Phase_2, Min (F * 1.47, 0.45 * Rate));
            Output := (0.55 * Sin_Turns (V.Phase_1) + 0.3 * Sin_Turns (V.Phase_2)) * V.Aux_1
              + (0.4 + 0.6 * V.Tone) * High + 0.8 * (1.0 - V.Tone) * Band + 0.5 * N * V.Aux_2;
         when Clap =>
            Noise (V, N);
            Process (V.Filter, N, Low, Band, High);
            declare
               Burst : constant Float := Fraction (T / 0.009);
               Clustered : constant Float :=
                 (if T < 0.027 then Exp2 (-Burst * 6.0) else 0.6);
            begin
               Output := 1.6 * Band * Clustered;
            end;
         when Hat | Open_Hat =>
            Noise (V, N);
            Process (V.Filter, N, Low, Band, High);
            Output := High;
         when Tom =>
            Noise (V, N);
            Advance (V.Phase_1, F * (1.0 + 0.7 * V.Aux_1));
            Output := Sin_Turns (V.Phase_1) + 0.1 * N * V.Aux_1;
         when Crash =>
            --  Noise and three inharmonic squares (a metal plate), high-passed.
            Noise (V, N);
            X := 0.5 * N + 0.17 * (Square (V.Phase_1) + Square (V.Phase_2) + Square (V.Phase_3));
            Advance (V.Phase_1, Crash_Hertz);
            Advance (V.Phase_2, Crash_Hertz * 1.447);
            Advance (V.Phase_3, Crash_Hertz * 1.617);
            Process (V.Filter, X, Low, Band, High);
            Output := High;
         when Blip =>
            if Retune then
               Tune (V.Filter, 4800.0, 0.15);
            end if;
            declare
               Current : constant Float := F * (1.0 + V.Aux_1);
            begin
               X := 0.6 * Sin_Turns (V.Phase_1) + 0.4 * Saw (V.Phase_1, Min (Current / Rate, 0.5));
               Advance (V.Phase_1, Current);
            end;
            Process (V.Filter, X, Low, Band, High);
            Output := Low;
         when Sparkle =>
            X := Sin_Turns (V.Phase_1 + 1.6 * V.Aux_1 * Sin_Turns (V.Phase_2) / Two_Pi);
            Advance (V.Phase_1, F);
            Advance (V.Phase_2, Min (F * 3.5, 0.45 * Rate));
            Output := X;
         when Boom =>
            Noise (V, N);
            if Retune then
               Tune (V.Filter, 120.0 + 3000.0 * V.Aux_1, 0.3);
            end if;
            Process (V.Filter, N, Low, Band, High);
            Advance (V.Phase_1, 38.0 + 50.0 * V.Aux_2);
            Output := Low + 0.8 * Sin_Turns (V.Phase_1);
         when Whoosh =>
            Noise (V, N);
            if Retune then
               Tune (V.Filter, 300.0 + 3500.0 * (1.0 - V.Aux_1), 0.6);
            end if;
            Process (V.Filter, N, Low, Band, High);
            Output := Band;
         when Chime =>
            X := Sin_Turns (V.Phase_1 + 2.2 * V.Aux_1 * Sin_Turns (V.Phase_2) / Two_Pi)
              + 0.3 * Sin_Turns (V.Phase_3);
            Advance (V.Phase_1, F);
            Advance (V.Phase_2, Min (F * 1.4, 0.45 * Rate));
            Advance (V.Phase_3, Min (F * 2.0, 0.45 * Rate));
            Output := X;
         when Hurt =>
            declare
               Current : constant Float := 60.0 + 260.0 * V.Aux_1;
            begin
               X := Soft_Clip (2.0 * Saw (V.Phase_1, Min (Current / Rate, 0.5)));
               Advance (V.Phase_1, Current);
            end;
            Process (V.Filter, X, Low, Band, High);
            Output := Low;
         when Rise =>
            declare
               Progress : constant Float := Saturate (Float (V.Age) / Float (V.Length));
            begin
               Noise (V, N);
               if Retune then
                  Tune (V.Filter, 200.0 * Exp2 (5.0 * Progress), 0.55);
               end if;
               Process (V.Filter, N, Low, Band, High);
               Advance (V.Phase_1, 110.0 * Exp2 (3.0 * Progress));
               Output := (Band + 0.25 * Sin_Turns (V.Phase_1)) * Progress * Progress;
            end;
         when Impact =>
            --  A sub drop: a sine falling from four times its pitch, and a
            --  dark noise burst.
            Noise (V, N);
            Process (V.Filter, N, Low, Band, High);
            Advance (V.Phase_1, F * (1.0 + 3.0 * V.Aux_1));
            Output := 0.9 * Soft_Clip (1.5 * Sin_Turns (V.Phase_1)) + 0.5 * Low * V.Aux_2;
         when Zap =>
            declare
               Hz : constant Float := F * Slide (V);
            begin
               if Retune then
                  Tune (V.Filter, 1500.0 + 4000.0 * V.Aux_1, 0.4);
               end if;
               X := Pulse (V.Phase_1, 0.3, Min (Hz / Rate, 0.5));
               Advance (V.Phase_1, Hz);
            end;
            Process (V.Filter, X, Low, Band, High);
            Output := Low;
         when Growl =>
            --  FM growl: the modulator an octave down, its index swept by a
            --  6 Hz wobble, over a saw.
            X := Sin_Turns (V.Phase_1 + (2.0 + 1.5 * Sin_Turns (V.Phase_3)) * Sin_Turns (V.Phase_2) / Two_Pi)
              + 0.5 * Saw (V.Phase_4, Dt);
            Advance (V.Phase_1, F);
            Advance (V.Phase_2, F * 0.5);
            Advance (V.Phase_3, Growl_Wobble_Hertz);
            Advance (V.Phase_4, F);
            Process (V.Filter, X, Low, Band, High);
            Output := Soft_Clip (1.5 * Low);
         when Vinyl =>
            --  Crackle: rare clicks over a faint hiss, band-limited.
            Noise (V, N);
            X := (if abs N > Crackle_Threshold then N else 0.0) + Hiss_Level * N;
            Process (V.Filter, X, Low, Band, High);
            Output := 2.0 * Band;
         when Click =>
            Advance (V.Phase_1, F);
            Output := Sin_Turns (V.Phase_1);
      end case;
      V.Aux_1 := Flush (V.Aux_1 * V.Aux_Decay_1);
      V.Aux_2 := Flush (V.Aux_2 * V.Aux_Decay_2);
   end Sound;

   procedure Step_Envelope (V : in out Voice) is
      Sh : constant Shape := Shapes (V.Kind);
   begin
      if not V.Releasing and then V.Age >= V.Length then
         V.Releasing := True;
      end if;
      if V.Releasing then
         V.Envelope := Flush (V.Envelope * V.Release_Factor);
      elsif Float (V.Age) < Sh.Attack * Rate then
         V.Envelope := Min (1.0, V.Envelope + V.Attack_Step);
      else
         V.Envelope := Flush (V.Sustain + (V.Envelope - V.Sustain) * V.Decay_Factor);
      end if;
      V.Age := V.Age + 1;
      if (V.Releasing or else (V.Sustain = 0.0 and then V.Age > Natural (Sh.Attack * Rate) + 1))
        and then V.Envelope < Stop_Level
      then
         V.Active := False;
      elsif Float (V.Age) > Sh.Longest * Rate then
         V.Active := False;
      end if;
   end Step_Envelope;

   ---------------------------------------------------------------------------

   procedure Render (S : in out Synth; Left, Right : out Block; Frames : Block_Frames) is
      Dry_L, Dry_R, Chorus_L, Chorus_R, Reverb_In, Delay_L, Delay_R, Room_In : Float;
      Value, Gained, Ducking : Float;
      Out_L, Out_R, A, B, Low, Band, High, Rumble : Float;
      Retune : Boolean;
   begin
      Left := [others => 0.0];
      Right := [others => 0.0];
      for Frame in 1 .. Frames loop
         --  Start every note due at this sample.
         while S.Queued > 0 and then S.Queue (1).At_Time <= S.Head loop
            Begin_Voice (S, S.Queue (1));
            for I in 1 .. S.Queued - 1 loop
               S.Queue (I) := S.Queue (I + 1);
            end loop;
            S.Queued := S.Queued - 1;
         end loop;

         while S.Changes_Queued > 0 and then S.Changes (1).At_Time <= S.Head loop
            Apply (S, S.Changes (1));
            S.Changes (1 .. Change_Ceiling - 1) := S.Changes (2 .. Change_Ceiling);
            S.Changes_Queued := S.Changes_Queued - 1;
         end loop;
         Retune := S.Head mod Control_Period = 0;
         S.Pad_Level := S.Pad_Level + (S.Pad_Target - S.Pad_Level) * Pad_Glide;
         S.Cutoff := S.Cutoff + (S.Cutoff_Target - S.Cutoff) * Cutoff_Glide;
         S.Delay_Time := S.Delay_Time + (S.Delay_Target - S.Delay_Time) * Delay_Glide;
         S.Echo_Feedback := S.Echo_Feedback + (S.Echo_Target - S.Echo_Feedback) * Pad_Glide;
         S.Reverb_Wet := S.Reverb_Wet + (S.Wet_Target - S.Reverb_Wet) * Pad_Glide;
         S.Room_Size := S.Room_Size + (S.Room_Target - S.Room_Size) * Pad_Glide;
         S.Room_Level := S.Room_Level + (S.Room_Level_Target - S.Room_Level) * Pad_Glide;
         S.Duck_Level := Flush (S.Duck_Level * Duck_Release);
         if S.Gate_Open > 0 then
            S.Gate_Open := S.Gate_Open - 1;
         end if;
         S.Gate_Level := S.Gate_Level
           + ((if S.Gate_Open > 0 then 1.0 else 0.0) - S.Gate_Level) * Gate_Glide;
         Ducking := 1.0 - Duck_Depth * S.Duck_Level;

         Dry_L := 0.0;
         Dry_R := 0.0;
         Chorus_L := 0.0;
         Chorus_R := 0.0;
         Reverb_In := 0.0;
         Delay_L := 0.0;
         Delay_R := 0.0;
         Room_In := 0.0;
         for I in S.Voices'Range loop
            if S.Voices (I).Active then
               declare
                  V : Voice renames S.Voices (I);
                  Sh : constant Shape := Shapes (V.Kind);
               begin
                  Step_Envelope (V);
                  Sound (S, V, Retune, Value);
                  Gained := Value * V.Envelope * V.Velocity * Sh.Gain
                    * (1.0 - Sh.Ducked + Sh.Ducked * Ducking);
                  Dry_L := Dry_L + Gained * Sh.Dry * V.Gain_Left;
                  Dry_R := Dry_R + Gained * Sh.Dry * V.Gain_Right;
                  Chorus_L := Chorus_L + Gained * Sh.Chorus_Send * V.Gain_Left;
                  Chorus_R := Chorus_R + Gained * Sh.Chorus_Send * V.Gain_Right;
                  Reverb_In := Reverb_In + Gained * Sh.Reverb_Send;
                  Delay_L := Delay_L + Gained * Sh.Delay_Send * V.Gain_Left;
                  Delay_R := Delay_R + Gained * Sh.Delay_Send * V.Gain_Right;
                  Room_In := Room_In + Gained * Sh.Room_Send;
               end;
            end if;
         end loop;

         --  Buses into the master.
         Process (S.Pad_Chorus, Chorus_L, Chorus_R, A, B);
         Out_L := Dry_L + A;
         Out_R := Dry_R + B;
         if Retune then
            S.Reverb.Feedback := Room_Feedback (S.Room_Size);
         end if;
         Process (S.Reverb, Reverb_In, A, B);
         Out_L := Out_L + (Wet_Scale * S.Reverb_Wet) * A * Ducking;
         Out_R := Out_R + (Wet_Scale * S.Reverb_Wet) * B * Ducking;
         Process (S.Echo, Delay_L, Delay_R, S.Delay_Time, S.Echo_Feedback, A, B);
         Out_L := Out_L + 0.6 * A;
         Out_R := Out_R + 0.6 * B;
         Process (S.Room, Room_In, A, B);
         Out_L := Out_L + (Room_Return * S.Room_Level) * A * S.Gate_Level;
         Out_R := Out_R + (Room_Return * S.Room_Level) * B * S.Gate_Level;

         --  The underwater sweep: bypassed when fully open.
         if S.Cutoff < 0.985 then
            if Retune or else S.Master_Tuned = 0 then
               Tune (S.Master_Left, 160.0 * Exp2 (7.0 * S.Cutoff), 0.25);
               S.Master_Right := (S.Master_Right with delta
                                  A1 => S.Master_Left.A1, A2 => S.Master_Left.A2,
                                  A3 => S.Master_Left.A3, K => S.Master_Left.K);
               S.Master_Tuned := 1;
            end if;
            Process (S.Master_Left, Out_L, Low, Band, High);
            Out_L := Low;
            Process (S.Master_Right, Out_R, Low, Band, High);
            Out_R := Low;
         else
            S.Master_Tuned := 0;
            S.Master_Left.IC1 := 0.0;
            S.Master_Left.IC2 := 0.0;
            S.Master_Right.IC1 := 0.0;
            S.Master_Right.IC2 := 0.0;
         end if;

         Process (S.Wow, Out_L, Out_R, S.Wobble, A, B);
         --  DC and sub-sonic rumble out (one-pole high-pass near 15 Hz).
         Low_Pass (S.Rumble_Left, A, 0.002, Rumble);
         A := A - Rumble;
         Low_Pass (S.Rumble_Right, B, 0.002, Rumble);
         B := B - Rumble;
         Process (S.Limit, A * Master_Pre, B * Master_Pre, Out_L, Out_R);
         S.Peak := Max (S.Peak, Max (abs Out_L, abs Out_R));
         Left (Frame) := Out_L;
         Right (Frame) := Out_R;
         S.Head := S.Head + 1;
      end loop;
   end Render;

end Neon_Synth;
