------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Math; use Neon_Math;

package body Neon_DSP with SPARK_Mode is

   function Smoothing (Seconds : Float) return Float is
     (if Seconds <= 0.0 then 1.0
      else Saturate (1.0 - Exp (-1.0 / (Clamp (Seconds, 1.0e-5, 100.0) * Sample_Rate))));

   procedure Low_Pass (F : in out One_Pole; Input, Coefficient : Float; Output : out Float) is
   begin
      F.Z := Flush (F.Z + (Input - F.Z) * Coefficient);
      Output := F.Z;
   end Low_Pass;

   procedure Tune (F : in out State_Variable; Cutoff, Resonance : Float) is
      Turns : constant Float := Clamp (Cutoff, 20.0, 0.45 * Sample_Rate) / (2.0 * Sample_Rate);
      G : constant Float := Sin_Turns (Turns) / Max (Cos_Turns (Turns), 0.05);
   begin
      F.K := 2.0 - 2.0 * Clamp (Resonance, 0.0, 0.95);
      F.A1 := 1.0 / (1.0 + G * (G + F.K));
      F.A2 := G * F.A1;
      F.A3 := G * F.A2;
   end Tune;

   procedure Process (F : in out State_Variable; Input : Float; Low, Band, High : out Float) is
      V3 : constant Float := Input - F.IC2;
      V1 : constant Float := F.A1 * F.IC1 + F.A2 * V3;
      V2 : constant Float := F.IC2 + F.A2 * F.IC1 + F.A3 * V3;
   begin
      F.IC1 := Flush (2.0 * V1 - F.IC1);
      F.IC2 := Flush (2.0 * V2 - F.IC2);
      Low := V2;
      Band := V1;
      High := Input - F.K * V1 - V2;
   end Process;

   procedure Push (D : in out Delay_Line; Input : Float) is
   begin
      D.Head := D.Head + 1;
      D.Data (D.Head) := Flush (Input);
   end Push;

   function Tap (D : Delay_Line; Lag : Float) return Float is
      Clamped : constant Float := Clamp (Lag, 1.0, Float (Line_Size - 2));
      Whole : constant Natural := Natural (Floor (Clamped));
      Part : constant Float := Clamped - Float (Whole);
      A : constant Float := D.Data (D.Head - Line_Index (Whole));
      B : constant Float := D.Data (D.Head - Line_Index (Whole) - 1);
   begin
      return A + (B - A) * Part;
   end Tap;

   --  Freeverb's tunings at 44.1 kHz, and its stereo spread.
   type Comb_Tunings is array (1 .. Comb_Count) of Positive;
   Comb_Tuning : constant Comb_Tunings := [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617];
   type Allpass_Tunings is array (1 .. Allpass_Count) of Positive;
   Allpass_Tuning : constant Allpass_Tunings := [556, 441, 341, 225];
   Stereo_Spread : constant := 23;
   Freeverb_Rate : constant := 44_100.0;
   Allpass_Feedback : constant := 0.5;
   Reverb_Input_Gain : constant := 0.015;

   procedure Configure (R : out Reverb; Room, Damping : Float; Comb_Scale : Float := 1.0) is
      Scale : constant Float := Sample_Rate / Freeverb_Rate * Clamp (Comb_Scale, 0.05, 1.0);
      function Length (Tuning : Positive; Spread : Natural; Size : Positive) return Positive is
        (Positive'Max (1, Natural'Min (Size - 1, Natural (Float (Tuning + Spread) * Scale))));
   begin
      R := (Left => (others => <>), Right => (others => <>),
            Feedback => 0.7 + 0.28 * Saturate (Room), Damping => Saturate (Damping));
      for I in 1 .. Comb_Count loop
         R.Left.Combs (I).Length := Length (Comb_Tuning (I), 0, Comb_Size);
         R.Right.Combs (I).Length := Length (Comb_Tuning (I), Stereo_Spread, Comb_Size);
      end loop;
      for I in 1 .. Allpass_Count loop
         R.Left.Allpasses (I).Length := Length (Allpass_Tuning (I), 0, Allpass_Size);
         R.Right.Allpasses (I).Length := Length (Allpass_Tuning (I), Stereo_Spread, Allpass_Size);
      end loop;
   end Configure;

   procedure Run_Channel (Ch : in out Reverb_Channel; Input, Feedback, Damping : Float;
                          Output : out Float) is
      Sum : Float := 0.0;
      Value, Buffered : Float;
   begin
      for I in 1 .. Comb_Count loop
         declare
            C : Comb renames Ch.Combs (I);
            P : constant Natural := C.Position mod C.Length;
         begin
            Value := C.Data (P);
            C.Store := Flush (Value * (1.0 - Damping) + C.Store * Damping);
            C.Data (P) := Flush (Input + C.Store * Feedback);
            C.Position := (P + 1) mod C.Length;
            Sum := Sum + Value;
         end;
      end loop;
      for I in 1 .. Allpass_Count loop
         declare
            A : Allpass renames Ch.Allpasses (I);
            P : constant Natural := A.Position mod A.Length;
         begin
            Buffered := A.Data (P);
            Value := Buffered - Sum;
            A.Data (P) := Flush (Sum + Buffered * Allpass_Feedback);
            A.Position := (P + 1) mod A.Length;
            Sum := Value;
         end;
      end loop;
      Output := Sum;
   end Run_Channel;

   procedure Process (R : in out Reverb; Input : Float; Left, Right : out Float) is
      Scaled : constant Float := Input * Reverb_Input_Gain;
   begin
      Run_Channel (R.Left, Scaled, R.Feedback, R.Damping, Left);
      Run_Channel (R.Right, Scaled, R.Feedback, R.Damping, Right);
   end Process;

   Loop_Damping : constant := 0.45;   --  one-pole coefficient: repeats darken

   procedure Process (P : in out Ping_Pong; Input_Left, Input_Right, Lag, Feedback : Float;
                      Left, Right : out Float) is
      Back_Left : constant Float := Tap (P.Left, Lag);
      Back_Right : constant Float := Tap (P.Right, Lag);
      Dark_Left, Dark_Right : Float;
   begin
      Low_Pass (P.Damp_Left, Back_Left, Loop_Damping, Dark_Left);
      Low_Pass (P.Damp_Right, Back_Right, Loop_Damping, Dark_Right);
      --  Mono input enters the left line; each repeat crosses sides.
      Push (P.Left, 0.5 * (Input_Left + Input_Right) + Dark_Right * Feedback);
      Push (P.Right, Dark_Left * Feedback);
      Left := Back_Left;
      Right := Back_Right;
   end Process;

   Chorus_Base : constant := 0.013 * Sample_Rate;     --  13 ms
   Chorus_Depth : constant := 0.0035 * Sample_Rate;   --  +-3.5 ms
   Chorus_Rate : constant := 0.55;                    --  Hz

   procedure Process (C : in out Chorus; Input_Left, Input_Right : Float; Left, Right : out Float) is
      Mono : constant Float := 0.5 * (Input_Left + Input_Right);
   begin
      Push (C.Line, Mono);
      C.Phase := Fraction (C.Phase + Chorus_Rate / Sample_Rate);
      Left := 0.7 * Input_Left + 0.55 * Tap (C.Line, Chorus_Base + Chorus_Depth * Sin_Turns (C.Phase));
      Right := 0.7 * Input_Right
        + 0.55 * Tap (C.Line, Chorus_Base + Chorus_Depth * Sin_Turns (C.Phase + 0.33));
   end Process;

   Tape_Base : constant := Float (Tape_Delay_Samples);   --  7 ms
   Wow_Depth : constant := 0.0011 * Sample_Rate;      --  cassette wow, slow
   Wow_Rate : constant := 0.45;
   Flutter_Depth : constant := 0.00009 * Sample_Rate;
   Flutter_Rate : constant := 6.8;

   procedure Process (T : in out Tape; Input_Left, Input_Right, Depth : Float;
                      Left, Right : out Float) is
      Offset : Float;
   begin
      Push (T.Left, Input_Left);
      Push (T.Right, Input_Right);
      T.Wow_Phase := Fraction (T.Wow_Phase + Wow_Rate / Sample_Rate);
      T.Flutter_Phase := Fraction (T.Flutter_Phase + Flutter_Rate / Sample_Rate);
      Offset := Tape_Base + Depth * (Wow_Depth * Sin_Turns (T.Wow_Phase)
                                     + Flutter_Depth * Sin_Turns (T.Flutter_Phase));
      Left := Tap (T.Left, Offset);
      Right := Tap (T.Right, Offset);
   end Process;

   Release_Coefficient : constant := 0.0004;   --  ~50 ms back to unity

   procedure Process (L : in out Limiter; Input_Left, Input_Right : Float;
                      Left, Right : out Float) is
      Peak : constant Float := Max (abs Input_Left, abs Input_Right);
      Needed : Float;
   begin
      L.Gain := Min (1.0, L.Gain + (1.0 - L.Gain) * Release_Coefficient);
      if Peak * L.Gain > Limiter_Ceiling then
         Needed := Limiter_Ceiling / Peak;
         L.Gain := Needed;
         L.Reduction_Peak := Max (L.Reduction_Peak, 1.0 - Needed);
      end if;
      --  The gain already holds peaks at the ceiling; the clamp only
      --  guards the conversion.
      Left := Clamp (Input_Left * L.Gain, -1.0, 1.0);
      Right := Clamp (Input_Right * L.Gain, -1.0, 1.0);
   end Process;

end Neon_DSP;
