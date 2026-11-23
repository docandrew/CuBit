------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast's signal processing: filters, delay lines and the effects the
--  soundtrack runs through, all in single-precision float at 48 kHz.
--
--  @description
--  - State_Variable: Zavalishin's topology-preserving SVF ("The Art of VA
--    Filter Design", 2012), low/band/high outputs, stable under modulation.
--  - Reverb: Freeverb by Jezar at Dreampoint (public domain, June 2000):
--    eight lowpass-feedback combs and four allpasses a channel, its
--    tunings scaled to 48 kHz.
--  - Oscillators (Neon_Synth): Valimaki and Huovilainen's polyBLEP; the
--    piano and bells are two-operator FM after Chowning (JAES, 1973).
--  - Ping_Pong: a stereo delay whose repeats cross sides, damped in the loop.
--  - Chorus: two modulated delay taps, one a side.
--  - Tape: wow and flutter as a slowly modulated delay (pitch wobble).
--  - Limiter: instant attack, smooth release, then a soft clip.
--  Feedback paths flush values below Denormal_Floor to zero so tails never
--  fall into the slow denormal range.
------------------------------------------------------------------------------
package Neon_DSP with SPARK_Mode is

   Sample_Rate : constant := 48_000.0;
   Denormal_Floor : constant := 1.0e-18;

   function Flush (X : Float) return Float is (if abs X < Denormal_Floor then 0.0 else X);

   ---------------------------------------------------------------------------
   type One_Pole is record
      Z : Float := 0.0;
   end record;
   --  Coefficient for a one-pole smoother with time constant Seconds.
   function Smoothing (Seconds : Float) return Float;
   procedure Low_Pass (F : in out One_Pole; Input, Coefficient : Float; Output : out Float);

   ---------------------------------------------------------------------------
   type State_Variable is record
      IC1, IC2 : Float := 0.0;
      A1, A2, A3, K : Float := 0.0;
   end record;
   --  Cutoff in Hz (clamped to 20 .. 0.45 * Sample_Rate), Resonance 0 .. 0.95.
   procedure Tune (F : in out State_Variable; Cutoff, Resonance : Float);
   procedure Process (F : in out State_Variable; Input : Float; Low, Band, High : out Float);

   ---------------------------------------------------------------------------
   --  A delay line of a fixed power-of-two size.
   Line_Size : constant := 2**16;   --  1.37 s at 48 kHz
   type Line_Index is mod Line_Size;
   type Line_Buffer is array (Line_Index) of Float;
   type Delay_Line is record
      Data : Line_Buffer := [others => 0.0];
      Head : Line_Index := 0;
   end record;
   procedure Push (D : in out Delay_Line; Input : Float);
   --  The sample Lag samples ago (fractional, linear interpolation),
   --  Lag in 1 .. Line_Size - 2.
   function Tap (D : Delay_Line; Lag : Float) return Float;

   ---------------------------------------------------------------------------
   Comb_Count : constant := 8;
   Allpass_Count : constant := 4;
   Comb_Size : constant := 2048;
   Allpass_Size : constant := 1024;
   type Comb_Buffer is array (0 .. Comb_Size - 1) of Float;
   type Allpass_Buffer is array (0 .. Allpass_Size - 1) of Float;
   type Comb is record
      Data : Comb_Buffer := [others => 0.0];
      Length : Positive := 1;
      Position : Natural := 0;
      Store : Float := 0.0;
   end record;
   type Allpass is record
      Data : Allpass_Buffer := [others => 0.0];
      Length : Positive := 1;
      Position : Natural := 0;
   end record;
   type Comb_Bank is array (1 .. Comb_Count) of Comb;
   type Allpass_Bank is array (1 .. Allpass_Count) of Allpass;
   type Reverb_Channel is record
      Combs : Comb_Bank;
      Allpasses : Allpass_Bank;
   end record;
   type Reverb is record
      Left, Right : Reverb_Channel;
      Feedback : Float := 0.84;
      Damping : Float := 0.25;
   end record;
   --  Room 0 .. 1 (0.98 is huge), Damping 0 .. 1 (high-frequency loss).
   --  Comb_Scale shortens every delay (a small dense room is 0.3).
   procedure Configure (R : out Reverb; Room, Damping : Float; Comb_Scale : Float := 1.0);
   procedure Process (R : in out Reverb; Input : Float; Left, Right : out Float);

   ---------------------------------------------------------------------------
   type Ping_Pong is record
      Left, Right : Delay_Line;
      Damp_Left, Damp_Right : One_Pole;
   end record;
   --  Lag in samples (1 .. Line_Size - 2); Feedback below 1.
   procedure Process (P : in out Ping_Pong; Input_Left, Input_Right, Lag, Feedback : Float;
                      Left, Right : out Float);

   ---------------------------------------------------------------------------
   type Chorus is record
      Line : Delay_Line;
      Phase : Float := 0.0;
   end record;
   procedure Process (C : in out Chorus; Input_Left, Input_Right : Float; Left, Right : out Float);

   ---------------------------------------------------------------------------
   --  The tape stage's fixed delay in samples (7 ms; Neon_Synth.Output_Latency).
   Tape_Delay_Samples : constant := 336;
   type Tape is record
      Left, Right : Delay_Line;
      Wow_Phase, Flutter_Phase : Float := 0.0;
   end record;
   --  Depth 0 .. 1 scales the wobble (0: a plain fixed delay).
   procedure Process (T : in out Tape; Input_Left, Input_Right, Depth : Float;
                      Left, Right : out Float);

   ---------------------------------------------------------------------------
   type Limiter is record
      Gain : Float := 1.0;
      Reduction_Peak : Float := 0.0;   --  deepest gain reduction seen, dB below 0
   end record;
   Limiter_Ceiling : constant := 0.89;  --  -1 dBFS
   procedure Process (L : in out Limiter; Input_Left, Input_Right : Float;
                      Left, Right : out Float)
     with Post => Left in -1.0 .. 1.0 and then Right in -1.0 .. 1.0;

end Neon_DSP;
