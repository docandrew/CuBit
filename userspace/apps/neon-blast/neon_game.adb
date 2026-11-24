------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Neon_Notes; use Neon_Notes;

package body Neon_Game is
   package C renames Neon_Composer;
   use type Neon_Sim.Sound_Kind;

   Octave : constant := 12.0;
   Arpeggio_Notes : constant := 4;
   Flash_Seconds : constant := 0.3;
   Flash_Particles : constant := 26;
   Boss_Cascade : constant := 8;
   Zap_Fall : constant := 12.0;     --  semitones a bolt's zap falls from
   Zap_Lift : constant := 24.0;     --  a warp sits two octaves above the chord

   function To_Synth (M : C.Mix_Targets) return Neon_Synth.Targets is
     ((Pad_Level => M.Pad_Level, Cutoff => M.Cutoff, Delay_Samples => M.Delay_Samples,
       Echo_Feedback => M.Echo_Feedback, Wobble => M.Wobble, Reverb_Wet => M.Reverb_Wet,
       Reverb_Room => M.Reverb_Room, Room_Level => M.Room_Level));

   procedure Compose_Until (G : in out Game; Time : Samples) is
      Batch : C.Note_Batch;
      Info : C.Step_Record;
      Mix : C.Mix_Targets;
   begin
      while C.Next_Start (G.Composer) <= Time loop
         Batch.Count := 0;
         C.Compose_Next (G.Composer, Neon_Sim.Mood (G.Sim), Batch, Info, Mix);
         for I in 1 .. Batch.Count loop
            Neon_Synth.Schedule (G.Synth, Batch.Items (I));
         end loop;
         Neon_Synth.Set_Targets (G.Synth, Info.Start, To_Synth (Mix));
      end loop;
   end Compose_Until;

   procedure Start (G : out Game; Seed : Unsigned_64) is
   begin
      Neon_Sim.Reset (G.Sim, Seed);
      C.Start (G.Composer, Seed);
      Neon_Synth.Start (G.Synth, Seed);
      G.Next_Tick := 0;
      G.Current := (others => <>);
      G.Sounds.Count := 0;
      Compose_Until (G, 0);
   end Start;

   function Chord_At (G : Game; Time : Samples) return C.Chord_Value is
      Info : C.Step_Record;
      Found : Boolean;
   begin
      C.Find (G.Composer, Time, Info, Found);
      return Info.Chord;
   end Chord_At;

   procedure Play_Sounds (G : in out Game) is
      Head : constant Samples := Write_Head (G);
      At_Time : constant Samples := (if Head > G.Next_Tick then Head else G.Next_Tick);
   begin
      for I in 1 .. G.Sounds.Count loop
         declare
            R : constant Neon_Sim.Sound_Request := G.Sounds.Items (I);
            Chord : constant C.Chord_Value := Chord_At (G, At_Time);
            procedure Play (Voice : Instrument; Pitch, Velocity : Float; Length : Positive;
                            When_At : Samples := At_Time; Glide : Glide_Semitones := 0.0;
                            Tone : Unit_Float := 0.5) is
            begin
               Neon_Synth.Schedule (G.Synth, (At_Time => When_At, Voice => Voice, Pitch => Pitch,
                                              Velocity => Velocity, Length => Length, Pan => R.Pan,
                                              Glide => Glide, Tone => Tone, others => <>));
            end Play;
         begin
            case R.Kind is
               when Neon_Sim.Shot =>
                  Play (Blip, C.Chord_Pitch (Chord, R.Degree), 0.8, 2_400);
               when Neon_Sim.Explosion =>
                  Play (Boom, 40.0, 0.5 + 0.35 * R.Size, 9_600);
                  declare
                     First : constant Samples := C.Next_Boundary (G.Composer, At_Time);
                     Info : C.Step_Record;
                     Found : Boolean;
                     Gap : Samples;
                  begin
                     C.Find (G.Composer, First, Info, Found);
                     Gap := Samples (Natural'Max (1, Info.Length / 2));
                     for K in 0 .. Arpeggio_Notes - 1 loop
                        Play (Sparkle, C.Chord_Pitch (Info.Chord, 2 * K) + Octave,
                              0.55 - 0.08 * Float (K), 4_800, First + Samples (K) * Gap);
                     end loop;
                     Neon_Sim.Add_Flash (G.Sim, (R.X, 0.8, R.Z), R.Color, First, Flash_Seconds,
                                         Flash_Particles);
                  end;
               when Neon_Sim.Dash_Sound =>
                  Play (Whoosh, 60.0, 0.9, 6_000);
               when Neon_Sim.Hurt_Sound =>
                  Play (Hurt, 50.0, 1.0, 12_000);
                  Play (Boom, 36.0, 1.0, 12_000);
               when Neon_Sim.Death_Sound =>
                  Play (Boom, 33.0, 1.6, 24_000);
                  Play (Rise, 40.0, 0.4, 48_000);
               when Neon_Sim.Combo_Chime =>
                  Play (Chime, C.Chord_Pitch (Chord, 3 + R.Degree mod 5) + Octave, 0.55, 9_600);
               when Neon_Sim.Wave_Chime =>
                  for K in 0 .. 2 loop
                     Play (Chime, C.Chord_Pitch (Chord, 2 + K), 0.45, 24_000,
                           At_Time + Samples (K) * 2_400);
                  end loop;
               when Neon_Sim.Spawn_Warning =>
                  Play (Sparkle, Chord.Bass + Octave, 0.22, 4_800);
               when Neon_Sim.Tap_Sound =>
                  null;
               when Neon_Sim.Enemy_Fire =>
                  Play (Zap, Chord.Bass + 2.0 * Octave, 0.32, 4_000, Glide => Zap_Fall);
               when Neon_Sim.Warp =>
                  Play (Zap, C.Chord_Pitch (Chord, 0) + Octave, 0.26, 6_000, Glide => -Zap_Lift / 2.0);
               when Neon_Sim.Burst_Sound =>
                  Play (Boom, 36.0, 0.9, 12_000);
                  Play (Zap, Chord.Bass + 3.0 * Octave, 0.35, 6_000, Glide => Zap_Fall);
               when Neon_Sim.Boss_Arrive =>
                  Play (Growl, Chord.Bass, 0.85, 72_000);
                  Play (Impact, 28.0, 0.9, 24_000);
               when Neon_Sim.Boss_Hit =>
                  Play (Bell, C.Chord_Pitch (Chord, 1), 0.22, 2_400, Tone => 0.9);
               when Neon_Sim.Boss_Down =>
                  Play (Impact, 26.0, 1.0, 36_000);
                  Play (Crash, 70.0, 0.9, 48_000);
                  Play (Boom, 30.0, 1.6, 36_000);
                  for K in 0 .. Boss_Cascade - 1 loop
                     Play (Sparkle, C.Chord_Pitch (Chord, K) + Octave, 0.5 - 0.04 * Float (K), 9_600,
                           At_Time + Samples (K) * 3_000);
                  end loop;
               when Neon_Sim.Title_Tick =>
                  Play (Blip, C.Chord_Pitch (Chord, 2), 0.35, 1_200);
            end case;
         end;
      end loop;
      G.Sounds.Count := 0;
   end Play_Sounds;

   procedure Advance (G : in out Game; Input : Neon_Sim.Controls; Target : Samples) is
      Step_Input : Neon_Sim.Controls := Input;
      Info : C.Step_Record;
      Found : Boolean;
      New_Step : Boolean;
   begin
      while G.Next_Tick < Target loop
         Compose_Until (G, G.Next_Tick + Tick_Samples);
         C.Find (G.Composer, G.Next_Tick, Info, Found);
         New_Step := Found and then Info.Serial /= G.Current.Serial;
         if Found then
            G.Current := Info;
         end if;
         Neon_Sim.Step (G.Sim, Step_Input, G.Next_Tick, G.Current, New_Step, G.Sounds);
         Play_Sounds (G);
         G.Next_Tick := G.Next_Tick + Tick_Samples;
         Step_Input.Dash := False;
         Step_Input.Confirm := False;
         Step_Input.Calibrate := False;
         Step_Input.Tap := False;
         Step_Input.Press_Judged := False;
      end loop;
   end Advance;

   procedure Render_Audio (G : in out Game; Left, Right : out Neon_Synth.Block;
                           Frames : Neon_Synth.Block_Frames) is
   begin
      Compose_Until (G, Write_Head (G) + Samples (Frames));
      Neon_Synth.Render (G.Synth, Left, Right, Frames);
   end Render_Audio;

   procedure Beat_Error_Ms (G : Game; Time : Samples; Error : out Float; Found : out Boolean) is
      Offset : Sample_Offset;
   begin
      C.Beat_Error (G.Composer, Time, Offset, Found);
      Error := Milliseconds (Offset);
   end Beat_Error_Ms;

end Neon_Game;
