------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Neon Blast (APP-008): a musical vaporwave arena shooter, and an
--  instrument for CuBit's keyboard, pointer, audio and Vulkan paths.
--
--  @description
--  The window's event loop runs on a 1 ms deadline (CuBit.UI.App.Run with
--  Activity_Wait, like Teapots). Each turn it:
--  - tops up the audio ring lent by mixer.svc from the synthesizer (the
--    data plane: no IPC per buffer), and counts underruns;
--  - estimates the song position being heard now (frames written, minus
--    those queued in the ring and in the device's periods) and keeps a
--    smoothed song clock on the monotonic clock;
--  - steps the game to that position, with the held keys of this turn;
--  - renders a frame: on the GPU canvas when the launch has a render
--    session, otherwise in software straight into the window's frame.
--
--  F3 toggles the overlay: frame and render time, present latency,
--  input-to-present per press, key-to-sound, audio queue and underruns,
--  audio/video drift and the beat judgement, and the held-keys strip. A
--  stats line goes to the log every five seconds (the headless test reads
--  it). The latencies are measured on this process's clock, from the
--  moment the app took the event from its ring, not from the hardware
--  (input events carry no device timestamp yet).
--
--  Idle on the title for a while and the autopilot plays an attract demo;
--  any key or click hands the game back.
------------------------------------------------------------------------------
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Audio;
with CuBit.Desktop_Protocol;
with CuBit.Logging;
with CuBit.Monotonic;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.GPU_Canvas;
with CuBit.UI.Input;
with CuBit.UI.State;
with Startup;
with Neon_Autopilot;
with Neon_Game;
with Neon_Replay;
with Neon_GPU;
with Neon_Keys; use Neon_Keys;
with Neon_Math; use Neon_Math;
with Neon_Scene;
with Neon_Sim;
with Neon_Soft;
with Neon_Synth;
with Neon_Time; use Neon_Time;

procedure Main is
   package GC renames CuBit.UI.GPU_Canvas;
   package Input renames CuBit.UI.Input;
   use type GC.Availability, GC.Acquire_Status;
   use type Neon_Sim.Phase_Kind;

   INITIAL_WIDTH : constant := 1024;
   INITIAL_HEIGHT : constant := 640;
   MINIMUM_WIDTH : constant := 480;
   MINIMUM_HEIGHT : constant := 300;
   POLL_MILLISECONDS : constant := 1;
   SEED : constant := 1987;
   --  Software frames: one every 1/60 s at most.
   SOFT_FRAME_US : constant := 16_667;
   --  Audio: keep this many frames queued in the ring (37.3 ms at 48 kHz,
   --  of its 2048): enough to ride out a software frame (12-16 ms under
   --  QEMU) plus the loop's wake-ups; 1024 underran under QEMU.
   AUDIO_TARGET_FRAMES : constant := 1_792;
   --  How often the device queue depth is asked of the mixer (an IPC call).
   PLAYBACK_POLL_US : constant := 50_000;
   --  Song clock: further than this from the audio estimate snaps; nearer,
   --  it is pulled by CLOCK_PULL a turn.
   CLOCK_SNAP_SAMPLES : constant := 2_400;
   CLOCK_PULL : constant := 0.02;
   --  The mixer answers opens once its device is ready: until then, try
   --  again this often, this many times.
   AUDIO_RETRY_US : constant := 1_000_000;
   AUDIO_RETRIES : constant := 30;
   --  Without audio the write head runs this far ahead of the song clock.
   SILENT_LEAD_SAMPLES : constant := 1_600;
   DEMO_IDLE_US : constant := 12_000_000;
   STATS_INTERVAL_US : constant := 5_000_000;
   MICROSECONDS_PER_SAMPLE : constant := 1.0e6 / Float (Sample_Rate);
   BUTTON_LEFT : constant Unsigned_64 := 1;
   BUTTON_RIGHT : constant Unsigned_64 := 2;
   OVERLAY_BACKGROUND : constant Color := 16#120A20#;
   OVERLAY_INK : constant Color := 16#E8E0FF#;
   OVERLAY_ACCENT : constant Color := 16#18F0FF#;
   OVERLAY_WARN : constant Color := 16#FF6A5E#;
   OVERLAY_INSET : constant := 8;
   OVERLAY_WIDTH : constant := 520;

   type Game_Access is access Neon_Game.Game;
   type Frame_Access is access Neon_Scene.Frame;
   G : constant Game_Access := new Neon_Game.Game;
   Scene : constant Frame_Access := new Neon_Scene.Frame;
   Soft : Neon_Soft.Renderer;

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Canvas : GC.Canvas_Widget;
   Opened, GPU_Started : Boolean := False;

   Board : Neon_Keys.Board := Empty_Board;
   Pointer_X, Pointer_Y : Natural := 0;    --  logical window coordinates
   Buttons : Unsigned_64 := 0;
   Click_Pressed, Right_Pressed : Boolean := False;
   Last_Press_Us : Unsigned_64 := 0;       --  newest press not yet applied
   Sound_Press_Us : Unsigned_64 := 0;      --  newest press that makes a sound (fire, dash)
   Overlay : Boolean := False;
   Benchmark_Requested : Boolean := False;
   Last_Input_Us : Unsigned_64 := 0;

   --  Audio.
   Stream : CuBit.Audio.StreamHandle;
   Audio_On : Boolean := False;
   Audio_Attempts : Natural := 0;
   Next_Audio_Try_Us : Unsigned_64 := 0;
   Device_Frames : Natural := 0;
   Last_Playback_Us : Unsigned_64 := 0;
   Underruns_Start : Unsigned_32 := 0;
   Underruns_Now : Unsigned_32 := 0;
   Queued_Now : Natural := 0;
   Mix_Left, Mix_Right : Neon_Synth.Block;

   --  The song clock: Song_At (Us) = Base_Song + (Us - Base_Us) * 0.048.
   Base_Us : Unsigned_64 := 0;
   Base_Song : Float := 0.0;
   Clock_Ready : Boolean := False;
   Drift_Samples : Float := 0.0;           --  audio estimate - clock, last turn

   --  Measurements.
   type Average is record
      Count : Natural := 0;
      Sum, Peak, Last : Float := 0.0;
   end record;
   procedure Note (A : in out Average; Value : Float) is
   begin
      A.Count := A.Count + 1;
      A.Sum := A.Sum + Value;
      A.Peak := Max (A.Peak, Value);
      A.Last := Value;
   end Note;
   function Mean (A : Average) return Float is (if A.Count = 0 then 0.0 else A.Sum / Float (A.Count));
   Frame_Interval, Render_Time, Present_Latency, Input_To_Present, Key_To_Sound, Beat_To_Photon :
     Average;
   Window_Frames : Natural := 0;
   Last_Frame_Us, Next_Soft_Us, Stats_Us : Unsigned_64 := 0;
   Painted_At_Us : Unsigned_64 := 0;       --  Render began painting this frame
   Painted_Song : Float := 0.0;            --  the song time the frame shows
   Painted_Input_Us : Unsigned_64 := 0;    --  press the frame first reflects (0: none)
   Pending_Input_Us : Unsigned_64 := 0;    --  press applied, not yet in a frame
   Awaiting_Publish : Boolean := False;
   Display_Lead : Float := 0.0;            --  samples: frames show this much ahead

   function Now_Us return Unsigned_64 is
      Reading : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Reading.Available then Reading.Microseconds
              else CuBit.Monotonic.Milliseconds * 1_000);
   end Now_Us;

   function Decimal (Value : Natural) return String is
      Image : constant String := Natural'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   --  Value with one decimal ("12.3", "-0.4").
   function Tenths (Value : Float) return String is
      Scaled : constant Integer := Integer (Clamp (Value, -1.0e6, 1.0e6) * 10.0);
      Magnitude : constant Natural := abs Scaled;
   begin
      return (if Scaled < 0 then "-" else "") & Decimal (Magnitude / 10) & "." & Decimal (Magnitude mod 10);
   end Tenths;

   function Song_At (Us : Unsigned_64) return Float is
     (Base_Song + Float (Long_Long_Integer (Us) - Long_Long_Integer (Base_Us)) / MICROSECONDS_PER_SAMPLE);

   function Physical (Logical : Natural; Win_Canvas : CuBit.UI.Canvas) return Float is
     (Float (Logical) * Float (Win_Canvas.densityNumerator) / Float (Win_Canvas.densityDenominator));

   ---------------------------------------------------------------------------
   --  Audio
   ---------------------------------------------------------------------------

   procedure Fill (Span : CuBit.Audio.BufferSpan; From : Positive) is
      function PCM (Sample : Float) return Integer_16 renames Neon_Replay.PCM;
   begin
      for Frame in 0 .. Span.frames - 1 loop
         declare
            Pair : array (0 .. 1) of Integer_16
              with Import, Address => Span.address + Storage_Offset (Frame * 4);
         begin
            Pair (0) := PCM (Mix_Left (From + Frame));
            Pair (1) := PCM (Mix_Right (From + Frame));
         end;
      end loop;
   end Fill;

   --  Open the mixer stream (at start, and retried while the mixer's device
   --  comes up). The stream starts with a full ring.
   procedure Open_Audio (Now : Unsigned_64);

   procedure Refill_Audio (Now : Unsigned_64) is
      Status : CuBit.Audio.Stream_Statistics;
      Reservation : CuBit.Audio.WriteReservation;
      Wanted, Frames : Natural;
      Committed : Boolean;
   begin
      if not Audio_On then
         --  No device: the synthesizer still runs (the composer is the
         --  game's clock), a fixed lead ahead of the song clock, once
         --  that clock has started.
         if not Clock_Ready then
            return;
         end if;
         declare
            Wanted_Head : constant Samples :=
              Samples (Max (0.0, Song_At (Now))) + SILENT_LEAD_SAMPLES;
         begin
            while Neon_Game.Write_Head (G.all) < Wanted_Head loop
               Frames := Natural'Min (Neon_Synth.Maximum_Block,
                                      Natural (Wanted_Head - Neon_Game.Write_Head (G.all)));
               Neon_Game.Render_Audio (G.all, Mix_Left, Mix_Right, Frames);
            end loop;
         end;
         return;
      end if;
      for Round in 1 .. 4 loop
         Status := CuBit.Audio.Statistics (Stream);
         exit when not Status.Valid;
         Queued_Now := Status.Queued_Frames;
         Underruns_Now := Status.Underruns;
         exit when Status.Queued_Frames >= AUDIO_TARGET_FRAMES;
         Wanted := Natural'Min (Neon_Synth.Maximum_Block, AUDIO_TARGET_FRAMES - Status.Queued_Frames);
         Reservation := CuBit.Audio.reserveWrite (Stream, Wanted);
         exit when not CuBit.Audio.isReservationValid (Reservation);
         Frames := CuBit.Audio.reservedFrames (Reservation);
         Neon_Game.Render_Audio (G.all, Mix_Left, Mix_Right, Frames);
         Fill (CuBit.Audio.firstSpan (Reservation), 1);
         Fill (CuBit.Audio.secondSpan (Reservation), 1 + CuBit.Audio.firstSpan (Reservation).frames);
         CuBit.Audio.commitWrite (Stream, Reservation, Frames, Committed);
         exit when not Committed;
      end loop;
      if Now - Last_Playback_Us >= PLAYBACK_POLL_US then
         Last_Playback_Us := Now;
         declare
            P : constant CuBit.Audio.Playback_Status := CuBit.Audio.Playback (Stream);
         begin
            if P.Valid then
               Device_Frames := P.Device_Frames;
            end if;
         end;
      end if;
   end Refill_Audio;

   procedure Open_Audio (Now : Unsigned_64) is
   begin
      if Audio_On or else not Startup.Mixer.Present or else Audio_Attempts >= AUDIO_RETRIES
        or else Now < Next_Audio_Try_Us
      then
         return;
      end if;
      Audio_Attempts := Audio_Attempts + 1;
      Next_Audio_Try_Us := Now + AUDIO_RETRY_US;
      if Audio_Attempts = 1 then
         CuBit.Audio.Bind (Startup.Mixer.Value);
      end if;
      Stream := CuBit.Audio.open (Sample_Rate, 2);
      if not CuBit.Audio.isValid (Stream) then
         if Audio_Attempts = AUDIO_RETRIES then
            debugPrint ("neon-blast: audio off (the mixer refused the stream "
                        & Decimal (AUDIO_RETRIES) & " times)" & ASCII.LF);
         end if;
         return;
      end if;
      Audio_On := True;
      Clock_Ready := False;   --  the song clock follows the audio from now on
      Refill_Audio (Now);
      Underruns_Start := CuBit.Audio.Statistics (Stream).Underruns;
      CuBit.Audio.start (Stream);
      debugPrint ("neon-blast: audio stream open (48 kHz stereo, lent ring) after "
                  & Decimal (Audio_Attempts) & " attempt(s)" & ASCII.LF);
   end Open_Audio;

   --  The song position being heard now, from the audio side: what was
   --  written, less what waits in the ring and the device's periods, less
   --  the master chain's own delay.
   function Heard_Estimate return Float is
     (Float (Neon_Game.Write_Head (G.all)) - Float (Queued_Now) - Float (Device_Frames)
      - Float (Neon_Synth.Output_Latency));

   procedure Update_Clock (Now : Unsigned_64) is
   begin
      if not Clock_Ready then
         Base_Us := Now;
         Base_Song := (if Audio_On then Max (0.0, Heard_Estimate) else 0.0);
         Clock_Ready := True;
         return;
      end if;
      if Audio_On then
         Drift_Samples := Heard_Estimate - Song_At (Now);
         if abs Drift_Samples > Float (CLOCK_SNAP_SAMPLES) then
            Base_Us := Now;
            Base_Song := Max (0.0, Heard_Estimate);
         else
            Base_Song := Base_Song + CLOCK_PULL * Drift_Samples;
         end if;
      end if;
   end Update_Clock;

   ---------------------------------------------------------------------------
   --  Input to controls
   ---------------------------------------------------------------------------

   function Axis (Snap : Snapshot; Positive_Key, Negative_Key : Key) return Float is
     ((if Active (Snap, Positive_Key) then 1.0 else 0.0) - (if Active (Snap, Negative_Key) then 1.0 else 0.0));

   function Gather (Snap : Snapshot) return Neon_Sim.Controls is
      C : Neon_Sim.Controls;
      Win_Canvas : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (Win);
      Cam : constant Neon_Scene.Camera :=
        Neon_Scene.Camera_For (Positive'Max (1, Win_Canvas.width), Positive'Max (1, Win_Canvas.height));
      Playing : constant Boolean := G.Sim.Phase = Neon_Sim.Playing;
   begin
      C.Move := (Axis (Snap, D, A) + Axis (Snap, Right, Left), Axis (Snap, W, S) + Axis (Snap, Up, Down));
      C.Aim := Neon_Scene.Floor_Point (Cam, Physical (Pointer_X, Win_Canvas), Physical (Pointer_Y, Win_Canvas));
      C.Fire := (Buttons and BUTTON_LEFT) /= 0 or else Is_Down (Snap, J);
      C.Dash := Was_Pressed (Snap, Space) or else Right_Pressed;
      C.Confirm := Was_Pressed (Snap, Enter) or else (Click_Pressed and then not Playing);
      C.Calibrate := Was_Pressed (Snap, Neon_Keys.C);
      C.Tap := Was_Pressed (Snap, Space) or else Click_Pressed;
      if Last_Press_Us /= 0 then
         declare
            Error : Float;
            Found : Boolean;
         begin
            Neon_Game.Beat_Error_Ms (G.all, Samples (Max (0.0, Song_At (Last_Press_Us))), Error, Found);
            C.Press_Judged := Found;
            C.Press_Error_Ms := Error;
         end;
      end if;
      return C;
   end Gather;

   procedure Step_Game (Now : Unsigned_64) is
      Snap : Snapshot;
      Target : constant Float := Song_At (Now) + Display_Lead;
      Controls_Now : Neon_Sim.Controls;
   begin
      --  Nothing to step this turn (the loop wakes every millisecond, the
      --  simulation steps every 4.2): keep the presses for the next step.
      if Target <= 0.0 or else Samples (Target) <= Neon_Game.Sim_Time (G.all) then
         return;
      end if;
      Take_Snapshot (Board, Snap);
      if G.Sim.Demo and then G.Sim.Phase /= Neon_Sim.Title and then Last_Press_Us /= 0 then
         --  A person is back: end the demo and show the title.
         declare
            Keep_Scores : constant Neon_Sim.Score_Table := G.Sim.Scores;
            Keep_Offset : constant Float := G.Sim.Calibration_Offset_Ms;
         begin
            Neon_Sim.Reset (G.Sim, SEED);
            G.Sim.Scores := Keep_Scores;
            G.Sim.Calibration_Offset_Ms := Keep_Offset;
            G.Sim.Clock := Neon_Game.Sim_Time (G.all);
         end;
         Last_Press_Us := 0;
         Click_Pressed := False;
      end if;
      if G.Sim.Demo and then G.Sim.Phase /= Neon_Sim.Title then
         Controls_Now := Neon_Autopilot.Drive (G.Sim, Neon_Game.Sim_Time (G.all));
      elsif G.Sim.Phase = Neon_Sim.Title and then Now - Last_Input_Us > DEMO_IDLE_US then
         Controls_Now := (Confirm => True, Demo => True, others => <>);
         Last_Input_Us := Now;
      else
         Controls_Now := Gather (Snap);
      end if;
      if Samples (Target) > Neon_Game.Sim_Time (G.all) then
         Neon_Game.Advance (G.all, Controls_Now, Samples (Target));
         if Last_Press_Us /= 0 then
            Pending_Input_Us := Last_Press_Us;
            Last_Press_Us := 0;
         end if;
         if Sound_Press_Us /= 0 then
            --  Its sound starts at the write head: heard once the queue
            --  ahead of it (ring, device periods, the tape stage) has played.
            Note (Key_To_Sound,
                  Float (Now - Sound_Press_Us) / 1000.0
                  + (Float (Neon_Game.Write_Head (G.all)) - Song_At (Now)) * MICROSECONDS_PER_SAMPLE / 1000.0);
            Sound_Press_Us := 0;
         end if;
         Click_Pressed := False;
         Right_Pressed := False;
      end if;
   end Step_Game;

   ---------------------------------------------------------------------------
   --  Painting
   ---------------------------------------------------------------------------

   procedure Draw_Overlay (Into : CuBit.UI.Canvas) is
      Lines : constant Natural := 8;
      Line_H : constant Natural := UI_Text_Height + 3;
      Box : constant Rect := (4, 4, OVERLAY_WIDTH, Lines * Line_H + 2 * OVERLAY_INSET);
      Y : Natural := Box.y + OVERLAY_INSET;
      procedure Put (Text : String; Ink : Color := OVERLAY_INK) is
      begin
         Draw_UI_Text (Into, Box.x + OVERLAY_INSET, Y, Text, Ink, OVERLAY_BACKGROUND);
         Y := Y + Line_H;
      end Put;
      Held_Text : String (1 .. 80) := [others => ' '];
      Held_Last : Natural := 0;
      Path : constant String := (if GPU_Started then "Vulkan (GPU canvas)" else "software");
      Kills : constant Neon_Sim.Judgement := G.Sim.Kill_Timing;
      Presses : constant Neon_Sim.Judgement := G.Sim.Press_Timing;
   begin
      Fill_Rect (Into, Box, OVERLAY_BACKGROUND);
      Put ("NEON BLAST  " & Path & "  " & Decimal (Into.width) & "x" & Decimal (Into.height), OVERLAY_ACCENT);
      Put ("frame " & Tenths (Frame_Interval.Last) & " ms (avg " & Tenths (Mean (Frame_Interval))
           & ")  render " & Tenths (Render_Time.Last) & " ms  GPU "
           & (if Neon_GPU.Last_GPU_Us = 0 then "n/a" else Tenths (Float (Neon_GPU.Last_GPU_Us) / 1000.0) & " ms"));
      Put ("present " & Tenths (Present_Latency.Last) & " ms  input->present " & Tenths (Input_To_Present.Last)
           & " ms (avg " & Tenths (Mean (Input_To_Present)) & ", max " & Tenths (Input_To_Present.Peak) & ")");
      Put ("audio " & (if Audio_On then "on" else "off (no mixer)") & "  queue " & Decimal (Queued_Now)
           & "+" & Decimal (Device_Frames) & " fr  key->sound " & Tenths (Key_To_Sound.Last) & " ms",
           (if Audio_On then OVERLAY_INK else OVERLAY_WARN));
      Put ("underruns " & Decimal (Natural (Underruns_Now - Underruns_Start)) & "  voices "
           & Decimal (Neon_Synth.Voices_Sounding (G.Synth)) & "  dropped notes "
           & Decimal (Neon_Synth.Dropped_Notes (G.Synth)),
           (if Underruns_Now /= Underruns_Start then OVERLAY_WARN else OVERLAY_INK));
      Put ("A/V drift " & Tenths (Drift_Samples * MICROSECONDS_PER_SAMPLE / 1000.0) & " ms  beat->photon "
           & Tenths (Beat_To_Photon.Last) & " ms");
      Put ("key->beat " & Tenths (Neon_Sim.Mean (Presses)) & " +- " & Tenths (Neon_Sim.Spread (Presses))
           & " ms  kill->beat " & Tenths (Neon_Sim.Mean (Kills)) & " ms  offset "
           & Tenths (G.Sim.Calibration_Offset_Ms));
      for K in Known_Key loop
         if Held (Board, K) and then Held_Last + Label (K)'Length + 1 <= Held_Text'Last then
            Held_Text (Held_Last + 1 .. Held_Last + Label (K)'Length) := Label (K);
            Held_Last := Held_Last + Label (K)'Length + 1;
         end if;
      end loop;
      Put ("held (" & Decimal (Held_Count (Board)) & "): " & Held_Text (1 .. Held_Last), OVERLAY_ACCENT);
   end Draw_Overlay;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      pragma Unreferenced (Damage);
      Window_Canvas : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (Win);
      Full : constant Rect := CuBit.UI.App.Full_Rect (Win);
      Started_Us : constant Unsigned_64 := Now_Us;
   begin
      if GPU_Started then
         GC.Set_Area (Canvas, Window_Canvas, Full);
         GC.Paint (Canvas, Window_Canvas);
      elsif Window_Canvas.width > 0 and then Window_Canvas.height > 0
        and then Window_Canvas.pitch >= 4 * Window_Canvas.width
      then
         Neon_Scene.Build (Scene.all, G.all, Window_Canvas.width, Window_Canvas.height);
         Neon_Soft.Render (Soft, Scene.all, Window_Canvas.addr, Window_Canvas.width, Window_Canvas.height,
                           Window_Canvas.pitch / 4);
         Note (Render_Time, Float (Now_Us - Started_Us) / 1000.0);
         Painted_Song := Float (G.Sim.Clock);
      end if;
      if Overlay then
         Draw_Overlay (Window_Canvas);
      end if;
      Painted_At_Us := Started_Us;
      Painted_Input_Us := Pending_Input_Us;
      Pending_Input_Us := 0;
      Awaiting_Publish := True;
      if Last_Frame_Us /= 0 then
         Note (Frame_Interval, Float (Started_Us - Last_Frame_Us) / 1000.0);
      end if;
      Last_Frame_Us := Started_Us;
      Window_Frames := Window_Frames + 1;
      if Window_Frames = 1 then
         debugPrint ("neon-blast: first frame painted (" & (if GPU_Started then "vulkan" else "software")
                     & ")" & ASCII.LF);
      end if;
   end Render;

   --  After the frame was published: its latencies.
   procedure Note_Publish is
      Publish : constant CuBit.UI.App.Publish_Record := CuBit.UI.App.Last_Publish;
   begin
      if not Awaiting_Publish or else Publish.At_Us < Painted_At_Us then
         return;
      end if;
      Awaiting_Publish := False;
      if not Publish.Accepted then
         return;
      end if;
      Note (Present_Latency, Float (Publish.At_Us - Painted_At_Us) / 1000.0);
      if Painted_Input_Us /= 0 and then Publish.At_Us >= Painted_Input_Us then
         Note (Input_To_Present, Float (Publish.At_Us - Painted_Input_Us) / 1000.0);
      end if;
      --  When the frame was published versus when its song moment is heard.
      Note (Beat_To_Photon,
            (Float (Long_Long_Integer (Publish.At_Us) - Long_Long_Integer (Base_Us))
             - (Painted_Song - Base_Song) * MICROSECONDS_PER_SAMPLE) / 1000.0);
      --  Show frames that much ahead next time (at most 50 ms).
      Display_Lead := Clamp (Display_Lead + 0.1 * (Float (Publish.At_Us - Painted_At_Us) / MICROSECONDS_PER_SAMPLE
                                                     - Display_Lead), 0.0, 2_400.0);
   end Note_Publish;

   procedure Report_Stats (Now : Unsigned_64) is
   begin
      if Stats_Us = 0 then
         Stats_Us := Now;
         return;
      end if;
      if Now - Stats_Us < STATS_INTERVAL_US then
         return;
      end if;
      Stats_Us := Now;
      debugPrint ("neon-blast: stats path=" & (if GPU_Started then "vulkan" else "software")
                  & " frames=" & Decimal (Frame_Interval.Count)
                  & " frame_ms_avg=" & Tenths (Mean (Frame_Interval)) & " frame_ms_max=" & Tenths (Frame_Interval.Peak)
                  & " render_ms_avg=" & Tenths (Mean (Render_Time)) & " render_ms_max=" & Tenths (Render_Time.Peak)
                  & " present_ms_avg=" & Tenths (Mean (Present_Latency))
                  & " input_to_present_ms_avg=" & Tenths (Mean (Input_To_Present))
                  & " input_to_present_ms_max=" & Tenths (Input_To_Present.Peak)
                  & " presses=" & Decimal (Input_To_Present.Count)
                  & " key_to_sound_ms_avg=" & Tenths (Mean (Key_To_Sound))
                  & " audio=" & (if Audio_On then "on" else "off")
                  & " underruns=" & Decimal (Natural (Underruns_Now - Underruns_Start))
                  & " queue=" & Decimal (Queued_Now) & "+" & Decimal (Device_Frames)
                  & " drift_ms=" & Tenths (Drift_Samples * MICROSECONDS_PER_SAMPLE / 1000.0)
                  & " beat_to_photon_ms_avg=" & Tenths (Mean (Beat_To_Photon))
                  & " phase=" & (case G.Sim.Phase is
                                   when Neon_Sim.Title => "title", when Neon_Sim.Playing => "playing",
                                   when Neon_Sim.Dying => "dying", when Neon_Sim.Game_Over => "game-over",
                                   when Neon_Sim.Calibrating => "calibrating")
                  & " score=" & Decimal (G.Sim.Score) & " wave=" & Decimal (G.Sim.Wave)
                  & " demo=" & (if G.Sim.Demo then "yes" else "no") & ASCII.LF);
      Frame_Interval := (others => <>);
      Render_Time := (others => <>);
      Present_Latency := (others => <>);
      Input_To_Present := (others => <>);
      Key_To_Sound := (others => <>);
      Beat_To_Photon := (others => <>);
   end Report_Stats;

   ---------------------------------------------------------------------------
   --  The loop's hooks
   ---------------------------------------------------------------------------

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event; Dirty : in out Rect;
      Running : in out Boolean)
   is
      Now : constant Unsigned_64 := Now_Us;
      Stamp : constant Timestamp := Timestamp (Now);
   begin
      if Event.kind = Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = Input.INPUT_CONFIGURE then
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = Input.INPUT_RESYNC then
         --  Input may have been lost: no key stays held.
         Release_All (Board, Stamp);
         Buttons := 0;
      elsif Event.kind = Input.INPUT_KEY_DOWN then
         declare
            K : constant Key := Decode (Event.payload0);
         begin
            Press (Board, K, Input.Is_Repeat (Event), Stamp);
            if not Input.Is_Repeat (Event) then
               Last_Input_Us := Now;
               case K is
                  when Escape =>
                     Running := False;
                  when F3 =>
                     Overlay := not Overlay;
                  when F9 =>
                     Benchmark_Requested := G.Sim.Phase = Neon_Sim.Title;
                  when Space =>
                     Last_Press_Us := Now;
                     Sound_Press_Us := Now;
                  when others =>
                     Last_Press_Us := Now;
               end case;
            end if;
         end;
      elsif Event.kind = Input.INPUT_KEY_UP then
         Release (Board, Decode (Event.payload0), Stamp);
      elsif Event.kind in Input.INPUT_POINTER_MOVE | Input.INPUT_POINTER_DOWN | Input.INPUT_POINTER_UP then
         declare
            Pressed : constant Unsigned_64 := Input.Pointer_Buttons (Event) and not Buttons;
         begin
            Pointer_X := Input.Pointer_X (Event);
            Pointer_Y := Input.Pointer_Y (Event);
            if (Pressed and BUTTON_LEFT) /= 0 then
               Click_Pressed := True;
               Last_Press_Us := Now;
               Sound_Press_Us := Now;
               Last_Input_Us := Now;
            end if;
            if (Pressed and BUTTON_RIGHT) /= 0 then
               Right_Pressed := True;
               Last_Press_Us := Now;
               Sound_Press_Us := Now;
               Last_Input_Us := Now;
            end if;
            Buttons := Input.Pointer_Buttons (Event);
         end;
      end if;
   end Handle_Event;

   --  F9 on the title: the replay benchmark (Neon_Replay), the same run as
   --  tests/neon-blast/run.sh's (seed 7, 15 s, 640 x 400), so its hashes
   --  must equal the Linux-hosted ones. The music pauses while it runs.
   BENCH_SEED : constant := 7;
   BENCH_SECONDS : constant := 15;
   BENCH_WIDTH : constant := 640;
   BENCH_HEIGHT : constant := 400;
   procedure Run_Benchmark is
      type Replay_Access is access Neon_Replay.Replay;
      type Bench_Pixels is array (0 .. BENCH_WIDTH * BENCH_HEIGHT - 1) of Unsigned_32;
      type Pixels_Access is access Bench_Pixels;
      R : constant Replay_Access := new Neon_Replay.Replay;
      Pixels : constant Pixels_Access := new Bench_Pixels;
      Steps : Average;
      Began : Unsigned_64;
   begin
      debugPrint ("neon-blast: replay benchmark starting" & ASCII.LF);
      if Audio_On then
         CuBit.Audio.pause (Stream);
      end if;
      Neon_Replay.Start (R.all, BENCH_SEED);
      for Frame in 1 .. BENCH_SECONDS * Neon_Replay.Frames_Per_Second loop
         Began := Now_Us;
         Neon_Replay.Step (R.all, Pixels.all'Address, BENCH_WIDTH, BENCH_HEIGHT);
         Note (Steps, Float (Now_Us - Began) / 1000.0);
      end loop;
      declare
         O : constant Neon_Replay.Result := Neon_Replay.Outcome (R.all);
      begin
         debugPrint ("neon-blast: replay frames=" & Decimal (O.Frames) & " seed=" & Decimal (BENCH_SEED)
                     & " size=" & Decimal (BENCH_WIDTH) & "x" & Decimal (BENCH_HEIGHT)
                     & " frame_hash=" & Unsigned_64'Image (O.Frame_Hash)
                     & " audio_hash=" & Unsigned_64'Image (O.Audio_Hash)
                     & " score=" & Decimal (O.Score) & " wave=" & Decimal (O.Wave)
                     & " step_ms_avg=" & Tenths (Mean (Steps)) & " step_ms_max=" & Tenths (Steps.Peak)
                     & ASCII.LF);
      end;
      if Audio_On then
         CuBit.Audio.start (Stream);
      end if;
      Clock_Ready := False;   --  the song clock restarts from the audio side
   end Run_Benchmark;

   function Deadline return Unsigned_64 is (CuBit.Monotonic.Milliseconds + POLL_MILLISECONDS);

   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Running);
      Now : constant Unsigned_64 := Now_Us;
   begin
      if Benchmark_Requested then
         Benchmark_Requested := False;
         Run_Benchmark;
      end if;
      Note_Publish;
      Open_Audio (Now);
      Refill_Audio (Now);
      Update_Clock (Now);
      Step_Game (Now);
      if GPU_Started then
         declare
            New_Frame : Boolean;
            Status : GC.Acquire_Status;
            Recorded : constant Unsigned_64 := Now_Us;
         begin
            GC.Poll (Canvas, New_Frame);
            if New_Frame then
               Dirty := Union_Rect (Dirty, GC.Area (Canvas));
            end if;
            Neon_GPU.Draw (Canvas, Scene.all, G.all, Status);
            if Status = GC.Acquired then
               Note (Render_Time, Float (Now_Us - Recorded) / 1000.0);
               Painted_Song := Float (G.Sim.Clock);
            end if;
            if GC.State (Canvas) /= GC.Ready then
               GPU_Started := False;    --  fall back to software
               debugPrint ("neon-blast: GPU canvas lost; software rendering" & ASCII.LF);
            end if;
         end;
      elsif (Now >= Next_Soft_Us or else Pending_Input_Us /= 0)
        and then not CuBit.UI.App.Frame_Pending (Win)
      then
         --  A press is drawn at once, not at the next 60 Hz slot: the
         --  frame pacing would otherwise add up to a frame of input lag.
         Next_Soft_Us := Now + SOFT_FRAME_US;
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
      Report_Stats (Now);
   end Tick;

   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event, Next_Deadline => Deadline,
      On_Deadline => Tick, Activity_Wait => True);
begin
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("neon-blast: starting" & ASCII.LF);
   CuBit.Logging.Announce ("neon-blast: started", Opened, Startup.Logstore);
   declare
      Open_Outcome : CuBit.UI.App.Open_Result;
   begin
      CuBit.UI.App.Open
        (Win, INITIAL_WIDTH, INITIAL_HEIGHT,
         CuBit.Desktop_Protocol.Feature_Bits
           ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
             CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
             CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
             others => False]),
         Open_Outcome, title => "Neon Blast", protected_frames => True,
         minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
      Opened := Open_Outcome in CuBit.UI.App.Opened;
   end;
   if not Opened then
      debugPrint ("neon-blast: no window" & ASCII.LF);
      return;
   end if;
   CuBit.UI.App.Trace_Wakes := True;   --  publish times for the latency overlay

   Neon_Game.Start (G.all, SEED);

   Open_Audio (Now_Us);
   if not Startup.Mixer.Present then
      debugPrint ("neon-blast: audio off (no mixer endpoint)" & ASCII.LF);
   end if;

   GC.Open (Canvas);
   Neon_GPU.Start (Canvas, GPU_Started);
   GPU_Started := GPU_Started and then GC.State (Canvas) = GC.Ready and then Neon_GPU.Started;
   debugPrint ("neon-blast: renderer " &
     (if GPU_Started then "vulkan (GPU canvas)"
      else "software (" & (case GC.State (Canvas) is
                             when GC.Ready => "pipeline creation failed",
                             when GC.Unavailable => "no render session",
                             when GC.Failed => "GPU canvas failed",
                             when GC.Closed => "closed") & ")") & ASCII.LF);
   Last_Input_Us := Now_Us;
   Run (Win);
   if Audio_On then
      CuBit.Audio.pause (Stream);
      CuBit.Audio.close (Stream);
   end if;
   Neon_GPU.Stop;
   GC.Close (Canvas);
   CuBit.UI.App.Close (Win);
   debugPrint ("neon-blast: closed" & ASCII.LF);
end Main;
