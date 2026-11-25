------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted: Neon Blast's dynamic resolution against the real software
--  renderer (APP-008). Not a CuBit result.
--
--  @description
--  dynres_replay same SECONDS
--    The replay (seed 7) three ways: at a fixed 640 x 400 (Neon_Replay.Step,
--    the benchmark's hashes), at a size that changes every frame through
--    the ladder, and through Neon_Present on a 2560 x 1600 window at
--    whatever levels this host's timing picks. The game's outcome (audio
--    hash, score, stage, kills, on-beat kills) must be the same all three
--    ways: the game does not depend on the render resolution.
--  dynres_replay cost WIDTH HEIGHT SECONDS
--    Each level's render and scale time on a WIDTH x HEIGHT window, the
--    mean over SECONDS of the replay.
--  dynres_replay loop WIDTH HEIGHT SECONDS SMALL_WIDTH SMALL_HEIGHT
--    The controller closed on the real renderer: SECONDS on the big window,
--    then SECONDS on the small one; the level and times once a second.
------------------------------------------------------------------------------
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Neon_Present;
with Neon_Replay;
with Neon_Resolution; use Neon_Resolution;
with Neon_Scene;
with Neon_Soft;
with Neon_Upscale;

procedure Dynres_Replay is
   Epoch : constant Time := Clock;
   function Now_Us return Unsigned_64 is
     (Unsigned_64 (To_Duration (Clock - Epoch) * 1_000_000));
   package Present is new Neon_Present (Now_Us);

   type Replay_Access is access Neon_Replay.Replay;
   type Pixels_Access is access Neon_Upscale.Pixel_Array;
   type Index_Access is access Neon_Upscale.Index_Array;
   type Weight_Access is access Neon_Upscale.Weight_Array;
   Failed : Boolean := False;

   function Image (N : Integer) return String is
      S : constant String := Integer'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   function Milliseconds (Us : Natural) return String is
      Tenths : constant Natural := (Us + 50) / 100;
   begin
      return Image (Tenths / 10) & "." & Image (Tenths mod 10);
   end Milliseconds;

   procedure Check (Ok : Boolean; What : String) is
   begin
      Put_Line ("NEON-DYNRES: " & (if Ok then "PASS " else "FAIL ") & What);
      Failed := Failed or else not Ok;
   end Check;

   function Outcome_Line (O : Neon_Replay.Result) return String is
     ("audio_hash=" & Unsigned_64'Image (O.Audio_Hash) & " score=" & Image (O.Score) & " stage="
      & Image (O.Stage) & " kills=" & Image (O.Kills) & " on_beat=" & Image (O.On_Beat));

   function Same_Game (A, B : Neon_Replay.Result) return Boolean is
     (A.Audio_Hash = B.Audio_Hash and then A.Score = B.Score and then A.Stage = B.Stage
      and then A.Kills = B.Kills and then A.On_Beat = B.On_Beat and then A.Frames = B.Frames);

   procedure Same (Seconds : Positive) is
      Frames : constant Positive := Seconds * Neon_Replay.Frames_Per_Second;
      Seed : constant := 7;
      Big_W : constant := 2560;
      Big_H : constant := 1600;
      Fixed, Cycled, Presented : constant Replay_Access := new Neon_Replay.Replay;
      Small : constant Pixels_Access := new Neon_Upscale.Pixel_Array (0 .. 640 * 400 - 1);
      Big : constant Pixels_Access := new Neon_Upscale.Pixel_Array (0 .. Big_W * Big_H - 1);
      P : Present.Presenter;
      Levels_Seen : array (Level) of Boolean := [others => False];
      Distinct : Natural := 0;
      L : Level;
   begin
      Neon_Replay.Start (Fixed.all, Seed);
      Neon_Replay.Start (Cycled.all, Seed);
      Neon_Replay.Start (Presented.all, Seed);
      for Frame in 1 .. Frames loop
         Neon_Replay.Step (Fixed.all, Small.all'Address, 640, 400);

         --  Every frame a different level of the ladder.
         Neon_Replay.Advance (Cycled.all);
         L := Level (Frame mod (Natural (Lowest) + 1));
         Neon_Scene.Build (Cycled.Frame, Cycled.Game, Internal (640, L), Internal (400, L));
         Neon_Soft.Render (Cycled.Renderer, Cycled.Frame, Small.all'Address, Internal (640, L),
                           Internal (400, L), Internal (640, L));

         Neon_Replay.Advance (Presented.all);
         Present.Paint (P, Presented.Renderer, Presented.Frame, Presented.Game, Big.all'Address,
                        Big_W, Big_H, Big_W);
         L := Current (Present.Control (P));
         if not Levels_Seen (L) then
            Levels_Seen (L) := True;
            Distinct := Distinct + 1;
         end if;
      end loop;
      declare
         A : constant Neon_Replay.Result := Neon_Replay.Outcome (Fixed.all);
         B : constant Neon_Replay.Result := Neon_Replay.Outcome (Cycled.all);
         C : constant Neon_Replay.Result := Neon_Replay.Outcome (Presented.all);
      begin
         Put_Line ("NEON-DYNRES: fixed 640x400     " & Outcome_Line (A) & " frame_hash="
                   & Unsigned_64'Image (A.Frame_Hash));
         Put_Line ("NEON-DYNRES: cycled levels     " & Outcome_Line (B));
         Put_Line ("NEON-DYNRES: presented 2560x1600 " & Outcome_Line (C) & " levels_seen=" & Image (Distinct)
                   & " drops=" & Image (Integer (Drops (Present.Control (P))))
                   & " raises=" & Image (Integer (Raises (Present.Control (P)))));
         Check (Same_Game (A, B), "a render size changing every frame leaves the game unchanged");
         Check (Same_Game (A, C), "the dynamic resolution's levels leave the game unchanged");
         Check (A.Score > 0, "the replay played (score" & Natural'Image (A.Score) & ")");
      end;
   end Same;

   --  Each level's cost on a Width x Height window.
   procedure Cost (Width, Height : Extent; Seconds : Positive) is
      Frames : constant Positive := Seconds * Neon_Replay.Frames_Per_Second;
      R : constant Replay_Access := new Neon_Replay.Replay;
      Window : constant Pixels_Access := new Neon_Upscale.Pixel_Array (0 .. Width * Height - 1);
      Inner : constant Pixels_Access := new Neon_Upscale.Pixel_Array (0 .. Width * Height - 1);
      Rows : constant Pixels_Access := new Neon_Upscale.Pixel_Array (0 .. 2 * Width - 1);
      Left : constant Index_Access := new Neon_Upscale.Index_Array (0 .. Width - 1);
      Right : constant Index_Access := new Neon_Upscale.Index_Array (0 .. Width - 1);
      Weights : constant Weight_Access := new Neon_Upscale.Weight_Array (0 .. Width - 1);
      Render_Total, Scale_Total : array (Level) of Duration := [others => 0.0];
      T0, T1, T2 : Time;
      IW, IH : Extent;
   begin
      Neon_Replay.Start (R.all, 7);
      for Frame in 1 .. Frames loop
         Neon_Replay.Advance (R.all);
         for L in Level loop
            IW := Internal (Width, L);
            IH := Internal (Height, L);
            T0 := Clock;
            Neon_Scene.Build (R.Frame, R.Game, IW, IH);
            R.Frame.Scanlines := L = Native;
            if L = Native then
               Neon_Soft.Render (R.Renderer, R.Frame, Window.all'Address, Width, Height, Width);
            else
               Neon_Soft.Render (R.Renderer, R.Frame, Inner.all'Address, IW, IH, IW);
            end if;
            R.Frame.Scanlines := True;
            T1 := Clock;
            if L /= Native then
               Neon_Upscale.Scale (Inner.all, IW, IH, Window.all, Width, Height, Width, Rows.all, Left.all,
                                   Right.all, Weights.all, True);
            end if;
            T2 := Clock;
            Render_Total (L) := Render_Total (L) + To_Duration (T1 - T0);
            Scale_Total (L) := Scale_Total (L) + To_Duration (T2 - T1);
         end loop;
      end loop;
      for L in Level loop
         Put_Line ("NEON-DYNRES: cost " & Image (Width) & "x" & Image (Height) & " level " & Image (Integer (L))
                   & " " & Image (Scale_Of (L)) & "% internal " & Image (Internal (Width, L)) & "x"
                   & Image (Internal (Height, L)) & " render_ms="
                   & Milliseconds (Natural (Render_Total (L) / Frames * 1_000_000))
                   & " scale_ms=" & Milliseconds (Natural (Scale_Total (L) / Frames * 1_000_000)));
      end loop;
   end Cost;

   --  The controller on the real renderer: the big window, then the small.
   procedure Closed_Loop (Width, Height : Extent; Seconds : Positive; Small_Width, Small_Height : Extent) is
      Frames : constant Positive := Seconds * Neon_Replay.Frames_Per_Second;
      R : constant Replay_Access := new Neon_Replay.Replay;
      Window : constant Pixels_Access := new Neon_Upscale.Pixel_Array (0 .. Width * Height - 1);
      P : Present.Presenter;
      Lowest_Seen : Level := Native;
      Late_Worst_Us : Natural := 0;
      Big_Level : Level;
      Second_Began : Unsigned_64;
      procedure Phase (W, H : Extent; Label : String) is
         Frames_In_Second, Sum_Us : Natural := 0;
      begin
         Late_Worst_Us := 0;
         Second_Began := Now_Us;
         for Frame in 1 .. Frames loop
            Neon_Replay.Advance (R.all);
            Present.Paint (P, R.Renderer, R.Frame, R.Game, Window.all'Address, W, H, W);
            declare
               Shown : constant Present.Report := Present.Last (P);
               Cost_Us : constant Natural := Shown.Render_Us + Shown.Scale_Us;
            begin
               Frames_In_Second := Frames_In_Second + 1;
               Sum_Us := Sum_Us + Cost_Us;
               if Current (Present.Control (P)) > Lowest_Seen then
                  Lowest_Seen := Current (Present.Control (P));
               end if;
               if Frame > Frames / 2 then
                  Late_Worst_Us := Natural'Max (Late_Worst_Us, Cost_Us);
               end if;
               if Now_Us - Second_Began >= 1_000_000 or else Frame = Frames then
                  Put_Line ("NEON-DYNRES: loop " & Label & " " & Image (W) & "x" & Image (H) & " internal "
                            & Image (Shown.Internal_Width) & "x" & Image (Shown.Internal_Height) & " ("
                            & Image (Shown.Scale) & "%) render+scale avg "
                            & Milliseconds (Sum_Us / Frames_In_Second) & " ms over "
                            & Image (Frames_In_Second) & " frames");
                  Frames_In_Second := 0;
                  Sum_Us := 0;
                  Second_Began := Now_Us;
               end if;
            end;
         end loop;
      end Phase;
   begin
      Neon_Replay.Start (R.all, 7);
      Phase (Width, Height, "big");
      Big_Level := Current (Present.Control (P));
      Put_Line ("NEON-DYNRES: loop big settled at " & Image (Scale_Of (Big_Level)) & "% worst late "
                & Milliseconds (Late_Worst_Us) & " ms");
      Phase (Small_Width, Small_Height, "small");
      Put_Line ("NEON-DYNRES: loop small settled at " & Image (Scale_Of (Current (Present.Control (P))))
                & "% worst late " & Milliseconds (Late_Worst_Us) & " ms; drops "
                & Image (Integer (Drops (Present.Control (P)))) & " raises "
                & Image (Integer (Raises (Present.Control (P)))));
      Check (Lowest_Seen > Native, "the big window dropped the resolution");
      Check (Current (Present.Control (P)) < Big_Level, "the small window raised it again");
   end Closed_Loop;

   Mode : constant String := (if Argument_Count >= 1 then Argument (1) else "");
begin
   if Mode = "same" then
      Same (Positive'Value (Argument (2)));
   elsif Mode = "cost" then
      Cost (Extent'Value (Argument (2)), Extent'Value (Argument (3)), Positive'Value (Argument (4)));
   elsif Mode = "loop" then
      Closed_Loop (Extent'Value (Argument (2)), Extent'Value (Argument (3)), Positive'Value (Argument (4)),
                   Extent'Value (Argument (5)), Extent'Value (Argument (6)));
   else
      Put_Line ("usage: dynres_replay same SECONDS | cost W H SECONDS | loop W H SECONDS SMALL_W SMALL_H");
      Failed := True;
   end if;
   if Failed then
      Set_Exit_Status (Failure);
   end if;
end Dynres_Replay;
