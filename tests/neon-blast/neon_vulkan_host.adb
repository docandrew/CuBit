------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted run of Neon Blast's Vulkan path: the GPU canvas widget on a
--  host Vulkan device (Mesa lavapipe, a CPU rasterizer), the autopilot
--  playing, frames read back into an in-memory window and written as PPM.
--  Checks the renderer works end to end and shows its look; says nothing
--  about Intel hardware or CuBit.
--
--  Usage: neon_vulkan_host OUT_DIR SECONDS SHOT_EVERY_FRAMES
------------------------------------------------------------------------------
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.GPU_Canvas;
with Neon_Autopilot;
with Neon_Game;
with Neon_GPU;
with Neon_Scene;
with Neon_Synth;
with Neon_Time; use Neon_Time;

procedure Neon_Vulkan_Host is
   package GC renames CuBit.UI.GPU_Canvas;
   use type GC.Availability, GC.Acquire_Status;

   Width : constant := 1280;
   Height : constant := 720;
   Out_Dir : constant String := Argument (1);
   Seconds_Run : constant Natural := Natural'Value (Argument (2));
   Shot_Every : constant Positive := Positive'Value (Argument (3));
   Frame_Samples : constant := Sample_Rate / 60;
   Gap : constant Samples := 1_600;
   Poll_Limit : constant := 2_000_000;

   type Pixel_Array is array (0 .. Width * Height - 1) of Unsigned_32;
   type Pixel_Access is access Pixel_Array;
   Pixels : constant Pixel_Access := new Pixel_Array'(others => 0);
   Window : constant Canvas :=
     (addr => Pixels.all'Address, width => Width, height => Height, pitch => Width * 4, others => <>);
   type Game_Access is access Neon_Game.Game;
   G : constant Game_Access := new Neon_Game.Game;
   type Frame_Access is access Neon_Scene.Frame;
   F : constant Frame_Access := new Neon_Scene.Frame;
   Widget : GC.Canvas_Widget;
   Left, Right : Neon_Synth.Block;
   OK : Boolean;
   Status : GC.Acquire_Status;
   Failures : Natural := 0;

   function Image (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   procedure Write_PPM (Name : String) is
      package SIO renames Ada.Streams.Stream_IO;
      File : SIO.File_Type;
      Row : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Width * 3));
      P : Unsigned_32;
   begin
      SIO.Create (File, SIO.Out_File, Out_Dir & "/" & Name & ".ppm");
      String'Write (SIO.Stream (File), "P6" & ASCII.LF & Natural'Image (Width) & Natural'Image (Height)
                    & ASCII.LF & "255" & ASCII.LF);
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            P := Pixels (Y * Width + X);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 1)) := Ada.Streams.Stream_Element (Shift_Right (P, 16) and 255);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 2)) := Ada.Streams.Stream_Element (Shift_Right (P, 8) and 255);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 3)) := Ada.Streams.Stream_Element (P and 255);
         end loop;
         SIO.Write (File, Row);
      end loop;
      SIO.Close (File);
   end Write_PPM;

   function Wait_Frame return Boolean is
      New_Frame : Boolean;
   begin
      for Attempt in 1 .. Poll_Limit loop
         GC.Poll (Widget, New_Frame);
         if New_Frame then
            return True;
         end if;
         exit when GC.State (Widget) /= GC.Ready;
      end loop;
      return False;
   end Wait_Frame;
begin
   GC.Open (Widget);
   GC.Set_Area (Widget, Window, (0, 0, Width, Height));
   Neon_GPU.Start (Widget, OK);
   if not (OK and then GC.State (Widget) = GC.Ready and then Neon_GPU.Started) then
      Put_Line ("FAIL Vulkan renderer did not start");
      Set_Exit_Status (Failure);
      return;
   end if;
   Put_Line ("PASS Vulkan renderer started on the host device");
   Neon_Game.Start (G.all, 7);
   for Frame in 1 .. Seconds_Run * 60 loop
      Neon_Game.Render_Audio (G.all, Left, Right, Frame_Samples);
      declare
         Head : constant Samples := Neon_Game.Write_Head (G.all);
         Target : constant Samples := (if Head > Gap then Head - Gap else 0);
      begin
         while Neon_Game.Sim_Time (G.all) < Target loop
            Neon_Game.Advance (G.all, Neon_Autopilot.Drive (G.Sim, Neon_Game.Sim_Time (G.all)),
                               Neon_Game.Sim_Time (G.all) + Tick_Samples);
         end loop;
      end;
      if Frame mod Shot_Every = 0 then
         Neon_GPU.Draw (Widget, F.all, G.all, Status);
         if Status /= GC.Acquired or else not Wait_Frame then
            Put_Line ("FAIL frame" & Natural'Image (Frame) & " not rendered: " & Status'Image);
            Failures := Failures + 1;
         else
            GC.Paint (Widget, Window);
            Write_PPM ("vulkan-" & Image (Frame));
            Put_Line ("PASS frame" & Natural'Image (Frame) & " lines" & Natural'Image (F.Line_Count)
                      & " points" & Natural'Image (F.Point_Count) & " gpu_us"
                      & Unsigned_64'Image (Neon_GPU.Last_GPU_Us));
         end if;
      end if;
   end loop;
   Neon_GPU.Stop;
   GC.Close (Widget);
   Put_Line (if Failures = 0 then "ALL PASS" else "FAILURES" & Natural'Image (Failures));
   Set_Exit_Status (if Failures = 0 then Success else Failure);
end Neon_Vulkan_Host;
