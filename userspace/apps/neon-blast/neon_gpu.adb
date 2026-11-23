------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with System;
with CuBit.Monotonic;
with Neon_Math; use Neon_Math;
with Neon_Scene; use Neon_Scene;

package body Neon_GPU is
   package GC renames CuBit.UI.GPU_Canvas;
   use type GC.Availability, GC.Acquire_Status;

   Status_OK : constant Unsigned_32 := 0;
   Reference_Height : constant := 1080.0;
   Kind_Line : constant := 0.0;
   Kind_Stroke : constant := 1.0;
   Kind_Point : constant := 2.0;

   --  shaders/scene.glsl's Globals: 8 rows, then lights and ripples.
   Header_Rows : constant := 8;
   Rows : constant := Header_Rows + 2 * Light_Ceiling + 2 * Ripple_Ceiling;
   type Globals is array (0 .. 4 * Rows - 1) of Float with Convention => C;

   --  One instance (shaders/segment.vert): A + width, B + kind, colour + size.
   type Instance is array (0 .. 11) of Float with Convention => C;
   Instance_Ceiling : constant := Line_Ceiling + Stroke_Ceiling + Point_Ceiling;
   type Instance_Array is array (1 .. Instance_Ceiling) of Instance with Convention => C;
   type Instance_Access is access Instance_Array;

   type Target_View is record
      Image, View, Ready : Unsigned_64 := 0;
      Width, Height, Format, Slot : Unsigned_32 := 0;
      Generation, Reserved : Unsigned_32 := 0;
   end record with Convention => C;

   function VK_Create (Device : access constant GC.Device_View; Format : Unsigned_32;
                       Maximum : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "neon_vk_create";
   function VK_Render (T : access constant Target_View; G : System.Address; Items : System.Address;
                       Count : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "neon_vk_render";
   function VK_GPU_Ns return Unsigned_64
     with Import, Convention => C, External_Name => "neon_vk_gpu_ns";
   procedure VK_Destroy
     with Import, Convention => C, External_Name => "neon_vk_destroy";

   Is_Started : Boolean := False;
   Items : Instance_Access;
   Header : Globals;
   Record_Us : Unsigned_64 := 0;

   procedure Start (Canvas : in out GC.Canvas_Widget; OK : out Boolean) is
      D : aliased constant GC.Device_View := GC.Device (Canvas);
   begin
      OK := True;
      if Is_Started or else GC.State (Canvas) /= GC.Ready then
         OK := Is_Started;
         return;
      end if;
      Items := new Instance_Array;
      OK := VK_Create (D'Access, GC.Target_Format, Instance_Ceiling) = Status_OK;
      Is_Started := OK;
   end Start;

   function Started return Boolean is (Is_Started);
   function Last_Record_Us return Unsigned_64 is (Record_Us);
   function Last_GPU_Us return Unsigned_64 is (if Is_Started then VK_GPU_Ns / 1000 else 0);

   procedure Fill_Globals (F : Frame) is
      Scale : constant Float := Float (F.Height) / Reference_Height;
      procedure Row (Index : Natural; A, B, C, D : Float) is
      begin
         Header (4 * Index) := A;
         Header (4 * Index + 1) := B;
         Header (4 * Index + 2) := C;
         Header (4 * Index + 3) := D;
      end Row;
      Cam : constant Camera := F.Cam;
   begin
      Header := [others => 0.0];
      Row (0, Float (F.Width), Float (F.Height), Scale, F.Seconds);
      Row (1, Cam.Position.X, Cam.Position.Y, Cam.Position.Z, Cam.Focal);
      Row (2, Cam.Up.X, Cam.Up.Y, Cam.Up.Z, Cam.Center_X + F.Shake.X * Scale);
      Row (3, Cam.Forward.X, Cam.Forward.Y, Cam.Forward.Z, Cam.Center_Y + F.Shake.Y * Scale);
      Row (4, Cam.Bend_Start, Cam.Bend, F.Scroll, F.Kick);
      Row (5, F.Sun_Swell, F.Glow, F.Underwater, (if F.Scanlines then 1.0 else 0.0));
      Row (6, 0.15 * F.Flash + 0.45 * F.Hurt, 0.12 * F.Flash, 0.15 * F.Flash + 0.08 * F.Hurt, F.Hurt);
      Row (7, Float (F.Light_Count), Float (F.Ripple_Count), Sun_Radius, 0.0);
      for I in 1 .. F.Light_Count loop
         declare
            L : constant Light := F.Lights (I);
         begin
            Row (Header_Rows + 2 * (I - 1), L.Position.X, L.Position.Y, L.Position.Z, L.Radius);
            Row (Header_Rows + 2 * (I - 1) + 1, L.Color.R, L.Color.G, L.Color.B, 0.0);
         end;
      end loop;
      for I in 1 .. F.Ripple_Count loop
         declare
            R : constant Floor_Ripple := F.Ripples (I);
            Base : constant Natural := Header_Rows + 2 * Light_Ceiling + 2 * (I - 1);
         begin
            Row (Base, R.Center.X, R.Center.Y, R.Radius, R.Width);
            Row (Base + 1, R.Color.R, R.Color.G, R.Color.B, R.Strength);
         end;
      end loop;
   end Fill_Globals;

   function Fill_Instances (F : Frame) return Natural is
      Count : Natural := 0;
      procedure Put (A, B : Vec3; Width, Kind : Float; C : RGB; Size : Float) is
      begin
         Count := Count + 1;
         Items (Count) := [A.X, A.Y, A.Z, Width, B.X, B.Y, B.Z, Kind, C.R, C.G, C.B, Size];
      end Put;
   begin
      for I in 1 .. F.Line_Count loop
         Put (F.Lines (I).A, F.Lines (I).B, F.Lines (I).Width, Kind_Line, F.Lines (I).Color, 0.0);
      end loop;
      for I in 1 .. F.Point_Count loop
         Put (F.Points (I).Position, F.Points (I).Position, 0.0, Kind_Point, F.Points (I).Color,
              F.Points (I).Size);
      end loop;
      for I in 1 .. F.Stroke_Count loop
         Put ((F.Strokes (I).A.X, F.Strokes (I).A.Y, 0.0), (F.Strokes (I).B.X, F.Strokes (I).B.Y, 0.0),
              F.Strokes (I).Width, Kind_Stroke, F.Strokes (I).Color, 0.0);
      end loop;
      return Count;
   end Fill_Instances;

   procedure Draw (Canvas : in out GC.Canvas_Widget; F : in out Frame; G : Neon_Game.Game;
                   Status : out GC.Acquire_Status) is
      T : GC.Target;
      Began : constant Unsigned_64 := CuBit.Monotonic.Microseconds;
   begin
      if not Is_Started then
         Status := GC.Unavailable;
         return;
      end if;
      GC.Acquire (Canvas, T, Status);
      if Status /= GC.Acquired then
         return;
      end if;
      Build (F, G, Positive'Max (1, T.Width), Positive'Max (1, T.Height));
      Fill_Globals (F);
      declare
         Count : constant Natural := Fill_Instances (F);
         View : aliased constant Target_View :=
           (Image => Unsigned_64 (T.Image), View => Unsigned_64 (T.View),
            Ready => Unsigned_64 (T.Ready), Width => Unsigned_32 (T.Width),
            Height => Unsigned_32 (T.Height), Format => T.Format,
            Slot => Unsigned_32 (T.Slot) - 1,   --  widget slots count from 1
            Generation => Unsigned_32 (T.Generation), Reserved => 0);
      begin
         if VK_Render (View'Access, Header'Address, Items.all'Address, Unsigned_32 (Count)) = Status_OK then
            GC.Present (Canvas, T);
         else
            GC.Cancel (Canvas, T);
            Status := GC.Unavailable;
         end if;
      end;
      Record_Us := CuBit.Monotonic.Microseconds - Began;
   end Draw;

   procedure Stop is
   begin
      if Is_Started then
         VK_Destroy;
         Is_Started := False;
      end if;
   end Stop;

end Neon_GPU;
