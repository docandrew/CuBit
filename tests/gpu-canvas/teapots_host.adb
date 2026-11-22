------------------------------------------------------------------------------
--  Linux-hosted run of the GPU canvas widget and the teapot gallery: the
--  same widget and gallery units as the CuBit app, a host Vulkan device
--  (lavapipe, CPU) and an in-memory window canvas. Checks the widget's
--  contract and writes screenshots. Not a CuBit or Intel hardware result.
--
--  Usage: teapots_host OUTPUT_DIR [unavailable]
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.GPU_Canvas;
with GPU_Canvas_Host_Control;
with Teapot_Gallery;
with Host_Pixels;

procedure Teapots_Host is
   package GC renames CuBit.UI.GPU_Canvas;
   use type GC.Availability, GC.Acquire_Status, GC.Generation_Count;

   Window_Width : constant := Host_Pixels.Window_Width;
   Window_Height : constant := Host_Pixels.Window_Height;
   Status_Height : constant := 24;
   Bytes_Per_Pixel : constant := 4;
   Frame_Seconds : constant := 1.0 / 30.0;
   --  Bound on polls for one readback (lavapipe finishes in milliseconds).
   Poll_Limit : constant := 200_000;
   Window_Color : constant Color := 16#2D3343#;
   Text_Color : constant Color := 16#D9DEE8#;
   Clear_Pixel : constant Unsigned_32 := 16#FF0E1016#;  --  Teapot_Gallery's clear, sRGB-free
   Clear_Tolerance : constant := 2;

   subtype Pixel_Array is Host_Pixels.Pixel_Array;
   Pixels : Pixel_Array renames Host_Pixels.Pixels;
   Pixels_2x : Pixel_Array renames Host_Pixels.Pixels_2x;

   Window : constant Canvas :=
     (addr => Pixels'Address, width => Window_Width, height => Window_Height,
      pitch => Window_Width * Bytes_Per_Pixel, others => <>);
   Canvas_Area : constant Rect := (0, 0, Window_Width, Window_Height - Status_Height);

   Output : constant String := Ada.Command_Line.Argument (1);
   Unavailable_Run : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (2) = "unavailable";

   Widget : GC.Canvas_Widget;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Put_Line ("PASS " & What);
      else
         Put_Line ("FAIL " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   procedure Write_PPM (Name : String; Source : Pixel_Array; Width, Height : Natural) is
      package SIO renames Ada.Streams.Stream_IO;
      F : SIO.File_Type;
      Header : constant String :=
        "P6" & ASCII.LF & Natural'Image (Width) & Natural'Image (Height) & ASCII.LF & "255" & ASCII.LF;
      Row : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Width * 3));
      P : Unsigned_32;
   begin
      SIO.Create (F, SIO.Out_File, Output & "/" & Name & ".ppm");
      String'Write (SIO.Stream (F), Header);
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            P := Source (Source'First + Y * Width + X);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 1)) :=
              Ada.Streams.Stream_Element (Shift_Right (P, 16) and 16#FF#);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 2)) :=
              Ada.Streams.Stream_Element (Shift_Right (P, 8) and 16#FF#);
            Row (Ada.Streams.Stream_Element_Offset (X * 3 + 3)) :=
              Ada.Streams.Stream_Element (P and 16#FF#);
         end loop;
         SIO.Write (F, Row);
      end loop;
      SIO.Close (F);
   end Write_PPM;

   function Near (A, B : Unsigned_32) return Boolean is
      function Channel (P : Unsigned_32; Shift : Natural) return Integer is
        (Integer (Shift_Right (P, Shift) and 16#FF#));
   begin
      return abs (Channel (A, 0) - Channel (B, 0)) <= Clear_Tolerance and then
             abs (Channel (A, 8) - Channel (B, 8)) <= Clear_Tolerance and then
             abs (Channel (A, 16) - Channel (B, 16)) <= Clear_Tolerance;
   end Near;

   --  Wait for the presented frame's readback.
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

   procedure Paint_Window (Seconds_Label : String) is
   begin
      Fill_Rect (Window, (0, 0, Window_Width, Window_Height), Window_Color);
      GC.Paint (Widget, Window);
      Teapot_Gallery.Draw_Labels (Window, Canvas_Area);
      Draw_UI_Text (Window, 8, Window_Height - Status_Height + 4,
                    "Teapots  (Linux-hosted, lavapipe CPU Vulkan)  t=" & Seconds_Label,
                    Text_Color, Window_Color);
   end Paint_Window;

   Status : GC.Acquire_Status;
   T, T2 : GC.Target;
   OK : Boolean;
   Seconds : Float := 0.0;
   Snapshot : Pixel_Array renames Host_Pixels.Snapshot;
begin
   GPU_Canvas_Host_Control.Force_Unavailable := Unavailable_Run;
   GC.Open (Widget);
   GC.Set_Area (Widget, Window, Canvas_Area);
   Teapot_Gallery.Start (Widget, OK);
   Check (OK, "gallery start");

   if Unavailable_Run then
      Check (GC.State (Widget) = GC.Unavailable, "no device: widget Unavailable");
      GC.Acquire (Widget, T, Status);
      Check (Status = GC.Unavailable, "no device: Acquire answers Unavailable");
      Paint_Window ("-");
      Write_PPM ("teapots-unavailable", Pixels, Window_Width, Window_Height);
      GC.Close (Widget);
      Put_Line (if Failures = 0 then "ALL PASS" else "FAILURES" & Natural'Image (Failures));
      Ada.Command_Line.Set_Exit_Status (if Failures = 0 then 0 else 1);
      return;
   end if;

   Check (GC.State (Widget) = GC.Ready, "host device Ready");

   --  Contract: two targets can be in flight; a third waits (Busy) until a
   --  frame is painted.
   GC.Acquire (Widget, T, Status);
   Check (Status = GC.Acquired and then T.Generation = 1 and then
          T.Width = Canvas_Area.w and then T.Height = Canvas_Area.h and then
          T.Format = GC.Target_Format, "first target: generation 1, area-sized, BGRA");
   GC.Cancel (Widget, T);
   Teapot_Gallery.Step (Widget, 0.0, Status);
   Check (Status = GC.Acquired, "frame 1 rendered and presented");
   Teapot_Gallery.Step (Widget, Frame_Seconds, Status);
   Check (Status = GC.Acquired, "frame 2 in flight with frame 1");
   GC.Acquire (Widget, T2, Status);
   Check (Status = GC.Busy, "no third target while two are in flight");
   Check (Wait_Frame, "frame readback completes");
   GC.Acquire (Widget, T2, Status);
   Check (Status = GC.Busy, "Busy while the newest frame is unpainted");
   Paint_Window ("0.0");
   Snapshot := Pixels;
   declare
      Centre_X : constant Natural := Canvas_Area.w / 6;
      Centre_Y : constant Natural := Canvas_Area.h / 6;
   begin
      Check (not Near (Pixels (Centre_Y * Window_Width + Centre_X), Clear_Pixel),
             "a teapot covers the first cell's centre");
      Check (Near (Pixels (2 * Window_Width + 2), Clear_Pixel),
             "the gap outside the cells shows the clear colour");
   end;
   Write_PPM ("teapots-hosted-t0", Pixels, Window_Width, Window_Height);

   --  Animate: render frames, painting each one as a window would.
   for Frame in 3 .. 90 loop
      Seconds := Float (Frame) * Frame_Seconds;
      loop
         Teapot_Gallery.Step (Widget, Seconds, Status);
         exit when Status = GC.Acquired;
         exit when not Wait_Frame;
         Paint_Window ("");
      end loop;
   end loop;
   OK := Wait_Frame;
   Paint_Window ("3.0");
   Check ((for some I in Pixels'Range => Pixels (I) /= Snapshot (I)), "the pots move");
   Write_PPM ("teapots-hosted-t3", Pixels, Window_Width, Window_Height);
   OK := Wait_Frame;  --  drain the frame still in flight

   --  Visibility: hidden canvases lend nothing.
   GC.Set_Visible (Widget, False);
   GC.Acquire (Widget, T, Status);
   Check (Status = GC.Hidden, "hidden canvas answers Hidden");
   GC.Set_Visible (Widget, True);

   --  Resize: the next target is rebuilt at the new size, new generation.
   GC.Set_Area (Widget, Window, (40, 40, 600, 400));
   GC.Paint (Widget, Window);
   OK := Wait_Frame;
   GC.Paint (Widget, Window);
   GC.Acquire (Widget, T, Status);
   Check (Status = GC.Acquired and then T.Width = 600 and then T.Height = 400 and then
          T.Generation = 2, "resize rebuilds targets at 600x400, generation 2");
   GC.Cancel (Widget, T);

   --  Density 2: a 480x360 logical area is a 960x720 target.
   declare
      Dense : constant Canvas :=
        (addr => Pixels_2x'Address, width => Window_Width, height => Window_Height,
         pitch => Window_Width * 2 * Bytes_Per_Pixel, densityNumerator => 2,
         densityDenominator => 1, others => <>);
   begin
      GC.Set_Area (Widget, Dense, (0, 0, 480, 360));
      GC.Acquire (Widget, T, Status);
      Check (Status = GC.Acquired and then T.Width = 960 and then T.Height = 720 and then
             T.Generation = 3, "density 2: 480x360 logical is a 960x720 target");
      GC.Cancel (Widget, T);
      Teapot_Gallery.Step (Widget, 1.5, Status);
      Check (Status = GC.Acquired and then Wait_Frame, "density-2 frame rendered");
      GC.Paint (Widget, Dense);
      Teapot_Gallery.Draw_Labels (Dense, (0, 0, 480, 360));
      Write_PPM ("teapots-hosted-density2", Pixels_2x (0 .. 960 * 2 * 720 - 1), 960 * 2, 720);
   end;

   Teapot_Gallery.Stop;
   GC.Close (Widget);
   Check (GC.State (Widget) = GC.Closed, "closed");
   Put_Line (if Failures = 0 then "ALL PASS" else "FAILURES" & Natural'Image (Failures));
   Ada.Command_Line.Set_Exit_Status (if Failures = 0 then 0 else 1);
end Teapots_Host;
