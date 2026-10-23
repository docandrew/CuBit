------------------------------------------------------------------------------
--  CuBit Teapots: nine Utah teapots, nine shading models, rendered with
--  Vulkan on the GPU through CuBit.UI.GPU_Canvas. The window draws the
--  status bar and the labels with the toolkit; the canvas brings the GPU
--  frames into the same window. Space pauses, Escape closes.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
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
with Teapot_Gallery;

procedure Main is
   package GC renames CuBit.UI.GPU_Canvas;
   use type GC.Availability, GC.Acquire_Status;

   INITIAL_WIDTH : constant := 960;
   INITIAL_HEIGHT : constant := 744;
   MINIMUM_WIDTH : constant := 480;
   MINIMUM_HEIGHT : constant := 360;
   STATUS_HEIGHT : constant := 24;
   STATUS_INSET : constant := 8;
   --  How often the loop looks for finished GPU frames while animating.
   POLL_MILLISECONDS : constant := 1;
   MICROSECONDS_PER_SECOND : constant := 1_000_000.0;
   MICROSECONDS_PER_MILLISECOND : constant := 1_000;
   KEY_SPACE : constant Unsigned_64 := 16#39#;
   STATUS_BACKGROUND : constant Color := 16#2D3343#;
   STATUS_INK : constant Color := 16#D9DEE8#;

   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Canvas : GC.Canvas_Widget;
   Opened, Started : Boolean := False;
   Paused : Boolean := False;
   Start_Us, Paused_At_Us, Paused_Total_Us : Unsigned_64 := 0;
   Shown_Rate : Natural := Natural'Last;
   First_Frame : Boolean := True;

   function Now_Us return Unsigned_64 is
      Reading : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Reading.Available then Reading.Microseconds
              else syscall (SYSCALL_GETTIME) * MICROSECONDS_PER_MILLISECOND);
   end Now_Us;

   function Scene_Seconds return Float is
      Now : constant Unsigned_64 := (if Paused then Paused_At_Us else Now_Us);
      Elapsed : constant Unsigned_64 :=
        (if Now >= Start_Us + Paused_Total_Us then Now - Start_Us - Paused_Total_Us else 0);
   begin
      return Float (Elapsed) / MICROSECONDS_PER_SECOND;
   end Scene_Seconds;

   function Canvas_Rect return Rect is
      Full : constant Rect := CuBit.UI.App.Full_Rect (Win);
   begin
      return (Full.x, Full.y, Full.w, (if Full.h > STATUS_HEIGHT then Full.h - STATUS_HEIGHT else 0));
   end Canvas_Rect;

   function Status_Rect return Rect is
      Full : constant Rect := CuBit.UI.App.Full_Rect (Win);
      Top : constant Natural := (if Full.h > STATUS_HEIGHT then Full.h - STATUS_HEIGHT else 0);
   begin
      return (Full.x, Top, Full.w, Full.h - Top);
   end Status_Rect;

   function Decimal (Value : Natural) return String is
      Image : constant String := Natural'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Status_Text return String is
      Rate : constant Natural := Teapot_Gallery.Rate_Tenths;
      Tenths_Per_Unit : constant := 10;
   begin
      case GC.State (Canvas) is
         when GC.Ready =>
            return "Vulkan on the GPU   " & Decimal (Rate / Tenths_Per_Unit) & "." &
              Decimal (Rate mod Tenths_Per_Unit) & " frames/s" &
              (if Paused then "   paused (Space)" else "   Space pauses");
         when GC.Unavailable =>
            return "No GPU rendering for this launch (software start or no Intel GPU)";
         when GC.Failed =>
            return "GPU rendering failed";
         when GC.Closed =>
            return "";
      end case;
   end Status_Text;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      Window_Canvas : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (Win, Damage);
      Status : constant Rect := Status_Rect;
   begin
      GC.Set_Area (Canvas, Window_Canvas, Canvas_Rect);
      GC.Paint (Canvas, Window_Canvas);
      if GC.State (Canvas) = GC.Ready then
         Teapot_Gallery.Draw_Labels (Window_Canvas, Canvas_Rect);
      end if;
      Fill_Rect (Window_Canvas, Status, STATUS_BACKGROUND);
      if Status.h > UI_Text_Height then
         Draw_UI_Text (Window_Canvas, Status.x + STATUS_INSET,
                       Status.y + (Status.h - UI_Text_Height) / 2, Status_Text,
                       STATUS_INK, STATUS_BACKGROUND);
      end if;
      if First_Frame and then GC.State (Canvas) = GC.Ready then
         First_Frame := False;
         debugPrint ("teapots: first frame painted" & ASCII.LF);
      end if;
   end Render;

   procedure Handle_Event
     (Win : in out CuBit.UI.App.Window; Event : CuBit.UI.App.Input_Event; Dirty : in out Rect;
      Running : in out Boolean)
   is
   begin
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
         if Event.payload0 = CuBit.UI.App.KEY_ESC then
            Running := False;
         elsif Event.payload0 = KEY_SPACE then
            if Paused then
               Paused_Total_Us := Paused_Total_Us + (Now_Us - Paused_At_Us);
            else
               Paused_At_Us := Now_Us;
            end if;
            Paused := not Paused;
            Dirty := Union_Rect (Dirty, Status_Rect);
         end if;
      end if;
   end Handle_Event;

   function Deadline return Unsigned_64 is
     (if GC.State (Canvas) = GC.Ready
      then syscall (SYSCALL_GETTIME) + POLL_MILLISECONDS else 0);

   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Running);
      New_Frame : Boolean;
      Status : GC.Acquire_Status;
   begin
      GC.Poll (Canvas, New_Frame);
      if New_Frame then
         Dirty := Union_Rect (Dirty, GC.Area (Canvas));
         Teapot_Gallery.Note_Presented (syscall (SYSCALL_GETTIME));
         if Teapot_Gallery.Rate_Tenths /= Shown_Rate then
            Shown_Rate := Teapot_Gallery.Rate_Tenths;
            Dirty := Union_Rect (Dirty, Status_Rect);
         end if;
      end if;
      Teapot_Gallery.Step (Canvas, Scene_Seconds, Status);
      if GC.State (Canvas) /= GC.Ready then
         Dirty := CuBit.UI.App.Full_Rect (Win);  --  placeholder and status change
      end if;
   end Tick;

   --  Run never blocks in a call to Desktop (Activity_Wait): its input
   --  wait is submitted, and completes when input, or the deadline, arrives.
   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event, Next_Deadline => Deadline,
      On_Deadline => Tick, Activity_Wait => True);
begin
   debugPrint ("teapots: starting" & ASCII.LF);
   CuBit.Logging.Announce ("teapots: started", Opened, Startup.Logstore);
   CuBit.UI.App.Open
     (Win, INITIAL_WIDTH, INITIAL_HEIGHT,
      CuBit.Desktop_Protocol.Feature_Bits
        ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
          CuBit.Desktop_Protocol.Minimizable | CuBit.Desktop_Protocol.Maximizable |
          CuBit.Desktop_Protocol.Closeable | CuBit.Desktop_Protocol.Graceful_Close => True,
          others => False]),
      Opened, title => "Teapots", protected_frames => True, batched_input => False,
      minimum_width => MINIMUM_WIDTH, minimum_height => MINIMUM_HEIGHT);
   if not Opened then
      debugPrint ("teapots: no window" & ASCII.LF);
      return;
   end if;
   GC.Open (Canvas);
   Teapot_Gallery.Start (Canvas, Started);
   debugPrint ("teapots: canvas " &
     (case GC.State (Canvas) is
        when GC.Ready => "ready (Vulkan device)",
        when GC.Unavailable => "unavailable (no render session)",
        when GC.Failed => "failed",
        when GC.Closed => "closed") &
     (if Started then "" else "; pipeline creation failed") & ASCII.LF);
   Start_Us := Now_Us;
   Run (Win);
   Teapot_Gallery.Stop;
   GC.Close (Canvas);
   CuBit.UI.App.Close (Win);
   debugPrint ("teapots: closed" & ASCII.LF);
end Main;
