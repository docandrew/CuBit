------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A test window that animates every refresh (LAT-002): a bar moves across
--  Area (its bottom-right corner), the only thing it repaints, every FRAME_MS. Headless latency tests
--  run it beside the Calculator to stand for Teapots on the laptop: a
--  continuously animating surface while someone types elsewhere.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.State;
with CuBit.UI.Controls;
with CuBit.UI.Input;
procedure Main is
   Win : CuBit.UI.App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   Opened : Boolean;
   --  One refresh at 60 Hz, rounded down: never slower than the display.
   FRAME_MS : constant := 16;
   --  It opens small (so it opens before the Calculator and leaves it the
   --  focus), then grows to a large window with its animation in the far
   --  corner: the Calculator, over its other end, types far from it. Their
   --  envelope would be most of the screen; the two boxes are small.
   OPEN_W : constant := 240;
   OPEN_H : constant := 120;
   GROWN_W : constant := 960;
   GROWN_H : constant := 640;
   GROW_AFTER_MS : constant := 1_500;
   AREA_W : constant := 200;
   AREA_H : constant := 96;
   BAR_W : constant := 24;
   Opened_At : Unsigned_64 := 0;
   Grown : Boolean := False;
   function Area return Rect is
     ((Natural'Max (CuBit.UI.App.Width (Win), AREA_W + 8) - AREA_W - 8,
       Natural'Max (CuBit.UI.App.Height (Win), AREA_H + 8) - AREA_H - 8, AREA_W, AREA_H));
   Step : Natural := 0;
   Due : Unsigned_64 := 0;
   Announced : Boolean := False;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      C : constant Canvas := CuBit.UI.App.Canvas (Win, Damage);
      Colors : constant Theme := Current_Theme;
      X : constant Natural := Area.x + (Step * 4) mod (Area.w - BAR_W);
   begin
      Fill_Rect (C, CuBit.UI.App.Full_Rect (Win), Colors.panel);
      Fill_Rect (C, Area, Colors.text);
      Fill_Rect (C, (X, Area.y, BAR_W, Area.h), Colors.danger);
   end Render;

   procedure Handle_Event (Win : in out CuBit.UI.App.Window;
     Event : CuBit.UI.App.Input_Event; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Running);
   begin
      --  Only a new size repaints all; input changes nothing shown.
      if Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Dirty := CuBit.UI.App.Full_Rect (Win);
      end if;
   end Handle_Event;

   function Deadline return Unsigned_64 is (Due);

   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Running);
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      OK : Boolean;
   begin
      Due := Now + FRAME_MS;
      if not Grown and then Now >= Opened_At + GROW_AFTER_MS then
         Grown := True;
         CuBit.UI.App.Request_Size (Win, GROWN_W, GROWN_H, OPEN_W, OPEN_H, OK);
         debugPrint ("frame-ticker: grow " & (if OK then "accepted" else "refused") & ASCII.LF);
      end if;
      Step := Step + 1;
      Dirty := Area;
      if not Announced then
         Announced := True;
         debugPrint ("frame-ticker: animating" & ASCII.LF);
      end if;
   end Tick;

   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event,
      Next_Deadline => Deadline, On_Deadline => Tick, Activity_Wait => True);
begin
   CuBit.UI.App.Open (Win, OPEN_W, OPEN_H,
     CuBit.Desktop_Protocol.Feature_Bits
       ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Resizable |
         CuBit.Desktop_Protocol.Closeable => True, others => False]),
     Opened, maximum_width => GROWN_W, maximum_height => GROWN_H, title => "Frame ticker");
   if Opened then
      Opened_At := syscall (SYSCALL_GETTIME);
      Due := Opened_At + 1;
      Run (Win);
      CuBit.UI.App.Close (Win);
   end if;
end Main;
