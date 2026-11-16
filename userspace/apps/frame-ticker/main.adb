------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A test window that animates every refresh and takes typing (LAT-002):
--  a bar moves across Bar_Area, repainted every FRAME_MS, and each key
--  press redraws Typed_Area, a counter far from the bar. Headless latency
--  tests type into it while it animates, as one types beside Teapots on
--  the laptop. It is the only window of its test profile, so it always
--  has the focus and nothing covers either area: every run is valid, with
--  no window-order race (two apps cascaded by Desktop raced over which
--  opened on top).
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
   --  Opened at exactly this size (preferred_size): the areas are fixed.
   WINDOW_W : constant := 640;
   WINDOW_H : constant := 360;
   Bar_Area : constant Rect := (8, 8, 200, 96);
   Typed_Area : constant Rect := (WINDOW_W - 208, WINDOW_H - 104, 200, 96);
   BAR_W : constant := 24;
   Step : Natural := 0;
   Typed : Natural := 0;
   Due : Unsigned_64 := 0;
   Announced, First_Typed_Frame : Boolean := False;

   procedure Render (Win : in out CuBit.UI.App.Window; Damage : Rect) is
      C : constant Canvas := CuBit.UI.App.Canvas (Win, Damage);
      Colors : constant Theme := Current_Theme;
      X : constant Natural := Bar_Area.x + (Step * 4) mod (Bar_Area.w - BAR_W);
      --  The counter as a row of cells, one filled per press (mod 8).
      Cell_W : constant := 24;
   begin
      Fill_Rect (C, CuBit.UI.App.Full_Rect (Win), Colors.panel);
      Fill_Rect (C, Bar_Area, Colors.text);
      Fill_Rect (C, (X, Bar_Area.y, BAR_W, Bar_Area.h), Colors.danger);
      Fill_Rect (C, Typed_Area, Colors.text);
      for I in 0 .. Typed mod 8 loop
         Fill_Rect (C, (Typed_Area.x + I * Cell_W, Typed_Area.y + 8, Cell_W - 4, Typed_Area.h - 16),
                    Colors.danger);
      end loop;
      if not First_Typed_Frame then
         First_Typed_Frame := True;
         debugPrint ("frame-ticker: first frame presented" & ASCII.LF);
      end if;
   end Render;

   procedure Handle_Event (Win : in out CuBit.UI.App.Window;
     Event : CuBit.UI.App.Input_Event; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Running);
   begin
      if Event.kind = CuBit.UI.Input.INPUT_CONFIGURE then
         Dirty := CuBit.UI.App.Full_Rect (Win);
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN then
         Typed := Typed + 1;
         Dirty := Union_Rect (Dirty, Typed_Area);
      end if;
   end Handle_Event;

   function Deadline return Unsigned_64 is (Due);

   procedure Tick (Win : in out CuBit.UI.App.Window; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (Win, Running);
   begin
      Due := syscall (SYSCALL_GETTIME) + FRAME_MS;
      Step := Step + 1;
      Dirty := Union_Rect (Dirty, Bar_Area);
      if not Announced then
         Announced := True;
         debugPrint ("frame-ticker: animating" & ASCII.LF);
      end if;
   end Tick;

   procedure Run is new CuBit.UI.App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event,
      Next_Deadline => Deadline, On_Deadline => Tick, Activity_Wait => True);
begin
   CuBit.UI.App.Open (Win, WINDOW_W, WINDOW_H,
     CuBit.Desktop_Protocol.Feature_Bits
       ([CuBit.Desktop_Protocol.Decorated | CuBit.Desktop_Protocol.Closeable => True, others => False]),
     Opened, title => "Frame ticker", protected_frames => True, preferred_size => True);
   if Opened then
      Due := syscall (SYSCALL_GETTIME) + 1;
      Run (Win);
      CuBit.UI.App.Close (Win);
   end if;
end Main;
