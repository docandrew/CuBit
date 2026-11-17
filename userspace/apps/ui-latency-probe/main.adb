------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  UI latency probe (tests/headless --test ui-latency): a toolkit window
--  that measures what the toolkit's frame and input paths cost.
--
--  @description
--  Busy phase: Busy_Frames frames back to back (the timed hook is always
--  due), counting this thread's system calls (kernel READ_STATISTICS).
--  Key phase: every key press repaints a small box; Esc ends it and prints
--  the calls the phase made, the keys and the frames. Desktop reports
--  each key's way from the driver to the submitted frame (LAT-002,
--  "desktop: latency stage=..."), and its timing build its own calls per
--  loop phase (DESKTOP-CALLS, DESKTOP-CALL-PHASES). Also: main's start ->
--  first frame done.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Published_Clock;
with CuBit.System_Facts;
with Startup;
pragma Unreferenced (Startup);
with CuBit.UI; use CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.State;
with CuBit.UI.Controls;
with CuBit.UI.Input;

procedure Main is
   package App renames CuBit.UI.App;

   WINDOW_W : constant := 320;
   WINDOW_H : constant := 200;
   BOX : constant := 24;
   Busy_Frames : constant := 600;
   KEY_ESC : constant Unsigned_64 := 16#01#;
   Background : constant Color := 16#FF20_2830#;
   Ink : constant Color := 16#FFE0_A040#;

   type Phase is (Starting, Busy, Keys);

   Win : App.Window;
   UI : CuBit.UI.State.UI_State;
   Map : CuBit.UI.Controls.Control_Map;
   Opened : Boolean;
   Current : Phase := Starting;
   Started_Ms : constant Unsigned_64 := CuBit.Published_Clock.Milliseconds;
   Phase_Calls, Phase_Ms : Unsigned_64 := 0;
   Frames, Keys_Seen : Unsigned_64 := 0;
   Box_X : Natural := 0;

   function Thread_Calls return Unsigned_64 is
      Value : CuBit.System_Facts.SP.Statistics;
      Read : Boolean;
   begin
      CuBit.System_Facts.Read_Statistics (Value, Read);
      return (if Read then Value.Thread_System_Calls else 0);
   end Thread_Calls;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Value'Image;
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   --  Calls since the phase began, less the READ_STATISTICS that asks.
   function Calls_So_Far return Unsigned_64 is (Thread_Calls - Phase_Calls - 1);

   procedure Begin_Phase (Next : Phase) is
   begin
      Current := Next;
      Frames := 0;
      Keys_Seen := 0;
      Phase_Ms := CuBit.Published_Clock.Milliseconds;
      Phase_Calls := Thread_Calls;
   end Begin_Phase;

   procedure Render (W : in out App.Window; Damage : Rect) is
      C : constant Canvas := App.Canvas (W, Damage);
   begin
      Fill_Rect (C, App.Full_Rect (W), Background);
      Fill_Rect (C, (Box_X, WINDOW_H / 2, BOX, BOX), Ink);
      Frames := Frames + 1;
   end Render;

   function Box_Rect return Rect is ((0, WINDOW_H / 2, WINDOW_W, BOX));

   procedure Handle_Event
     (W : in out App.Window; Event : App.Input_Event; Dirty : in out Rect; Running : in out Boolean)
   is
      pragma Unreferenced (W);
   begin
      if Event.kind = CuBit.UI.Input.INPUT_CLOSE_REQUEST then
         Running := False;
      elsif Event.kind = CuBit.UI.Input.INPUT_KEY_DOWN and then Current = Keys then
         if Event.payload0 = KEY_ESC then
            declare
               Calls : constant Unsigned_64 := Calls_So_Far;
            begin
               debugPrint ("UI-PROBE: keys keys=" & Decimal (Keys_Seen) & " frames=" & Decimal (Frames) &
                           " calls=" & Decimal (Calls) & ASCII.LF);
            end;
            Running := False;
         else
            Keys_Seen := Keys_Seen + 1;
            Box_X := (Box_X + BOX) mod (WINDOW_W - BOX);
            Dirty := Union_Rect (Dirty, Box_Rect);
         end if;
      end if;
   end Handle_Event;

   --  Busy: always due. Otherwise no timed work.
   function Next_Deadline return Unsigned_64 is
   begin
      if Current = Starting then
         --  The first frame is done (Run painted it before its loop).
         debugPrint ("UI-PROBE: first frame ms=" &
                     Decimal (CuBit.Published_Clock.Milliseconds - Started_Ms) & ASCII.LF);
         Begin_Phase (Busy);
      end if;
      return (if Current = Busy then 1 else 0);
   end Next_Deadline;

   procedure On_Deadline (W : in out App.Window; Dirty : in out Rect; Running : in out Boolean) is
      pragma Unreferenced (W, Running);
   begin
      if Current /= Busy then
         return;
      end if;
      if Frames >= Busy_Frames then
         declare
            Calls : constant Unsigned_64 := Calls_So_Far;
         begin
            debugPrint ("UI-PROBE: busy frames=" & Decimal (Frames) & " calls=" & Decimal (Calls) &
                        " ms=" & Decimal (CuBit.Published_Clock.Milliseconds - Phase_Ms) & ASCII.LF);
         end;
         Begin_Phase (Keys);
         debugPrint ("UI-PROBE: keys ready" & ASCII.LF);
         return;
      end if;
      Box_X := (Box_X + 1) mod (WINDOW_W - BOX);
      Dirty := Union_Rect (Dirty, Box_Rect);
   end On_Deadline;

   procedure Run is new App.Run
     (UI, Map, Render => Render, Handle_Event => Handle_Event,
      Next_Deadline => Next_Deadline, On_Deadline => On_Deadline, Activity_Wait => True);
begin
   App.Open (Win, WINDOW_W, WINDOW_H, App.WINDOW_FLAG_DECORATED, Opened,
             title => "Latency probe", protected_frames => True, preferred_size => True);
   if Opened then
      Run (Win);
      App.Close (Win);
   end if;
   debugPrint ("UI-PROBE: closed" & ASCII.LF);
end Main;
