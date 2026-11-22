with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.State;

-- Native fixture: no clock, animation or application deadline. Only configure
-- and resynchronization events request a repaint after initial publication.
procedure Native_DPI_Client is
   package App renames CuBit.UI.App;
   Win : App.Window;
   UI : CuBit.UI.State.UI_State;
   Controls : CuBit.UI.Controls.Control_Map;
   OK : Boolean;
   Paints : Natural := 0;

   procedure Render (Window : in out App.Window; Damage : CuBit.UI.Rect) is
      C : constant CuBit.UI.Canvas := App.Canvas (Window, Damage);
      Fill : constant CuBit.UI.Color :=
        (if C.densityNumerator = C.densityDenominator then 16#B02040#
         elsif Natural (C.densityNumerator) * 4 = Natural (C.densityDenominator) * 5
         then 16#20B040#
         elsif Natural (C.densityNumerator) * 2 = Natural (C.densityDenominator) * 3
         then 16#2040B0# else 16#B08020#);
   begin
      CuBit.UI.Fill_Rect (C, App.Full_Rect (Window), Fill);
      Paints := Paints + 1;
      debugPrint ("DPI-CLIENT: paint=" & Paints'Image &
        " density=" & C.densityNumerator'Image & "/" & C.densityDenominator'Image &
        " logical=" & C.width'Image & "x" & C.height'Image & ASCII.LF);
   end Render;

   procedure Handle_Event
     (Window : in out App.Window; Event : App.Input_Event;
      Dirty : in out CuBit.UI.Rect; Running : in out Boolean)
   is
   begin
      if Event.kind = App.INPUT_CONFIGURE or else Event.kind = App.INPUT_RESYNC then
         Dirty := App.Full_Rect (Window);
         debugPrint ("DPI-CLIENT: configure serial=" & Event.serial'Image & ASCII.LF);
      elsif Event.kind = App.INPUT_KEY_DOWN and then Event.payload0 = App.KEY_ESC then
         Running := False;
      end if;
   end Handle_Event;

   procedure Run is new App.Run
     (UI, Controls, Render => Render, Handle_Event => Handle_Event);
begin
   App.Open (Win, 320, 234, 2, OK, title => "Idle DPI fixture", protected_frames => True);
   if not OK then
      debugPrint ("DPI-CLIENT: FAIL open" & ASCII.LF);
      return;
   end if;
   debugPrint ("DPI-CLIENT: ready" & ASCII.LF);
   Run (Win);
   App.Close (Win);
   debugPrint ("DPI-CLIENT: closed" & ASCII.LF);
end Native_DPI_Client;
