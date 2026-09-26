pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Input; use CuBit.UI.Input;
with CuBit.UI.Surfaces; use CuBit.UI.Surfaces;

--  Linux-hosted checks for CuBit.UI.Surfaces (assertions enabled).
procedure Main is
   W : constant := 64;
   H : constant := 48;
   type Pixels is array (0 .. H - 1, 0 .. W - 1) of Color;
   Buffer : Pixels := [others => [others => 0]];
   Parent : constant Canvas :=
     (addr => Buffer'Address, width => W, height => H, pitch => W * 4,
      clipEnabled => False, clip => (others => 0));

   function Ev (Kind : Unsigned_64; X, Y : Natural; P1 : Unsigned_64 := 0)
     return Input_Event is
     (kind => Kind, serial => 0,
      payload0 => Unsigned_64 (X) or Shift_Left (Unsigned_64 (Y), 32),
      payload1 => P1);

   procedure Test_Place is
      P : Placement;
   begin
      P := Place (W, H, False, (others => 0), (10, 5, 20, 10));
      pragma Assert (P.x = 10 and P.y = 5 and P.width = 20 and P.height = 10
                     and not P.clipEnabled);
      --  Clamped at the right and bottom edges.
      P := Place (W, H, False, (others => 0), (50, 40, 100, 100));
      pragma Assert (P.width = 14 and P.height = 8);
      --  Off-canvas or empty.
      P := Place (W, H, False, (others => 0), (64, 0, 10, 10));
      pragma Assert (P.width = 0 and P.height = 0);
      P := Place (W, H, False, (others => 0), (5, 5, 0, 10));
      pragma Assert (P.width = 0);
      --  Parent clip is intersected and translated to view coordinates.
      P := Place (W, H, True, (0, 0, 20, 12), (10, 5, 20, 10));
      pragma Assert (P.clipEnabled and P.clip = (0, 0, 10, 7));
      P := Place (W, H, True, (40, 40, 5, 5), (10, 5, 20, 10));
      pragma Assert (P.clipEnabled and P.clip.w = 0); -- fully clipped
   end Test_Place;

   procedure Test_View is
      V : Canvas;
      Count : Natural := 0;
   begin
      V := View (Parent, (10, 5, 20, 10));
      pragma Assert (V.width = 20 and V.height = 10 and V.pitch = W * 4);
      --  Painting the whole view (and beyond) touches exactly the area.
      Fill_Rect (V, (0, 0, 100, 100), 16#FF0000#);
      for Y in 0 .. H - 1 loop
         for X in 0 .. W - 1 loop
            if Buffer (Y, X) = 16#FF0000# then
               Count := Count + 1;
               pragma Assert (X in 10 .. 29 and Y in 5 .. 14);
            end if;
         end loop;
      end loop;
      pragma Assert (Count = 200);
      pragma Assert (Buffer (5, 10) = 16#FF0000# and Buffer (14, 29) = 16#FF0000#);
      --  A clipped parent keeps its clip inside the view.
      Buffer := [others => [others => 0]];
      V := View (With_Clip (Parent, (0, 0, 15, 8)), (10, 5, 20, 10));
      Fill_Rect (V, (0, 0, 20, 10), 16#00FF00#);
      Count := 0;
      for Y in 0 .. H - 1 loop
         for X in 0 .. W - 1 loop
            if Buffer (Y, X) /= 0 then
               Count := Count + 1;
               pragma Assert (X in 10 .. 14 and Y in 5 .. 7);
            end if;
         end loop;
      end loop;
      pragma Assert (Count = 15);
      --  Off-canvas view is empty; drawing is a no-op.
      V := View (Parent, (100, 100, 5, 5));
      pragma Assert (V.width = 0 and V.height = 0);
   end Test_View;

   procedure Test_Route is
      S : Surface := (area => (10, 20, 100, 50), others => <>);
      R : Surface_Event;
   begin
      --  Outside and never entered: nothing.
      Route (S, Ev (INPUT_POINTER_MOVE, 5, 5), R);
      pragma Assert (R.kind = No_Event);
      --  Inside: surface-relative.
      Route (S, Ev (INPUT_POINTER_MOVE, 15, 25), R);
      pragma Assert (R.kind = Pointer_Move and R.x = 5 and R.y = 5 and not R.primaryDown);
      --  Leaving yields one Pointer_Leave, then nothing.
      Route (S, Ev (INPUT_POINTER_MOVE, 200, 25), R);
      pragma Assert (R.kind = Pointer_Leave);
      Route (S, Ev (INPUT_POINTER_MOVE, 201, 25), R);
      pragma Assert (R.kind = No_Event);
      --  Keys are ignored until the surface is focused by a press inside.
      Route (S, Ev (INPUT_KEY_DOWN, 30, 0), R);
      pragma Assert (R.kind = No_Event);
      Route (S, Ev (INPUT_POINTER_DOWN, 50, 40), R);
      pragma Assert (R.kind = Pointer_Down and R.x = 40 and R.y = 20 and S.focused);
      --  A drag outside keeps reporting, clamped to the area, while captured.
      Route (S, Ev (INPUT_POINTER_MOVE, 500, 1, 1), R);
      pragma Assert (R.kind = Pointer_Move and R.x = 99 and R.y = 0 and R.primaryDown);
      Route (S, Ev (INPUT_POINTER_UP, 500, 1), R);
      pragma Assert (R.kind = Pointer_Up and not S.captured);
      --  Keyboard and text now routed, payloads unchanged.
      Route (S, (kind => INPUT_KEY_DOWN, serial => 0, payload0 => 16#1C#, payload1 => 2), R);
      pragma Assert (R.kind = Key_Down and R.code = 16#1C# and R.modifiers = 2);
      Route (S, (kind => INPUT_TEXT, serial => 0, payload0 => 65, payload1 => 0), R);
      pragma Assert (R.kind = Text and R.code = 65);
      --  Wheel inside, with a negative delta.
      Route (S, Ev (INPUT_POINTER_WHEEL, 20, 30, 16#FFFF_FFFD#), R);
      pragma Assert (R.kind = Wheel and R.wheelDelta = -3);
      --  A press outside unfocuses; keys stop.
      Route (S, Ev (INPUT_POINTER_DOWN, 1, 1), R);
      pragma Assert (not S.focused);
      Route (S, Ev (INPUT_KEY_DOWN, 30, 0), R);
      pragma Assert (R.kind = No_Event);
      --  Resync drops capture.
      Route (S, Ev (INPUT_POINTER_DOWN, 20, 30), R);
      pragma Assert (S.captured);
      Route (S, (kind => INPUT_RESYNC, others => 0), R);
      pragma Assert (not S.captured and not S.pointerInside);
   end Test_Route;
begin
   Test_Place;
   Test_View;
   Test_Route;
   Ada.Text_IO.Put_Line
     ("Surfaces: placement/clipping, views paint only their area, " &
      "input routing (relative, capture, leave, focus, keys, wheel, resync) PASS");
end Main;
