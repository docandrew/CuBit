with System.Storage_Elements; use System.Storage_Elements;

package body CuBit.UI.Surfaces is
   use type System.Address;

   function Place
     (parentWidth, parentHeight : Natural;
      parentClipEnabled : Boolean; parentClip : Rect;
      area : Rect) return Placement
   is
      result : Placement;
   begin
      if area.x >= parentWidth or else area.y >= parentHeight or else
        area.w = 0 or else area.h = 0
      then
         return result;
      end if;
      result.x := area.x;
      result.y := area.y;
      result.width := Natural'Min (area.w, parentWidth - area.x);
      result.height := Natural'Min (area.h, parentHeight - area.y);
      if parentClipEnabled then
         --  Intersect the parent's clip with the area, in view coordinates.
         declare
            left : constant Natural := Natural'Max (parentClip.x, result.x);
            top : constant Natural := Natural'Max (parentClip.y, result.y);
            clipRight : constant Natural :=
              (if parentClip.w > Natural'Last - parentClip.x then Natural'Last
               else parentClip.x + parentClip.w);
            clipBottom : constant Natural :=
              (if parentClip.h > Natural'Last - parentClip.y then Natural'Last
               else parentClip.y + parentClip.h);
            right : constant Natural :=
              Natural'Min (clipRight, result.x + result.width);
            bottom : constant Natural :=
              Natural'Min (clipBottom, result.y + result.height);
         begin
            result.clipEnabled := True;
            if left < right and then top < bottom then
               result.clip :=
                 (x => left - result.x, y => top - result.y,
                  w => right - left, h => bottom - top);
            else
               result.clip := (others => 0);  --  fully clipped
            end if;
         end;
      end if;
      return result;
   end Place;

   function View (parent : Canvas; area : Rect) return Canvas is
      p : constant Placement :=
        Place (parent.width, parent.height, parent.clipEnabled, parent.clip,
               area);
      result : Canvas;
   begin
      if p.width = 0 or else p.height = 0 or else
        parent.addr = System.Null_Address
      then
         return result;  --  empty canvas: every draw is a no-op
      end if;
      result :=
        (addr => parent.addr + Storage_Offset (p.y * parent.pitch + p.x * 4),
         width => p.width,
         height => p.height,
         pitch => parent.pitch,
         clipEnabled => p.clipEnabled,
         clip => p.clip);
      return result;
   end View;

   procedure Route
     (item : in out Surface; event : CuBit.UI.Input.Input_Event;
      result : out Surface_Event)
   is
      use CuBit.UI.Input;
      x, y : Natural;
      inside : Boolean;

      function Relative_X return Natural is
        (if x <= item.area.x then 0
         elsif item.area.w = 0 then 0
         else Natural'Min (x - item.area.x, item.area.w - 1));
      function Relative_Y return Natural is
        (if y <= item.area.y then 0
         elsif item.area.h = 0 then 0
         else Natural'Min (y - item.area.y, item.area.h - 1));
   begin
      result := (others => <>);
      if event.kind in INPUT_POINTER_MOVE | INPUT_POINTER_DOWN |
                       INPUT_POINTER_UP | INPUT_POINTER_WHEEL
      then
         x := Pointer_X (event);
         y := Pointer_Y (event);
         inside := Point_In_Rect (x, y, item.area);
         if event.kind = INPUT_POINTER_DOWN then
            item.focused := inside;
            item.captured := inside;
         end if;
         if not inside and then not item.captured then
            if item.pointerInside then
               item.pointerInside := False;
               result.kind := Pointer_Leave;
            end if;
            return;
         end if;
         item.pointerInside := inside;
         result.x := Relative_X;
         result.y := Relative_Y;
         case event.kind is
            when INPUT_POINTER_MOVE =>
               result.kind := Pointer_Move;
               result.primaryDown := (event.payload1 and 1) /= 0;
            when INPUT_POINTER_DOWN =>
               result.kind := Pointer_Down;
               result.primaryDown := True;
            when INPUT_POINTER_UP =>
               result.kind := Pointer_Up;
               item.captured := False;
            when others =>
               result.kind := Wheel;
               result.wheelDelta := Pointer_Wheel_Delta (event);
         end case;
      elsif item.focused and then
        event.kind in INPUT_KEY_DOWN | INPUT_KEY_UP | INPUT_TEXT
      then
         result.kind :=
           (case event.kind is
               when INPUT_KEY_DOWN => Key_Down,
               when INPUT_KEY_UP => Key_Up,
               when others => Text);
         result.code := event.payload0;
         result.modifiers := event.payload1;
      elsif event.kind = INPUT_RESYNC then
         --  The compositor lost track of pointer state; drop any capture.
         item.captured := False;
         item.pointerInside := False;
      end if;
   end Route;
end CuBit.UI.Surfaces;
