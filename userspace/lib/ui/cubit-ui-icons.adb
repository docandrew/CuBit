with CuBit.UI.Icons.Atlas;

package body CuBit.UI.Icons is
   --  At 1.5x and above the 32-pixel artwork is the nearer.
   function Dense (c : Canvas) return Boolean is
     (Natural (c.densityNumerator) * 2 >= Natural (c.densityDenominator) * 3);

   procedure Draw (c : Canvas; x, y : Natural; Item : Icon; Enabled : Boolean := True) is
   begin
      if c.densityNumerator = c.densityDenominator then
         Draw_Bitmap (c, x, y, Atlas.Pixels_16 (Item), Enabled);
      elsif Dense (c) then
         Draw_Bitmap_Fitted (c, (x, y, ICON_SIZE, ICON_SIZE), Atlas.Pixels_32 (Item), Enabled);
      else
         Draw_Bitmap_Fitted (c, (x, y, ICON_SIZE, ICON_SIZE), Atlas.Pixels_16 (Item), Enabled);
      end if;
   end Draw;

   procedure Draw_Tool_Button
     (c : Canvas; Bounds : Rect; Colors : Theme; Item : Icon; Enabled, Hot, Pressed : Boolean;
      Checked : Boolean := False)
   is
      X : constant Natural := Bounds.x + (if Bounds.w > ICON_SIZE then (Bounds.w - ICON_SIZE) / 2 else 0);
      Y : constant Natural := Bounds.y + (if Bounds.h > ICON_SIZE then (Bounds.h - ICON_SIZE) / 2 else 0);
      Shift : constant Natural := (if Pressed or else Checked then 1 else 0);
   begin
      if Is_Empty (Bounds) then
         return;
      end if;
      if Enabled and then (Pressed or else Checked) then
         Draw_Button_Frame (c, Bounds, Colors, Button_Pressed);
      elsif Enabled and then Hot then
         Draw_Button_Frame (c, Bounds, Colors, Button_Hot);
      else
         Fill_Rect (c, Bounds, Colors.face);
      end if;
      Draw (With_Clip (c, Bounds), X + Shift, Y + Shift, Item, Enabled);
   end Draw_Tool_Button;
end CuBit.UI.Icons;
