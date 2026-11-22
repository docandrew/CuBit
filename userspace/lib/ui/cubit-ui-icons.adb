with CuBit.UI.Icons.Atlas;
with Icon_Edge_Choice;

package body CuBit.UI.Icons is
   --  The two exports, in Edges order.
   SMALL_EDGE : constant := 16;
   LARGE_EDGE : constant := 32;
   Edges : constant Icon_Edge_Choice.Edge_List := [SMALL_EDGE, LARGE_EDGE];

   --  Drawn 1:1 from the export nearest the canvas's density, never
   --  resampled (Icon_Edge_Choice): crisp at 100%, 125% or 200%.
   procedure Draw (c : Canvas; x, y : Natural; Item : Icon; Enabled : Boolean := True) is
   begin
      if c.densityNumerator = c.densityDenominator then
         Draw_Bitmap (c, x, y, Atlas.Pixels_16 (Item), Enabled);
      elsif Edges (Icon_Edge_Choice.Nearest (Edges, ICON_SIZE, c.densityNumerator, c.densityDenominator)) =
            LARGE_EDGE
      then
         Draw_Bitmap_Native (c, (x, y, ICON_SIZE, ICON_SIZE), Atlas.Pixels_32 (Item), Enabled);
      else
         Draw_Bitmap_Native (c, (x, y, ICON_SIZE, ICON_SIZE), Atlas.Pixels_16 (Item), Enabled);
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
