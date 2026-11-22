with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI.Widgets;

procedure Main is
   use CuBit.UI;
   use CuBit.UI.Widgets;
   Width : constant := 306;
   Height : constant := 108;
   type Surface is array (0 .. Height - 1, 0 .. Width - 1) of Color;
   Sentinel : constant Color := 16#204060#;
   Buffer : aliased Surface := [others => [others => Sentinel]];
   Reference : Surface;
   C : constant Canvas :=
     (addr => Buffer'Address, width => Width, height => Height,
      pitch => Width * 4, others => <>);
   --  Nonzero array bounds exercise the reusable bitmap API, not just atlas
   --  images. Half-alpha red is deliberately NOT premultiplied.
   Sample : constant ARGB_Bitmap (3 .. 4, 7 .. 8) :=
     [[16#FFFF0000#, 16#80FF0000#], [16#0000FF00#, 16#FF00FF00#]];
   Bounds : constant Rect := (10, 10, 27, 27);
   Output : Ada.Text_IO.File_Type;
begin
   Draw_Bitmap (C, 5, 6, Sample);
   pragma Assert (Buffer (6, 5) = 16#FF0000#);
   pragma Assert (Buffer (6, 6) = 16#902030#);
   pragma Assert (Buffer (7, 5) = Sentinel);
   pragma Assert (Buffer (7, 6) = 16#00FF00#);

   Buffer := [others => [others => Sentinel]];
   Draw_Bitmap (With_Clip (C, (6, 7, 1, 1)), 5, 6, Sample);
   for Y in Buffer'Range (1) loop
      for X in Buffer'Range (2) loop
         pragma Assert
           (Buffer (Y, X) = (if X = 6 and Y = 7 then 16#00FF00# else Sentinel));
      end loop;
   end loop;
   Buffer := [others => [others => 16#FFFFFF#]];
   Draw_Bitmap (C, 0, 0, Sample, enabled => False);
   pragma Assert (Buffer (0, 0) = 16#B1B1B1#);
   pragma Assert (Buffer (1, 0) = 16#FFFFFF#);
   Reference := Buffer;
   Draw_Bitmap (With_Clip (C, (0, 0, 0, 0)), 0, 0, Sample);
   Draw_Bitmap (C, Width, Height, Sample);
   Draw_Bitmap ((others => <>), 0, 0, Sample);
   pragma Assert (Buffer = Reference);

   for Icon in Toolbar_Icon loop
      for Enabled in Boolean loop
         for Pressed in Boolean loop
            Buffer := [others => [others => Sentinel]];
            Toolbar_Button (C, Bounds, CuBit_Alloy, Icon, Enabled, Pressed);
            for Y in Buffer'Range (1) loop
               for X in Buffer'Range (2) loop
                  if not Point_In_Rect (X, Y, Bounds) then
                     pragma Assert (Buffer (Y, X) = Sentinel);
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      Buffer := [others => [others => Sentinel]];
      Toolbar_Button (C, Bounds, CuBit_Alloy, Icon, False, False);
      Reference := Buffer;
      Toolbar_Button (C, Bounds, CuBit_Alloy, Icon, False, True);
      pragma Assert (Buffer = Reference);
      --  Tiny buttons and parent damage clips must not leak icon pixels.
      Buffer := [others => [others => Sentinel]];
      Toolbar_Button (C, (10, 10, 3, 3), CuBit_Alloy, Icon, True, True);
      for Y in Buffer'Range (1) loop
         for X in Buffer'Range (2) loop
            if not Point_In_Rect (X, Y, (10, 10, 3, 3)) then
               pragma Assert (Buffer (Y, X) = Sentinel);
            end if;
         end loop;
      end loop;
      Buffer := [others => [others => Sentinel]];
      Toolbar_Button
        (With_Clip (C, (15, 15, 4, 4)), Bounds, CuBit_Alloy, Icon);
      for Y in Buffer'Range (1) loop
         for X in Buffer'Range (2) loop
            if not Point_In_Rect (X, Y, (15, 15, 4, 4)) then
               pragma Assert (Buffer (Y, X) = Sentinel);
            end if;
         end loop;
      end loop;
   end loop;

   --  Optional visual gallery: normal, pressed, disabled rows. Hosted only.
   if Ada.Command_Line.Argument_Count = 1 then
      Buffer := [others => [others => CuBit_Alloy.face]];
      for Icon in Toolbar_Icon loop
         for Row in 0 .. 2 loop
            Toolbar_Button
              (C, (5 + Toolbar_Icon'Pos (Icon) * 33, 5 + Row * 34, 27, 27),
               CuBit_Alloy, Icon, enabled => Row /= 2, pressed => Row = 1);
         end loop;
      end loop;
      Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Ada.Command_Line.Argument (1));
      Ada.Text_IO.Put_Line (Output, "P3");
      Ada.Text_IO.Put_Line (Output, "306 108");
      Ada.Text_IO.Put_Line (Output, "255");
      for Row in Buffer'Range (1) loop
         for Col in Buffer'Range (2) loop
            Ada.Text_IO.Put_Line
              (Output,
               Unsigned_32'Image (Shift_Right (Buffer (Row, Col), 16) and 255) &
               Unsigned_32'Image (Shift_Right (Buffer (Row, Col), 8) and 255) &
               Unsigned_32'Image (Buffer (Row, Col) and 255));
         end loop;
      end loop;
      Ada.Text_IO.Close (Output);
   end if;
   Ada.Text_IO.Put_Line ("Toolbar bitmap and state rendering tests passed.");
end Main;
