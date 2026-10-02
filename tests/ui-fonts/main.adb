with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Fonts;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.State;
with CuBit.UI.Trees;
with CuBit.UI.Surfaces;
with Font8x16;
with Compositor_Glyph_FFI;
with Compositor_Glyph_Layout;

procedure Main is
   use type CuBit.Fonts.Glyph_Access;
   use type CuBit.Fonts.Face;
   use type CuBit.UI.Trees.Tree_Item_Icon;
   type Pixels is array (0 .. 49, 0 .. 95) of Color;
   Sentinel : constant Color := 16#335577#;
   Buffer : aliased Pixels := [others => [others => Sentinel]];
   Reference : Pixels;
   C : constant Canvas :=
     (addr => Buffer'Address, width => 80, height => 48, pitch => 96 * 4,
      others => <>);
   Clip : constant Rect := (7, 5, 51, 11);
   Gray : Boolean := False;
begin
   pragma Assert (CuBit.Fonts.Glyph'Size = (8 + 32 * 36) * 8);
   for Font in CuBit.Fonts.Face loop
      for Size in CuBit.Fonts.Raster_Size loop
         for Code in 32 .. 126 loop
            declare
               G : constant CuBit.Fonts.Glyph_Access :=
                 CuBit.Fonts.Get (Font, Character'Val (Code), Size);
            begin
               pragma Assert (G.Advance in 1 .. 32 and G.Height in 17 | 34);
               pragma Assert (G = CuBit.Fonts.Get (Font, Character'Val (Code), Size));
               for A of G.Alpha loop
                  Gray := Gray or else A in 1 .. 254;
               end loop;
            end;
         end loop;
      end loop;
   end loop;
   pragma Assert (Gray);
   pragma Assert (CuBit.Fonts.Get (CuBit.Fonts.Sans, Character'Val (255)) =
                  CuBit.Fonts.Get (CuBit.Fonts.Sans, '?'));
   pragma Assert (UI_Text_Height = 17 and Code_Text_Height = 17);
   pragma Assert (Code_Text_Width ("iiWW") = 32);
   pragma Assert (UI_Text_Width ("ii") < UI_Text_Width ("WW"));

   Draw_UI_Text (C, 0, 0, "AgjQ ./() IBM Plex", 16#FFFFFF#, 0);
   Reference := Buffer;
   Buffer := [others => [others => Sentinel]];
   Draw_UI_Text (With_Clip (C, Clip), 0, 0, "AgjQ ./() IBM Plex", 16#FFFFFF#, 0);
   for Y in Buffer'Range (1) loop
      for X in Buffer'Range (2) loop
         pragma Assert (Buffer (Y, X) =
           (if Point_In_Rect (X, Y, Clip) then Reference (Y, X) else Sentinel));
      end loop;
   end loop;
   Buffer := [others => [others => Sentinel]];
   Draw_Code_Text (C, 76, 43, "overflow", 16#FFFFFF#, 0);
   for Y in Buffer'Range (1) loop
      for X in Buffer'Range (2) loop
         if Y >= 48 or X >= 80 or X < 76 or Y < 43 then
            pragma Assert (Buffer (Y, X) = Sentinel);
         end if;
      end loop;
   end loop;
   Fill_Vertical_Gradient (C, (0, 0, 80, 48), 16#123456#, 16#ABCDEF#);
   Reference := Buffer;
   Draw_UI_Text_Transparent (C, 0, 0, "   ", 16#FFFFFF#);
   pragma Assert (Buffer = Reference);
   Draw_UI_Text_Transparent (With_Clip (C, Clip), 0, 0, "CuBit Alloy", 16#FFFFFF#);
   pragma Assert (Buffer /= Reference);
   for Y in Buffer'Range (1) loop
      for X in Buffer'Range (2) loop
         if not Point_In_Rect (X, Y, Clip) then
            pragma Assert (Buffer (Y, X) = Reference (Y, X));
         end if;
      end loop;
   end loop;
   -- Shared tabs are page selectors; ordinary list rows retain field colors.
   for Dark in Boolean loop
      declare
         Colors : constant Theme := (if Dark then CuBit_Alloy_Dark else CuBit_Alloy);
         R : constant Rect := (5, 5, 40, 25);
         Frame : Color;
      begin
         Buffer := [others => [others => Sentinel]];
         Draw_Tab (C, R, Colors, True, False, False, "", Vertical);
         pragma Assert (Buffer (15, 44) = Colors.face);
         pragma Assert (Buffer (15, 5) = Colors.accent);
         Frame := Buffer (29, 20);
         pragma Assert (Frame /= Colors.face and Frame /= Sentinel);
         Draw_Tab (C, R, Colors, True, False, False, "", Horizontal);
         pragma Assert (Buffer (29, 20) = Colors.face);
         pragma Assert (Buffer (15, 44) /= Colors.face and Buffer (15, 44) /= Sentinel);
         Draw_List_Item (C, R, Colors, False, False, "");
         pragma Assert (Buffer (15, 20) = Colors.field);
         Draw_List_Item (C, R, Colors, True, False, "");
         pragma Assert (Buffer (15, 20) = Colors.selection);
         Draw_Tab (C, R, Colors, False, True, False, "this label must not escape", Vertical);
         for Y in Buffer'Range (1) loop
            for X in Buffer'Range (2) loop
               if not Point_In_Rect (X, Y, R) then
                  pragma Assert (Buffer (Y, X) = Sentinel);
               end if;
            end loop;
         end loop;
      end;
   end loop;
   declare
      State : CuBit.UI.State.UI_State;
      Controls : CuBit.UI.Controls.Control_Map;
      Selected : Natural := 0;
      Result : Widget_Result;
      R : constant Rect := (5, 5, 60, 24);
      Painted : Boolean;
   begin
      for Icon in CuBit.UI.Trees.Tree_Item_Icon loop
         Buffer := [others => [others => Sentinel]];
         CuBit.UI.State.Begin_Frame (State);
         CuBit.UI.Controls.Clear (Controls);
         CuBit.UI.Trees.Tree_Item
           (C, State, Controls, 1, R, R, CuBit_Alloy, "", 1, Selected,
            icon => Icon, result => Result, retainedInput => True);
         Painted := False;
         for Y in Buffer'Range (1) loop
            for X in Buffer'Range (2) loop
               if not Point_In_Rect (X, Y, R) then pragma Assert (Buffer (Y, X) = Sentinel); end if;
               if X in 22 .. 37 and Y in 9 .. 24 then
                  Painted := Painted or Buffer (Y, X) /= CuBit_Alloy.field;
               end if;
            end loop;
         end loop;
         pragma Assert (Painted = (Icon /= CuBit.UI.Trees.No_Icon));
      end loop;
   end;
   pragma Assert (Is_Empty (Clamp_Rect (C, (Natural'Last, Natural'Last, 10, 10))));
   pragma Assert (Clamp_Rect (C, (0, 0, Natural'Last, Natural'Last)) = (0, 0, 80, 48));
   pragma Assert (Is_Empty (Clamp_Rect
     (With_Clip (C, (Natural'Last, Natural'Last, Natural'Last, Natural'Last)), (0, 0, 80, 48))));
   declare
      type Raster is array (0 .. 99, 0 .. 131) of Color;
      Pixels : aliased Raster;
      Target, Nested : Canvas;
      Expected : Color;
      PX, PY, LX, LY : Natural;
      Bitmap : constant ARGB_Bitmap :=
        [[16#FF00FF00#, 16#80FF0000#, 16#00000000#],
         [16#FFFFFFFF#, 16#FF102030#, 16#8000FF00#]];
      function Over_Blue (Source : Color) return Color is
         A : constant Unsigned_32 := Shift_Right (Source, 24);
         R : constant Unsigned_32 := ((Shift_Right (Source, 16) and 255) * A + 127) / 255;
         G : constant Unsigned_32 := ((Shift_Right (Source, 8) and 255) * A + 127) / 255;
         B : constant Unsigned_32 := ((Source and 255) * A + 255 * (255 - A) + 127) / 255;
      begin return Shift_Left (R, 16) or Shift_Left (G, 8) or B; end Over_Blue;
   begin
      for N in 1 .. 16 loop
         for D in 1 .. 16 loop
            Pixels := [others => [others => Sentinel]];
            Target := (addr => Pixels'Address, width => 8, height => 6,
              pitch => 132 * 4, densityNumerator => N, densityDenominator => D,
              others => <>);
            for Y in 0 .. 5 loop
               for X in 0 .. 7 loop
                  Set_Pixel (Target, X, Y, Color (100 * Y + X));
               end loop;
            end loop;
            PX := (8 * N + D - 1) / D;
            PY := (6 * N + D - 1) / D;
            for Y in Pixels'Range (1) loop
               for X in Pixels'Range (2) loop
                  Expected := Sentinel;
                  if X < PX and Y < PY then
                     LX := X * D / N; LY := Y * D / N;
                     Expected := Color (100 * LY + LX);
                  end if;
                  pragma Assert (Pixels (Y, X) = Expected);
               end loop;
            end loop;
            Pixels := [others => [others => Sentinel]];
            Target := With_Clip (Target, (4, 3, 1, 1));
            Nested := CuBit.UI.Surfaces.View
              (CuBit.UI.Surfaces.View (Target, (1, 1, 6, 4)), (2, 1, 3, 2));
            Fill_Rect (Nested, (0, 0, 3, 2), 16#ABCDEF#);
            for Y in Pixels'Range (1) loop
               for X in Pixels'Range (2) loop
                  Expected := Sentinel;
                  if X >= (4 * N + D - 1) / D and X < (5 * N + D - 1) / D and
                    Y >= (3 * N + D - 1) / D and Y < (4 * N + D - 1) / D
                  then Expected := 16#ABCDEF#; end if;
                  pragma Assert (Pixels (Y, X) = Expected);
               end loop;
            end loop;
            Pixels := [others => [others => 16#0000FF#]];
            Target.clipEnabled := False;
            Target := With_Clip (Target, (4, 2, 2, 2));
            Nested := CuBit.UI.Surfaces.View
              (CuBit.UI.Surfaces.View (Target, (1, 1, 6, 4)), (2, 1, 3, 2));
            Draw_Bitmap (Nested, 0, 0, Bitmap);
            for Y in Pixels'Range (1) loop
               for X in Pixels'Range (2) loop
                  Expected := 16#0000FF#;
                  if X >= (4 * N + D - 1) / D and X < (6 * N + D - 1) / D and
                    Y >= (2 * N + D - 1) / D and Y < (4 * N + D - 1) / D
                  then
                     LX := X * D / N; LY := Y * D / N;
                     Expected := Over_Blue (Bitmap (LY - 2, LX - 3));
                  end if;
                  pragma Assert (Pixels (Y, X) = Expected);
               end loop;
            end loop;
            Pixels := [others => [others => Sentinel]];
            Target.clipEnabled := False;
            Draw_Text (Target, 0, 0, "A", 16#FFFFFF#, 0);
            for Y in Pixels'Range (1) loop
               for X in Pixels'Range (2) loop
                  Expected := Sentinel;
                  if X < PX and Y < PY then
                     LX := X * D / N; LY := Y * D / N;
                     Expected := (if (Font8x16.font (Character'Pos ('A')) (LY) and
                       Shift_Right (Unsigned_8'(16#80#), LX)) /= 0 then 16#FFFFFF# else 0);
                  end if;
                  pragma Assert (Pixels (Y, X) = Expected);
               end loop;
            end loop;
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("PASS 256 densities: adjacent cells, padded rows, clipped nested views, alpha bitmaps and bitmap font");
   end;
   declare
      package L renames Compositor_Glyph_Layout;
      type Image is array (0 .. 387, 0 .. 643) of Color;
      Actual : aliased Image;
      type Bytes is array (Natural range <>) of Unsigned_8;
      Mask : aliased Bytes (0 .. L.Maximum_Bytes - 1);
      type Scales is array (Positive range <>) of L.G.UI_Scale;
      Values : constant Scales := [(5, 4), (3, 2), (2, 1), (16, 1), (1, 2)];
      Target : Canvas;
      Plan : L.Layout;
      OK, Different : Boolean := False;
      Advance, N, D, Left, Top, Clip_Left, Clip_Top, Clip_Right, Clip_Bottom : Natural;
      Width, A, R, G, B : Unsigned_32;
      Expected : Color;
      Alpha : Unsigned_8;
      Old : CuBit.Fonts.Glyph_Access;
   begin
      for Scale of Values loop
         N := Natural (Scale.Numerator); D := Natural (Scale.Denominator);
         Plan := L.Plan (Scale);
         Left := (N + D - 1) / D; Top := Left;
         Clip_Left := (3 * N + D - 1) / D; Clip_Right := (20 * N + D - 1) / D;
         Clip_Top := (2 * N + D - 1) / D; Clip_Bottom := (16 * N + D - 1) / D;
         for Face in CuBit.Fonts.Face loop
            Old := CuBit.Fonts.Get (Face, 'A');
            Compositor_Glyph_FFI.Rasterize
              (CuBit.Fonts.Face'Pos (Face), Character'Pos ('A'), Plan,
               Mask'Address, Mask'Length, Advance, OK);
            pragma Assert (OK);
            Actual := [others => [others => Sentinel]];
            Target := (addr => Actual'Address, width => 40, height => 24,
              pitch => 644 * 4, densityNumerator => N, densityDenominator => D,
              clipEnabled => True, clip => (3, 2, 17, 14), others => <>);
            if Face = CuBit.Fonts.Sans then
               Draw_UI_Text_Transparent (Target, 1, 1, "A", 16#FFFFFF#);
            else
               Draw_Code_Text (Target, 1, 1, "A", 16#FFFFFF#, 16#204060#);
            end if;
            Width := Unsigned_32 ((9 * N + D - 1) / D);
            for Y in Actual'Range (1) loop
               for X in Actual'Range (2) loop
                  Expected := Sentinel;
                  if X >= Clip_Left and X < Clip_Right and Y >= Clip_Top and Y < Clip_Bottom then
                     if Face = CuBit.Fonts.Monospace and X < Natural (Width) then Expected := 16#204060#; end if;
                     if X >= Left and Y >= Top and X - Left < Plan.Width and Y - Top < Plan.Height then
                        Alpha := Mask ((Y - Top) * Plan.Pitch + X - Left);
                        A := Unsigned_32 (Alpha);
                        R := (255 * A + (Shift_Right (Expected, 16) and 255) * (255 - A) + 127) / 255;
                        G := (255 * A + (Shift_Right (Expected, 8) and 255) * (255 - A) + 127) / 255;
                        B := (255 * A + (Expected and 255) * (255 - A) + 127) / 255;
                        Expected := Shift_Left (R, 16) or Shift_Left (G, 8) or B;
                        if N = 2 and D = 1 then
                           Different := Different or Alpha /= Old.Alpha ((Y - Top) / 2, (X - Left) / 2);
                        end if;
                     end if;
                  end if;
                  pragma Assert (Actual (Y, X) = Expected);
               end loop;
            end loop;
         end loop;
      end loop;
      pragma Assert (Different);
      Ada.Text_IO.Put_Line ("PASS native-density TrueType pixels: 5 scales, 2 faces, clipping, alpha, fresh outlines");
   end;
   Ada.Text_IO.Put_Line ("PASS TrueType, clipping, tabs, list colors and shared tree icons");
end Main;
