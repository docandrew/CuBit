with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Fonts;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.State;
with CuBit.UI.Trees;

procedure Main is
   use type CuBit.Fonts.Glyph_Access;
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
      begin
         Buffer := [others => [others => Sentinel]];
         Draw_Tab (C, R, Colors, True, False, False, "", Vertical);
         pragma Assert (Buffer (15, 44) = Colors.face);
         pragma Assert (Buffer (15, 5) = Colors.accent);
         pragma Assert (Buffer (29, 20) = Colors.shadow);
         Draw_Tab (C, R, Colors, True, False, False, "", Horizontal);
         pragma Assert (Buffer (29, 20) = Colors.face);
         pragma Assert (Buffer (15, 44) = Colors.shadow);
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
   Ada.Text_IO.Put_Line ("PASS TrueType, clipping, tabs, list colors and shared tree icons");
end Main;
