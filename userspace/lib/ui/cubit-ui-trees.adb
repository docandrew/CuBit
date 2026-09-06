------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tree controls
------------------------------------------------------------------------------
package body CuBit.UI.Trees is
   procedure View_Frame
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       focused : Boolean;
       content : out CuBit.UI.Rect)
   is
   begin
      CuBit.UI.Fill_Rect (c, bounds, colors.field);
      CuBit.UI.Stroke_Rect (c, bounds, colors.darkShadow, colors.highlight);
      if bounds.w > 3 and then bounds.h > 3 then
         CuBit.UI.Stroke_Rect
           (c, (x => bounds.x + 1, y => bounds.y + 1,
                w => bounds.w - 2, h => bounds.h - 2),
            colors.shadow, colors.edge);
      end if;
      if focused and then bounds.w > 7 and then bounds.h > 7 then
         CuBit.UI.Stroke_Rect
           (c, (x => bounds.x + 3, y => bounds.y + 3,
                w => bounds.w - 6, h => bounds.h - 6),
            colors.accent, colors.accent);
      end if;
      if bounds.w > 8 and then bounds.h > 8 then
         content := (x => bounds.x + 4, y => bounds.y + 4,
                     w => bounds.w - 8, h => bounds.h - 8);
      else
         content := (others => 0);
      end if;
   end View_Frame;

   procedure Draw_Disclosure
      (c : CuBit.UI.Canvas;
       x, y : Natural;
       colors : CuBit.UI.Theme;
       expanded : Boolean)
   is
      box : constant CuBit.UI.Rect := (x => x, y => y, w => 9, h => 9);
   begin
      CuBit.UI.Fill_Rect (c, box, colors.field);
      CuBit.UI.Stroke_Rect (c, box, colors.shadow, colors.highlight);
      CuBit.UI.Fill_Rect
        (c, (x => x + 2, y => y + 4, w => 5, h => 1), colors.text);
      if not expanded then
         CuBit.UI.Fill_Rect
           (c, (x => x + 4, y => y + 2, w => 1, h => 5), colors.text);
      end if;
   end Draw_Disclosure;

   procedure Draw_Item_Icon
      (c : CuBit.UI.Canvas;
       x, y : Natural;
       colors : CuBit.UI.Theme;
       icon : Tree_Item_Icon;
       bg : CuBit.UI.Color)
   is
      fill : CuBit.UI.Color := colors.accent;
   begin
      case icon is
         when No_Icon       => return;
         when Computer_Icon => fill := colors.accent;
         when Bus_Icon      => fill := colors.muted;
         when Device_Icon   => fill := colors.shadow;
         when Input_Icon    => fill := 16#866A3A#;
         when Storage_Icon  => fill := 16#506D8A#;
         when Network_Icon  => fill := 16#39795A#;
         when Display_Icon  => fill := 16#694E85#;
         when Audio_Icon    => fill := 16#94683F#;
         when Service_Icon  => fill := 16#527782#;
         when Warning_Icon  => fill := colors.danger;
      end case;
      --  A restrained 12 px pictogram: colored face, one-pixel outline, and
      --  a small highlight. It remains legible without introducing an asset
      --  dependency into the generic tree control.
      CuBit.UI.Fill_Rect (c, (x => x, y => y, w => 12, h => 12), fill);
      CuBit.UI.Stroke_Rect
        (c, (x => x, y => y, w => 12, h => 12), colors.darkShadow,
         colors.darkShadow);
      CuBit.UI.Fill_Rect (c, (x => x + 2, y => y + 2, w => 7, h => 1), bg);
   end Draw_Item_Icon;

   procedure Tree_Item
      (c : CuBit.UI.Canvas;
       st : in out CuBit.UI.State.UI_State;
       controls : in out CuBit.UI.Controls.Control_Map;
       id : CuBit.UI.Controls.Control_ID;
       bounds : CuBit.UI.Rect;
       damage : CuBit.UI.Rect;
       colors : CuBit.UI.Theme;
       label : String;
       itemIndex : Natural;
       selectedIndex : in out Natural;
       depth : Natural := 0;
       expanded : Boolean := False;
       hasChildren : Boolean := False;
       icon : Tree_Item_Icon := No_Icon;
       focused : Boolean := True;
       lastSibling : Boolean := False;
       ancestorBranches : Unsigned_64 := 0;
       result : out CuBit.UI.Widget_Result)
   is
      bg : CuBit.UI.Color := colors.field;
      fg : CuBit.UI.Color := colors.text;
      indent : constant Natural := depth * TREE_INDENT;
      branchX : Natural;
      centerY : constant Natural := bounds.y + bounds.h / 2;
      iconX : Natural;
      textX : Natural;
      clipped : constant CuBit.UI.Canvas := CuBit.UI.With_Clip (c, bounds);
   begin
      CuBit.UI.Controls.Add (controls, id, bounds, damage);
      result := CuBit.UI.State.Button
        (st, CuBit.UI.Controls.Bounds (controls, id),
         CuBit.UI.State.Widget_ID (id));
      if result.activated and then selectedIndex /= itemIndex then
         selectedIndex := itemIndex;
         CuBit.UI.State.Request_Followup_Render (st);
      end if;

      if selectedIndex = itemIndex then
         if focused then
            bg := colors.selection;
            fg := colors.selectionText;
         else
            bg := colors.panel;
         end if;
      elsif result.hot then
         bg := colors.highlight;
      end if;

      CuBit.UI.Fill_Rect (clipped, bounds, bg);

      --  Paint ancestor continuation lines first. The bit for depth N says
      --  that the branch at that depth has a following sibling.
      if depth > 0 then
         for level in 0 .. Natural'Min (depth - 1, 63) loop
            if (ancestorBranches and Shift_Left (Unsigned_64'(1), level)) /= 0
            then
               branchX := bounds.x + level * TREE_INDENT + 8;
               CuBit.UI.Fill_Rect
                 (clipped,
                  (x => branchX, y => bounds.y, w => 1, h => bounds.h),
                  colors.shadow);
            end if;
         end loop;

         branchX := bounds.x + (depth - 1) * TREE_INDENT + 8;
         CuBit.UI.Fill_Rect
           (clipped,
            (x => branchX, y => bounds.y, w => 1,
             h => (if lastSibling then bounds.h / 2 + 1 else bounds.h)),
            colors.shadow);
         CuBit.UI.Fill_Rect
           (clipped,
            (x => branchX, y => centerY, w => TREE_INDENT / 2 + 1, h => 1),
            colors.shadow);
      end if;

      if hasChildren then
         Draw_Disclosure
           (clipped, bounds.x + indent + 4, centerY - 4, colors, expanded);
      end if;

      iconX := bounds.x + indent + 17;
      if icon /= No_Icon then
         Draw_Item_Icon (clipped, iconX, centerY - 6, colors, icon, bg);
         textX := iconX + 17;
      else
         textX := iconX;
      end if;
      CuBit.UI.Draw_UI_Text
        (clipped, textX,
         bounds.y +
           (if bounds.h > CuBit.UI.UI_Text_Height
            then (bounds.h - CuBit.UI.UI_Text_Height) / 2 else 0),
         label, fg, bg);
   end Tree_Item;
end CuBit.UI.Trees;
