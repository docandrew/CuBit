with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Menus; use CuBit.UI.Menus;
with CuBit.UI.Controls;
procedure Menus_Tests is
   package Controls renames CuBit.UI.Controls;
   File_Text : aliased constant String := "File";
   Edit_Text : aliased constant String := "Edit";
   Open_Text : aliased constant String := "Open";
   Settings_Text : aliased constant String := "Settings";
   Shortcut_Text : aliased constant String := "Ctrl+O";
   D : Model;
   S : Menu_State;
   Map : Controls.Control_Map;
   Pixels : aliased array (0 .. 400 * 240 - 1) of Unsigned_32 := [others => 0];
   C : constant Canvas := (addr => Pixels'Address, width => 400, height => 240,
                          pitch => 1600, others => <>);
   Command : Natural;
   Handled, Changed : Boolean;
   procedure Press (K : Key; Letter : Character := ' ') is
   begin
      Handle_Key (S, D, K, Command, Handled, Letter);
   end Press;
   procedure Paint is
   begin
      Controls.Clear (Map);
      Controls.Add_Button (Map, 100, (0, 28, 400, 212), (0, 0, 400, 240));
      Draw (C, Map, S, D, 1, (0, 0, 400, 28), Classic);
      pragma Assert (Controls.Is_Valid (Map));
   end Paint;
   procedure Pointer (ID : Natural; Action : Controls.Pointer_Action;
                      X, Y : Natural) is
      Dispatched : Boolean;
   begin
      Controls.Dispatch_Pointer (Map, ID, Action, X, Y, Changed, Dispatched);
      Handle_Pointer (S, D, Map, 1, ID, Action, Command, Handled);
   end Pointer;
begin
   D.Menu_Count := 2; D.Item_Count := 5;
   D.Menus (1) := (File_Text'Unchecked_Access, 'F');
   D.Menus (2) := (Edit_Text'Unchecked_Access, 'E');
   D.Items (1) := (Parent => 1, Caption => Open_Text'Unchecked_Access,
     Shortcut => Shortcut_Text'Unchecked_Access, Mnemonic => 'O', Command => 10,
     others => <>);
   D.Items (2) := (Parent => 1, Separator => True, others => <>);
   D.Items (3) := (Parent => 1, Command => 11, Enabled => False, others => <>);
   D.Items (4) := (Parent => 1, Caption => Settings_Text'Unchecked_Access,
     Mnemonic => 'S', Command => 12, Checked => True, others => <>);
   D.Items (5) := (Parent => 2, Caption => Settings_Text'Unchecked_Access,
     Mnemonic => 'S', Command => 20, others => <>);
   pragma Assert (Valid (D));
   Paint;
   declare
      UY : constant Natural := (28 - UI_Text_Height) / 2 + UI_Text_Height - 2;
   begin
      for X in 10 .. 10 + UI_Text_Width ("F") - 1 loop
         pragma Assert (Pixels (UY * 400 + X) = Classic.text);
      end loop;
      -- Idle title repair must preserve the gradient underneath its padding.
      pragma Assert (Pixels (10 * 400 + 5) = Pixels (10 * 400 + 350));
      pragma Assert (Pixels (2 * 400 + 350) /= Pixels (25 * 400 + 350));
      Press (Mnemonic, 'f'); Paint;
      for X in 10 .. 10 + UI_Text_Width ("F") - 1 loop
         pragma Assert (Pixels (UY * 400 + X) = Classic.selectionText);
      end loop;
      for X in 28 .. 28 + UI_Text_Width ("O") - 1 loop
         pragma Assert (Pixels ((32 + UY) * 400 + X) = Classic.selectionText);
      end loop;
      Press (Escape);
   end;
   Press (Down); pragma Assert (not Handled);
   Press (Mnemonic, 'f'); pragma Assert (Open_Menu (S) = 1 and Selected_Item (S) = 1);
   Press (Down); pragma Assert (Selected_Item (S) = 4);
   Press (Down); pragma Assert (Selected_Item (S) = 1);
   Press (Up); pragma Assert (Selected_Item (S) = 4);
   Press (Home); pragma Assert (Selected_Item (S) = 1);
   Press (End_Key); pragma Assert (Selected_Item (S) = 4);
   Press (Enter); pragma Assert (Command = 12 and not Is_Open (S));
   Press (Activate); Press (Right); pragma Assert (Open_Menu (S) = 2);
   Press (Right); pragma Assert (Open_Menu (S) = 1);
   Press (Left); pragma Assert (Open_Menu (S) = 2);
   Press (Mnemonic, 's'); pragma Assert (Command = 20 and not Is_Open (S));
   Press (Activate); Press (Escape); pragma Assert (not Is_Open (S));
   Press (Activate); Press (Tab_Key); pragma Assert (not Is_Open (S));
   for Cycle in 1 .. 100 loop
      Paint;
      Pointer (Title_ID (1, 1), Controls.Pointer_Press, 10, 10);
      pragma Assert (Is_Open (S));
      Paint;
      Pointer (Title_ID (1, 1), Controls.Pointer_Release, 10, 10);
      pragma Assert (Is_Open (S) and Command = 0);
      pragma Assert (Controls.Hit (Map, 50, 40) = Item_ID (1, 1));
      pragma Assert (Controls.Hit (Map, 50, 64) = 73); -- separator shield
      pragma Assert (Controls.Hit (Map, 50, 80) = 73); -- disabled shield
      Pointer (Item_ID (1, 1), Controls.Pointer_Press, 50, 40);
      Paint; -- retained press survives a redraw
      Pointer (Item_ID (1, 1), Controls.Pointer_Release, 50, 40);
      pragma Assert (Command = 10 and not Is_Open (S));
      Paint;
      Pointer (Title_ID (1, 1), Controls.Pointer_Press, 10, 10);
      Paint;
      Pointer (Title_ID (1, 2), Controls.Pointer_Move, 75, 10);
      pragma Assert (Open_Menu (S) = 2);
      Paint;
      Handle_Pointer (S, D, Map, 1, 100, Controls.Pointer_Press, Command, Handled);
      pragma Assert (Handled and not Is_Open (S) and Command = 0);
      pragma Assert (not Controls.Is_Active (Map, 100));
   end loop;
   D.Items (1).Mnemonic := 'S';
   Press (Activate); Press (Mnemonic, 's'); pragma Assert (Selected_Item (S) = 1);
   Press (Mnemonic, 's'); pragma Assert (Selected_Item (S) = 4 and Command = 0);
   Press (Mnemonic, 's'); pragma Assert (Selected_Item (S) = 1 and Command = 0);
   Dismiss (S);
   -- Cancellation and release outside never activate an item.
   Press (Activate); Paint;
   Pointer (Item_ID (1, 1), Controls.Pointer_Press, 50, 40);
   Pointer (Item_ID (1, 1), Controls.Pointer_Cancel, 50, 40);
   Pointer (Item_ID (1, 1), Controls.Pointer_Release, 50, 40);
   pragma Assert (Command = 0 and not Is_Open (S));
   Press (Activate); Paint;
   Pointer (Item_ID (1, 1), Controls.Pointer_Press, 50, 40);
   Pointer (Item_ID (1, 1), Controls.Pointer_Release, 390, 230);
   pragma Assert (Command = 0);
   Dismiss (S);
   -- Standard menu press-drag-release: App retains the TITLE capture while
   -- the menu controller receives the current hit. A redraw never loses it.
   for Scenario in 1 .. 5 loop
      declare
         Dispatched : Boolean;
         Target : Natural;
         Y : constant Natural := (if Scenario = 2 then 80 else 40);
      begin
         Dismiss (S); Paint;
         Pointer (Title_ID (1, 1), Controls.Pointer_Press, 10, 10);
         Paint;
         Target := (if Scenario = 3 then 100 else Controls.Hit (Map, 50, Y));
         Controls.Dispatch_Pointer (Map, Title_ID (1, 1),
           Controls.Pointer_Move, 50, Y, Changed, Dispatched);
         Handle_Pointer (S, D, Map, 1, Target, Controls.Pointer_Move, Command, Handled);
         Paint;
         if Scenario = 4 then
            Pointer (Title_ID (1, 1), Controls.Pointer_Cancel, 50, Y);
         elsif Scenario = 5 then
            -- Releasing on the title ends the drag; a later stray item
            -- release cannot activate without an item press.
            Pointer (Title_ID (1, 1), Controls.Pointer_Release, 10, 10);
         end if;
         Controls.Dispatch_Pointer (Map, Title_ID (1, 1),
           Controls.Pointer_Release, 50, Y, Changed, Dispatched);
         Handle_Pointer (S, D, Map, 1, Target, Controls.Pointer_Release, Command, Handled);
         pragma Assert (Command = (if Scenario = 1 then 10 else 0));
         if Scenario = 3 or Scenario = 4 then
            pragma Assert (not Is_Open (S));
         end if;
         pragma Assert (not Controls.Is_Active (Map, 100));
      end;
   end loop;
   Dismiss (S);
   -- Clipping applies to both native pixels and control registrations at
   -- 100% and 200% density; untouched pixels are exact sentinels.
   for Density in 1 .. 2 loop
      declare
         DC : Canvas := C;
      begin
         DC.width := 400 / Density; DC.height := 240 / Density;
         DC.densityNumerator := Density;
         Pixels := [others => 16#123456#];
         Controls.Clear (Map); Press (Activate);
         Draw (With_Clip (DC, (20, 10, 160, 100)), Map, S, D, 1,
               (0, 0, 200, 28), CuBit_Alloy_Dark);
         pragma Assert (Controls.Hit (Map, 19, 20) = 0);
         pragma Assert (Controls.Hit (Map, 21, 20) = Title_ID (1, 1));
         for Y in 0 .. 239 loop
            for X in 0 .. 399 loop
               if X < 20 * Density or X >= 180 * Density or
                  Y < 10 * Density or Y >= 110 * Density
               then
                  pragma Assert (Pixels (Y * 400 + X) = 16#123456#);
               end if;
            end loop;
         end loop;
         Dismiss (S);
      end;
   end loop;
   -- Long menus retain keyboard access to every row, scroll to selection,
   -- and stay bounded by the canvas. Only visible controls are registered.
   D.Item_Count := Max_Items;
   for I in 1 .. Max_Items loop
      D.Items (I) := (Parent => 1, Command => I, others => <>);
   end loop;
   Press (Activate); Press (End_Key); Paint;
   pragma Assert (Controls.Hit (Map, 50, 210) = Item_ID (1, 64));
   pragma Assert (Controls.Bounds (Map, Item_ID (1, 1)).h = 0);
   Press (Enter); pragma Assert (Command = 64);
   -- Empty, invalid and zero-sized models are inert.
   D.Items (1).Command := 0;
   pragma Assert (not Valid (D));
   Press (Activate); pragma Assert (not Handled and not Is_Open (S));
   Paint; pragma Assert (Controls.Hit (Map, 10, 10) = 0);
   D := (others => <>); Press (Activate); pragma Assert (not Handled);
   Draw (C, Map, S, D, 1, (0, 0, 0, 0), Classic);
   Ada.Text_IO.Put_Line ("native menu controller: keyboard, retained pointer, clipping and overflow PASS");
end Menus_Tests;
