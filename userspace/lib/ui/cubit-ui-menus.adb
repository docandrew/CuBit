package body CuBit.UI.Menus is
   use type Controls.Pointer_Action;
   function Title_ID (Base : ID_Base; Index : Positive)
      return Controls.Control_ID is (Base + Index - 1);
   function Item_ID (Base : ID_Base; Index : Positive)
      return Controls.Control_ID is (Base + Max_Menus + Index - 1);
   function Is_Menu_Control (Base : ID_Base; Target : Controls.Control_ID)
      return Boolean is (Target >= Base and then Target <= Base + 72);
   function Center_Text_Y (R : Rect) return Natural is
     (R.y + (if R.h > UI_Text_Height then (R.h - UI_Text_Height) / 2 else 0));
   function Value (S : Text) return String is
     (if S = null then "" else S.all);
   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
   procedure Underline_Mnemonic
     (C : Canvas; X, Y : Natural; Caption : String;
      Mnemonic : Character; Ink : Color)
   is
   begin
      if Mnemonic = ' ' then return; end if;
      for I in Caption'Range loop
         if Lower (Caption (I)) = Lower (Mnemonic) then
            Fill_Rect (C,
              (X + UI_Text_Width (Caption (Caption'First .. I - 1)),
               Y + UI_Text_Height - 2,
               UI_Text_Width (Caption (I .. I)), 1), Ink);
            return;
         end if;
      end loop;
   end Underline_Mnemonic;

   function Valid (Definition : Model) return Boolean is
   begin
      for I in 1 .. Definition.Item_Count loop
         if Definition.Items (I).Parent > Definition.Menu_Count or else
           (not Definition.Items (I).Separator and then
            Definition.Items (I).Command = 0)
         then
            return False;
         end if;
      end loop;
      return True;
   end Valid;
   function Is_Open (State : Menu_State) return Boolean is (State.Opened /= 0);
   function Open_Menu (State : Menu_State) return Natural is (State.Opened);
   function Selected_Item (State : Menu_State) return Natural is (State.Selected);
   procedure Dismiss (State : in out Menu_State) is
   begin
      State := (others => <>);
   end Dismiss;
   function Selectable (D : Model; M, I : Natural) return Boolean is
     (I in 1 .. D.Item_Count and then D.Items (I).Parent = M and then
      D.Items (I).Enabled and then not D.Items (I).Separator);
   procedure Move (S : in out Menu_State; D : Model; Forward : Boolean) is
      I : Natural := (if S.Selected <= D.Item_Count then S.Selected else 0);
   begin
      if D.Item_Count = 0 then S.Selected := 0; return; end if;
      for Attempt in 1 .. D.Item_Count loop
         if Forward then
            I := (if I >= D.Item_Count then 1 else I + 1);
         else
            I := (if I <= 1 then D.Item_Count else I - 1);
         end if;
         if Selectable (D, S.Opened, I) then S.Selected := I; return; end if;
      end loop;
      S.Selected := 0;
   end Move;
   procedure Handle_Key
     (State : in out Menu_State; Definition : Model; Event : Key;
      Command : out Natural; Handled : out Boolean;
      Letter : Character := ' ')
   is
   begin
      Command := 0;
      Handled := False;
      State.Title_Drag := False;
      if not Valid (Definition) or else Definition.Menu_Count = 0 then
         Dismiss (State); return;
      end if;
      if State.Opened > Definition.Menu_Count then Dismiss (State); end if;
      if Event = Activate then
         if Is_Open (State) then Dismiss (State);
         else State.Opened := 1; State.Selected := 0;
         end if;
         Handled := True; return;
      elsif Event = Mnemonic and then not Is_Open (State) then
         for I in 1 .. Definition.Menu_Count loop
            if Letter /= ' ' and then
              Lower (Definition.Menus (I).Mnemonic) = Lower (Letter)
            then
               State.Opened := I; State.Selected := 0;
               Move (State, Definition, True); Handled := True; return;
            end if;
         end loop;
         return;
      end if;
      if not Is_Open (State) then return; end if;
      Handled := True;
      case Event is
         when Escape | Tab_Key => Dismiss (State);
         when Left | Right =>
            if Event = Right then
               State.Opened := (if State.Opened = Definition.Menu_Count then 1
                                else State.Opened + 1);
            else
               State.Opened := (if State.Opened = 1 then Definition.Menu_Count
                                else State.Opened - 1);
            end if;
            State.Selected := 0; Move (State, Definition, True);
         when Home | End_Key =>
            State.Selected := 0;
            Move (State, Definition, Event = Home);
         when Up | Down => Move (State, Definition, Event = Down);
         when Enter | Space =>
            if Selectable (Definition, State.Opened, State.Selected) then
               Command := Definition.Items (State.Selected).Command;
               Dismiss (State);
            else
               Move (State, Definition, True);
            end if;
         when Mnemonic =>
            --  Duplicate mnemonics cycle; a unique enabled mnemonic activates.
            declare
               Matches, Match : Natural := 0;
               Old : constant Natural := State.Selected;
            begin
               for I in 1 .. Definition.Item_Count loop
                  if Selectable (Definition, State.Opened, I) and then
                    Letter /= ' ' and then
                    Lower (Definition.Items (I).Mnemonic) = Lower (Letter)
                  then
                     Matches := Matches + 1;
                     if Match = 0 or else (Match <= Old and then I > Old) then
                        Match := I;
                     end if;
                  end if;
               end loop;
               if Matches = 1 then
                  Command := Definition.Items (Match).Command; Dismiss (State);
               elsif Matches > 1 then State.Selected := Match;
               end if;
            end;
         when Activate => null;
      end case;
   end Handle_Key;

   procedure Handle_Pointer
     (State : in out Menu_State; Definition : Model;
      Map : in out Controls.Control_Map; Base : ID_Base;
      Target : Controls.Control_ID; Action : Controls.Pointer_Action;
      Command : out Natural; Handled : out Boolean)
   is
      Was_Open : constant Boolean := Is_Open (State);
      Was_Drag : constant Boolean := State.Title_Drag;
      I : Natural;
      Ignored : Boolean;
   begin
      Command := 0; Handled := Was_Open;
      if Action = Controls.Pointer_Release then State.Title_Drag := False; end if;
      if not Valid (Definition) or else not Controls.Is_Valid (Map) then
         Dismiss (State); return;
      end if;
      if State.Opened > Definition.Menu_Count then Dismiss (State); end if;
      if Action = Controls.Pointer_Cancel then Dismiss (State); return; end if;
      if Action = Controls.Pointer_Move then
         State.Hot_Title := (if Target >= Base and then
           Target < Base + Definition.Menu_Count then Target - Base + 1 else 0);
      end if;
      if Target >= Base and then Target < Base + Definition.Menu_Count then
         I := Target - Base + 1;
         Handled := True;
         Ignored := Controls.Take_Activated (Map, Target);
         if Action = Controls.Pointer_Press then
            if State.Opened = I then Dismiss (State);
            else
               State.Opened := I; State.Selected := 0; State.Title_Drag := True;
            end if;
         elsif Action = Controls.Pointer_Move and then Is_Open (State) and then
           State.Opened /= I
         then
            State.Opened := I; State.Selected := 0;
         end if;
      elsif Is_Open (State) and then Target >= Base + Max_Menus and then
        Target < Base + Max_Menus + Definition.Item_Count
      then
         I := Target - Base - Max_Menus + 1;
         if Selectable (Definition, State.Opened, I) then
            if Action = Controls.Pointer_Move or else
               Action = Controls.Pointer_Press
            then State.Selected := I;
            elsif Action = Controls.Pointer_Release and then
              (Controls.Take_Activated (Map, Target) or Was_Drag)
            then
               Command := Definition.Items (I).Command; Dismiss (State);
            end if;
         end if;
      elsif Was_Open and then
        (Action = Controls.Pointer_Press or else
         (Action = Controls.Pointer_Release and then Was_Drag)) and then
        Target /= Base + Max_Menus + Max_Items
      then
         Dismiss (State);
      end if;
   end Handle_Pointer;

   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; State : Menu_State;
      Definition : Model; Base : ID_Base; Bounds : Rect; Colors : Theme;
      Popup_Width : Positive := 260; Row_Height : Positive := 28)
   is
      Available : constant Rect := Input_Rect (C, (0, 0, C.width, C.height));
      Bar : constant Rect := Input_Rect (C, Bounds);
      X, Anchor : Natural := Bar.x;
      R, Popup : Rect;
      Total, Rank, Selected_Rank, First, Rows : Natural := 0;
      PC : Canvas;
      Bg, Fg : Color;
      W : Natural;
      Popup_Height, Offset : Natural := 0;
      Padding : constant := 4;
   begin
      if not Valid (Definition) or else Is_Empty (Bar) then return; end if;
      Draw_Menu_Bar (With_Clip (C, Bar), Bar, Colors);
      for I in 1 .. Definition.Menu_Count loop
         W := Natural'Min (Bar.x + Bar.w - X,
           UI_Text_Width (Value (Definition.Menus (I).Caption)) + 20);
         R := (X, Bar.y, W, Bar.h);
         if State.Opened = I then Anchor := X; end if;
         PC := With_Clip (C, R);
         Draw_Menu_Title (PC, R, Colors, State.Hot_Title = I, State.Opened = I,
                          Value (Definition.Menus (I).Caption));
         Underline_Mnemonic
           (With_Clip (PC, (R.x + Natural'Min (8, R.w), R.y,
              R.w - Natural'Min (16, R.w), R.h)),
            R.x + 10, Center_Text_Y (R), Value (Definition.Menus (I).Caption),
            Definition.Menus (I).Mnemonic,
            (if State.Opened = I then Colors.selectionText else Colors.text));
         Controls.Add_Button (Map, Title_ID (Base, I),
                              Input_Rect (PC, R), Rect'(0, 0, C.width, C.height));
         X := X + W;
      end loop;
      -- Titles repaint their backgrounds; restore the single outer strip rim.
      Stroke_Rect (With_Clip (C, Bar), Bar, Colors.highlight, Colors.shadow);
      if State.Opened not in 1 .. Definition.Menu_Count then return; end if;
      for I in 1 .. Definition.Item_Count loop
         if Definition.Items (I).Parent = State.Opened then
            if I = State.Selected then Selected_Rank := Total; end if;
            Total := Total + 1;
         end if;
      end loop;
      Rows := Natural'Min (Total, (Available.y + Available.h - (Bar.y + Bar.h) -
                                  Natural'Min (8, Available.y + Available.h - (Bar.y + Bar.h))) / Row_Height);
      if Rows = 0 then return; end if;
      if Selected_Rank >= Rows then First := Selected_Rank - Rows + 1; end if;
      Rank := 0; Popup_Height := 2 * Padding;
      for I in 1 .. Definition.Item_Count loop
         if Definition.Items (I).Parent = State.Opened then
            if Rank >= First and then Rank < First + Rows then
               Popup_Height := Popup_Height +
                 (if Definition.Items (I).Separator then Natural'Min (8, Row_Height) else Row_Height);
            end if;
            Rank := Rank + 1;
         end if;
      end loop;
      W := Natural'Min (Popup_Width, Available.w);
      Popup := (Natural'Min (Anchor, Available.x + Available.w - W),
                Bar.y + Bar.h, W, Popup_Height);
      PC := With_Clip (C, Popup);
      Fill_Rect (PC, Popup, Colors.panel);
      Controls.Add_Button (Map, Base + Max_Menus + Max_Items,
                           Input_Rect (PC, Popup), Rect'(0, 0, C.width, C.height));
      Rank := 0; Offset := Padding;
      for I in 1 .. Definition.Item_Count loop
         if Definition.Items (I).Parent = State.Opened then
            if Rank >= First and then Rank < First + Rows then
               R := (Popup.x + Natural'Min (Padding, Popup.w), Popup.y + Offset,
                     Popup.w - Natural'Min (2 * Padding, Popup.w),
                     (if Definition.Items (I).Separator then Natural'Min (8, Row_Height) else Row_Height));
               Offset := Offset + R.h;
               declare
                  RC : constant Canvas := With_Clip (PC, R);
                  It : Item renames Definition.Items (I);
               begin
                  if It.Separator then
                     if R.w > 16 then
                        Fill_Rect (RC, (R.x + 8, R.y + R.h / 2, R.w - 16, 1),
                                   Colors.shadow);
                     end if;
                  else
                     Bg := (if State.Selected = I and then It.Enabled then
                              Colors.selection else Colors.panel);
                     Fg := (if not It.Enabled then Colors.muted
                            elsif State.Selected = I then Colors.selectionText
                            else Colors.text);
                     Fill_Rect (RC, R, Bg);
                     if It.Checked then
                        -- A small native checkmark, independent of font glyphs.
                        for Step in 0 .. 2 loop
                           Fill_Rect (RC, (R.x + 6 + Step, R.y + R.h / 2 + Step,
                                          2, 2), Fg);
                        end loop;
                        for Step in 0 .. 5 loop
                           if R.h / 2 + 2 >= Step then
                              Fill_Rect (RC, (R.x + 9 + Step,
                                R.y + R.h / 2 + 2 - Step, 2, 2), Fg);
                           end if;
                        end loop;
                     end if;
                     declare
                        SW : constant Natural := UI_Text_Width (Value (It.Shortcut));
                        Caption_Width : constant Natural :=
                          (if R.w > SW + 40 then R.w - SW - 40 else 0);
                     begin
                        Draw_UI_Text
                          (With_Clip (RC, (R.x + 24, R.y, Caption_Width, R.h)),
                           R.x + 24, Center_Text_Y (R), Value (It.Caption), Fg, Bg);
                        Underline_Mnemonic
                          (With_Clip (RC, (R.x + 24, R.y, Caption_Width, R.h)),
                           R.x + 24, Center_Text_Y (R), Value (It.Caption),
                           It.Mnemonic, Fg);
                        if SW + 8 < R.w then
                           Draw_UI_Text (RC, R.x + R.w - SW - 8,
                             Center_Text_Y (R), Value (It.Shortcut), Fg, Bg);
                        end if;
                     end;
                     if It.Enabled then
                        Controls.Add_Button (Map, Item_ID (Base, I),
                          Input_Rect (RC, R), Rect'(0, 0, C.width, C.height));
                     end if;
                  end if;
               end;
            end if;
            Rank := Rank + 1;
         end if;
      end loop;
      Stroke_Rect (PC, Popup, Colors.shadow, Colors.shadow);
   end Draw;
end CuBit.UI.Menus;
