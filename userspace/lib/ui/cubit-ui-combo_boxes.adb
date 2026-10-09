package body CuBit.UI.Combo_Boxes is
   use type Controls.Pointer_Action;
   function Choice_ID (Base : ID_Base; Index : Positive)
      return Controls.Control_ID is (Base + Index);
   function Is_Combo_Control (Base : ID_Base; Target : Controls.Control_ID)
      return Boolean is (Target >= Base and then Target <= Base + Max_Choices + 1);
   function Selection (State : Combo_State) return Natural is (State.Selected);
   function Is_Open (State : Combo_State) return Boolean is (State.Opened);
   function Value (S : Text) return String is (if S = null then "" else S.all);
   function Selectable (D : Model; I : Natural) return Boolean is
     (I in 1 .. D.Count and then D.Choices (I).Enabled);
   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
   function Next_Item (D : Model; From : Natural; Forward : Boolean;
                       Letter : Character := ' ') return Natural is
      I : Natural := (if From <= D.Count then From else 0);
   begin
      for Step in 1 .. D.Count loop
         I := (if Forward then (if I >= D.Count then 1 else I + 1)
               else (if I <= 1 then D.Count else I - 1));
         if Selectable (D, I) and then
           (Letter = ' ' or else
            (D.Choices (I).Caption /= null and then
             D.Choices (I).Caption.all'Length > 0 and then
             Lower (D.Choices (I).Caption (D.Choices (I).Caption.all'First)) = Lower (Letter)))
         then return I; end if;
      end loop;
      return 0;
   end Next_Item;
   procedure Dismiss (State : in out Combo_State) is
   begin State.Opened := False; State.Dragging := False; end Dismiss;
   procedure Set_Selection (State : in out Combo_State; Definition : Model;
                            Index : Natural) is
   begin
      State.Selected := (if Selectable (Definition, Index) then Index else 0);
      State.Highlighted := State.Selected; Dismiss (State);
   end Set_Selection;
   procedure Open_List (State : in out Combo_State; D : Model) is
   begin
      State.Highlighted := (if Selectable (D, State.Selected) then State.Selected
                            else Next_Item (D, 0, True));
      State.Opened := State.Highlighted /= 0;
   end Open_List;
   procedure Handle_Key
     (State : in out Combo_State; Definition : Model; Event : Key;
      Changed, Handled : out Boolean; Letter : Character := ' ';
      Enabled : Boolean := True) is
      Before : constant Natural := State.Selected;
      Candidate : Natural := (if State.Opened then State.Highlighted else State.Selected);
   begin
      Changed := False; Handled := False;
      if not Enabled or else Next_Item (Definition, 0, True) = 0 then
         Dismiss (State); return;
      end if;
      case Event is
         when Tab_Key => Dismiss (State); return;
         when Cancel => Handled := State.Opened; Dismiss (State); return;
         when Toggle =>
            if State.Opened then Dismiss (State); else Open_List (State, Definition); end if;
         when Commit =>
            if State.Opened then
               if Selectable (Definition, State.Highlighted) then
                  State.Selected := State.Highlighted;
               end if;
               Dismiss (State);
            else Open_List (State, Definition); end if;
         when Up | Down | Home | End_Key | Type_Character =>
            if Event = Type_Character and Letter = ' ' then return; end if;
            Candidate := Next_Item (Definition,
              (if Event in Home | End_Key then 0 else Candidate),
              Event not in Up | End_Key,
              (if Event = Type_Character then Letter else ' '));
            if Candidate = 0 then return; end if;
            if State.Opened then State.Highlighted := Candidate;
            else State.Selected := Candidate; end if;
      end case;
      Handled := True; Changed := State.Selected /= Before;
   end Handle_Key;
   procedure Handle_Wheel
     (State : in out Combo_State; Definition : Model; Wheel_Delta : Integer;
      Handled : out Boolean; Enabled : Boolean := True) is
      Changed : Boolean;
   begin
      Handled := Enabled and State.Opened and Wheel_Delta /= 0;
      if not Handled then return; end if;
      -- One bounded move per input event; do not iterate over an untrusted delta.
      Handle_Key (State, Definition, (if Wheel_Delta > 0 then Up else Down), Changed, Handled);
   end Handle_Wheel;
   procedure Handle_Pointer
     (State : in out Combo_State; Definition : Model;
      Map : in out Controls.Control_Map; Base : ID_Base;
      Target : Controls.Control_ID; Action : Controls.Pointer_Action;
      Changed, Handled : out Boolean; Enabled : Boolean := True) is
      Was_Open : constant Boolean := State.Opened;
      Was_Drag : constant Boolean := State.Dragging;
      Before : constant Natural := State.Selected;
      I : Natural;
      Ignored : Boolean;
   begin
      Changed := False; Handled := Was_Open;
      if Action = Controls.Pointer_Release then State.Dragging := False; end if;
      if not Enabled or else not Controls.Is_Valid (Map) or else
        Action = Controls.Pointer_Cancel
      then Dismiss (State); return; end if;
      if Action = Controls.Pointer_Move then State.Hot := Target = Base; end if;
      if Target = Base then
         Handled := True; Ignored := Controls.Take_Activated (Map, Target);
         if Action = Controls.Pointer_Press then
            if Was_Open then Dismiss (State);
            else Open_List (State, Definition); State.Dragging := State.Opened; end if;
         end if;
      elsif State.Opened and Target > Base and Target <= Base + Definition.Count then
         I := Target - Base;
         if Selectable (Definition, I) then
            if Action in Controls.Pointer_Move | Controls.Pointer_Press then
               State.Highlighted := I;
            elsif Action = Controls.Pointer_Release and then
              (Controls.Take_Activated (Map, Target) or Was_Drag)
            then State.Selected := I; Dismiss (State);
            end if;
         end if;
      elsif Was_Open and then
        (Action = Controls.Pointer_Press or else
         (Action = Controls.Pointer_Release and Was_Drag)) and then
        Target /= Base + Max_Choices + 1
      then Dismiss (State);
      end if;
      Changed := State.Selected /= Before;
   end Handle_Pointer;
   procedure Draw
     (C : Canvas; Map : in out Controls.Control_Map; State : Combo_State;
      Definition : Model; Base : ID_Base; Bounds : Rect; Colors : Theme;
      Focused : Boolean := False; Enabled : Boolean := True;
      Row_Height : Positive := 26; Visible_Rows : Positive := 8) is
      -- Damage clipping must not move the field or change popup placement.
      Layout_Canvas : constant Canvas := (C with delta clipEnabled => False);
      R : constant Rect := Clamp_Rect (Layout_Canvas, Bounds);
      PC : constant Canvas := With_Clip (C, R);
      --  Match a 16px scrollbar arrow and the tree frame's 4px content inset.
      Arrow_Inset : constant Natural := Natural'Min (4, R.w / 2);
      Arrow_W : constant Natural := Natural'Min
        (16, Natural'Min (R.w - 2 * Arrow_Inset, R.h - Natural'Min (4, R.h)));
      Caption_W : constant Natural := (if R.w > Arrow_W + 16 then R.w - Arrow_W - 16 else 0);
      Arrow : constant Rect :=
        (R.x + R.w - Arrow_Inset - Arrow_W, R.y + (R.h - Arrow_W) / 2,
         Arrow_W, Arrow_W);
      Usable : constant Boolean := Enabled and Next_Item (Definition, 0, True) /= 0;
      Ink : constant Color := (if Usable then Colors.text else Colors.muted);
      TY : constant Natural := R.y + (if R.h > UI_Text_Height then (R.h - UI_Text_Height) / 2 else 0)
        + (if R.h >= UI_Text_Height + 8 then 2 else 0);
      Below, Room, Rows, First, H : Natural;
      Popup, Row : Rect;
      Above : Boolean;
      RC : Canvas;
      BG, FG : Color;
   begin
      if Is_Empty (R) then return; end if;
      Draw_Text_Field (PC, R, Colors, "", False, Usable and (Focused or State.Hot));
      if Selectable (Definition, State.Selected) then
         Draw_UI_Text (With_Clip (PC, (R.x + Natural'Min (8, R.w), R.y + Natural'Min (2, R.h),
           Caption_W, R.h - Natural'Min (4, R.h))), R.x + 8, TY,
           Value (Definition.Choices (State.Selected).Caption), Ink, Colors.field);
      end if;
      Draw_Arrow_Button (PC, Arrow, Colors,
        (if not Usable then Button_Disabled elsif State.Opened then Button_Pressed
         elsif State.Hot then Button_Hot else Button_Normal), Arrow_Down);
      if not Usable then return; end if;
      Controls.Add_Button (Map, Base, Input_Rect (PC, R), (0, 0, C.width, C.height));
      if not State.Opened then return; end if;
      Below := C.height - (R.y + R.h);
      H := Natural'Min (Definition.Count, Natural'Min (Max_Choices, Visible_Rows));
      Above := Below / Row_Height < H and then R.y > Below;
      Room := (if Above then R.y else Below);
      Rows := Natural'Min (H, (if Room > 4 then (Room - 4) / Row_Height else 0));
      if Rows = 0 then return; end if;
      H := Rows * Row_Height + 4;
      First := (if State.Highlighted > Rows then State.Highlighted - Rows + 1 else 1);
      First := Natural'Min (First, Definition.Count - Rows + 1);
      Popup := (R.x, (if Above then R.y - H else R.y + R.h), R.w, H);
      RC := With_Clip (C, Popup);
      Draw_Table_Viewport (RC, Popup, Colors);
      Controls.Add_Button (Map, Base + Max_Choices + 1, Input_Rect (RC, Popup),
        (0, 0, C.width, C.height));
      for I in First .. First + Rows - 1 loop
         Row := (Popup.x + Natural'Min (2, Popup.w), Popup.y + 2 + (I - First) * Row_Height,
                 Popup.w - Natural'Min (4, Popup.w), Row_Height);
         BG := (if I = State.Highlighted then Colors.selection else Colors.field);
         FG := (if not Definition.Choices (I).Enabled then Colors.muted
                elsif I = State.Highlighted then Colors.selectionText else Colors.text);
         Fill_Rect (With_Clip (RC, Row), Row, BG);
         Draw_UI_Text (With_Clip (RC, (Row.x + Natural'Min (8, Row.w), Row.y + 2,
           Row.w - Natural'Min (16, Row.w), Row.h - Natural'Min (4, Row.h))),
           Row.x + 8, Row.y + (if Row.h > UI_Text_Height then (Row.h - UI_Text_Height) / 2 else 0),
           Value (Definition.Choices (I).Caption), FG, BG);
         if Definition.Choices (I).Enabled then
            Controls.Add_Button (Map, Choice_ID (Base, I), Input_Rect (RC, Row),
              (0, 0, C.width, C.height));
         end if;
      end loop;
   end Draw;
end CuBit.UI.Combo_Boxes;
