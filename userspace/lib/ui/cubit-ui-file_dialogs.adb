with CuBit.UI.Widgets;

package body CuBit.UI.File_Dialogs is
   use CuBit.File_Selection;
   Row_Height : constant := 22;
   type Geometry is record
      Frame, List, Rows, Scroll, Name, Confirm, Cancel : Rect;
      Capacity : Positive := 1;
   end record;

   function Layout (Width, Height : Natural) return Geometry is
      G : Geometry;
   begin
      if Width < 320 or else Height < 280 then return G; end if;
      G.Frame := (0, 0, Natural'Min (600, Width - 24), Natural'Min (364, Height - 24));
      G.Frame.x := (Width - G.Frame.w) / 2;
      G.Frame.y := (Height - G.Frame.h) / 2;
      G.List := (G.Frame.x + 12, G.Frame.y + 62, G.Frame.w - 24, G.Frame.h - 190);
      G.Rows := Layout_Table (G.List).Rows;
      G.Scroll := (G.Rows.x + G.Rows.w - 16, G.Rows.y, 16, G.Rows.h);
      G.Rows.w := G.Rows.w - 16;
      G.Capacity := Positive'Max (1, G.Rows.h / Row_Height);
      G.Name := (G.Frame.x + 80, G.Frame.y + G.Frame.h - 108, G.Frame.w - 92, 26);
      G.Confirm := (G.Frame.x + G.Frame.w - 204, G.Frame.y + G.Frame.h - 39, 92, 27);
      G.Cancel := (G.Frame.x + G.Frame.w - 104, G.Confirm.y, 92, 27);
      return G;
   end Layout;

   function Bounds (Width, Height : Natural) return Rect is (Layout (Width, Height).Frame);

   function Is_Open (State : Dialog_State) return Boolean is (State.Visible);
   function Mode (State : Dialog_State) return Dialog_Mode is (State.Operation);
   function Filename (State : Dialog_State) return String is (Editor.Content (State.Name));
   procedure Close (State : in out Dialog_State) is
   begin
      State.Visible := False;
      State.Press := No_Press;
   end Close;
   procedure Set_Error (State : in out Dialog_State; Message : String) is
   begin
      State.Error_Length := Natural'Min (Message'Length, State.Error_Text'Length);
      State.Error_Text (1 .. State.Error_Length) :=
        Message (Message'First .. Message'First + State.Error_Length - 1);
   end Set_Error;
   procedure Choose (State : in out Dialog_State; Index : File_Count) is
      Accepted : Boolean;
   begin
      if Index = 0 or else Index > State.Files.Count then return; end if;
      State.Selected := Index;
      Editor.Initialize (State.Name, Value (State.Files.Names (Index)), Accepted);
      State.Error_Length := 0;
   end Choose;
   procedure Show
     (State : out Dialog_State; Mode : Dialog_Mode;
      Files : CuBit.File_Selection.File_List; Location : String;
      Suggested_Name : String := "")
   is
      Accepted : Boolean;
   begin
      State := (others => <>);
      State.Visible := True;
      State.Operation := Mode;
      State.Files := Files;
      State.Location_Length := Natural'Min (Location'Length, State.Location_Text'Length);
      State.Location_Text (1 .. State.Location_Length) :=
        Location (Location'First .. Location'First + State.Location_Length - 1);
      Editor.Initialize (State.Name, Suggested_Name, Accepted);
      if Mode = Open_File and then Files.Count > 0 then Choose (State, 1); end if;
      if Mode = Save_New_File then
         State.Focus := Name_Field;
         Editor.Select_All (State.Name);
      end if;
   end Show;

   procedure Handle
     (State : in out Dialog_State; Event : Dialog_Event;
      Width, Height : Natural; Action : out Dialog_Action)
   is
      G : constant Geometry := Layout (Width, Height);
      Maximum : constant Natural := Natural'Max
        (1, State.Files.Count - Natural'Min (State.Files.Count, G.Capacity) + 1);
      Changed : Boolean;
      Scroll_Result : Widget_Result;
      Step : Natural := 1;
      Position : Editor.Text_Position := 1;

      procedure Request_Submit is
      begin
         if not Valid_Leaf (Filename (State)) then
            Set_Error (State, "Enter a filename (letters, digits, spaces, . _ -; no paths). ");
         else
            Action := Submit;
         end if;
      end Request_Submit;

      procedure Place_Name_Cursor (Extend : Boolean) is
         Text : constant String := Filename (State);
         X : Natural := G.Name.x + 8;
      begin
         for I in Text'Range loop
            exit when Event.X < X + UI_Text_Width (Text (I .. I)) / 2;
            X := X + UI_Text_Width (Text (I .. I));
            Position := Position + 1;
         end loop;
         Editor.Place_Cursor (State.Name, Position, Extend);
      end Place_Name_Cursor;
   begin
      Action := No_Action;
      if not State.Visible then return; end if;
      if Event.Kind = Escape then
         Close (State);
         Action := Cancelled;
         return;
      end if;
      if Is_Empty (G.Frame) then return; end if;
      State.First_Row := Natural'Min (State.First_Row, Maximum);
      case Event.Kind is
         when Tab =>
            if Event.Shift then
               State.Focus := (if State.Focus = Focus_Target'First then Focus_Target'Last
                 else Focus_Target'Pred (State.Focus));
            else
               State.Focus := (if State.Focus = Focus_Target'Last then Focus_Target'First
                 else Focus_Target'Succ (State.Focus));
            end if;
         when Enter =>
            if State.Focus = Cancel_Button then Close (State); Action := Cancelled;
            else Request_Submit; end if;
         when Text_Input | Backspace | Delete | Left | Right | Home | End_Key | Select_All =>
            if State.Focus = Name_Field then
               case Event.Kind is
                  when Text_Input =>
                     if Event.Character_Value in ' ' .. '~' and then
                       Editor.Length (State.Name) -
                         (Editor.Selection_Last (State.Name) - Editor.Selection_First (State.Name)) <
                           Maximum_Name_Length
                     then
                        Editor.Insert (State.Name, String'(1 => Event.Character_Value), Changed);
                     end if;
                  when Backspace => Editor.Backspace (State.Name, Changed);
                  when Delete => Editor.Delete_Forward (State.Name, Changed);
                  when Left => Editor.Move (State.Name,
                    (if Event.Control then Editor.Move_Word_Left else Editor.Move_Left), Event.Shift);
                  when Right => Editor.Move (State.Name,
                    (if Event.Control then Editor.Move_Word_Right else Editor.Move_Right), Event.Shift);
                  when Home => Editor.Move (State.Name, Editor.Move_Start, Event.Shift);
                  when End_Key => Editor.Move (State.Name, Editor.Move_End, Event.Shift);
                  when Select_All => Editor.Select_All (State.Name);
                  when others => null;
               end case;
            end if;
         when Up | Down | Page_Up | Page_Down =>
            State.Focus := File_List;
            if Event.Kind in Page_Up | Page_Down then Step := G.Capacity; end if;
            if State.Files.Count > 0 then
               if Event.Kind in Up | Page_Up then
                  Choose (State, Natural'Max (1, State.Selected - Natural'Min (State.Selected, Step)));
               else
                  Choose (State, Natural'Min (State.Files.Count, State.Selected + Step));
               end if;
               if State.Selected < State.First_Row then State.First_Row := State.Selected;
               elsif State.Selected >= State.First_Row + G.Capacity then
                  State.First_Row := State.Selected - G.Capacity + 1;
               end if;
            end if;
         when Wheel_Up | Wheel_Down =>
            Apply_Wheel_Scroll (State.First_Row, 1, Maximum,
              (if Event.Kind = Wheel_Up then 1 else -1));
         when Pointer_Down | Double_Click | Pointer_Drag | Pointer_Up =>
            CuBit.UI.State.Begin_Frame (State.Scroll_State);
            CuBit.UI.State.Set_Pointer
              (State.Scroll_State, Event.X, Event.Y, Event.Kind /= Pointer_Up,
               pressed => Event.Kind in Pointer_Down | Double_Click,
               released => Event.Kind = Pointer_Up);
            Scroll_Result := CuBit.UI.State.Vertical_Scrollbar
              (State.Scroll_State, G.Scroll, State.First_Row, 1, Maximum,
               pageSize => G.Capacity, widgetID => 1);
            if Scroll_Result.active then State.Focus := File_List; end if;
            CuBit.UI.State.Finish_Frame (State.Scroll_State);
            if Event.Kind in Pointer_Down | Double_Click then
               State.Press := No_Press;
               if Point_In_Rect (Event.X, Event.Y, G.Confirm) then
                  State.Press := Accept_Press; State.Focus := Accept_Button;
               elsif Point_In_Rect (Event.X, Event.Y, G.Cancel) then
                  State.Press := Cancel_Press; State.Focus := Cancel_Button;
               elsif Point_In_Rect (Event.X, Event.Y, G.Name) then
                  State.Focus := Name_Field; State.Press := Name_Press;
                  Place_Name_Cursor (Event.Shift);
                  if Event.Kind = Double_Click then Editor.Select_Word_At (State.Name, Position); end if;
               elsif Point_In_Rect (Event.X, Event.Y, G.Rows) then
                  State.Focus := File_List;
                  declare
                     Row : constant Natural := (Event.Y - G.Rows.y) / Row_Height;
                     Index : constant Natural := State.First_Row + Row;
                  begin
                     if Row < G.Capacity and then Index <= State.Files.Count then
                        Choose (State, Index);
                        if Event.Kind = Double_Click then Request_Submit; end if;
                     end if;
                  end;
               end if;
            elsif Event.Kind = Pointer_Drag and then State.Press = Name_Press then
               Place_Name_Cursor (True);
            elsif Event.Kind = Pointer_Up then
               if State.Press = Accept_Press and then Point_In_Rect (Event.X, Event.Y, G.Confirm) then
                  Request_Submit;
               elsif State.Press = Cancel_Press and then Point_In_Rect (Event.X, Event.Y, G.Cancel) then
                  Close (State); Action := Cancelled;
               end if;
               State.Press := No_Press;
            end if;
         when others => null;
      end case;
   end Handle;

   procedure Draw (C : Canvas; State : Dialog_State; Colors : Theme) is
      G : constant Geometry := Layout (C.width, C.height);
      Header : constant Rect := Layout_Table (G.List).Header;
      Columns : constant Table_Column_Layout :=
        (First_Width => (if G.Rows.w > 110 then G.Rows.w - 110 else 0),
         Second_Width => 110, Cell_Padding => 5);
      Maximum : constant Natural := Natural'Max
        (1, State.Files.Count - Natural'Min (State.Files.Count, G.Capacity) + 1);
      First : constant Natural := Natural'Min (State.First_Row, Maximum);
   begin
      if not State.Visible or else Is_Empty (G.Frame) then return; end if;
      Draw_Button (C, G.Frame, Colors, Button_Normal, "");
      Fill_Vertical_Gradient
        (C, (G.Frame.x + 2, G.Frame.y + 2, G.Frame.w - 4, 24),
         Colors.activeTitleTop, Colors.activeTitleBottom);
      Draw_UI_Text_Transparent
        (With_Clip (C, G.Frame), G.Frame.x + 10, G.Frame.y + 5,
         (if State.Operation = Open_File then "Open CCL source" else "Save CCL source as a new file"),
         Colors.selectionText);
      Widgets.Label (C, (G.Frame.x + 12, G.Frame.y + 34, G.Frame.w - 24, 20), Colors,
        State.Location_Text (1 .. State.Location_Length));
      Draw_Table_Viewport (C, G.List, Colors);
      Draw_Table_Header (C, Header, Colors, "Name", "Type", "", Columns);
      for Row in 0 .. G.Capacity - 1 loop
         exit when First + Row > State.Files.Count;
         Draw_Table_Row
           (With_Clip (C, G.Rows), (G.Rows.x, G.Rows.y + Row * Row_Height, G.Rows.w, Row_Height),
            Colors, State.Selected = First + Row, False,
            Value (State.Files.Names (First + Row)), "CCL source", "", Columns);
      end loop;
      if State.Files.Count = 0 then
         Draw_UI_Text_Transparent (With_Clip (C, G.Rows), G.Rows.x + 6, G.Rows.y + 4,
           "No saved CCL files in this workspace", Colors.muted);
      end if;
      if State.Focus = File_List then Stroke_Rect (C, G.List, Colors.accent, Colors.accent); end if;
      Draw_Vertical_Scrollbar
        (C, G.Scroll, Colors, 1, Maximum, First, False,
         CuBit.UI.State.Active_Scrollbar_Part (State.Scroll_State) /= Scrollbar_None,
         G.Capacity, CuBit.UI.State.Active_Scrollbar_Part (State.Scroll_State));
      Widgets.Label (C, (G.Frame.x + 12, G.Name.y, 65, G.Name.h), Colors, "Filename:");
      Draw_Text_Edit_Field
        (C, G.Name, Colors, Filename (State), Editor.Cursor (State.Name) - 1,
         Editor.Selection_First (State.Name) - 1, Editor.Selection_Last (State.Name) - 1,
         State.Focus = Name_Field, False);
      Widgets.Label (C, (G.Frame.x + 12, G.Name.y + 33, G.Frame.w - 24, 20), Colors,
        (if State.Error_Length > 0 then State.Error_Text (1 .. State.Error_Length)
         elsif State.Operation = Save_New_File then "New .ccl file only; existing files will not be overwritten."
         else "Authorized workspace only. Enter opens; Escape cancels."));
      Draw_Button (C, G.Confirm, Colors,
        (if State.Press = Accept_Press then Button_Pressed else Button_Normal),
        (if State.Operation = Open_File then "Open" else "Save new"));
      Draw_Button (C, G.Cancel, Colors,
        (if State.Press = Cancel_Press then Button_Pressed else Button_Normal), "Cancel");
      if State.Focus in Accept_Button | Cancel_Button then
         declare
            B : constant Rect := (if State.Focus = Accept_Button then G.Confirm else G.Cancel);
         begin
            Stroke_Rect (C, (B.x + 3, B.y + 3, B.w - 6, B.h - 6), Colors.accent, Colors.accent);
         end;
      end if;
   end Draw;
end CuBit.UI.File_Dialogs;
