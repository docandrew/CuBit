with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with CCL.Language;
with CCL.VM;
with CuBit.UI;
with CuBit.UI.Editor;
with CuBit.UI.Editor.Cursors;
with CuBit.UI.Editor.Documents;
with CuBit.UI.Editor.Transactions;
with CuBit.UI.Editor.Viewports;
with CuBit.UI.Widgets;

--  HOSTED/LINUX Workbench preview.  Rendering below uses the SHARED CuBit UI
--  canvas.  Native_Window is the only hosted presentation/input boundary.
procedure Main is
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned;
   use type System.Address;
   use type CCL.Language.Interpretation_Status;
   use type CCL.VM.Value_Kind;
   use type CuBit.UI.Editor.Documents.Edit_Result;
   use type CuBit.UI.Editor.Cursors.Toggle_Result;
   use type CuBit.UI.Scrollbar_Part;

   --  Compact native canvas: never downscale the toolkit's 11 px UI font.
   --  The hosted adapter scales this canvas upward when space permits.
   WIDTH  : constant Natural := 640;
   HEIGHT : constant Natural := 400;

   type Pixel_Buffer is array (Natural range 0 .. WIDTH * HEIGHT - 1)
     of aliased Unsigned_32;
   Pixels : aliased Pixel_Buffer := [others => 0];

   Canvas : constant CuBit.UI.Canvas :=
     (addr => Pixels'Address, width => WIDTH, height => HEIGHT,
      pitch => WIDTH * 4, clipEnabled => False, clip => (others => 0));
   Colors : constant CuBit.UI.Theme := CuBit.UI.CuBit_Classic;

   Input       : CuBit.UI.Editor.Edit_State;
   Result_Text : String (1 .. 96) := [others => ' '];
   Result_Last : Natural := 5;
   Input_Bounds : CuBit.UI.Rect := (others => 0);
   Source : CuBit.UI.Editor.Documents.Document (4_096);
   Source_Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
   Source_View : CuBit.UI.Editor.Viewports.Viewport;
   Source_Bounds : CuBit.UI.Rect := (others => 0);
   Source_Scrollbar : CuBit.UI.Rect := (others => 0);
   Source_Scrollbar_Pressed : CuBit.UI.Scrollbar_Part :=
     CuBit.UI.Scrollbar_None;
   type Focus_Target is (Repl_Field, Source_Editor);
   Focus : Focus_Target := Source_Editor;

   function Window_Open (Width, Height : Interfaces.C.int) return System.Address
   with Import, Convention => C, External_Name => "ccl_window_open";
   function Window_Poll
     (Handle : System.Address; Kind : access Interfaces.C.int;
      Code, Modifiers : access Interfaces.C.unsigned;
      X, Y : access Interfaces.C.int) return Interfaces.C.int
   with Import, Convention => C, External_Name => "ccl_window_poll";
   function Window_Present
     (Handle, Pixels : System.Address;
      Pitch : Interfaces.C.int) return Interfaces.C.int
   with Import, Convention => C, External_Name => "ccl_window_present";
   procedure Window_Wait
   with Import, Convention => C, External_Name => "ccl_window_wait";
   procedure Window_Close (Handle : System.Address)
   with Import, Convention => C, External_Name => "ccl_window_close";

   procedure Set_Result (Text : String) is
      Length : constant Natural := Natural'Min (Text'Length, Result_Text'Length);
   begin
      Result_Text := [others => ' '];
      if Length > 0 then
         Result_Text (1 .. Length) := Text (Text'First .. Text'First + Length - 1);
      end if;
      Result_Last := Length;
   end Set_Result;

   procedure Submit is
      Outcome : CCL.Language.Interpretation_Result;
   begin
      if CuBit.UI.Editor.Length (Input) = 0 then return; end if;
      CCL.Language.Interpret (CuBit.UI.Editor.Content (Input), 1_024, Outcome);
      if Outcome.Status /= CCL.Language.Succeeded then
         Set_Result ("error: " & CCL.Language.Diagnostic_Code'Image (Outcome.Diagnostic));
      elsif not Outcome.Has_Value then
         Set_Result ("ok");
      elsif Outcome.Result_Value.Kind = CCL.VM.Integer_Value then
         Set_Result (Integer_64'Image (Outcome.Result_Value.Integer));
      else
         Set_Result ((if Outcome.Result_Value.Boolean then "true" else "false"));
      end if;
   end Submit;

   function Source_Cursor return CuBit.UI.Editor.Cursors.Cursor_State is
     (CuBit.UI.Editor.Cursors.Element
        (Source_Cursors,
         CuBit.UI.Editor.Cursors.Primary_Index (Source_Cursors)));

   procedure Store_Source_Cursor
     (Value : CuBit.UI.Editor.Cursors.Cursor_State)
   is
   begin
      CuBit.UI.Editor.Cursors.Set_Element
        (Source_Cursors,
         CuBit.UI.Editor.Cursors.Primary_Index (Source_Cursors), Value);
   end Store_Source_Cursor;

   procedure Collapse_Source_Cursors is
      Position : constant CuBit.UI.Editor.Documents.Document_Position :=
        Source_Cursor.Position;
   begin
      CuBit.UI.Editor.Cursors.Initialize (Source_Cursors, Position);
   end Collapse_Source_Cursors;

   procedure Reveal_Source_Cursor is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Position : CuBit.UI.Editor.Documents.Document_Position := 1;
      Line, Column : Positive;
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         Position := CuBit.UI.Editor.Documents.Document_Position'Max
           (Position, State.Position);
      end loop;
      CuBit.UI.Editor.Documents.Position_To_Line_Column
        (Source, Position, Line, Column);
      CuBit.UI.Editor.Viewports.Ensure_Visible
        (Source_View, Line, CuBit.UI.Editor.Documents.Line_Count (Source));
   end Reveal_Source_Cursor;

   procedure Place_Source_Cursor
     (Position : CuBit.UI.Editor.Documents.Document_Position;
      Extend_Selection : Boolean; Preserve_Column : Boolean := False)
   is
      State : CuBit.UI.Editor.Cursors.Cursor_State := Source_Cursor;
      Line, Column : Positive;
   begin
      State.Position := Position;
      if not Extend_Selection then State.Anchor := Position; end if;
      if not Preserve_Column then
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, Position, Line, Column);
         State.Preferred_Column := Column;
      end if;
      Store_Source_Cursor (State);
      Reveal_Source_Cursor;
   end Place_Source_Cursor;

   procedure Insert_Source (Text : String; Changed : out Boolean) is
      Result : CuBit.UI.Editor.Documents.Edit_Result;
   begin
      CuBit.UI.Editor.Transactions.Replace_All
        (Source, Source_Cursors, Text, Result);
      Changed := Result = CuBit.UI.Editor.Documents.Applied and then
        Text'Length > 0;
      if Changed then
         Reveal_Source_Cursor;
      end if;
   end Insert_Source;

   procedure Backspace_Source (Changed : out Boolean) is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Result : CuBit.UI.Editor.Documents.Edit_Result;
      Has_Deletion : Boolean := False;
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         if State.Position = State.Anchor and then State.Position > 1 then
            State.Anchor := State.Position - 1;
            CuBit.UI.Editor.Cursors.Set_Element
              (Source_Cursors, Index, State);
         end if;
         if State.Position /= State.Anchor then
            Has_Deletion := True;
         end if;
      end loop;
      CuBit.UI.Editor.Transactions.Replace_All
        (Source, Source_Cursors, "", Result);
      Changed := Has_Deletion and then
        Result = CuBit.UI.Editor.Documents.Applied;
      if Changed then
         Reveal_Source_Cursor;
      end if;
   end Backspace_Source;

   procedure Delete_Source_Forward (Changed : out Boolean) is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Result : CuBit.UI.Editor.Documents.Edit_Result;
      Has_Deletion : Boolean := False;
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         if State.Position = State.Anchor and then
           State.Position <= CuBit.UI.Editor.Documents.Length (Source)
         then
            State.Anchor := State.Position + 1;
            CuBit.UI.Editor.Cursors.Set_Element
              (Source_Cursors, Index, State);
         end if;
         if State.Position /= State.Anchor then
            Has_Deletion := True;
         end if;
      end loop;
      CuBit.UI.Editor.Transactions.Replace_All
        (Source, Source_Cursors, "", Result);
      Changed := Has_Deletion and then
        Result = CuBit.UI.Editor.Documents.Applied;
      if Changed then
         Reveal_Source_Cursor;
      end if;
   end Delete_Source_Forward;

   procedure Move_Source_Horizontal
     (Right, By_Word, Extend_Selection : Boolean)
   is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Text : constant String := CuBit.UI.Editor.Documents.Content (Source);
      Line, Column : Positive;

      function Is_Word_Character (Value : Character) return Boolean is
        ((Value >= 'a' and then Value <= 'z') or else
         (Value >= 'A' and then Value <= 'Z') or else
         (Value >= '0' and then Value <= '9') or else Value = '_');
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         Position := State.Position;
         if not Extend_Selection and then State.Position /= State.Anchor then
            Position :=
              (if Right then
                 CuBit.UI.Editor.Documents.Document_Position'Max
                   (State.Position, State.Anchor)
               else
                 CuBit.UI.Editor.Documents.Document_Position'Min
                   (State.Position, State.Anchor));
         elsif By_Word and then Right then
            while Position <= Text'Length and then
              Is_Word_Character (Text (Position))
            loop
               Position := Position + 1;
            end loop;
            while Position <= Text'Length and then
              not Is_Word_Character (Text (Position))
            loop
               Position := Position + 1;
            end loop;
         elsif By_Word then
            while Position > 1 and then
              not Is_Word_Character (Text (Position - 1))
            loop
               Position := Position - 1;
            end loop;
            while Position > 1 and then
              Is_Word_Character (Text (Position - 1))
            loop
               Position := Position - 1;
            end loop;
         elsif Right and then Position <= Text'Length then
            Position := Position + 1;
         elsif not Right and then Position > 1 then
            Position := Position - 1;
         end if;
         State.Position := Position;
         if not Extend_Selection then State.Anchor := Position; end if;
         CuBit.UI.Editor.Documents.Position_To_Line_Column
           (Source, Position, Line, Column);
         State.Preferred_Column := Column;
         CuBit.UI.Editor.Cursors.Set_Element (Source_Cursors, Index, State);
      end loop;
      CuBit.UI.Editor.Cursors.Coalesce (Source_Cursors);
      Reveal_Source_Cursor;
   end Move_Source_Horizontal;

   procedure Move_Source_Vertical
     (Direction : CuBit.UI.Editor.Documents.Vertical_Direction;
      Extend_Selection : Boolean)
   is
      State : CuBit.UI.Editor.Cursors.Cursor_State;
      Position : CuBit.UI.Editor.Documents.Document_Position;
   begin
      for Index in 1 .. CuBit.UI.Editor.Cursors.Length (Source_Cursors) loop
         State := CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         CuBit.UI.Editor.Documents.Move_Vertically
           (Source, State.Position, State.Preferred_Column,
            Direction, Position);
         State.Position := Position;
         if not Extend_Selection then State.Anchor := Position; end if;
         CuBit.UI.Editor.Cursors.Set_Element (Source_Cursors, Index, State);
      end loop;
      CuBit.UI.Editor.Cursors.Coalesce (Source_Cursors);
      Reveal_Source_Cursor;
   end Move_Source_Vertical;

   procedure Move_Source_Line_End
     (To_End, Extend_Selection : Boolean)
   is
      State : constant CuBit.UI.Editor.Cursors.Cursor_State := Source_Cursor;
      Line, Column : Positive;
      Position : CuBit.UI.Editor.Documents.Document_Position;
   begin
      CuBit.UI.Editor.Documents.Position_To_Line_Column
        (Source, State.Position, Line, Column);
      Position := CuBit.UI.Editor.Documents.Line_Column_To_Position
        (Source, Line,
         (if To_End then
            CuBit.UI.Editor.Documents.Line_Length (Source, Line) + 1
          else 1));
      Place_Source_Cursor (Position, Extend_Selection);
   end Move_Source_Line_End;

   procedure Select_All_Source is
      State : CuBit.UI.Editor.Cursors.Cursor_State := Source_Cursor;
   begin
      State.Anchor := 1;
      State.Position := CuBit.UI.Editor.Documents.Length (Source) + 1;
      Store_Source_Cursor (State);
      Reveal_Source_Cursor;
   end Select_All_Source;

   function Source_Position_At (Pixel_X, Pixel_Y : Natural)
     return CuBit.UI.Editor.Documents.Document_Position
   is
      Line_Height : constant Natural := CuBit.UI.UI_Text_Height + 2;
      Relative_Line : constant Natural :=
        (if Pixel_Y <= Source_Bounds.y + 5 then 0
         else (Pixel_Y - Source_Bounds.y - 5) / Line_Height);
      Line : Positive := CuBit.UI.Editor.Viewports.First_Line (Source_View);
      Column : Positive := 1;
      Offset : constant Natural :=
        (if Pixel_X <= Source_Bounds.x + 6 then 0
         else Pixel_X - Source_Bounds.x - 6);
      Draw_X : Natural := 0;
      Width : Natural;
      Position : CuBit.UI.Editor.Documents.Document_Position;
      Text : constant String := CuBit.UI.Editor.Documents.Content (Source);
   begin
      Line := Positive'Min
        (Line + Relative_Line, CuBit.UI.Editor.Documents.Line_Count (Source));
      for Candidate in 1 .. CuBit.UI.Editor.Documents.Line_Length (Source, Line)
      loop
         Position := CuBit.UI.Editor.Documents.Line_Column_To_Position
           (Source, Line, Candidate);
         Width := CuBit.UI.UI_Text_Width (Text (Position .. Position));
         exit when Offset < Draw_X + (Width + 1) / 2;
         Draw_X := Draw_X + Width;
         Column := Candidate + 1;
      end loop;
      return CuBit.UI.Editor.Documents.Line_Column_To_Position
        (Source, Line, Column);
   end Source_Position_At;

   procedure Source_Scrollbar_Metrics
     (Track, Thumb : out CuBit.UI.Rect; Maximum_First : out Positive)
   is
      Lines : constant Positive :=
        CuBit.UI.Editor.Documents.Line_Count (Source);
      Page : constant Positive :=
        CuBit.UI.Editor.Viewports.Line_Capacity (Source_View);
      Extent : constant Natural := Natural'Min
        (Source_Scrollbar.w, Source_Scrollbar.h / 2);
      Track_Frame : constant CuBit.UI.Rect :=
        (x => Source_Scrollbar.x, y => Source_Scrollbar.y + Extent,
         w => Source_Scrollbar.w,
         h => (if Source_Scrollbar.h > Extent * 2 then
                  Source_Scrollbar.h - Extent * 2 else 0));
      Total : constant Natural := Lines;
      Shown : constant Natural := Natural'Min (Page, Total);
      Thumb_Height : Natural;
      Travel : Natural;
      Position : Natural;
   begin
      Track :=
        (x => Track_Frame.x + 2, y => Track_Frame.y + 2,
         w => (if Track_Frame.w > 4 then Track_Frame.w - 4 else 0),
         h => (if Track_Frame.h > 4 then Track_Frame.h - 4 else 0));
      Maximum_First := (if Shown >= Total then 1 else Lines - Shown + 1);
      Thumb_Height := Natural'Min
        (Track.h, Natural'Max (12, Track.h * Shown / Total));
      Travel := Track.h - Thumb_Height;
      Position := CuBit.UI.Editor.Viewports.First_Line (Source_View) - 1;
      Thumb :=
        (x => Track.x,
         y => Track.y +
           (if Maximum_First = 1 then 0
            else Position * Travel / (Maximum_First - 1)),
         w => Track.w, h => Thumb_Height);
   end Source_Scrollbar_Metrics;

   function Position_At (Pixel_X : Natural)
     return CuBit.UI.Editor.Text_Position
   is
      Text : constant String := CuBit.UI.Editor.Content (Input);
      Text_X : constant Natural := Input_Bounds.x + 8;
      Offset : Natural;
      Draw_X : Natural := 0;
      Width : Natural;
   begin
      if Pixel_X <= Text_X then return 1; end if;
      Offset := Pixel_X - Text_X;
      for Index in Text'Range loop
         Width := CuBit.UI.UI_Text_Width (Text (Index .. Index));
         if Offset < Draw_X + (Width + 1) / 2 then
            return CuBit.UI.Editor.Text_Position (Index);
         end if;
         Draw_X := Draw_X + Width;
      end loop;
      return CuBit.UI.Editor.Length (Input) + 1;
   end Position_At;

   procedure Draw_Title_Controls is
      type Icon_Rows is array (Natural range 0 .. 8) of String (1 .. 9);
      --  Compact mask from CuBit's attributed Bluecurve window-icon atlas.
      Close_Icon : constant Icon_Rows :=
        [".........",
         "..#...#..",
         ".###.###.",
         "..#####..",
         "...###...",
         "..#####..",
         ".###.###.",
         "..#...#..",
         "........."];
      Minimize : constant CuBit.UI.Rect :=
        (x => WIDTH - 60, y => 3, w => 18, h => 17);
      Maximize : constant CuBit.UI.Rect :=
        (x => WIDTH - 40, y => 3, w => 18, h => 17);
      Close : constant CuBit.UI.Rect :=
        (x => WIDTH - 20, y => 3, w => 18, h => 17);
   begin
      CuBit.UI.Draw_Button
        (Canvas, Minimize, Colors, CuBit.UI.Button_Normal, "");
      CuBit.UI.Fill_Rect
        (Canvas, (x => Minimize.x + 5, y => Minimize.y + 11,
                  w => 7, h => 2), Colors.text);
      CuBit.UI.Draw_Button
        (Canvas, Maximize, Colors, CuBit.UI.Button_Normal, "");
      CuBit.UI.Stroke_Rect
        (Canvas, (x => Maximize.x + 4, y => Maximize.y + 4,
                  w => 10, h => 9), Colors.text, Colors.text);
      CuBit.UI.Fill_Rect
        (Canvas, (x => Maximize.x + 5, y => Maximize.y + 5,
                  w => 8, h => 1), Colors.text);
      CuBit.UI.Draw_Button
        (Canvas, Close, Colors, CuBit.UI.Button_Normal, "");
      for Icon_Y in Close_Icon'Range loop
         for Icon_X in Close_Icon (Icon_Y)'Range loop
            if Close_Icon (Icon_Y) (Icon_X) = '#' then
               CuBit.UI.Set_Pixel
                 (Canvas, Close.x + 3 + Icon_X, Close.y + 4 + Icon_Y,
                  Colors.text);
            end if;
         end loop;
      end loop;
   end Draw_Title_Controls;

   procedure Render is
      Content : CuBit.UI.Rect;
      Repl_Content : CuBit.UI.Rect;
      Editor_Content : CuBit.UI.Rect;
      Cursor_State : CuBit.UI.Editor.Cursors.Cursor_State;
      Cursor_Visuals : CuBit.UI.Text_Cursor_States
        (1 .. CuBit.UI.Editor.Cursors.MAX_CURSORS) :=
          [others => (cursor => 1, selectionStart => 1, selectionEnd => 1)];
      Cursor_Count : constant Positive :=
        CuBit.UI.Editor.Cursors.Length (Source_Cursors);
      Prompt_W : constant Natural := CuBit.UI.UI_Text_Width ("ccl>");
   begin
      CuBit.UI.Fill_Rect
        (Canvas, (x => 0, y => 0, w => WIDTH, h => HEIGHT), Colors.desktop);

      CuBit.UI.Fill_Rect
        (Canvas, (x => 0, y => 0, w => WIDTH, h => 22), Colors.accent);
      CuBit.UI.Draw_UI_Text
        (Canvas, 6, 4, "CuBit CCL Workbench",
         Colors.selectionText, Colors.accent);
      Draw_Title_Controls;

      CuBit.UI.Draw_Menu_Bar
        (Canvas, (x => 0, y => 22, w => WIDTH, h => 22), Colors);
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 3, y => 22, w => 34, h => 21), Colors,
         False, False, "File");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 38, y => 22, w => 34, h => 21), Colors,
         False, False, "Edit");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 73, y => 22, w => 38, h => 21), Colors,
         False, False, "View");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 112, y => 22, w => 34, h => 21), Colors,
         False, False, "Run");
      CuBit.UI.Draw_Menu_Title
        (Canvas, (x => 147, y => 22, w => 38, h => 21), Colors,
         False, False, "Help");

      CuBit.UI.Widgets.Group_Box
        (Canvas, (x => 8, y => 50, w => 220, h => 312), Colors,
         "CCL REPL", Repl_Content, 8);
      CuBit.UI.Draw_UI_Text
        (Canvas, Repl_Content.x, Repl_Content.y,
         "ccl>", Colors.text, Colors.face);
      Input_Bounds :=
        (x => Repl_Content.x + Prompt_W + 8, y => Repl_Content.y - 5,
         w => Repl_Content.w - Prompt_W - 8, h => 25);
      CuBit.UI.Draw_Text_Edit_Field
        (CuBit.UI.With_Clip (Canvas, Input_Bounds), Input_Bounds,
         Colors, CuBit.UI.Editor.Content (Input),
         CuBit.UI.Editor.Cursor (Input) - 1,
         CuBit.UI.Editor.Selection_First (Input) - 1,
         CuBit.UI.Editor.Selection_Last (Input) - 1,
         focused => Focus = Repl_Field, hot => False);
      CuBit.UI.Draw_UI_Text
        (Canvas, Repl_Content.x, Repl_Content.y + 32,
         Result_Text (1 .. Result_Last), Colors.muted, Colors.face);
      CuBit.UI.Widgets.Group_Box
        (Canvas,
         (x => Repl_Content.x, y => Repl_Content.y + 84,
          w => Repl_Content.w, h => 132),
         Colors, "Session authority", Content, 8);
      CuBit.UI.Widgets.Key_Value
        (Canvas, (x => Content.x, y => Content.y, w => Content.w, h => 24),
         Colors, "Network", "observe only");
      CuBit.UI.Widgets.Key_Value
        (Canvas, (x => Content.x, y => Content.y + 30,
                  w => Content.w, h => 24),
         Colors, "UI", "surface 7");
      CuBit.UI.Widgets.Key_Value
        (Canvas, (x => Content.x, y => Content.y + 60,
                  w => Content.w, h => 24),
         Colors, "Control", "not granted", True);

      CuBit.UI.Widgets.Group_Box
        (Canvas, (x => 236, y => 50, w => 396, h => 312), Colors,
         "CCL source - shared multiline editor", Editor_Content, 8);
      CuBit.UI.Draw_UI_Text
        (Canvas, Editor_Content.x, Editor_Content.y,
         "Bounded document; wheel and Page Up/Down scroll",
         Colors.muted, Colors.face);
      Source_Bounds :=
        (x => Editor_Content.x, y => Editor_Content.y + 24,
         w => Editor_Content.w - 18, h => Editor_Content.h - 48);
      declare
         Line_Height : constant Positive := CuBit.UI.UI_Text_Height + 2;
         Usable_Height : constant Natural :=
           (if Source_Bounds.h > 6 then Source_Bounds.h - 6 else 1);
         Visible_Lines : constant Positive :=
           Positive'Max (1, Usable_Height / Line_Height);
      begin
         CuBit.UI.Editor.Viewports.Set_Line_Capacity
           (Source_View, Visible_Lines,
            CuBit.UI.Editor.Documents.Line_Count (Source));
      end;
      Source_Scrollbar :=
        (x => Source_Bounds.x + Source_Bounds.w + 2,
         y => Source_Bounds.y, w => 16, h => Source_Bounds.h);
      for Index in 1 .. Cursor_Count loop
         Cursor_State :=
           CuBit.UI.Editor.Cursors.Element (Source_Cursors, Index);
         Cursor_Visuals (Index) :=
           (cursor => Cursor_State.Position,
            selectionStart => Positive'Min
              (Cursor_State.Position, Cursor_State.Anchor),
            selectionEnd => Positive'Max
              (Cursor_State.Position, Cursor_State.Anchor));
      end loop;
      CuBit.UI.Draw_Multiline_Text_Edit_Multiple
        (CuBit.UI.With_Clip (Canvas, Source_Bounds), Source_Bounds, Colors,
         CuBit.UI.Editor.Documents.Content (Source),
         CuBit.UI.Editor.Viewports.First_Line (Source_View),
         CuBit.UI.Editor.Viewports.Line_Capacity (Source_View),
         Cursor_Visuals (1 .. Cursor_Count),
         focused => Focus = Source_Editor, hot => False);
      CuBit.UI.Draw_Vertical_Scrollbar
        (Canvas, Source_Scrollbar, Colors, 1,
         CuBit.UI.Editor.Documents.Line_Count (Source),
         CuBit.UI.Editor.Viewports.First_Line (Source_View),
         hot => False,
         active => Source_Scrollbar_Pressed /= CuBit.UI.Scrollbar_None,
         pageSize => CuBit.UI.Editor.Viewports.Line_Capacity (Source_View),
         pressedPart => Source_Scrollbar_Pressed);
      CuBit.UI.Draw_UI_Text
        (Canvas, Editor_Content.x,
         Editor_Content.y + Editor_Content.h - 18,
         "SHARED core - Linux presentation adapter",
         Colors.muted, Colors.face);

      CuBit.UI.Draw_Status_Bar
        (Canvas, (x => 0, y => HEIGHT - 26, w => WIDTH, h => 26), Colors,
         "Multiline editor uses the CuBit widget toolkit",
         "bounded document • proved viewport");
   end Render;

begin
   declare
      Accepted : Boolean;
      Source_Result : CuBit.UI.Editor.Documents.Edit_Result;
   begin
      CuBit.UI.Editor.Initialize (Input, "(+ 20 22)", Accepted);
      if not Accepted then raise Program_Error; end if;
      CuBit.UI.Editor.Documents.Initialize
        (Source,
         "; CCL Workbench scratch buffer" & ASCII.LF &
         "let samples = (18 25 21 34 30 42 48)" & ASCII.LF &
         "let peak = reduce max samples" & ASCII.LF &
         ASCII.LF &
         "; Authority remains explicit in values and launch arguments" &
         ASCII.LF &
         "observe network with authority network.observe" & ASCII.LF &
         "show dashboard peak" & ASCII.LF,
         Source_Result);
      if Source_Result /= CuBit.UI.Editor.Documents.Applied then
         raise Program_Error;
      end if;
      CuBit.UI.Editor.Cursors.Initialize (Source_Cursors, 1);
      CuBit.UI.Editor.Viewports.Initialize (Source_View, 15);
   end;
   Result_Text (1 .. Result_Last) := "ready";
   declare
      Handle : constant System.Address :=
        Window_Open (Interfaces.C.int (WIDTH), Interfaces.C.int (HEIGHT));
      Kind : aliased Interfaces.C.int := 0;
      Code : aliased Interfaces.C.unsigned := 0;
      Modifiers : aliased Interfaces.C.unsigned := 0;
      Mouse_X : aliased Interfaces.C.int := 0;
      Mouse_Y : aliased Interfaces.C.int := 0;
      Running : Boolean := Handle /= System.Null_Address;
      Dragging : Boolean := False;
      Dragging_Scrollbar : Boolean := False;
      Scrollbar_Grab_Offset : Natural := 0;
      Changed : Boolean;
      Extend : Boolean;
      By_Word : Boolean;
      Cursor_Toggle : CuBit.UI.Editor.Cursors.Toggle_Result;
   begin
      if not Running then raise Program_Error; end if;
      Render;
      while Running loop
         while Running and then
           Window_Poll
             (Handle, Kind'Access, Code'Access, Modifiers'Access,
              Mouse_X'Access, Mouse_Y'Access) /= 0
         loop
            case Kind is
               when 1 => Running := False;
               when 2 =>
                  if Code >= 32 and then Code <= 126 then
                     if Focus = Repl_Field then
                        CuBit.UI.Editor.Insert
                          (Input, String'(1 => Character'Val (Code)), Changed);
                     else
                        Insert_Source
                          (String'(1 => Character'Val (Code)), Changed);
                     end if;
                  end if;
               when 3 =>
                  if Focus = Repl_Field then
                     CuBit.UI.Editor.Backspace (Input, Changed);
                  else
                     Backspace_Source (Changed);
                  end if;
               when 4 =>
                  if Focus = Repl_Field then
                     Submit;
                  else
                     Insert_Source (String'(1 => ASCII.LF), Changed);
                  end if;
               when 5 | 6 =>
                  Extend := (Modifiers and 1) /= 0;
                  By_Word := (Modifiers and 2) /= 0;
                  if Focus = Repl_Field then
                     CuBit.UI.Editor.Move
                       (Input,
                        (if Kind = 5 then
                           (if By_Word then CuBit.UI.Editor.Move_Word_Left
                            else CuBit.UI.Editor.Move_Left)
                         else
                           (if By_Word then CuBit.UI.Editor.Move_Word_Right
                            else CuBit.UI.Editor.Move_Right)),
                        Extend);
                  else
                     Move_Source_Horizontal
                       (Right => Kind = 6, By_Word => By_Word,
                        Extend_Selection => Extend);
                  end if;
               when 7 =>
                  if Focus = Repl_Field then
                     CuBit.UI.Editor.Move
                       (Input, CuBit.UI.Editor.Move_Start,
                        (Modifiers and 1) /= 0);
                  else
                     Move_Source_Line_End
                       (To_End => False,
                        Extend_Selection => (Modifiers and 1) /= 0);
                  end if;
               when 8 =>
                  if Focus = Repl_Field then
                     CuBit.UI.Editor.Move
                       (Input, CuBit.UI.Editor.Move_End,
                        (Modifiers and 1) /= 0);
                  else
                     Move_Source_Line_End
                       (To_End => True,
                        Extend_Selection => (Modifiers and 1) /= 0);
                  end if;
               when 9 =>
                  if Focus = Repl_Field then
                     CuBit.UI.Editor.Delete_Forward (Input, Changed);
                  else
                     Delete_Source_Forward (Changed);
                  end if;
               when 10 =>
                  if Focus = Repl_Field then
                     CuBit.UI.Editor.Select_All (Input);
                  else
                     Select_All_Source;
                  end if;
               when 11 | 14 | 15 =>
                  Dragging_Scrollbar := False;
                  Source_Scrollbar_Pressed := CuBit.UI.Scrollbar_None;
                  if Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Input_Bounds)
                  then
                     Focus := Repl_Field;
                     if Kind = 15 then
                        CuBit.UI.Editor.Select_All (Input);
                     elsif Kind = 14 then
                        CuBit.UI.Editor.Select_Word_At
                          (Input, Position_At (Natural (Mouse_X)));
                     else
                        CuBit.UI.Editor.Place_Cursor
                          (Input, Position_At (Natural (Mouse_X)),
                           (Modifiers and 1) /= 0);
                     end if;
                     Dragging := True;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Source_Bounds)
                  then
                     Focus := Source_Editor;
                     if (Modifiers and 2) /= 0 then
                        CuBit.UI.Editor.Cursors.Toggle_At
                          (Source_Cursors,
                           Source_Position_At
                             (Natural (Mouse_X), Natural (Mouse_Y)),
                           Cursor_Toggle);
                        if Cursor_Toggle =
                          CuBit.UI.Editor.Cursors.Cursor_Limit_Reached
                        then
                           Set_Result ("cursor limit reached");
                        end if;
                        Dragging := False;
                        Reveal_Source_Cursor;
                     else
                        declare
                           Click_Position : constant
                             CuBit.UI.Editor.Documents.Document_Position :=
                               Source_Position_At
                                 (Natural (Mouse_X), Natural (Mouse_Y));
                        begin
                           CuBit.UI.Editor.Cursors.Initialize
                             (Source_Cursors, Click_Position);
                           Place_Source_Cursor
                             (Click_Position,
                              Extend_Selection => (Modifiers and 1) /= 0);
                        end;
                        Dragging := True;
                     end if;
                  elsif Mouse_X >= 0 and then Mouse_Y >= 0 and then
                    CuBit.UI.Point_In_Rect
                      (Natural (Mouse_X), Natural (Mouse_Y), Source_Scrollbar)
                  then
                     declare
                        Lines : constant Positive :=
                          CuBit.UI.Editor.Documents.Line_Count (Source);
                        Extent : constant Natural := Source_Scrollbar.w;
                        Track, Thumb : CuBit.UI.Rect;
                        Maximum_First : Positive;
                        Relative_Y : Natural;
                        Target : Positive;
                     begin
                        Source_Scrollbar_Metrics
                          (Track, Thumb, Maximum_First);
                        Dragging := False;
                        if Maximum_First > 1 and then
                          CuBit.UI.Point_In_Rect
                            (Natural (Mouse_X), Natural (Mouse_Y), Thumb)
                        then
                           Dragging_Scrollbar := True;
                           Source_Scrollbar_Pressed :=
                             CuBit.UI.Scrollbar_Thumb;
                           Scrollbar_Grab_Offset :=
                             Natural (Mouse_Y) - Thumb.y;
                        elsif Natural (Mouse_Y) <
                          Source_Scrollbar.y + Extent
                        then
                           Source_Scrollbar_Pressed :=
                             CuBit.UI.Scrollbar_Decrement;
                           CuBit.UI.Editor.Viewports.Scroll_Lines
                             (Source_View, -1, Lines);
                        elsif Natural (Mouse_Y) >=
                          Source_Scrollbar.y + Source_Scrollbar.h - Extent
                        then
                           Source_Scrollbar_Pressed :=
                             CuBit.UI.Scrollbar_Increment;
                           CuBit.UI.Editor.Viewports.Scroll_Lines
                             (Source_View, 1, Lines);
                        else
                           Source_Scrollbar_Pressed :=
                             CuBit.UI.Scrollbar_Track;
                           Relative_Y :=
                             (if Natural (Mouse_Y) <= Track.y then 0
                              else Natural'Min
                                (Natural (Mouse_Y) - Track.y, Track.h - 1));
                           Target := 1 + Relative_Y * (Maximum_First - 1) /
                             Natural'Max (1, Track.h - 1);
                           CuBit.UI.Editor.Viewports.Scroll_Lines
                             (Source_View,
                              Integer (Target) - Integer
                                (CuBit.UI.Editor.Viewports.First_Line
                                   (Source_View)),
                              Lines);
                        end if;
                     end;
                  end if;
               when 12 =>
                  if Dragging_Scrollbar and then Mouse_Y >= 0 then
                     declare
                        Track, Thumb : CuBit.UI.Rect;
                        Maximum_First : Positive;
                        Travel, Relative_Y : Natural;
                        Pointer_Y : constant Natural := Natural (Mouse_Y);
                        Target : Positive;
                     begin
                        Source_Scrollbar_Metrics
                          (Track, Thumb, Maximum_First);
                        Travel := Track.h - Thumb.h;
                        if Pointer_Y <= Track.y + Scrollbar_Grab_Offset then
                           Relative_Y := 0;
                        else
                           Relative_Y := Natural'Min
                             (Pointer_Y - Track.y - Scrollbar_Grab_Offset,
                              Travel);
                        end if;
                        Target := 1 + Relative_Y * (Maximum_First - 1) /
                          Natural'Max (1, Travel);
                        CuBit.UI.Editor.Viewports.Scroll_Lines
                          (Source_View,
                           Integer (Target) - Integer
                             (CuBit.UI.Editor.Viewports.First_Line
                                (Source_View)),
                           CuBit.UI.Editor.Documents.Line_Count (Source));
                     end;
                  elsif Dragging and then Mouse_X >= 0 then
                     if Focus = Repl_Field then
                        CuBit.UI.Editor.Place_Cursor
                          (Input, Position_At (Natural (Mouse_X)),
                           Extend_Selection => True);
                     elsif Mouse_Y >= 0 then
                        Place_Source_Cursor
                          (Source_Position_At
                             (Natural (Mouse_X), Natural (Mouse_Y)),
                           Extend_Selection => True);
                     end if;
                  end if;
               when 13 =>
                  Dragging := False;
                  Dragging_Scrollbar := False;
                  Source_Scrollbar_Pressed := CuBit.UI.Scrollbar_None;
               when 16 | 17 =>
                  if Focus = Source_Editor then
                     Move_Source_Vertical
                       ((if Kind = 16 then
                           CuBit.UI.Editor.Documents.Up
                         else CuBit.UI.Editor.Documents.Down),
                        Extend_Selection => (Modifiers and 1) /= 0);
                  end if;
               when 18 =>
                  CuBit.UI.Editor.Viewports.Scroll_Lines
                    (Source_View, -3,
                     CuBit.UI.Editor.Documents.Line_Count (Source));
               when 19 =>
                  CuBit.UI.Editor.Viewports.Scroll_Lines
                    (Source_View, 3,
                     CuBit.UI.Editor.Documents.Line_Count (Source));
               when 20 | 21 =>
                  CuBit.UI.Editor.Viewports.Scroll_Lines
                    (Source_View,
                     (if Kind = 20 then
                         -Integer
                           (CuBit.UI.Editor.Viewports.Line_Capacity
                              (Source_View))
                      else Integer
                        (CuBit.UI.Editor.Viewports.Line_Capacity
                           (Source_View))),
                     CuBit.UI.Editor.Documents.Line_Count (Source));
               when 22 =>
                  if Focus = Source_Editor then
                     Collapse_Source_Cursors;
                     Reveal_Source_Cursor;
                  end if;
               when others => null;
            end case;
         end loop;
         exit when not Running;
         Render;
         exit when Window_Present
           (Handle, Pixels'Address, Interfaces.C.int (WIDTH * 4)) /= 0;
         Window_Wait;
      end loop;
      Window_Close (Handle);
   end;
end Main;
