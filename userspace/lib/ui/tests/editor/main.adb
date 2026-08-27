with Ada.Text_IO;
with CuBit.UI.Editor;
with CuBit.UI.Editor.Cursors;
with CuBit.UI.Editor.Documents;
with CuBit.UI.Editor.Viewports;

procedure Main is
   use CuBit.UI.Editor;
   use type CuBit.UI.Editor.Cursors.Toggle_Result;
   use type CuBit.UI.Editor.Documents.Edit_Result;
   State : Edit_State;
   Accepted : Boolean;
   Changed : Boolean;
   Cursors : CuBit.UI.Editor.Cursors.Cursor_Set;
   Toggle : CuBit.UI.Editor.Cursors.Toggle_Result;
   Doc : CuBit.UI.Editor.Documents.Document (64);
   Edit : CuBit.UI.Editor.Documents.Edit_Result;
   Line : Positive;
   Column : Positive;
   Position : CuBit.UI.Editor.Documents.Document_Position;
   Preferred : CuBit.UI.Editor.Documents.Display_Column;
   View : CuBit.UI.Editor.Viewports.Viewport;
begin
   Initialize (State, "alpha beta", Accepted);
   pragma Assert (Accepted and then Cursor (State) = 11);

   Move (State, Move_Word_Left);
   pragma Assert (Cursor (State) = 7);
   Move (State, Move_Word_Left, Extend_Selection => True);
   pragma Assert
     (Selection_First (State) = 1 and then Selection_Last (State) = 7);
   Insert (State, "X", Changed);
   pragma Assert (Changed and then Content (State) = "Xbeta");

   Move (State, Move_Right);
   Move (State, Move_Left, Extend_Selection => True);
   Insert (State, "Y", Changed);
   pragma Assert (Changed and then Content (State) = "XYeta");

   Move (State, Move_End);
   Backspace (State, Changed);
   pragma Assert (Changed and then Content (State) = "XYet");
   Move (State, Move_Start);
   Delete_Forward (State, Changed);
   pragma Assert (Changed and then Content (State) = "Yet");

   Select_All (State);
   Backspace (State, Changed);
   pragma Assert
     (Changed and then Length (State) = 0 and then Cursor (State) = 1);

   Initialize (State, "one two", Accepted);
   Place_Cursor (State, 2);
   pragma Assert
     (Cursor (State) = 2 and then Selection_First (State) = 2);
   Place_Cursor (State, 5, Extend_Selection => True);
   pragma Assert
     (Selection_First (State) = 2 and then Selection_Last (State) = 5);
   Select_Word_At (State, 6);
   pragma Assert
     (Selection_First (State) = 5 and then Selection_Last (State) = 8);

   CuBit.UI.Editor.Cursors.Initialize (Cursors, 2);
   CuBit.UI.Editor.Cursors.Toggle_At (Cursors, 5, Toggle);
   pragma Assert
     (Toggle = CuBit.UI.Editor.Cursors.Cursor_Added and then
      CuBit.UI.Editor.Cursors.Length (Cursors) = 2 and then
      CuBit.UI.Editor.Cursors.Primary_Index (Cursors) = 2);
   CuBit.UI.Editor.Cursors.Toggle_At (Cursors, 2, Toggle);
   pragma Assert
     (Toggle = CuBit.UI.Editor.Cursors.Cursor_Removed and then
      CuBit.UI.Editor.Cursors.Length (Cursors) = 1 and then
      CuBit.UI.Editor.Cursors.Element (Cursors, 1).Position = 5);

   CuBit.UI.Editor.Cursors.Initialize (Cursors, 1);
   for Position in 2 .. CuBit.UI.Editor.Cursors.MAX_CURSORS loop
      CuBit.UI.Editor.Cursors.Toggle_At
        (Cursors, Position, Toggle);
      pragma Assert (Toggle = CuBit.UI.Editor.Cursors.Cursor_Added);
   end loop;
   CuBit.UI.Editor.Cursors.Toggle_At (Cursors, 33, Toggle);
   pragma Assert
     (Toggle = CuBit.UI.Editor.Cursors.Cursor_Limit_Reached and then
      CuBit.UI.Editor.Cursors.Length (Cursors) =
        CuBit.UI.Editor.Cursors.MAX_CURSORS);

   CuBit.UI.Editor.Documents.Initialize
     (Doc, "one" & ASCII.LF & "two" & ASCII.LF, Edit);
   pragma Assert
     (Edit = CuBit.UI.Editor.Documents.Applied and then
      CuBit.UI.Editor.Documents.Line_Count (Doc) = 3 and then
      CuBit.UI.Editor.Documents.Line_Length (Doc, 1) = 3 and then
      CuBit.UI.Editor.Documents.Line_Length (Doc, 3) = 0);
   CuBit.UI.Editor.Documents.Position_To_Line_Column (Doc, 6, Line, Column);
   pragma Assert (Line = 2 and then Column = 2);
   pragma Assert
     (CuBit.UI.Editor.Documents.Line_Column_To_Position (Doc, 2, 2) = 6);
   CuBit.UI.Editor.Documents.Insert (Doc, 5, "X", Edit);
   pragma Assert
     (Edit = CuBit.UI.Editor.Documents.Applied and then
      CuBit.UI.Editor.Documents.Content (Doc) =
        "one" & ASCII.LF & "Xtwo" & ASCII.LF);
   CuBit.UI.Editor.Documents.Delete (Doc, 5, 1);
   pragma Assert
     (CuBit.UI.Editor.Documents.Content (Doc) =
        "one" & ASCII.LF & "two" & ASCII.LF);
   CuBit.UI.Editor.Documents.Insert
     (Doc, 1, String'(1 .. 64 => 'x'), Edit);
   pragma Assert
     (Edit = CuBit.UI.Editor.Documents.Capacity_Exceeded and then
      CuBit.UI.Editor.Documents.Content (Doc) =
        "one" & ASCII.LF & "two" & ASCII.LF);

   CuBit.UI.Editor.Documents.Initialize
     (Doc, "abcd" & ASCII.LF & "x" & ASCII.LF & "abcdef", Edit);
   Position := 5;
   Preferred := 5;
   CuBit.UI.Editor.Documents.Move_Vertically
     (Doc, Position, Preferred, CuBit.UI.Editor.Documents.Down, Position);
   CuBit.UI.Editor.Documents.Position_To_Line_Column
     (Doc, Position, Line, Column);
   pragma Assert (Line = 2 and then Column = 2 and then Preferred = 5);
   CuBit.UI.Editor.Documents.Move_Vertically
     (Doc, Position, Preferred, CuBit.UI.Editor.Documents.Down, Position);
   CuBit.UI.Editor.Documents.Position_To_Line_Column
     (Doc, Position, Line, Column);
   pragma Assert (Line = 3 and then Column = 5 and then Preferred = 5);

   CuBit.UI.Editor.Viewports.Initialize (View, Visible_Lines => 3);
   CuBit.UI.Editor.Viewports.Ensure_Visible (View, 5, 8);
   pragma Assert
     (CuBit.UI.Editor.Viewports.First_Line (View) = 3 and then
      CuBit.UI.Editor.Viewports.Last_Visible_Line (View, 8) = 5);
   CuBit.UI.Editor.Viewports.Ensure_Visible (View, 2, 8);
   pragma Assert (CuBit.UI.Editor.Viewports.First_Line (View) = 2);
   CuBit.UI.Editor.Viewports.Scroll_Lines (View, 100, 8);
   pragma Assert (CuBit.UI.Editor.Viewports.First_Line (View) = 6);
   CuBit.UI.Editor.Viewports.Scroll_Lines (View, -100, 8);
   pragma Assert (CuBit.UI.Editor.Viewports.First_Line (View) = 1);

   Ada.Text_IO.Put_Line ("PASS: bounded one-based editor commands");
end Main;
