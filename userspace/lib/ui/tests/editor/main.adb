with Ada.Text_IO;
with CuBit.UI.Controls;
with CuBit.UI.State;
with CuBit.UI.Trees;
with CuBit.UI.Editor;
with CuBit.UI.Editor.Cursors;
with CuBit.UI.Editor.Documents;
with CuBit.UI.Editor.Viewports;
with CuBit.UI.Editor.Search;
with CuBit.UI.Editor.Transactions;
with CuBit.UI.Editor.Buffers;

procedure Main is
   use CuBit.UI.Editor;
   use type CuBit.UI.Editor.Cursors.Toggle_Result;
   use type CuBit.UI.Editor.Cursors.Add_Result;
   use type CuBit.UI.Editor.Documents.Edit_Result;
   use type CuBit.UI.Editor.Buffers.Append_Result;
   use type CuBit.UI.Editor.Search.Search_Status;
   use type CuBit.UI.Scrollbar_Part;
   use type CuBit.UI.Rect;
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
   Plan : CuBit.UI.Editor.Transactions.Edit_Plan;
   Buffer : CuBit.UI.Editor.Buffers.Candidate_Buffer (8);
   Buffer_Result : CuBit.UI.Editor.Buffers.Append_Result;
   Search : CuBit.UI.Editor.Search.Search_Result;
   Widget_State : CuBit.UI.State.UI_State;
   Widget_Result : CuBit.UI.Widget_Result;
   Widget_Bounds : constant CuBit.UI.Rect :=
     (x => 10, y => 10, w => 20, h => 20);
   Scrollbar_State : CuBit.UI.State.UI_State;
   Scrollbar_Value : Natural := 0;
   Scrollbar_Bounds : constant CuBit.UI.Rect :=
     (x => 40, y => 10, w => 16, h => 180);
   Scrollbar_Layout : CuBit.UI.Vertical_Scrollbar_Layout;
   Horizontal_State : CuBit.UI.State.UI_State;
   Horizontal_Value : Natural := 1;
   Horizontal_Bounds : constant CuBit.UI.Rect :=
     (x => 10, y => 210, w => 220, h => 16);
   Horizontal_Layout : CuBit.UI.Horizontal_Scrollbar_Layout;
   Slider_State : CuBit.UI.State.UI_State;
   Slider_Value : Natural := 50;
   Slider_Bounds : constant CuBit.UI.Rect :=
     (x => 10, y => 300, w => 200, h => 20);
   Slider_Layout : CuBit.UI.Horizontal_Slider_Layout;
   Control_Map : CuBit.UI.Controls.Control_Map;
   Tree_State : CuBit.UI.State.UI_State;
   Tree_Controls : CuBit.UI.Controls.Control_Map;
   Tree_Selected : Natural := 1;
   Tree_Result : CuBit.UI.Widget_Result;
   Null_Canvas : CuBit.UI.Canvas;
   Tree_Damage : constant CuBit.UI.Rect :=
     (x => 0, y => 0, w => 240, h => 80);
   Tree_Row_One : constant CuBit.UI.Rect :=
     (x => 0, y => 0, w => 240, h => 20);
   Tree_Row_Two : constant CuBit.UI.Rect :=
     (x => 0, y => 20, w => 240, h => 20);
   Identity_State : CuBit.UI.State.UI_State;
   Decoy_Result : CuBit.UI.Widget_Result;
   Target_Result : CuBit.UI.Widget_Result;
   Text_State : CuBit.UI.State.UI_State;
   Short_Field : constant CuBit.UI.Rect :=
     (x => 10, y => 240, w => 120, h => 24);
   Long_Field : constant CuBit.UI.Rect :=
     (x => 10, y => 270, w => 220, h => 24);
   Retained_Changed : Boolean;
   Retained_Handled : Boolean;
   Retained_Available : Boolean;
   Retained_Value : Natural;
   Drag_Bounds : constant CuBit.UI.Rect :=
     (x => 117, y => 10, w => 7, h => 24);
   Button_Bounds : constant CuBit.UI.Rect :=
     (x => 250, y => 10, w => 80, h => 24);
begin
   --  Held motion only invalidates controls that continuously manipulate a
   --  value.  Ordinary rows and buttons must not repaint their containing
   --  view for every input packet.
   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add
     (Control_Map, 1, Widget_Bounds, Widget_Bounds);
   CuBit.UI.Controls.Add
     (Control_Map, 2, Scrollbar_Bounds, Scrollbar_Bounds,
      continuousAction => True);
   pragma Assert (not CuBit.UI.Controls.Has_Continuous_Action (Control_Map, 1));
   pragma Assert (CuBit.UI.Controls.Has_Continuous_Action (Control_Map, 2));

   --  Retained controls interpret input before paint and preserve capture,
   --  grab geometry, and pending values across declarative frame rebuilds.
   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Vertical_Scrollbar
     (Control_Map, 2, Scrollbar_Bounds, Scrollbar_Bounds,
      0, 0, 24, 17);
   Scrollbar_Layout := CuBit.UI.Layout_Vertical_Scrollbar
     (Scrollbar_Bounds, 0, 24, 0, 17);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 2, CuBit.UI.Controls.Pointer_Press,
      Scrollbar_Layout.incrementButton.x + 2,
      Scrollbar_Layout.incrementButton.y + 2,
      Retained_Changed, Retained_Handled);
   pragma Assert
     (Retained_Handled and then Retained_Changed and then
      CuBit.UI.Controls.Is_Active (Control_Map, 2));
   CuBit.UI.Controls.Take_Value
     (Control_Map, 2, Retained_Value, Retained_Available);
   pragma Assert (Retained_Available and then Retained_Value = 1);

   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Vertical_Scrollbar
     (Control_Map, 2, Scrollbar_Bounds, Scrollbar_Bounds,
      Retained_Value, 0, 24, 17);
   pragma Assert
     (CuBit.UI.Controls.Is_Active (Control_Map, 2) and then
      CuBit.UI.Controls.Active_Scrollbar_Part (Control_Map, 2) =
        CuBit.UI.Scrollbar_Increment);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 2, CuBit.UI.Controls.Pointer_Release,
      Scrollbar_Layout.incrementButton.x + 2,
      Scrollbar_Layout.incrementButton.y + 2,
      Retained_Changed, Retained_Handled);
   pragma Assert
     (Retained_Handled and then not Retained_Changed and then
      not CuBit.UI.Controls.Is_Active (Control_Map, 2));

   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Vertical_Scrollbar
     (Control_Map, 2, Scrollbar_Bounds, Scrollbar_Bounds,
      8, 0, 24, 17);
   Scrollbar_Layout := CuBit.UI.Layout_Vertical_Scrollbar
     (Scrollbar_Bounds, 0, 24, 8, 17);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 2, CuBit.UI.Controls.Pointer_Press,
      Scrollbar_Layout.incrementButton.x + 2,
      Scrollbar_Layout.incrementButton.y + 2,
      Retained_Changed, Retained_Handled);
   pragma Assert
     (Retained_Handled and then not Retained_Changed and then
      not CuBit.UI.Controls.Is_Active (Control_Map, 2) and then
      CuBit.UI.Controls.Active_Scrollbar_Part (Control_Map, 2) =
        CuBit.UI.Scrollbar_None);

   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Vertical_Scrollbar
     (Control_Map, 2, Scrollbar_Bounds, Scrollbar_Bounds,
      0, 0, 24, 17);
   Scrollbar_Layout := CuBit.UI.Layout_Vertical_Scrollbar
     (Scrollbar_Bounds, 0, 24, 0, 17);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 2, CuBit.UI.Controls.Pointer_Press,
      Scrollbar_Layout.thumb.x + 1,
      Scrollbar_Layout.thumb.y + Scrollbar_Layout.thumb.h / 2,
      Retained_Changed, Retained_Handled);
   pragma Assert (Retained_Handled and then not Retained_Changed);
   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Vertical_Scrollbar
     (Control_Map, 2, Scrollbar_Bounds, Scrollbar_Bounds,
      0, 0, 24, 17);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 2, CuBit.UI.Controls.Pointer_Move,
      Scrollbar_Layout.thumb.x + 1,
      Scrollbar_Layout.track.y + Scrollbar_Layout.track.h - 1,
      Retained_Changed, Retained_Handled);
   CuBit.UI.Controls.Take_Value
     (Control_Map, 2, Retained_Value, Retained_Available);
   pragma Assert
     (Retained_Handled and then Retained_Changed and then
      Retained_Available and then Retained_Value = 8);

   --  Retained dividers keep the exact point grabbed within a wider hit area.
   --  Moving twenty pixels therefore changes the model by twenty pixels
   --  without snapping the visible divider under the pointer.
   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Horizontal_Drag
     (Control_Map, 3, Drag_Bounds, Tree_Damage,
      20, 10, 100, 100);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 3, CuBit.UI.Controls.Pointer_Press,
      118, 16, Retained_Changed, Retained_Handled);
   pragma Assert (Retained_Handled and then not Retained_Changed);
   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Horizontal_Drag
     (Control_Map, 3, Drag_Bounds, Tree_Damage,
      20, 10, 100, 100);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 3, CuBit.UI.Controls.Pointer_Move,
      138, 16, Retained_Changed, Retained_Handled);
   CuBit.UI.Controls.Take_Value
     (Control_Map, 3, Retained_Value, Retained_Available);
   pragma Assert
     (Retained_Handled and then Retained_Changed and then
      Retained_Available and then Retained_Value = 40);

   --  Button activation belongs to one press/release transaction and is
   --  consumed by event handling, never inferred later while painting.
   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Button
     (Control_Map, 4, Button_Bounds, Button_Bounds);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 4, CuBit.UI.Controls.Pointer_Press,
      260, 16, Retained_Changed, Retained_Handled);
   pragma Assert
     (Retained_Handled and then not Retained_Changed and then
      CuBit.UI.Controls.Is_Active (Control_Map, 4));
   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add_Button
     (Control_Map, 4, Button_Bounds, Button_Bounds);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 4, CuBit.UI.Controls.Pointer_Release,
      260, 16, Retained_Changed, Retained_Handled);
   pragma Assert
     (Retained_Handled and then Retained_Changed and then
      not CuBit.UI.Controls.Is_Active (Control_Map, 4));
   pragma Assert (CuBit.UI.Controls.Take_Activated (Control_Map, 4));
   pragma Assert (not CuBit.UI.Controls.Take_Activated (Control_Map, 4));

   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 4, CuBit.UI.Controls.Pointer_Press,
      260, 16, Retained_Changed, Retained_Handled);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 4, CuBit.UI.Controls.Pointer_Release,
      400, 16, Retained_Changed, Retained_Handled);
   pragma Assert
     (Retained_Handled and then not Retained_Changed and then
      not CuBit.UI.Controls.Take_Activated (Control_Map, 4));

   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 4, CuBit.UI.Controls.Pointer_Press,
      260, 16, Retained_Changed, Retained_Handled);
   CuBit.UI.Controls.Dispatch_Pointer
     (Control_Map, 4, CuBit.UI.Controls.Pointer_Cancel,
      260, 16, Retained_Changed, Retained_Handled);
   pragma Assert
     (Retained_Handled and then not Retained_Changed and then
      not CuBit.UI.Controls.Is_Active (Control_Map, 4) and then
      not CuBit.UI.Controls.Take_Activated (Control_Map, 4));

   --  Capture belongs to the control under the physical press. Merely moving
   --  into a control while another region owns a held button must not begin a
   --  drag transaction.
   CuBit.UI.State.Set_Pointer
     (Widget_State, 2, 2, True, pressed => True);
   CuBit.UI.State.Begin_Frame (Widget_State);
   Widget_Result := CuBit.UI.State.Button (Widget_State, Widget_Bounds);
   pragma Assert (not Widget_Result.activated);
   CuBit.UI.State.Finish_Frame (Widget_State);
   pragma Assert (not Widget_Result.active);
   CuBit.UI.State.Set_Pointer (Widget_State, 15, 15, True);
   CuBit.UI.State.Begin_Frame (Widget_State);
   Widget_Result := CuBit.UI.State.Button (Widget_State, Widget_Bounds);
   pragma Assert
     (not Widget_Result.active and then
      not CuBit.UI.State.Is_Last_Widget_Captured (Widget_State));
   CuBit.UI.State.Finish_Frame (Widget_State);
   CuBit.UI.State.Set_Pointer
     (Widget_State, 15, 15, False, released => True);
   CuBit.UI.State.Begin_Frame (Widget_State);
   Widget_Result := CuBit.UI.State.Button (Widget_State, Widget_Bounds);
   pragma Assert (not Widget_Result.activated);
   CuBit.UI.State.Finish_Frame (Widget_State);

   CuBit.UI.State.Set_Pointer
     (Widget_State, 15, 15, True, pressed => True);
   CuBit.UI.State.Begin_Frame (Widget_State);
   Widget_Result := CuBit.UI.State.Button (Widget_State, Widget_Bounds);
   pragma Assert
     (Widget_Result.active and then
      CuBit.UI.State.Is_Last_Widget_Captured (Widget_State));
   CuBit.UI.State.Finish_Frame (Widget_State);
   CuBit.UI.State.Set_Pointer (Widget_State, 40, 15, True);
   CuBit.UI.State.Begin_Frame (Widget_State);
   Widget_Result := CuBit.UI.State.Button (Widget_State, Widget_Bounds);
   pragma Assert
     (not Widget_Result.active and then
      CuBit.UI.State.Is_Last_Widget_Captured (Widget_State));
   CuBit.UI.State.Finish_Frame (Widget_State);

   --  Explicit IDs make capture independent of render order.  Inserting a
   --  sibling while the pointer is held must not transfer the transaction.
   CuBit.UI.State.Set_Pointer
     (Identity_State, 15, 15, True, pressed => True);
   CuBit.UI.State.Begin_Frame (Identity_State);
   Target_Result := CuBit.UI.State.Button
     (Identity_State, Widget_Bounds, CuBit.UI.State.Widget_ID'(20));
   pragma Assert (Target_Result.active);
   CuBit.UI.State.Finish_Frame (Identity_State);
   CuBit.UI.State.Set_Pointer (Identity_State, 15, 15, True);
   CuBit.UI.State.Begin_Frame (Identity_State);
   Decoy_Result := CuBit.UI.State.Button
     (Identity_State, Widget_Bounds, CuBit.UI.State.Widget_ID'(10));
   Target_Result := CuBit.UI.State.Button
     (Identity_State, Widget_Bounds, CuBit.UI.State.Widget_ID'(20));
   pragma Assert (not Decoy_Result.active and then Target_Result.active);
   CuBit.UI.State.Finish_Frame (Identity_State);
   CuBit.UI.State.Set_Pointer
     (Identity_State, 15, 15, False, released => True);
   CuBit.UI.State.Begin_Frame (Identity_State);
   Decoy_Result := CuBit.UI.State.Button
     (Identity_State, Widget_Bounds, CuBit.UI.State.Widget_ID'(10));
   Target_Result := CuBit.UI.State.Button
     (Identity_State, Widget_Bounds, CuBit.UI.State.Widget_ID'(20));
   pragma Assert
     (not Decoy_Result.activated and then Target_Result.activated);
   CuBit.UI.State.Finish_Frame (Identity_State);

   --  Only the focused text field owns the shared caret and selection state.
   --  Rendering a shorter sibling first must not clamp that caret.
   CuBit.UI.State.Set_Pointer
     (Text_State, Long_Field.x + 10, Long_Field.y + 4,
      True, pressed => True);
   CuBit.UI.State.Begin_Frame (Text_State);
   Decoy_Result := CuBit.UI.State.Text_Field
     (Text_State, Short_Field, "x", CuBit.UI.State.Widget_ID'(30));
   pragma Assert (not Decoy_Result.active);
   Target_Result := CuBit.UI.State.Text_Field
     (Text_State, Long_Field, "0123456789",
      CuBit.UI.State.Widget_ID'(31));
   pragma Assert (Target_Result.active);
   Text_State.textCursor := 8;
   Text_State.textSelectionStart := 8;
   Text_State.textSelectionEnd := 8;
   Text_State.textSelectionAnchor := 8;
   CuBit.UI.State.Finish_Frame (Text_State);
   CuBit.UI.State.Set_Pointer
     (Text_State, Long_Field.x + 10, Long_Field.y + 4, False,
      released => True);
   CuBit.UI.State.Begin_Frame (Text_State);
   Decoy_Result := CuBit.UI.State.Text_Field
     (Text_State, Short_Field, "x", CuBit.UI.State.Widget_ID'(30));
   pragma Assert (Text_State.textCursor = 8);
   Target_Result := CuBit.UI.State.Text_Field
     (Text_State, Long_Field, "0123456789",
      CuBit.UI.State.Widget_ID'(31));
   pragma Assert (Text_State.textCursor = 8);
   CuBit.UI.State.Finish_Frame (Text_State);

   --  Interaction and drawing share one scrollbar layout: arrow clicks move
   --  one row, the thumb retains its grab offset, and wheel steps clamp.
   Scrollbar_Layout := CuBit.UI.Layout_Vertical_Scrollbar
     (Scrollbar_Bounds, 0, 24, Scrollbar_Value, 17);
   CuBit.UI.State.Set_Pointer
     (Scrollbar_State,
      Scrollbar_Layout.incrementButton.x + 2,
      Scrollbar_Layout.incrementButton.y + 2,
      True, pressed => True);
   CuBit.UI.State.Begin_Frame (Scrollbar_State);
   Widget_Result := CuBit.UI.State.Vertical_Scrollbar
     (Scrollbar_State, Scrollbar_Bounds, Scrollbar_Value, 0, 24, 17);
   pragma Assert
     (Widget_Result.active and then Scrollbar_Value = 1 and then
      CuBit.UI.State.Active_Scrollbar_Part (Scrollbar_State) =
        CuBit.UI.Scrollbar_Increment);
   CuBit.UI.State.Finish_Frame (Scrollbar_State);
   CuBit.UI.State.Set_Pointer
     (Scrollbar_State,
      Scrollbar_Layout.incrementButton.x + 2,
      Scrollbar_Layout.incrementButton.y + 2,
      False, released => True);
   CuBit.UI.State.Begin_Frame (Scrollbar_State);
   Widget_Result := CuBit.UI.State.Vertical_Scrollbar
     (Scrollbar_State, Scrollbar_Bounds, Scrollbar_Value, 0, 24, 17);
   pragma Assert (not Widget_Result.active);
   CuBit.UI.State.Finish_Frame (Scrollbar_State);

   Scrollbar_Value := 0;
   Scrollbar_Layout := CuBit.UI.Layout_Vertical_Scrollbar
     (Scrollbar_Bounds, 0, 24, Scrollbar_Value, 17);
   CuBit.UI.State.Set_Pointer
     (Scrollbar_State,
      Scrollbar_Layout.thumb.x + 1,
      Scrollbar_Layout.thumb.y + Scrollbar_Layout.thumb.h / 2,
      True, pressed => True);
   CuBit.UI.State.Begin_Frame (Scrollbar_State);
   Widget_Result := CuBit.UI.State.Vertical_Scrollbar
     (Scrollbar_State, Scrollbar_Bounds, Scrollbar_Value, 0, 24, 17);
   pragma Assert
     (Widget_Result.active and then Scrollbar_Value = 0 and then
      CuBit.UI.State.Active_Scrollbar_Part (Scrollbar_State) =
        CuBit.UI.Scrollbar_Thumb);
   CuBit.UI.State.Finish_Frame (Scrollbar_State);
   CuBit.UI.State.Set_Pointer
     (Scrollbar_State,
      Scrollbar_Layout.thumb.x + 1,
      Scrollbar_Layout.track.y + Scrollbar_Layout.track.h - 1,
      True);
   CuBit.UI.State.Begin_Frame (Scrollbar_State);
   Widget_Result := CuBit.UI.State.Vertical_Scrollbar
     (Scrollbar_State, Scrollbar_Bounds, Scrollbar_Value, 0, 24, 17);
   pragma Assert (Widget_Result.active and then Scrollbar_Value = 8);
   CuBit.UI.State.Finish_Frame (Scrollbar_State);

   CuBit.UI.Apply_Wheel_Scroll (Scrollbar_Value, 0, 8, 1);
   pragma Assert (Scrollbar_Value = 5);
   CuBit.UI.Apply_Wheel_Scroll (Scrollbar_Value, 0, 8, -1);
   pragma Assert (Scrollbar_Value = 8);

   Horizontal_Layout := CuBit.UI.Layout_Horizontal_Scrollbar
     (Horizontal_Bounds, 1, 40, Horizontal_Value, 12);
   CuBit.UI.State.Set_Pointer
     (Horizontal_State,
      Horizontal_Layout.incrementButton.x + 2,
      Horizontal_Layout.incrementButton.y + 2,
      True, pressed => True);
   CuBit.UI.State.Begin_Frame (Horizontal_State);
   Widget_Result := CuBit.UI.State.Horizontal_Scrollbar
     (Horizontal_State, Horizontal_Bounds, Horizontal_Value, 1, 40, 12);
   pragma Assert
     (Widget_Result.active and then Horizontal_Value = 2 and then
      CuBit.UI.State.Active_Scrollbar_Part (Horizontal_State) =
        CuBit.UI.Scrollbar_Increment);
   CuBit.UI.State.Finish_Frame (Horizontal_State);

   --  Slider rendering and dragging use identical thumb geometry and retain
   --  the press offset rather than jumping under the pointer.
   Slider_Layout := CuBit.UI.Layout_Horizontal_Slider
     (Slider_Bounds, 0, 100, Slider_Value);
   CuBit.UI.State.Set_Pointer
     (Slider_State, Slider_Layout.thumb.x + 2, Slider_Layout.thumb.y + 2,
      True, pressed => True);
   CuBit.UI.State.Begin_Frame (Slider_State);
   Widget_Result := CuBit.UI.State.Horizontal_Slider
     (Slider_State, Slider_Bounds, Slider_Value, 0, 100);
   pragma Assert (Widget_Result.active and then Slider_Value = 50);
   CuBit.UI.State.Finish_Frame (Slider_State);
   CuBit.UI.State.Set_Pointer
     (Slider_State, Slider_Layout.maximumThumbX + 2,
      Slider_Layout.thumb.y + 2, True);
   CuBit.UI.State.Begin_Frame (Slider_State);
   Widget_Result := CuBit.UI.State.Horizontal_Slider
     (Slider_State, Slider_Bounds, Slider_Value, 0, 100);
   pragma Assert (Widget_Result.active and then Slider_Value = 100);
   CuBit.UI.State.Finish_Frame (Slider_State);

   --  Registration and state evaluation use the same clipped bounds.
   CuBit.UI.Controls.Clear (Control_Map);
   CuBit.UI.Controls.Add
     (Control_Map, 1, Widget_Bounds,
      (x => 20, y => 10, w => 20, h => 20));
   pragma Assert
     (CuBit.UI.Controls.Is_Valid (Control_Map) and then
      CuBit.UI.Controls.Bounds (Control_Map, 1) =
        (x => 20, y => 10, w => 10, h => 20) and then
      CuBit.UI.Controls.Hit (Control_Map, 15, 15) =
        CuBit.UI.Controls.NO_CONTROL and then
      CuBit.UI.Controls.Hit (Control_Map, 25, 15) = 1);

   --  A duplicate live control ID invalidates the bounded map, and invalid
   --  maps cannot return a hit or damage owner.
   CuBit.UI.Controls.Add
     (Control_Map, 1, Slider_Bounds, Slider_Bounds);
   pragma Assert
     (not CuBit.UI.Controls.Is_Valid (Control_Map) and then
      CuBit.UI.Controls.Hit (Control_Map, 15, 15) =
        CuBit.UI.Controls.NO_CONTROL);

   --  Group selection changes request a stable follow-up render. Without it,
   --  a previously selected row drawn before the clicked row remains painted.
   CuBit.UI.State.Set_Pointer
     (Tree_State, 10, 25, True, pressed => True);
   CuBit.UI.State.Begin_Frame (Tree_State);
   CuBit.UI.Controls.Clear (Tree_Controls);
   CuBit.UI.Trees.Tree_Item
     (Null_Canvas, Tree_State, Tree_Controls, 40, Tree_Row_One,
      Tree_Damage, CuBit.UI.CuBit_Alloy, "one", 1, Tree_Selected,
      result => Tree_Result);
   pragma Assert (not Tree_Result.active);
   CuBit.UI.Trees.Tree_Item
     (Null_Canvas, Tree_State, Tree_Controls, 41, Tree_Row_Two,
      Tree_Damage, CuBit.UI.CuBit_Alloy, "two", 2, Tree_Selected,
      result => Tree_Result);
   pragma Assert (Tree_Result.active and then Tree_Selected = 1);
   CuBit.UI.State.Finish_Frame (Tree_State);
   CuBit.UI.State.Set_Pointer
     (Tree_State, 10, 25, False, released => True);
   CuBit.UI.State.Begin_Frame (Tree_State);
   CuBit.UI.Controls.Clear (Tree_Controls);
   CuBit.UI.Trees.Tree_Item
     (Null_Canvas, Tree_State, Tree_Controls, 40, Tree_Row_One,
      Tree_Damage, CuBit.UI.CuBit_Alloy, "one", 1, Tree_Selected,
      result => Tree_Result);
   CuBit.UI.Trees.Tree_Item
     (Null_Canvas, Tree_State, Tree_Controls, 41, Tree_Row_Two,
      Tree_Damage, CuBit.UI.CuBit_Alloy, "two", 2, Tree_Selected,
      result => Tree_Result);
   pragma Assert
     (Tree_Result.activated and then Tree_Selected = 2 and then
      CuBit.UI.State.Followup_Render_Requested (Tree_State));
   CuBit.UI.State.Finish_Frame (Tree_State);
   CuBit.UI.State.Begin_Frame (Tree_State);
   pragma Assert
     (not CuBit.UI.State.Followup_Render_Requested (Tree_State));
   CuBit.UI.State.Finish_Frame (Tree_State);

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

   Initialize (State, "(answer (+ 20 22))", Accepted);
   Place_Cursor (State, 2);
   Move (State, Move_Word_Right, Extend_Selection => True);
   pragma Assert
     (Selection_First (State) = 2 and then Selection_Last (State) = 9);
   Move (State, Move_Word_Right, Extend_Selection => True);
   pragma Assert (Selection_Last (State) = 10);

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

   declare
      Added : CuBit.UI.Editor.Cursors.Add_Result;
   begin
      CuBit.UI.Editor.Cursors.Add_Selection
        (Cursors, Anchor => 7, Position => 11, Preferred_Column => 11,
         Result => Added);
      pragma Assert
        (Added = CuBit.UI.Editor.Cursors.Cursor_Added and then
         CuBit.UI.Editor.Cursors.Element (Cursors, 2).Anchor = 7 and then
         CuBit.UI.Editor.Cursors.Element (Cursors, 2).Position = 11);
      CuBit.UI.Editor.Cursors.Add_Selection
        (Cursors, Anchor => 7, Position => 11, Preferred_Column => 11,
         Result => Added);
      pragma Assert
        (Added = CuBit.UI.Editor.Cursors.Cursor_Already_Present and then
         CuBit.UI.Editor.Cursors.Length (Cursors) = 2);
   end;

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

   CuBit.UI.Editor.Cursors.Initialize (Cursors, 3);
   CuBit.UI.Editor.Cursors.Toggle_At (Cursors, 5, Toggle);
   CuBit.UI.Editor.Cursors.Set_Element
     (Cursors, 1, (Position => 4, Anchor => 2, Preferred_Column => 4));
   CuBit.UI.Editor.Cursors.Set_Element
     (Cursors, 2, (Position => 6, Anchor => 4, Preferred_Column => 6));
   CuBit.UI.Editor.Cursors.Coalesce (Cursors);
   pragma Assert
     (CuBit.UI.Editor.Cursors.Length (Cursors) = 1 and then
      CuBit.UI.Editor.Cursors.Element (Cursors, 1).Position = 6 and then
      CuBit.UI.Editor.Cursors.Element (Cursors, 1).Anchor = 2);

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
   CuBit.UI.Editor.Viewports.Set_Line_Capacity (View, 4, 8);
   pragma Assert
     (CuBit.UI.Editor.Viewports.Line_Capacity (View) = 4 and then
      CuBit.UI.Editor.Viewports.First_Line (View) = 5);
   CuBit.UI.Editor.Viewports.Scroll_Lines (View, -100, 8);
   pragma Assert (CuBit.UI.Editor.Viewports.First_Line (View) = 1);

   CuBit.UI.Editor.Viewports.Set_Column_Capacity
     (View, Visible_Columns => 8, Document_Columns => 24);
   CuBit.UI.Editor.Viewports.Ensure_Column_Visible (View, 12, 24);
   pragma Assert
     (CuBit.UI.Editor.Viewports.First_Column (View) = 5 and then
      CuBit.UI.Editor.Viewports.Column_Capacity (View) = 8);
   CuBit.UI.Editor.Viewports.Ensure_Column_Visible (View, 3, 24);
   pragma Assert (CuBit.UI.Editor.Viewports.First_Column (View) = 3);
   CuBit.UI.Editor.Viewports.Scroll_Columns (View, 100, 24);
   pragma Assert (CuBit.UI.Editor.Viewports.First_Column (View) = 17);
   CuBit.UI.Editor.Viewports.Set_Column_Capacity (View, 12, 24);
   pragma Assert (CuBit.UI.Editor.Viewports.First_Column (View) = 13);
   CuBit.UI.Editor.Viewports.Scroll_Columns (View, -100, 24);
   pragma Assert (CuBit.UI.Editor.Viewports.First_Column (View) = 1);

   CuBit.UI.Editor.Cursors.Initialize (Cursors, 2);
   CuBit.UI.Editor.Cursors.Toggle_At (Cursors, 4, Toggle);
   CuBit.UI.Editor.Cursors.Toggle_At (Cursors, 9, Toggle);
   CuBit.UI.Editor.Cursors.Set_Element
     (Cursors, 1, (Position => 2, Anchor => 5, Preferred_Column => 2));
   CuBit.UI.Editor.Cursors.Set_Element
     (Cursors, 2, (Position => 4, Anchor => 7, Preferred_Column => 4));
   CuBit.UI.Editor.Transactions.Build (Cursors, 10, Plan);
   pragma Assert
     (CuBit.UI.Editor.Transactions.Length (Plan) = 2 and then
      CuBit.UI.Editor.Transactions.Element (Plan, 1).First = 2 and then
      CuBit.UI.Editor.Transactions.Element (Plan, 1).Last = 7 and then
      CuBit.UI.Editor.Transactions.Element (Plan, 2).First = 9 and then
      CuBit.UI.Editor.Transactions.Removed_Characters (Plan) = 5 and then
      CuBit.UI.Editor.Transactions.Final_Length_Fits (Plan, 10, 2, 10) and then
      not CuBit.UI.Editor.Transactions.Final_Length_Fits
        (Plan, 10, 3, 10));

   CuBit.UI.Editor.Documents.Initialize (Doc, "abcdefghij", Edit);
   pragma Assert (Edit = CuBit.UI.Editor.Documents.Applied);
   CuBit.UI.Editor.Transactions.Replace_All (Doc, Cursors, "Z", Edit);
   pragma Assert
     (Edit = CuBit.UI.Editor.Documents.Applied and then
      CuBit.UI.Editor.Documents.Content (Doc) = "aZghZij" and then
      CuBit.UI.Editor.Cursors.Length (Cursors) = 2 and then
      CuBit.UI.Editor.Cursors.Element (Cursors, 1).Position = 3 and then
      CuBit.UI.Editor.Cursors.Element (Cursors, 2).Position = 6);

   CuBit.UI.Editor.Transactions.Replace_All
     (Doc, Cursors, String'(1 .. 64 => 'x'), Edit);
   pragma Assert
     (Edit = CuBit.UI.Editor.Documents.Capacity_Exceeded and then
      CuBit.UI.Editor.Documents.Content (Doc) = "aZghZij" and then
      CuBit.UI.Editor.Cursors.Length (Cursors) = 2 and then
      CuBit.UI.Editor.Cursors.Element (Cursors, 1).Position = 3 and then
      CuBit.UI.Editor.Cursors.Element (Cursors, 2).Position = 6);

   CuBit.UI.Editor.Buffers.Initialize (Buffer);
   CuBit.UI.Editor.Buffers.Append (Buffer, "abc", Buffer_Result);
   pragma Assert
     (Buffer_Result = CuBit.UI.Editor.Buffers.Appended and then
      CuBit.UI.Editor.Buffers.Content (Buffer) = "abc" and then
      CuBit.UI.Editor.Buffers.Remaining (Buffer) = 5);
   CuBit.UI.Editor.Buffers.Append (Buffer, "defghi", Buffer_Result);
   pragma Assert
     (Buffer_Result = CuBit.UI.Editor.Buffers.Capacity_Exceeded and then
      CuBit.UI.Editor.Buffers.Content (Buffer) = "abc");

   CuBit.UI.Editor.Search.Find_Next
     ("answer other answer", "answer", Start_At => 2, Wrap => False,
      Whole_Word => True, Case_Sensitive => True, Result => Search);
   pragma Assert
     (Search.Status = CuBit.UI.Editor.Search.Match_Found and then
      Search.First = 14 and then Search.Last = 20);
   CuBit.UI.Editor.Search.Find_Next
     ("answer other answer", "answer", Start_At => 20, Wrap => True,
      Whole_Word => True, Case_Sensitive => True, Result => Search);
   pragma Assert
     (Search.Status = CuBit.UI.Editor.Search.Match_Found and then
      Search.First = 1 and then Search.Last = 7);
   CuBit.UI.Editor.Search.Find_Next
     ("answering answer", "answer", Start_At => 1, Wrap => False,
      Whole_Word => True, Case_Sensitive => True, Result => Search);
   pragma Assert (Search.First = 11 and then Search.Last = 17);


   Ada.Text_IO.Put_Line ("PASS: bounded one-based editor commands");
end Main;
