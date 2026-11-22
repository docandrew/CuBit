------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A read-only text field: selectable, scrollable, for logs and transcripts
--  (UI-033). The app owns the text; the view keeps where it is scrolled and
--  what is selected, and draws through Draw_Multiline_Text_Edit (the same
--  monospace grid the editors use), with a vertical scroll bar.
--
--  @description
--  Following the end: while the view is scrolled to the last line, text
--  added at the end scrolls into view (a log's tail); scrolling up stops
--  that until the view is back at the end. Keys: Up/Down a line, Page
--  Up/Down a page, Home/End the start and the end; with Shift they extend
--  the selection instead; Ctrl+A selects everything. The pointer selects
--  by dragging (Shift+press extends), the wheel scrolls (Wheel), and a
--  press in the scroll bar pages. Copying is the app's: Selection is the
--  selected text, for CuBit.UI.Clipboard after the user's own chord.
--  Positions are 1-based character positions in the text (length + 1:
--  the end); text that shrinks clamps them.
------------------------------------------------------------------------------
with Box_Distribution;
with CuBit.UI_Metrics;

package CuBit.UI.Text_Views is
   type View is record
      First_Line : Positive := 1;
      Cursor, Anchor : Positive := 1;
      Follow_End : Boolean := True;
      Selecting : Boolean := False;
   end record;

   --  The text grid (Draw_Multiline_Text_Edit's).
   function Line_Pitch return Positive is (Code_Text_Height + Multiline_Line_Gap);
   Scroll_Bar_Width : constant := CuBit.UI_Metrics.Glyph_Box;
   --  Lines a wheel notch moves.
   Wheel_Lines : constant := 3;

   --  Size hints for a view showing at least Columns characters and Lines
   --  lines, and preferring Preferred_Columns x Preferred_Lines; it
   --  expands both ways (CuBit.UI.Boxes.Add_Block).
   function Width_Hint (Columns, Preferred_Columns : Positive) return Box_Distribution.Size_Hint;
   function Height_Hint (Lines, Preferred_Lines : Positive) return Box_Distribution.Size_Hint;

   function Line_Count (Text : String) return Positive;
   --  Whole lines Box shows.
   function Visible_Lines (Box : Rect) return Positive;
   --  The text area (Box less the scroll bar) and the scroll bar.
   function Text_Box (Box : Rect) return Rect;
   function Bar_Box (Box : Rect) return Rect;

   --  Clamp to Text and, when following the end, scroll to it. Call after
   --  the text changes and before drawing.
   procedure Sync (V : in out View; Text : String; Box : Rect);
   --  Scroll By lines (negative: toward the start).
   procedure Scroll (V : in out View; Text : String; Box : Rect; By : Integer);
   procedure Wheel (V : in out View; Text : String; Box : Rect; Notches : Integer);
   procedure Key
     (V : in out View; Text : String; Box : Rect; Scan_Code : Natural; Shift, Ctrl : Boolean;
      Handled : out Boolean);
   --  Down: the button is held; Pressed / Released: it went down / up now.
   procedure Pointer
     (V : in out View; Text : String; Box : Rect; X, Y : Natural; Down, Pressed, Released, Shift : Boolean);

   --  The position under X, Y (the nearest character boundary of the line).
   function Position_At (V : View; Text : String; Box : Rect; X, Y : Natural) return Positive;
   function Selection_First (V : View) return Positive is (Positive'Min (V.Cursor, V.Anchor));
   function Selection_Last (V : View) return Positive is (Positive'Max (V.Cursor, V.Anchor));
   function Selection (V : View; Text : String) return String;

   procedure Draw (C : Canvas; Box : Rect; Colors : Theme; V : View; Text : String; Focused : Boolean);

   --  Repainting only what changed (UI-043). Old and Old_Text are what the
   --  last frame drew, with the same Box, Colors and focus.
   --
   --  Changed: the part of Box that drawing V over Text changes: the rows
   --  whose line or selection differ, and the scroll bar when it moved.
   --  Empty when nothing changes.
   function Changed (Box : Rect; Old : View; Old_Text : String; V : View; Text : String) return Rect;
   --  Lines > 0: V shows Old's rows moved up by Lines (row R shows what old
   --  row R + Lines showed, for every row both show), with no selection in
   --  either, so those rows can be copied instead of drawn.
   function Moved_Lines (Box : Rect; Old : View; Old_Text : String; V : View; Text : String) return Natural;
   --  As Draw (the same pixels), but when Moved_Lines allows it the rows
   --  that only moved are copied from Previous, the frame that drew Old
   --  over Old_Text (CuBit.UI.App.Previous_Canvas): only the rows that
   --  appear and the scroll bar are drawn. Without a usable Previous it
   --  draws everything in C's clip, as Draw.
   procedure Draw_Moved
     (C, Previous : Canvas; Box : Rect; Colors : Theme;
      Old : View; Old_Text : String; V : View; Text : String; Focused : Boolean);
   --  The rows Draw_Moved copied in its last call (0: it drew everything).
   function Last_Copied_Rows return Natural;
end CuBit.UI.Text_Views;
