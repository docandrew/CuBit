with Interfaces;
with CuBit.UI;
with Text_Limits; use Text_Limits;
with Text_Documents;
with Text_Syntax;

--  One editor (docs/text-editor.md, "Embedding"): a view of a document,
--  with a gutter, an overview ruler and an optional minimap, drawn with the
--  shared toolkit. Any number coexist; several may show one document (a
--  split), each with its own scroll position. Its host lays it out
--  (Set_Area), feeds it pointer input and runs commands on its document.
--
--  Drawing is damage-only and bounded: Prepare lays out just the visible
--  rows (each from its segment, never from the whole line or document),
--  describes each as cells and hashes them; only rows whose hash changed
--  since the last frame are repainted. A typed character repaints its row,
--  a cursor move two rows.
package Text_Editors is
   use Interfaces;

   subtype Color is CuBit.UI.Color;
   type Token_Colors is array (Text_Syntax.Token_Kind) of Color;
   type Palette is record
      Background, Text, Gutter_Background, Gutter_Text, Gutter_Current, Current_Line : Color := 0;
      Selection, Selection_Unfocused, Caret, Match, Ruler_Background, Ruler_View, Ruler_Caret : Color := 0;
      --  Other occurrences of the identifier under the cursor.
      Occurrence : Color := 0;
      Error, Warning, Information, Invalid : Color := 0;
      Tokens : Token_Colors := [others => 0];
   end record;
   --  The editor's colors from the toolkit's theme (light or dark scheme).
   function Palette_For (Colors : CuBit.UI.Theme; Dark : Boolean) return Palette;

   ROW_HEIGHT : constant := 19;
   CELL_WIDTH : constant := 8;
   RULER_WIDTH : constant := 14;
   MINIMAP_WIDTH : constant := 96;

   type Editor is limited private;
   type Editor_Access is access Editor;

   function Create return Editor_Access;
   procedure Destroy (E : in out Editor_Access);

   procedure Attach (E : in out Editor; D : Text_Documents.Document_Access);
   function Document_Of (E : Editor) return Text_Documents.Document_Access;

   procedure Set_Area (E : in out Editor; Area : CuBit.UI.Rect);
   function Area (E : Editor) return CuBit.UI.Rect;
   procedure Set_Wrap (E : in out Editor; On : Boolean);
   function Wrap_On (E : Editor) return Boolean;
   procedure Set_Minimap (E : in out Editor; On : Boolean);
   function Minimap_On (E : Editor) return Boolean;
   procedure Set_Tab (E : in out Editor; Tab : Tab_Width);
   --  Show the document's search matches (while the host's find bar is open).
   procedure Set_Show_Matches (E : in out Editor; On : Boolean);
   --  Take the identifier under the primary cursor as the one whose other
   --  occurrences are shown (the host calls this once the cursor rests, so
   --  typing never repaints rows just for the highlight).
   procedure Refresh_Occurrence (E : in out Editor);
   function Settings (E : Editor) return Text_Documents.Layout_Settings;
   function Page_Rows (E : Editor) return Positive;
   function Text_Columns (E : Editor) return Positive;

   --  The viewport.
   procedure Scroll_Rows (E : in out Editor; Count : Integer);
   --  Line L near the top third of the view.
   procedure Scroll_To_Line (E : in out Editor; L : Line_Number);
   --  Scroll as little as needed to show the primary cursor.
   procedure Reveal_Primary (E : in out Editor);
   function Top_Line (E : Editor) return Line_Number;
   function First_Column (E : Editor) return Column_Count;

   --  A frame: lay out the visible rows; Damage is what differs from the
   --  last frame (empty when nothing does).
   procedure Prepare (E : in out Editor; Colors : Palette; Focused : Boolean; Damage : out CuBit.UI.Rect);
   --  Paint the prepared rows that changed and lie in the canvas's clip.
   procedure Draw (E : in out Editor; C : CuBit.UI.Canvas);
   --  Everything repaints next frame (theme, resize).
   procedure Invalidate (E : in out Editor);
   --  Rows painted by the last Draw (tests and the overlay).
   function Rows_Painted (E : Editor) return Natural;
   function Rows_Shown (E : Editor) return Natural;
   --  Highlighting was a guess on some row: the host repaints once the
   --  document's background pass has caught up.
   function Guessed_Highlight (E : Editor) return Boolean;

   --  Pointer input in the editor's area. Clicks: 1, 2 (word), 3 (line).
   procedure Pointer_Press
     (E : in out Editor; X, Y : Natural; Clicks : Positive; Control, Shift, Alt : Boolean);
   procedure Pointer_Drag (E : in out Editor; X, Y : Natural);
   procedure Pointer_Release (E : in out Editor);
   procedure Wheel (E : in out Editor; Steps : Integer);
   --  The document offset under X, Y (nearest cluster edge).
   function Offset_At (E : Editor; X, Y : Natural) return Byte_Offset;
   --  The screen position of an offset's caret (for popups); Shown False
   --  when it is not in the view.
   procedure Caret_Position (E : Editor; Offset : Byte_Offset; X, Y : out Natural; Shown : out Boolean);
private
   --  Rows a view can show: an 8K-tall view (4,320 pixels) is 228 rows.
   --  Each row's cells are allocated with the editor, so this bounds an
   --  editor's memory (about 10 KiB a row) and its creation time.
   MAXIMUM_ROWS : constant := 240;
   MAXIMUM_CELLS : constant := 1_024;
   subtype Row_Slot is Positive range 1 .. MAXIMUM_ROWS;
   subtype Cell_Index is Positive range 1 .. MAXIMUM_CELLS;

   --  What one cell shows.
   type Cell_Flags is record
      Selected : Boolean := False;
      Match : Boolean := False;
      Occurrence : Boolean := False;
      Caret : Boolean := False;       --  a caret before this cell
      Boxed : Boolean := False;       --  a character the font lacks, or an invalid byte
      Squiggle : Natural range 0 .. 3 := 0;   --  diagnostic severity + 1
   end record;
   type Char_Array is array (Cell_Index) of Character;
   type Kind_Array is array (Cell_Index) of Text_Syntax.Token_Kind;
   type Flag_Array is array (Cell_Index) of Cell_Flags;

   type Row_View is record
      Line : Line_Number := 1;
      Segment : Natural := 0;
      Row : Positive := 1;
      First, Last : Byte_Offset := 0;     --  the row's bytes (Last exclusive)
      Lead : Boolean := True;             --  the line's first row (shows its number)
      Final : Boolean := True;            --  the line's last row
      Current : Boolean := False;         --  the primary cursor's line
      Cells : Natural := 0;
      Chars : Char_Array;
      Kinds : Kind_Array;
      Flags : Flag_Array;
      Caret_After : Boolean := False;     --  a caret after the last cell
      Selected_End : Boolean := False;    --  the selection covers the line end
      Hash : Unsigned_64 := 0;
   end record;
   type Row_Array is array (Row_Slot) of Row_View;
   type Hash_Array is array (Row_Slot) of Unsigned_64;
   --  How far right of the text's left edge a row's content reaches, in
   --  pixels: its cells, then an end-of-line selection mark or a caret.
   --  Beyond it the row is plain background.
   subtype Content_Width is Natural range 0 .. (MAXIMUM_CELLS + 1) * CELL_WIDTH;
   type Width_Array is array (Row_Slot) of Content_Width;
   type Lit_Array is array (Row_Slot) of Boolean;
   type Row_Array_Access is access Row_Array;
   --  The last segment wrapped (rows of one segment are drawn in a run).
   type Wrap_Cache;
   type Wrap_Cache_Access is access Wrap_Cache;

   type Editor is limited record
      Document : Text_Documents.Document_Access;
      Bounds : CuBit.UI.Rect := (others => 0);
      Wrap : Boolean := False;
      Minimap : Boolean := False;
      Show_Matches : Boolean := False;
      --  The identifier under the primary cursor, its other occurrences
      --  shown (whole tokens; case-insensitive for Ada).
      Word : String (1 .. 64) := [others => ' '];
      Word_Length : Natural := 0;
      Word_Folded : Boolean := False;
      Tab : Tab_Width := 4;
      --  The top row: line, segment, row in the segment.
      Top : Line_Number := 1;
      Top_Segment : Natural := 0;
      Top_Row : Positive := 1;
      Column : Column_Count := 0;    --  first column shown (no wrap)
      Rows : Row_Array_Access;
      Wrapped : Wrap_Cache_Access;
      Shown : Natural := 0;
      Painted : Natural := 0;
      Previous : Hash_Array := [others => 0];
      Previous_Shown : Natural := 0;
      Dirty : Hash_Array := [others => 0];   --  nonzero: repaint the slot
      --  A dirty slot whose background stays the same repaints only up to
      --  the wider of its old and new content (Paint); the rest of the row
      --  is background already. Full: the slot repaints its whole width.
      Paint, Previous_Width : Width_Array := [others => 0];
      Full : Lit_Array := [others => True];
      Previous_Lit : Lit_Array := [others => False];   --  drawn as the current line
      Everything : Boolean := True;
      Chrome_Hash, Previous_Chrome : Unsigned_64 := 0;
      Colors : Palette;
      Focused : Boolean := False;
      Guessed : Boolean := False;
      --  Pointer state.
      Dragging : Boolean := False;
      Column_Drag : Boolean := False;
      Press_Offset : Byte_Offset := 0;
   end record;
end Text_Editors;
