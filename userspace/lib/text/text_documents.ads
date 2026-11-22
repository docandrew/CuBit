with Interfaces;
with Text_Limits; use Text_Limits;
with Text_Stores;
with Text_Pieces;
with Text_Cursors;
with Text_History;
with Text_Marks;
with Text_Syntax;
with Text_Search;

--  One document (docs/text-editor.md, "Data model"): its bytes (a store
--  and a piece table), its selections, its undo history, its marks, its
--  highlighting checkpoints and its search. A host creates any number of
--  them; nothing here is global and nothing here does I/O. Loading and
--  saving are driven by the host, a chunk at a time.
--
--  Every editing command acts on all selections and is one undo step.
--  Commands that would touch a protected mark (Text_Marks) change nothing
--  and say so (Last_Refusal). Positions are byte offsets; movement is by
--  grapheme cluster (Text_UTF8), within a segment (Text_Layout).
package Text_Documents is
   use Interfaces;

   --  A document's capacities, chosen by its host (no hidden limits).
   type Limits is record
      --  The most bytes ever stored: the file plus everything typed or
      --  pasted (the store is append-only).
      Store_Bytes : Byte_Count := 64 * 1_048_576;
      Piece_Blocks : Text_Pieces.Block_Index := 4_096;
      Selections : Text_Cursors.Selection_Index := 65_536;
      Marks : Text_Marks.Mark_Index := 4_096;
      History : Text_History.Limits := (others => <>);
   end record;
   --  Capacities for a file of File_Bytes with room for Edit_Bytes of
   --  typing and pasting on top.
   function Limits_For (File_Bytes, Edit_Bytes : Byte_Count) return Limits;

   type Document (<>) is limited private;
   type Document_Access is access Document;

   function Create (Settings : Limits; Name : String) return Document_Access;
   procedure Destroy (D : in out Document_Access);

   --  The name it was opened as (a granted file name), and its language.
   function Name (D : Document) return String;
   procedure Set_Name (D : in out Document; Value : String);
   function Language (D : Document) return Text_Syntax.Language;
   procedure Set_Language (D : in out Document; Value : Text_Syntax.Language);

   ---------------------------------------------------------------------------
   --  Loading: Begin_Load, any number of Load_Chunk, End_Load. The document
   --  is read-only meanwhile (its first lines can be shown at once).
   procedure Begin_Load (D : in out Document; Expected : Byte_Count);
   --  Ok False: more than the store's capacity.
   procedure Load_Chunk (D : in out Document; Bytes : String; Ok : out Boolean);
   procedure End_Load (D : in out Document);
   function Loading (D : Document) return Boolean;
   function Expected_Bytes (D : Document) return Byte_Count;

   ---------------------------------------------------------------------------
   --  Reading.
   function Length (D : Document) return Byte_Count;
   function Line_Total (D : Document) return Line_Number;
   --  The offset of line L's first byte.
   function Line_Start (D : Document; L : Line_Number) return Byte_Offset;
   --  The offset of line L's end: its CR LF or LF, or the document's end.
   function Line_End (D : Document; L : Line_Number) return Byte_Offset;
   function Line_Of (D : Document; Offset : Byte_Offset) return Line_Number;
   procedure Read (D : Document; Offset : Byte_Offset; Into : out String);
   function Byte_At (D : Document; Offset : Byte_Offset) return Character;
   --  Bytes First .. Last - 1 (the host bounds the size it asks for).
   function Slice (D : Document; First, Last : Byte_Offset) return String;
   --  The whole text (tests and small documents only).
   function Content (D : Document) return String;
   --  CR LF line ends (detected at load; new lines use the same).
   function Uses_CRLF (D : Document) return Boolean;
   --  Changes with every edit, undo or load: views compare it.
   function Revision (D : Document) return Unsigned_64;
   function Modified (D : Document) return Boolean;
   procedure Mark_Saved (D : in out Document);
   function Pieces_Used (D : Document) return Natural;

   --  A frozen version for saving: the piece list now (the store is
   --  append-only, so its bytes never change), readable while editing
   --  continues. Mark_Snapshot_Saved when the file holds it.
   type Snapshot is limited private;
   type Snapshot_Access is access Snapshot;
   function Take_Snapshot (D : Document) return Snapshot_Access;
   function Snapshot_Length (S : Snapshot) return Byte_Count;
   procedure Read_Snapshot (D : Document; S : Snapshot; Offset : Byte_Offset; Into : out String);
   procedure Mark_Snapshot_Saved (D : in out Document; S : Snapshot);
   procedure Free_Snapshot (S : in out Snapshot_Access);

   ---------------------------------------------------------------------------
   --  Segments and rows (docs/text-editor.md, "Layout").
   type Layout_Settings is record
      Tab : Tab_Width := 4;
      --  Soft wrap width in columns; 0 for none. Lines longer than one
      --  segment always wrap at Window_Columns.
      Wrap : Column_Count := 0;
      Window_Columns : Column_Count := 120;
   end record;
   function Effective_Wrap (D : Document; Settings : Layout_Settings; L : Line_Number) return Column_Count;
   --  The segment holding Offset: its line, index (from 0) and bytes.
   type Segment_Place is record
      Line : Line_Number := 1;
      Index : Natural := 0;
      First, Last : Byte_Offset := 0;   --  Last exclusive (before the line end)
   end record;
   function Segment_Of (D : Document; Offset : Byte_Offset) return Segment_Place;
   function Segment (D : Document; L : Line_Number; Index : Natural) return Segment_Place;
   function Segment_Count (D : Document; L : Line_Number) return Positive;

   ---------------------------------------------------------------------------
   --  Selections.
   function Selections (D : Document) return Text_Cursors.Set;
   function Selection_Count (D : Document) return Natural;
   function Selection (D : Document; Index : Positive) return Text_Cursors.Selection;
   function Primary (D : Document) return Text_Cursors.Selection;
   function Primary_Index (D : Document) return Positive;
   procedure Set_Caret (D : in out Document; Offset : Byte_Offset; Extend : Boolean := False);
   procedure Set_Selection (D : in out Document; Anchor, Head : Byte_Offset);
   --  Add a cursor (Ctrl+click) at Offset, or a selection.
   procedure Add_Selection (D : in out Document; Anchor, Head : Byte_Offset);
   procedure Keep_Primary (D : in out Document);
   --  Every selection becomes a cursor at its start.
   procedure Collapse (D : in out Document);
   procedure Select_All (D : in out Document);

   type Motion is
     (Left, Right, Word_Left, Word_Right, Line_Home, Line_End_Motion, Up, Down, Page_Up, Page_Down,
      Document_Start, Document_End);
   --  Move every selection's head (Extend keeps the anchor). Page moves go
   --  Page_Rows rows.
   procedure Move
     (D : in out Document; Where : Motion; Extend : Boolean; Settings : Layout_Settings; Page_Rows : Positive := 1);
   --  Add a cursor on the row above or below the primary one (column
   --  selection from the keyboard).
   procedure Add_Cursor_Vertically (D : in out Document; Down : Boolean; Settings : Layout_Settings);
   --  A box from Anchor_Offset's column to Head's column on every line
   --  between them: one selection per line (Alt+drag).
   procedure Select_Column_Box (D : in out Document; Anchor_Offset, Head_Offset : Byte_Offset; Settings : Layout_Settings);
   --  The word around every caret (or the next occurrence of the primary
   --  selection, added as a selection: Ctrl+D).
   procedure Select_Word_Or_Next (D : in out Document);
   procedure Select_Line (D : in out Document);
   --  One cursor per line of every selection (Ctrl+Shift+L).
   procedure Split_Into_Lines (D : in out Document);
   function Selected_Text (D : Document; Separator : String := [1 => ASCII.LF]) return String;

   ---------------------------------------------------------------------------
   --  Editing. Now_Us times coalescing. Each call is one undo step (typing
   --  may continue the last one).
   type Refusal is (None, Read_Only_Document, Protected_Region, Document_Full, History_Dropped);
   function Last_Refusal (D : Document) return Refusal;

   procedure Insert_Text (D : in out Document; Text : String; Now_Us : Unsigned_64 := 0; Typed : Boolean := False);
   --  Paste with one line per selection when the counts match.
   procedure Paste (D : in out Document; Text : String; Now_Us : Unsigned_64 := 0);
   procedure New_Line (D : in out Document; Now_Us : Unsigned_64 := 0);
   procedure Delete_Backward (D : in out Document; Now_Us : Unsigned_64 := 0; Whole_Word : Boolean := False);
   procedure Delete_Forward (D : in out Document; Now_Us : Unsigned_64 := 0; Whole_Word : Boolean := False);
   procedure Delete_Lines (D : in out Document);
   procedure Indent (D : in out Document; Settings : Layout_Settings);
   procedure Outdent (D : in out Document; Settings : Layout_Settings);
   procedure Toggle_Comment (D : in out Document);
   procedure Duplicate_Lines (D : in out Document);
   procedure Undo (D : in out Document);
   procedure Redo (D : in out Document);
   function Can_Undo (D : Document) return Boolean;
   function Can_Redo (D : Document) return Boolean;
   --  Break typing coalescing (the cursor moved, focus changed).
   procedure Seal_Typing (D : in out Document);

   ---------------------------------------------------------------------------
   --  Marks (Embedding): the host's diagnostics, annotations and regions.
   procedure Add_Mark (D : in out Document; Item : Text_Marks.Mark; Added : out Boolean);
   procedure Remove_Marks (D : in out Document; Id : Text_Marks.Mark_Id);
   function Mark_Count (D : Document) return Natural;
   function Mark (D : Document; Index : Positive) return Text_Marks.Mark;
   --  The host replaces a Generated region's text (an undoable edit).
   procedure Replace_Region (D : in out Document; Id : Text_Marks.Mark_Id; Text : String; Done : out Boolean);

   ---------------------------------------------------------------------------
   --  Search.
   type Search_Status is (No_Pattern, Ready, Bad_Pattern, Found, Not_Found, Searching);
   procedure Set_Search (D : in out Document; Pattern : String; Settings : Text_Search.Options;
                         Status : out Search_Status);
   function Search_Pattern (D : Document) return String;
   function Search_Settings (D : Document) return Text_Search.Options;
   --  Find the next (or previous) match from the primary selection, at
   --  most Budget bytes this call; Searching means call again. Wraps once.
   procedure Find
     (D : in out Document; Forward : Boolean; Budget : Work_Budget; Status : out Search_Status);
   --  Every match in the document as selections (up to their capacity).
   procedure Select_All_Matches (D : in out Document; Count : out Natural);
   --  Replace the primary selection when it is a match, then find the next.
   procedure Replace_One (D : in out Document; Replacement : String; Status : out Search_Status);
   procedure Replace_All (D : in out Document; Replacement : String; Count : out Natural);
   --  Matches in one line's segment text (for drawing): calls Visit for each.
   generic
      with procedure Visit (First : Positive; Length : Natural);
   procedure Matches_In (D : Document; Text : String);
   --  Count all matches, Budget bytes per call. Done when complete.
   procedure Count_Matches (D : in out Document; Budget : Work_Budget; Total : out Natural; Done : out Boolean);

   ---------------------------------------------------------------------------
   --  Highlighting (docs/text-editor.md, "Highlighting").
   CHECKPOINT_LINES : constant := 64;
   --  The spans of line L's first segment (the text the caller read).
   --  Exact False: the state at its start is a guess until the
   --  background pass gets there (the view repaints then).
   procedure Line_Spans
     (D : in out Document; L : Line_Number; Text : String; Spans : out Text_Syntax.Span_Array;
      Count : out Text_Syntax.Span_Count; Exact : out Boolean);
   --  Re-lex up to Budget lines past the checkpoint watermark. Changed:
   --  checkpoints moved (repaint highlighted rows).
   procedure Highlight_Pump (D : in out Document; Budget : Work_Budget; Busy, Changed : out Boolean);
   function Highlight_Watermark (D : Document) return Line_Number;
private
   type Piece_List is array (Positive range <>) of Text_Pieces.Piece;
   type Piece_List_Access is access Piece_List;
   type Snapshot is limited record
      Pieces : Piece_List_Access;
      Length : Byte_Count := 0;
      Done : Natural := 0;
      Era : Natural := 0;
   end record;
   --  Recent line starts (direct-mapped, by line number), valid for one
   --  revision: drawing asks for the same lines many times per frame.
   LINE_CACHE_SIZE : constant := 1_024;
   type Line_Cache_Entry is record
      Line : Line_Count := 0;
      Start : Byte_Offset := 0;
      --  Its end (before any CR LF), once Line_End has found it.
      Has_Stop : Boolean := False;
      Stop : Byte_Offset := 0;
   end record;
   type Line_Cache_Array is array (0 .. LINE_CACHE_SIZE - 1) of Line_Cache_Entry;
   type Line_Cache is record
      Revision : Unsigned_64 := 0;
      Entries : Line_Cache_Array;
   end record;
   type Line_Cache_Access is access Line_Cache;
   type Checkpoint_Array is array (Natural range <>) of Text_Syntax.Line_State;
   type Checkpoint_Access is access Checkpoint_Array;
   type Name_Access is access String;

   type Find_Job is record
      Active : Boolean := False;
      Forward : Boolean := True;
      Position : Byte_Offset := 0;    --  where the scan continues
      Origin : Byte_Offset := 0;      --  where it started (stop after wrapping)
      Wrapped : Boolean := False;
   end record;

   type Count_Job is record
      Active : Boolean := False;
      Position : Byte_Offset := 0;
      Total : Natural := 0;
      Revision : Unsigned_64 := 0;
      Complete : Boolean := False;
   end record;

   type Document (Blocks : Text_Pieces.Block_Index; Selection_Capacity : Text_Cursors.Selection_Index;
                  Mark_Capacity : Text_Marks.Mark_Index;
                  Groups, Records, Logged_Pieces, Logged_Selections : Text_History.Entry_Index) is limited record
      Store : Text_Stores.Store;
      Pieces : Text_Pieces.Table (Blocks);
      Cursors : Text_Cursors.Set (Selection_Capacity);
      History : Text_History.History (Groups, Records, Logged_Pieces, Logged_Selections);
      Marks : Text_Marks.Set (Mark_Capacity);
      Title : Name_Access;
      Lines_Cache : Line_Cache_Access;
      Kind : Text_Syntax.Language := Text_Syntax.Plain_Text;
      Is_Loading : Boolean := False;
      Expected : Byte_Count := 0;
      CRLF : Boolean := False;
      Revision_Count : Unsigned_64 := 1;
      Refused : Refusal := None;
      --  Highlighting: the state at the start of line K * CHECKPOINT_LINES + 1,
      --  exact for K < Valid_Checkpoints.
      Checkpoints : Checkpoint_Access;
      Valid_Checkpoints : Natural := 1;
      --  The furthest line a view asked spans for: the pass goes no further.
      Demand : Line_Count := 0;
      --  Checkpoints valid before the last edits, when those edits kept the
      --  line count (they still line up): re-adopted once the state at a
      --  re-lexed checkpoint equals the stored one.
      Reusable_Checkpoints : Natural := 0;
      Known_Lines : Line_Count := 1;
      --  The last line lexed and the state after it (rows are drawn in
      --  order, so the next line continues from it).
      Cache_Line : Text_Limits.Line_Count := 0;
      Cache_State : Text_Syntax.Line_State := Text_Syntax.INITIAL_STATE;
      Cache_Revision : Unsigned_64 := 0;
      Cache_Exact : Boolean := False;
      --  Search.
      Pattern : String (1 .. Text_Search.MAXIMUM_PATTERN) := [others => ' '];
      Pattern_Length : Natural := 0;
      Search_Options : Text_Search.Options;
      Program : Text_Search.Program;
      Program_Ready : Boolean := False;
      --  A regex's leading literal (letters, digits, '_', ' ' before any
      --  operator): candidates are found by literal search first.
      Prefix_Length : Natural := 0;
      Finding : Find_Job;
      Counting : Count_Job;
   end record;
end Text_Documents;
