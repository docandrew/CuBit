with Text_Limits; use Text_Limits;

--  Marks (docs/text-editor.md, "Embedding"): ranges a host anchors to the
--  text, moved by every edit. Diagnostics (squiggles, gutter and overview
--  ruler), gutter annotations (breakpoints, a form designer's "handler of
--  Button1"), and protected regions: Read_Only and Generated text cannot be
--  changed by editing commands; only the host changes Generated text, as
--  an ordinary undoable edit (Text_Documents.Replace_Region).
--
--  Marks are kept sorted by start. An insert before a mark moves it; an
--  insert strictly inside it grows it; an insert at either edge stays
--  outside it; a deletion shrinks it, possibly to nothing. Every edit
--  moves the marks in one pass. Proved: no runtime errors and every mark
--  keeps First <= Last.
package Text_Marks with SPARK_Mode is
   MAXIMUM_MARKS : constant := 2 ** 20;
   subtype Mark_Count is Natural range 0 .. MAXIMUM_MARKS;
   subtype Mark_Index is Mark_Count range 1 .. MAXIMUM_MARKS;

   type Mark_Kind is (Diagnostic, Annotation, Read_Only, Generated);
   type Severity is (Hint, Information, Warning, Error);
   --  What a gutter annotation shows.
   type Gutter_Glyph is (No_Glyph, Breakpoint, Bookmark, Handler_Link, Information_Glyph);
   --  The host's own identifier for a mark (what it gets back on a click).
   type Mark_Id is range 0 .. 2 ** 31 - 1;

   type Mark is record
      Kind : Mark_Kind := Diagnostic;
      First, Last : Byte_Offset := 0;
      Level : Severity := Information;
      Glyph : Gutter_Glyph := No_Glyph;
      Id : Mark_Id := 0;
   end record;

   function Protects (M : Mark) return Boolean is (M.Kind in Read_Only | Generated);

   type Set (Capacity : Mark_Index) is private;

   function Count (S : Set) return Mark_Count;
   function Element (S : Set; Index : Mark_Index) return Mark
     with Pre => Index <= Count (S), Post => Element'Result.First <= Element'Result.Last;
   procedure Initialize (S : in out Set) with Post => Count (S) = 0;
   --  Add a mark in start order; False when the set is full.
   procedure Add (S : in out Set; Item : Mark; Added : out Boolean)
     with Pre => Item.First <= Item.Last;
   --  Remove every mark with this identifier.
   procedure Remove (S : in out Set; Id : Mark_Id);
   procedure Clear (S : in out Set) with Post => Count (S) = 0;
   --  The index of the first mark with this identifier, 0 for none.
   function Find (S : Set; Id : Mark_Id) return Mark_Count
     with Post => Find'Result <= Count (S);

   --  Would an edit of [First, Last) (an insert when First = Last) touch a
   --  protected region? Inserting at a region's edge does not.
   function Blocks_Edit (S : Set; First, Last : Byte_Offset) return Boolean
     with Pre => First <= Last;

   procedure Adjust_Insert (S : in out Set; Offset : Byte_Offset; Length : Byte_Count)
     with Pre => Length <= MAXIMUM_BYTES / 2;
   procedure Adjust_Delete (S : in out Set; Offset : Byte_Offset; Length : Byte_Count)
     with Pre => Offset <= MAXIMUM_BYTES - Length;
   --  Set the extent of mark Index (the host replaced a region's text).
   procedure Set_Extent (S : in out Set; Index : Mark_Index; First, Last : Byte_Offset)
     with Pre => Index <= Count (S) and then First <= Last;
private
   type Mark_Array is array (Mark_Index range <>) of Mark;
   type Set (Capacity : Mark_Index) is record
      Items : Mark_Array (1 .. Capacity) := [others => (others => <>)];
      Used : Mark_Count := 0;
   end record
     with Type_Invariant => Set.Used <= Set.Capacity
                            and then (for all K in 1 .. Set.Used => Set.Items (K).First <= Set.Items (K).Last);
   function Count (S : Set) return Mark_Count is (S.Used);
end Text_Marks;
