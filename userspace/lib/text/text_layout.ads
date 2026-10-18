with Text_Limits; use Text_Limits;

--  Row layout (docs/text-editor.md, "Layout"). Every operation works on one
--  segment's text (at most SEGMENT_BYTES, starting at a cluster boundary:
--  a line start or a segment start, without the line's end), so its cost
--  never depends on the length of the line or the document.
--
--  Columns count clusters (Text_UTF8): one cell each, two for wide
--  characters and invalid bytes, and a tab advances to the next multiple of
--  the tab width. Soft wrap breaks after the last space that fits, or
--  before the cluster that does not fit when a row has no space; a row
--  always holds at least one cluster. Proved: no runtime errors, row starts
--  strictly increase and stay within the text.
package Text_Layout with SPARK_Mode is
   MAXIMUM_ROWS : constant := SEGMENT_BYTES;
   subtype Row_Count is Natural range 0 .. MAXIMUM_ROWS;
   subtype Row_Index is Row_Count range 1 .. MAXIMUM_ROWS;
   type Row_Starts is array (Row_Index) of Segment_Position;

   --  Cells the cluster at Text (Index) takes at Column.
   function Advance (Text : String; Index : Positive; Column : Column_Count; Tab : Tab_Width) return Column_Count
     with Pre => Text'First = 1 and then Index in Text'Range and then Text'Length <= SEGMENT_BYTES
                 and then Column <= MAXIMUM_COLUMNS - MAXIMUM_TAB_WIDTH,
          Post => Advance'Result <= MAXIMUM_TAB_WIDTH;

   --  The columns from Text'First up to (not including) Upto, a cluster
   --  boundary or Text'Last + 1.
   function Column_Of (Text : String; Upto : Positive; Tab : Tab_Width) return Column_Count
     with Pre => Text'First = 1 and then Text'Last >= 0 and then Text'Length <= SEGMENT_BYTES and then Upto in 1 .. Text'Last + 1;

   --  The boundary at Column: the start of the cluster that covers it (or,
   --  with Nearest, the closer edge of that cluster); Text'Last + 1 past
   --  the end.
   function Index_At_Column
     (Text : String; Column : Column_Count; Tab : Tab_Width; Nearest : Boolean := False) return Positive
     with Pre => Text'First = 1 and then Text'Last >= 0 and then Text'Length <= SEGMENT_BYTES,
          Post => Index_At_Column'Result in 1 .. Text'Last + 1;

   --  The cluster boundary before Index (Index > Text'First), found forward
   --  from Text'First.
   function Previous_Boundary (Text : String; Index : Positive) return Positive
     with Pre => Text'First = 1 and then Text'Last >= 0 and then Text'Length <= SEGMENT_BYTES and then Index in 2 .. Text'Last + 1,
          Post => Previous_Boundary'Result in 1 .. Index - 1;
   --  The cluster boundary after Index (a boundary in Text).
   function Next_Boundary (Text : String; Index : Positive) return Positive
     with Pre => Text'First = 1 and then Text'Last >= 0 and then Text'Length <= SEGMENT_BYTES and then Index in Text'Range,
          Post => Next_Boundary'Result in Index + 1 .. Text'Last + 1;

   --  Soft wrap of Text at Width columns: Count rows, row K starting at
   --  Starts (K) (positions relative to Text'First, from 1). Width 0: one
   --  row.
   procedure Wrap
     (Text : String; Width : Column_Count; Tab : Tab_Width; Starts : out Row_Starts; Count : out Row_Index)
     with Pre => Text'First = 1 and then Text'Last >= 0 and then Text'Length <= SEGMENT_BYTES,
          Post => Starts (1) = 1 and then Count <= Natural'Max (Text'Length, 1)
                  and then (for all K in 2 .. Count => Starts (K) > Starts (K - 1) and then Starts (K) <= Text'Length);
end Text_Layout;
