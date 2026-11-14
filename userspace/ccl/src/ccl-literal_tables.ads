with Ada.Finalization;
with CCL.Types;
with CCL.Types.Shapes;

--  The cells of a record's canonical literal, or of a list of them, as
--  spans of the literal: (C 1 "x") is one row of two cells, [(C 1 "x")
--  (C 2 "y")] two rows. A cell is one field's own canonical literal, so a
--  nested record or list stays whole in its cell. Front ends lay the cells
--  out as a table under CCL.Types.Shapes' field names. A list of a variant
--  (Shape.Variant_Rows): [(V.First (R 1 "x")) (V.Other (O 2 "y"))], each
--  element's record fields in the columns its member's map names, the
--  others empty.
--  A table holds every row of its literal (on the heap, grown as Split
--  reads them), up to ROW_CEILING; Total counts them all. The visible
--  part is SPARK; the storage is behind the private part.
package CCL.Literal_Tables with SPARK_Mode is
   --  The rows one table holds (CAPACITY): past it rows are only counted.
   ROW_CEILING : constant := 2 ** 20;
   subtype Row_Count is Natural range 0 .. ROW_CEILING;
   subtype Row_Index is Positive range 1 .. ROW_CEILING;
   type Span is record
      First : Positive := 1;
      Last : Natural := 0;    --  Last < First: empty
   end record;
   type Table is private;
   function Rows (Item : Table) return Row_Count;
   function Total (Item : Table) return Natural;
   function Columns (Item : Table) return CCL.Types.Component_Count;
   --  Every row had exactly Columns cells and the literal was well formed.
   function Complete (Item : Table) return Boolean;
   --  Row's cell in Column (an empty span outside the table).
   function Cell (Item : Table; Row : Row_Index; Column : CCL.Types.Component_Index) return Span;
   procedure Split
     (Literal : String; Shape : CCL.Types.Shapes.Row_Shape; Result : out Table)
   with Pre => Literal'Last < Positive'Last;
private
   pragma SPARK_Mode (Off);
   type Grid is array (Row_Index range <>, CCL.Types.Component_Index range <>) of Span;
   type Grid_Access is access Grid;
   type Table is new Ada.Finalization.Controlled with record
      Rows : Row_Count := 0;
      Total : Natural := 0;
      Columns : CCL.Types.Component_Count := 0;
      Cells : Grid_Access;
      Complete : Boolean := False;
   end record;
   overriding procedure Adjust (Item : in out Table);
   overriding procedure Finalize (Item : in out Table);
end CCL.Literal_Tables;
