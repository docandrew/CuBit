with CCL.Types;

--  The cells of a record's canonical literal, or of a list of them, as
--  spans of the literal: (C 1 "x") is one row of two cells, [(C 1 "x")
--  (C 2 "y")] two rows. A cell is one field's own canonical literal, so a
--  nested record or list stays whole in its cell. Front ends lay the cells
--  out as a table under CCL.Types.Shapes' field names.
package CCL.Literal_Tables with SPARK_Mode is
   --  Rows shown at once; Total counts them all.
   Maximum_Rows : constant := 64;
   subtype Row_Count is Natural range 0 .. Maximum_Rows;
   subtype Row_Index is Positive range 1 .. Maximum_Rows;
   type Span is record
      First : Positive := 1;
      Last : Natural := 0;    --  Last < First: empty
   end record;
   type Cell_Grid is array (Row_Index, CCL.Types.Component_Index) of Span;
   type Table is record
      Rows : Row_Count := 0;
      Total : Natural := 0;
      Columns : CCL.Types.Component_Count := 0;
      Cells : Cell_Grid := [others => [others => (others => <>)]];
      --  Every row had exactly Columns cells and the literal was well formed.
      Complete : Boolean := False;
   end record;
   procedure Split
     (Literal : String; Many : Boolean; Columns : CCL.Types.Component_Count;
      Result : out Table)
   with Pre => Literal'Last < Positive'Last;
end CCL.Literal_Tables;
