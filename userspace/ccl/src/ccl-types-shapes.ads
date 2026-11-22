--  How a value looks as rows, for a front end to lay it out as a table: the
--  record type a list holds (or a record itself), its fields' names and
--  their types' names. Pure type-registry data, computed where a result is
--  produced (the interpreter and the VM alike), so no front end re-derives a
--  type from printed text.
--
--  A list of a variant whose first member carries a record (logs.recent's
--  List<LogItem>: records, gaps, tallies) is rows too: that record's
--  columns, and a row of another member fills the columns its own record
--  shares by field name (a gap's time, source and message), the rest
--  empty.
package CCL.Types.Shapes with SPARK_Mode is
   type Field is record
      Identifier : Name;
      Type_Name : Name;
      --  Integer-valued (including range types): aligned as numbers.
      Numeric : Boolean := False;
   end record;
   type Field_Array is array (Component_Index) of Field;
   --  Of a member's record, the column each field fills (0: none).
   type Column_Map is array (Component_Index) of Component_Count;
   type Member_Row is record
      Member : Name;
      --  Its payload record's fields (0: it carries none).
      Fields : Component_Count := 0;
      Column_Of : Column_Map := [others => 0];
   end record;
   type Member_Rows is array (Component_Index) of Member_Row;
   type Row_Shape is record
      --  A list of records (many rows) or a single record (one row).
      Many : Boolean := False;
      Row_Type : Name;
      Count : Component_Count := 0;
      Fields : Field_Array := [others => (others => <>)];
      --  A list of a variant (above): each element names its member
      --  (Members (1 .. Member_Count)); the first's record is Row_Type.
      Variant_Rows : Boolean := False;
      Member_Count : Component_Count := 0;
      Members : Member_Rows := [others => (others => <>)];
   end record;
   --  The rows of a value of type Ref; Count = 0 when it has none (scalars,
   --  lists of scalars, other sums).
   function Shape_Of (Item : Registry; Ref : Type_Reference) return Row_Shape;
end CCL.Types.Shapes;
