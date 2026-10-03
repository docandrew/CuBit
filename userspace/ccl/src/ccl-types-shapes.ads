--  How a value looks as rows, for a front end to lay it out as a table: the
--  record type a list holds (or a record itself), its fields' names and
--  their types' names. Pure type-registry data, computed where a result is
--  produced (the interpreter and the VM alike), so no front end re-derives a
--  type from printed text.
package CCL.Types.Shapes with SPARK_Mode is
   type Field is record
      Identifier : Name;
      Type_Name : Name;
      --  Integer-valued (including range types): aligned as numbers.
      Numeric : Boolean := False;
   end record;
   type Field_Array is array (Component_Index) of Field;
   type Row_Shape is record
      --  A list of records (many rows) or a single record (one row).
      Many : Boolean := False;
      Row_Type : Name;
      Count : Component_Count := 0;
      Fields : Field_Array := [others => (others => <>)];
   end record;
   --  The rows of a value of type Ref; Count = 0 when it has none (scalars,
   --  lists of scalars, sums).
   function Shape_Of (Item : Registry; Ref : Type_Reference) return Row_Shape;
end CCL.Types.Shapes;
