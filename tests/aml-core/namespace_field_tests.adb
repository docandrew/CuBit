with Ada.Text_IO; use Ada.Text_IO;
with AML_Namespace;
with AML_Decode;
with AML_Execute;
procedure Namespace_Field_Tests is
   package NS is new AML_Namespace (Capacity => 7);
   use NS;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   Tree : State := Empty;
   Before : State;
   Node, Scope, Region_Node, Field_Node : Node_ID;
   Added : Insert_Status;
   Status : Bind_Status;
   Loaded : Load_Status;
   Checks : Natural := 0;
   Region : constant Table_Region := (Table => 2, Extent => 36);
   Field : constant Table_Field := (Region => Region, Offset => 7, Bits => 65);
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Insert (Tree, Root, "_SB_", Scope, Added);
   Check (Added = Inserted);
   Bind_Table_Region (Tree, Root, "RGN0", Region, Region_Node, Status);
   Check (Status = Bound and then Kind (Tree, Region_Node) = Table_Region_Object
     and then Region_Data (Tree, Region_Node) = Region and then not Has_Integer (Tree, Region_Node));
   Bind_Table_Field (Tree, Scope, "FLD0", Field, Field_Node, Status);
   Check (Status = Bound and then Kind (Tree, Field_Node) = Table_Field_Object
     and then Field_Data (Tree, Field_Node) = Field and then not Has_Integer (Tree, Field_Node));
   Before := Tree;
   Bind_Table_Region (Tree, Root, "RGN0", (1, 1), Node, Status);
   Check (Status = Binding_Duplicate and then Node = Root and then Tree = Before);
   Bind_Table_Field (Tree, Root, "0BAD", Field, Node, Status);
   Check (Status = Binding_Invalid and then Tree = Before);
   Bind_Table_Field (Tree, Region_Node, "FLD1", Field, Node, Status);
   Check (Status = Binding_Invalid and then Tree = Before);
   Bind_Table_Field (Tree, Root, "FLD1", Field, Node, Status, Owner => Region_Node);
   Check (Status = Binding_Invalid and then Tree = Before);
   for Offset in 280 .. 296 loop
      for Bits in 0 .. 16 loop
         declare
            T : State := Tree;
         begin
            Bind_Table_Field (T, Root, "FLD1", (Region, Offset, Bits), Node, Status);
            if Offset + Bits > 288 then
               Check (Status = Binding_Invalid and then T = Tree);
            else
               Check (Status = Bound and then Field_Data (T, Node) = Table_Field'(Region, Offset, Bits));
               Check (Region_Data (T, Region_Node) = Region and Field_Data (T, Field_Node) = Field);
            end if;
         end;
      end loop;
   end loop;
   Bind_Table_Field (Tree, Root, "FLD1", (Region, Natural'Last, 1), Node, Status);
   Check (Status = Binding_Invalid and then Tree = Before);
   -- ObjectType reports Region=10 and FieldUnit=5 without treating either as
   -- an integer value. This is namespace metadata, not a field read.
   Load_Names (Tree,
     [16#14#, 16#0C#, 16#54#, 16#59#, 16#50#, 16#45#, 0,
      16#A4#, 16#8E#, 16#52#, 16#47#, 16#4E#, 16#30#], AML_Decode.Bits_64, Loaded);
   Check (Loaded = NS.Loaded);
   Node := Child (Tree, Root, "TYPE");
   declare
      R : constant AML_Execute.Execution_Result := Invoke (Tree, Node, [others => 0], 0, 100);
   begin
      Check (R.Status = AML_Execute.Returned and then R.Value = 10);
   end;
   Load_Names (Tree,
     [16#14#, 16#12#, 16#54#, 16#59#, 16#50#, 16#46#, 0,
      16#A4#, 16#8E#, 16#5C#, 16#2E#, 16#5F#, 16#53#, 16#42#, 16#5F#,
      16#46#, 16#4C#, 16#44#, 16#30#], AML_Decode.Bits_64, Loaded);
   Check (Loaded = NS.Loaded);
   declare
      R : constant AML_Execute.Execution_Result := Invoke
        (Tree, Child (Tree, Root, "TYPF"), [others => 0], 0, 100);
   begin
      Check (R.Status = AML_Execute.Returned and then R.Value = 5);
   end;
   Before := Tree;
   Bind_Table_Field (Tree, Root, "TEMP", Field, Field_Node, Status, Owner => Node);
   Check (Status = Binding_Invalid and then Tree = Before);
   Bind_Table_Field (Tree, Root, "FLD1", Field, Node, Status);
   Check (Status = Bound);
   Bind_Table_Region (Tree, Root, "RGN1", Region, Node, Status);
   Check (Status = Bound and then NS.Count (Tree) = 7);
   Before := Tree;
   Bind_Table_Field (Tree, Root, "FULL", Field, Node, Status);
   Check (Status = Binding_Full and then Tree = Before and then Node = Root);
   Bind_Table_Region (Tree, Root, "RGN1", Region, Node, Status);
   Check (Status = Binding_Duplicate and then Tree = Before);
   Put_Line ("AML-NAMESPACE-FIELD-CHECK: PASS" & Checks'Image);
end Namespace_Field_Tests;
