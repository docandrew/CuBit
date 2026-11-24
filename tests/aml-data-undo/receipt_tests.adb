pragma Ada_2022;
with Ada.Text_IO;
with AML_Objects;
with AML_Objects.Construction_Test_Fixture;
with AML_Decode;
procedure Receipt_Tests is
   package O renames AML_Objects;
   use type O.State, O.Allocation_Status, O.Object_ID;
   Store : O.State := O.Empty (Object_Capacity => 8, Byte_Capacity => 32, Element_Capacity => 8);
   Status : O.Allocation_Status;
   Plan : O.Construction_Plan;
   ID, Parent, Child : O.Object_ID;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for Repeat in 1 .. 32 loop
      declare Before : constant O.State := Store; begin
         O.Plan_Construction (Store, 0, 0, Plan, Status);
         Check (Status = O.Allocated and then O.Construction_Current (Store, Plan));
         declare Item : O.Construction_Receipt := O.Capture_Construction (Store, Plan); begin
            O.New_Integer (Store, 17, ID, Status);
            Check (Status = O.Allocated and then not O.Construction_Current (Store, Plan));
            O.Restore_Construction (Store, Item);
            Check (Store = Before and then not O.Construction_Ready (Item));
         end;
      end;
      O.New_Integer (Store, 99, ID, Status);
      Check (Status = O.Allocated);
      O.Construction_Test_Fixture.Dirty_Unused (Store, ID);
      Check (O.Valid (Store));
      declare Before : constant O.State := Store; begin
         O.Plan_Construction (Store, 3, 0, Plan, Status);
         Check (Status = O.Allocated);
         declare Item : O.Construction_Receipt := O.Capture_Construction (Store, Plan); begin
            O.New_Buffer (Store, 3, AML_Decode.Bytes'[1,2,3], ID, Status);
            Check (Status = O.Allocated);
            O.Restore_Construction (Store, Item);
            Check (Store = Before);
         end;
         O.Plan_Construction (Store, 0, 2, Plan, Status);
         Check (Status = O.Allocated);
         declare Outer : O.Construction_Receipt := O.Capture_Construction (Store, Plan); begin
            O.New_Package (Store, 2, Parent, Status);
            Check (Status = O.Allocated);
            O.Plan_Construction (Store, 0, 0, Plan, Status);
            Check (Status = O.Allocated);
            declare Inner : O.Construction_Receipt := O.Capture_Construction (Store, Plan); begin
               O.New_Integer (Store, 42, Child, Status);
               Check (Status = O.Allocated);
               O.Set_Element (Store, Parent, 0, Child);
               O.Restore_Construction (Store, Inner);
               Check (not O.Valid (Store));
            end;
            O.Restore_Construction (Store, Outer);
            Check (O.Valid (Store) and then Store = Before);
         end;
         O.Plan_Construction (Store, 33, 0, Plan, Status);
         Check (Status = O.Byte_Limit and then not O.Construction_Admitted (Plan));
         Check (Store = Before);
      end;
      Store := O.Empty (Object_Capacity => 8, Byte_Capacity => 32, Element_Capacity => 8);
   end loop;
   for I in 1 .. 4 loop O.New_Integer (Store, AML_Decode.Integer_Value (I), ID, Status); end loop;
   O.Construction_Test_Fixture.Retire_Integer (Store, 2);
   Check (O.Valid (Store));
   declare Before : constant O.State := Store; begin
      O.Plan_Construction (Store, 0, 0, Plan, Status);
      declare Item : O.Construction_Receipt := O.Capture_Construction (Store, Plan); begin
         O.New_Integer (Store, 73, ID, Status);
         Check (Status = O.Allocated and then ID = 2);
         O.Restore_Construction (Store, Item);
         Check (Store = Before);
      end;
   end;
   Ada.Text_IO.Put_Line ("Construction receipt checks" & Checks'Image);
end Receipt_Tests;
