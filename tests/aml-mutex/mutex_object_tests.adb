with Ada.Text_IO;
with AML_Decode;
with AML_Objects;
with AML_Mutexes;
with AML_Objects.Copies;
procedure Mutex_Object_Tests is
   package O renames AML_Objects;
   use type O.Allocation_Status;
   use type O.Object_Kind;
   use type O.State;
   use type AML_Mutexes.Sync_Level;
   use type AML_Decode.Byte;
   Store : O.State := O.Empty;
   ID, Target, Parent : O.Object_ID;
   Status : O.Allocation_Status;
   Witness : O.Copies.Copy_Witness;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Text : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Text; end if; end Check;
begin
   for Flags in AML_Decode.Byte range 0 .. AML_Mutexes.Canonical_Flags_Last loop
      O.New_Mutex (Store, Flags, ID, Status);
      Check (Status = O.Allocated and then O.Valid (Store), "allocate mutex");
      Check (O.Kind (Store, ID) = O.Mutex_Object and then O.Length (Store, ID) = 0,
        "real leaf object");
      Check (O.Mutex_Flags (Store, ID) = Flags and then O.Mutex_Level (Store, ID) = AML_Mutexes.Sync_Level (Flags), "immutable level");
      declare Before : constant O.State := Store; begin
         O.Copies.Clone (Store, ID, Target, Status, Witness);
         Check (Status = O.Unsupported_Copy and then Target = O.No_Object and then Store = Before, "mutex copy rejects atomically");
      end;
   end loop;
   for Flags in AML_Decode.Byte range AML_Mutexes.Canonical_Flags_Last + 1 .. AML_Decode.Byte'Last loop
      declare Before : constant O.State := Store; begin
         O.New_Mutex (Store, Flags, ID, Status);
         Check (Status = O.Invalid_Mutex_Flags and then ID = O.No_Object and then Store = Before, "reserved flags rejected unchanged");
      end;
   end loop;
   O.New_Mutex (Store, 7, ID, Status); Check (Status = O.Allocated, "nested mutex");
   O.New_Package (Store, 1, Parent, Status); Check (Status = O.Allocated, "package");
   O.Set_Element (Store, Parent, 0, ID);
   declare Before : constant O.State := Store; begin
      O.Copies.Clone (Store, Parent, Target, Status, Witness);
      Check (Status = O.Unsupported_Copy and then Target = O.No_Object and then Store = Before, "nested copy rollback");
   end;
   Ada.Text_IO.Put_Line ("Mutex object checks" & Checks'Image);
end Mutex_Object_Tests;
