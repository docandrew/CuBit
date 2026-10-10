with Ada.Text_IO;
with AML_Decode;
with AML_Execute;
with AML_Objects; use AML_Objects;
with Namespace_Instance;
procedure Object_Tests is
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   package NS renames Namespace_Instance;
   use type NS.Load_Status;
   use type NS.State;
   use type AML_Execute.Execution_Status;
   Store : State := Empty;
   Before : State;
   ID, Number, Package_ID : Object_ID;
   Status : Allocation_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Check (Valid (Store) and Live_Count (Store) = 0);
   New_Integer (Store, 123, Number, Status);
   Check (Status = Allocated and Number /= No_Object and Integer_Data (Store, Number) = 123);
   New_Bytes (Store, String_Object, [7 => 65, 8 => 66], ID, Status);
   Check (Status = Allocated and Byte_Data (Store, ID) = [65,66] and Byte_Data (Store, ID)'First = 1);
   New_Bytes (Store, Buffer_Object, [Positive'Last => 77], ID, Status);
   Check (Status = Allocated and Byte_Data (Store, ID) = [1 => 77]);
   Check (Byte_Data (Store, 2) = [65,66]);
   New_Package (Store, 3, Package_ID, Status);
   Check (Status = Allocated and Length (Store, Package_ID) = 3);
   for I in 0 .. 2 loop Check (Element (Store, Package_ID, I) = No_Object); end loop;
   Set_Element (Store, Package_ID, 0, Number);
   Set_Element (Store, Package_ID, 1, Package_ID); -- cycles are representable
   Set_Element (Store, Package_ID, 2, ID);
   Check (Element (Store, Package_ID, 0) = Number and Element (Store, Package_ID, 1) = Package_ID
          and Element (Store, Package_ID, 2) = ID and Valid (Store));
   for I in Live_Count (Store) + 1 .. Max_Objects loop
      New_Integer (Store, AML_Decode.Integer_Value (I), ID, Status);
      Check (Status = Allocated and ID = I);
   end loop;
   Check (Integer_Data (Store, Number) = 123 and Element (Store, Package_ID, 1) = Package_ID);
   -- Mutation must still work in a full arena, preserve aliases/cycles and
   -- never allocate a replacement object for an existing integer.
   declare
      type Values is array (Positive range <>) of AML_Decode.Integer_Value;
      Prior_Usage : constant Usage := Usage_Of (Store);
   begin
      for Value of Values'(0, 1, 16#FFFF_FFFF#, 16#1_0000_0000#,
                            AML_Decode.Integer_Value'Last) loop
         Before := Store;
         Set_Integer (Store, Number, Value);
         Check (Valid (Store) and Usage_Of (Store) = Prior_Usage
                and Integer_Data (Store, Number) = Value);
         Check (Element (Store, Package_ID, 0) = Number
                and Integer_Data (Store, Element (Store, Package_ID, 0)) = Value
                and Element (Store, Package_ID, 1) = Package_ID
                and Byte_Data (Store, 2) = [65, 66]
                and Byte_Data (Store, 3) = [1 => 77]);
         for J in 5 .. Slot_Bound (Store) loop
            Check (Integer_Data (Store, J) = Integer_Data (Before, J));
         end loop;
         Set_Integer (Store, Number, Value);
         Check (Usage_Of (Store) = Prior_Usage);
      end loop;
      Set_Integer (Store, Max_Objects, 17);
      Check (Integer_Data (Store, Max_Objects) = 17
             and Integer_Data (Store, Number) = AML_Decode.Integer_Value'Last);
   end;
   Before := Store;
   New_Integer (Store, 0, ID, Status);
   Check (Status = Object_Limit and ID = 0 and Store = Before);
   New_Bytes (Store, Buffer_Object, [1 => 1], ID, Status);
   Check (Status = Object_Limit and ID = 0 and Store = Before);
   New_Package (Store, 1, ID, Status);
   Check (Status = Object_Limit and ID = 0 and Store = Before);
   Store := Empty;
   New_Bytes (Store, Buffer_Object, [1 .. Max_Bytes => 42], ID, Status);
   Check (Status = Allocated and Byte_Count (Store) = Max_Bytes
          and Byte_Data (Store, ID) = [1 .. Max_Bytes => 42]);
   Before := Store;
   New_Bytes (Store, String_Object, [1 => 65], ID, Status);
   Check (Status = Byte_Limit and ID = 0 and Store = Before);
   New_Bytes (Store, Buffer_Object, [1 .. 0 => 0], ID, Status);
   Check (Status = Allocated and Byte_Data (Store, ID)'Length = 0);
   Store := Empty;
   New_Package (Store, Max_Elements, Package_ID, Status);
   Check (Status = Allocated and Element_Count (Store) = Max_Elements);
   Set_Element (Store, Package_ID, Max_Elements - 1, Package_ID);
   Before := Store;
   New_Package (Store, 1, ID, Status);
   Check (Status = Element_Limit and ID = 0 and Store = Before);
   New_Package (Store, 0, ID, Status);
   Check (Status = Allocated and Length (Store, ID) = 0);
   declare
      Tree : NS.State := NS.Empty;
      Saved : NS.State;
      Loaded : NS.Load_Status;
      Data : AML_Decode.Bytes := [8,66,48,48,48,16#11#,4,16#0B#,0,4];
   begin
      for I in 0 .. 63 loop
         Data (4) := AML_Decode.Byte (48 + I / 10);
         Data (5) := AML_Decode.Byte (48 + I mod 10);
         NS.Load_Names (Tree, Data, AML_Decode.Bits_64, Loaded);
         Check (Loaded = NS.Loaded);
      end loop;
      Check (NS.Value_Usage (Tree).Objects = 64 and NS.Value_Usage (Tree).Bytes = Max_Bytes);
      Saved := Tree;
      Data (4 .. 5) := [54,52];
      NS.Load_Names (Tree, Data, AML_Decode.Bits_64, Loaded);
      Check (Loaded = NS.Value_Limit and Tree = Saved and NS.Count (Tree) = 64);
      Check (NS.Buffer_Data (Tree, 1) = [1 .. 1024 => 0]
             and NS.Buffer_Data (Tree, 64) = [1 .. 1024 => 0]);
   end;
   declare
      Tree : NS.State := NS.Empty;
      Loaded : NS.Load_Status;
      Result : AML_Execute.Execution_Result;
      type Values is array (Positive range <>) of AML_Decode.Integer_Value;
   begin
      -- Name(TEST, One); Method(GET0, 0) { Return(TEST) }
      NS.Load_Names (Tree,
        [8, 84, 69, 83, 84, 1, 16#14#, 11, 71, 69, 84, 48, 0,
         16#A4#, 84, 69, 83, 84], AML_Decode.Bits_64, Loaded);
      Check (Loaded = NS.Loaded);
      for Value of Values'(0, 1, 16#FFFF_FFFF#, 16#1_0000_0000#,
                            AML_Decode.Integer_Value'Last) loop
         NS.Set_Integer (Tree, 1, Value);
         Check (NS.Integer_Data (Tree, 1) = Value and NS.Count (Tree) = 2
                and NS.Data_Object (Tree, 1) = 1
                and NS.Value_Usage (Tree).Objects = 1);
         Result := NS.Invoke (Tree, 2, [others => 0], 0, 20);
         Check (Result.Status = AML_Execute.Returned and then Result.Value = Value);
      end loop;
   end;
   Ada.Text_IO.Put_Line ("AML-OBJECT-CHECK: PASS" & Checks'Image);
end Object_Tests;
