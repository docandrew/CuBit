with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Objects;
with AML_Frame_Handles;
with AML_Table_Backing;
with Test_Namespace;
procedure Tointeger_Quota_Tests is
   package NS renames Test_Namespace;
   package Owner renames NS.Owned;
   use type NS.Load_Status;
   use type Integer_Value;
   use type AML_Objects.Usage;
   use type AML_Objects.State;
   use type AML_Objects.Allocation_Status;
   use type AML_Frame_Handles.Invocation_Serial;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   function Method (Name : String; Args : Byte; Body_Code : Bytes) return Bytes is
     (Bytes'(16#14#, Byte (6 + Body_Code'Length),
       Character'Pos (Name (Name'First + 0)), Character'Pos (Name (Name'First + 1)),
       Character'Pos (Name (Name'First + 2)), Character'Pos (Name (Name'First + 3)), Args) & Body_Code);
   Source_Name : constant Bytes := [83,82,67,48];
   Target_Name : constant Bytes := [68,83,84,48];
   Package_Name : constant Bytes := [80,75,71,48];
   Data : constant Bytes :=
     Bytes'(1 => 16#08#) & Source_Name & Bytes'(16#0D#,49,48,0) &
     Bytes'(1 => 16#08#) & Target_Name & Bytes'(16#0A#,42) &
     Bytes'(1 => 16#08#) & Package_Name & Bytes'(16#12#,3,1,0);
   type Target_Kind is (Named, Argument_Reference, Package_Element, Local_Cell);
   type Quota_Kind is (Object_Quota, Byte_Quota);
begin
   for Width in Integer_Width loop
      for Target in Target_Kind loop
         for Quota in Quota_Kind loop
            declare
               A : Owner.Arena;
               Input : aliased AML_Table_Backing.State (1, 1);
               OK : Boolean;
               Loaded : NS.Load_Status;
               ID : AML_Objects.Object_ID;
               Allocated : AML_Objects.Allocation_Status;
               Result : Execution_Result;
               Target_Code : constant Bytes :=
                 (case Target is
                    when Named => Target_Name,
                    when Argument_Reference => Bytes'(1 => 16#68#),
                    when Package_Element => Bytes'(1 => 16#88#) & Package_Name & Bytes'(0,0),
                    when Local_Cell => Bytes'(1 => 16#60#));
               Conversion : constant Bytes := Bytes'(16#A4#,16#99#) & Source_Name & Target_Code;
               Definitions : constant Bytes :=
                 (if Target = Argument_Reference then
                    Method ("AUX0", 1, Conversion) &
                    Method ("TEST", 0, Bytes'(16#A4#,65,85,88,48,16#71#) & Target_Name)
                  else Method ("TEST", 0, Conversion));
            begin
               Owner.Reset (A, OK); Check (OK);
               Owner.Load (A, Data & Definitions, Width, Loaded); Check (Loaded = NS.Loaded);
               if Quota = Object_Quota then
                  while Owner.Values_Used (A).Objects < AML_Objects.Max_Objects loop
                     Owner.Append (A, Bytes'(1 .. 0 => 0), ID, Allocated);
                     Check (Allocated = AML_Objects.Allocated);
                  end loop;
               elsif Quota = Byte_Quota then
                  declare Padding : constant Bytes
                    (1 .. AML_Objects.Max_Bytes - Owner.Values_Used (A).Bytes) := [others => 0]; begin
                     Owner.Append (A, Padding, ID, Allocated); Check (Allocated = AML_Objects.Allocated);
                  end;
               end if;
               declare
                  Before : constant NS.State := Owner.Snapshot (A);
                  Used : constant AML_Objects.Usage := Owner.Values_Used (A);
                  Serial : constant AML_Frame_Handles.Invocation_Serial := Owner.Invocation_Count (A);
                  Node : constant NS.Node_ID := NS.Child (Before, NS.Root, "TEST");
               begin
                  Owner.Invoke (A, Input, Node, [others => <>], 0, 100, Result);
                  Check (Owner.Invocation_Count (A) = Serial + 1);
                  Check (Result.Charged <= 100);
                  if Quota = Object_Quota and Target in Argument_Reference | Package_Element then
                     Check (Result.Status = Value_Limit);
                     Check (Owner.Values_Used (A) = Used);
                     Check (NS.Value_Store (Owner.Snapshot (A)) = NS.Value_Store (Before));
                     Check (NS.Cleanup_Frame (Owner.Snapshot (A), Before));
                  else
                     Check (Result.Status = Returned and then Result.Value = 10);
                     Check (Owner.Values_Used (A).Bytes = Used.Bytes);
                     Check (Owner.Values_Used (A).Elements = Used.Elements);
                     if Target = Named then
                        Check (Owner.Values_Used (A) = Used);
                        Check (NS.Data_Object (Owner.Snapshot (A), NS.Child (Before, NS.Root, "DST0")) =
                          NS.Data_Object (Before, NS.Child (Before, NS.Root, "DST0")));
                     end if;
                     if Target = Local_Cell then
                        Check (Owner.Values_Used (A) = Used);
                        Check (NS.Cleanup_Frame (Owner.Snapshot (A), Before));
                     end if;
                  end if;
               end;
            end;
         end loop;
      end loop;
   end loop;
   -- Conversion target effects survive allocation failure. A control invocation
   -- performs only the target's Increment, allowing exact state comparison.
   for Width in Integer_Width loop
      declare
         A, Control : Owner.Arena;
         Input : aliased AML_Table_Backing.State (1, 1);
         OK : Boolean;
         Loaded : NS.Load_Status;
         ID : AML_Objects.Object_ID;
         Allocated : AML_Objects.Allocation_Status;
         R, Control_Result : Execution_Result;
         Mark : constant Bytes := [77,65,82,75];
         Effect : constant Bytes := Bytes'(1 => Increment_Op) & Mark;
         Fixture : constant Bytes :=
           Bytes'(1 => 16#08#) & Source_Name & Bytes'(16#0D#,49,48,0) &
           Bytes'(1 => 16#08#) & Package_Name & Bytes'(16#12#,4,2,0,0) &
           Bytes'(1 => 16#08#) & Mark & Bytes'(16#0A#,0) &
           Method ("TEST", 0, Bytes'(16#A4#,16#99#) & Source_Name &
             Bytes'(1 => 16#88#) & Package_Name & Effect & Bytes'(1 => 0)) &
           Method ("CHNG", 0, Bytes'(1 => 16#A4#) & Effect);
      begin
         Owner.Reset (A, OK); Check (OK);
         Owner.Reset (Control, OK); Check (OK);
         Owner.Load (A, Fixture, Width, Loaded); Check (Loaded = NS.Loaded);
         Owner.Load (Control, Fixture, Width, Loaded); Check (Loaded = NS.Loaded);
         while Owner.Values_Used (A).Objects < AML_Objects.Max_Objects loop
            Owner.Append (A, Bytes'(1 .. 0 => 0), ID, Allocated);
            Check (Allocated = AML_Objects.Allocated);
            Owner.Append (Control, Bytes'(1 .. 0 => 0), ID, Allocated);
            Check (Allocated = AML_Objects.Allocated);
         end loop;
         declare
            Before : constant NS.State := Owner.Snapshot (A);
            Used : constant AML_Objects.Usage := Owner.Values_Used (A);
         begin
            Owner.Invoke (Control, Input, NS.Child (Before, NS.Root, "CHNG"),
              [others => <>], 0, 100, Control_Result);
            Check (Control_Result.Status = Returned and then Control_Result.Value = 1);
            Owner.Invoke (A, Input, NS.Child (Before, NS.Root, "TEST"),
              [others => <>], 0, 100, R);
            Check (R.Status = Value_Limit);
            Check (Owner.Values_Used (A) = Used);
            Check (NS.Value_Store (Owner.Snapshot (A)) = NS.Value_Store (Owner.Snapshot (Control)));
            Check (NS.Cleanup_Frame (Owner.Snapshot (A), Owner.Snapshot (Control)));
            Check (NS.Value_Store (Owner.Snapshot (A)) /= NS.Value_Store (Before));
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("ToInteger quota checks" & Checks'Image);
end Tointeger_Quota_Tests;
