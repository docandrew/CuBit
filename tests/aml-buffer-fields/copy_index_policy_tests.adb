with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Identity;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_References;
procedure Copy_Index_Policy_Tests is
   use type AML_Decode.Bytes;
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   package O renames N.Owned;
   use type N.State;
   use type AML_Objects.Allocation_Status;
   use type N.Load_Status;
   use type N.Object_Kind;
   use type AML_References.Node_Incarnation;
   A, Foreign : O.Arena;
   Ref, Bad, Saved : O.Reference;
   Source : AML_References.Object_Handle;
   Copied, Resolved : Datum;
   Status : Execution_Status;
   Loaded : N.Load_Status;
   OK : Boolean;
   Checks : Natural := 0;
   Destination : constant Bytes := [68,83,84,48];
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Reject (R : O.Reference; Width : Integer_Width) is
      Before : constant N.State := O.Snapshot (A);
   begin
      O.Copy_Value (A, Width, (Reference_Datum, R), Copied, Status);
      Check (Status = Unsupported_Value and then O.Snapshot (A) = Before);
      O.Copy_And_Attach (A, (Named_Destination, 0, AML_Names.Read_Name (Destination)),
        Width, (Reference_Datum, R), Copied, Status);
      Check (Status = Unsupported_Value and then O.Snapshot (A) = Before);
   end Reject;
begin
   for Width in Integer_Width loop
      for Shape in 0 .. 2 loop
         O.Reset (A, OK); Check (OK);
         O.Reset (Foreign, OK); Check (OK);
         declare
            Value_Code : constant Bytes :=
              (if Shape = 0 then Bytes'(16#11#,3,1,65)
               elsif Shape = 1 then Bytes'(16#12#,4,1,16#0A#,65)
               else Bytes'(16#12#,2,1));
            Code : constant Bytes := Bytes'(16#08#,83,82,67,48) & Value_Code
              & Bytes'(1 => 16#08#) & Destination & Bytes'(16#0A#,42);
         begin
            O.Load (A, Code, Width, Loaded); Check (Loaded = N.Loaded);
            O.Load (Foreign, Code, Width, Loaded); Check (Loaded = N.Loaded);
         end;
         O.Make_Source (A, N.Data_Object (O.Snapshot (A), 1), Source, OK); Check (OK);
         O.Make_Index (A, Source, 0, Ref, Status); Check (Status = Returned);
         Saved := Ref;
         declare Before : constant N.State := O.Snapshot (A); begin
            O.Copy_Value (A, Width, (Reference_Datum, Ref), Copied, Status);
            Check (Status = Returned and then Copied = (Reference_Datum, Ref));
            Check (O.Snapshot (A) = Before);
            O.Clone_Value (A, Width, (Reference_Datum, Ref), Copied, Status);
            if Shape = 2 then
               Check (Status = Uninitialized);
            elsif Shape < 2 then
               Check (Status = Returned and then Copied = (Integer_Datum, 65, Ordinary_Integer));
            end if;
            Check (O.Snapshot (A) = Before);
            O.Copy_And_Attach (A, (Named_Destination, 0, AML_Names.Read_Name (Destination)),
              Width, (Reference_Datum, Ref), Copied, Status);
            Check (Status = Returned and then Copied = (Reference_Datum, Ref));
            Check (N.Kind (O.Snapshot (A), 2) = N.Reference_Object);
            Check (N.Incarnation_Of (O.Snapshot (A), 2) = N.Incarnation_Of (Before, 2));
         end;
         O.Make_Named_Reference (A, 2, Bad, OK); Check (OK);
         O.Resolve_Value (A, Bad, Resolved, Status);
         Check (Status = Returned and then Resolved = (Reference_Datum, Ref));
         O.Store_Reference_Value (A, Ref, Width, (Integer_Datum, 66, Ordinary_Integer), Status);
         Check (Status = Returned);
         O.Resolve_Value (A, Ref, Resolved, Status);
         Check (Status = Returned and then Resolved = (Integer_Datum, 66, Ordinary_Integer));
         if Shape = 0 then
            Bad := AML_References.Bind (AML_Identity.No_Identity, AML_References.Byte_Item (Ref));
         else Bad := AML_References.Bind (AML_Identity.No_Identity, AML_References.Package_Item (Ref)); end if;
         Reject (Bad, Width);
         O.Make_Source (Foreign, N.Data_Object (O.Snapshot (Foreign), 1), Source, OK); Check (OK);
         O.Make_Index (Foreign, Source, 0, Bad, Status); Check (Status = Returned);
         Reject (Bad, Width);
         -- An exhausted wrapper allocation must leave the existing binding intact.
         declare
            ID : AML_Objects.Object_ID;
            Allocated : AML_Objects.Allocation_Status;
         begin
            while O.Values_Used (A).Objects < AML_Objects.Max_Objects loop
               O.Append (A, Bytes'(1 .. 0 => 0), ID, Allocated);
               Check (Allocated = AML_Objects.Allocated);
            end loop;
            declare Before : constant N.State := O.Snapshot (A); begin
               O.Copy_And_Attach (A, (Named_Destination, 0, AML_Names.Read_Name (Destination)),
                 Width, (Reference_Datum, Ref), Copied, Status);
               Check (Status = Value_Limit and then O.Snapshot (A) = Before);
               Check (Copied = (Integer_Datum, 0, Ordinary_Integer));
            end;
         end;
         O.Reset (A, OK); Check (OK);
         O.Load (A, Bytes'(1 => 16#08#) & Destination & Bytes'(16#0A#,42), Width, Loaded);
         Check (Loaded = N.Loaded);
         Reject (Saved, Width);
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Copy index policy checks" & Checks'Image);
end Copy_Index_Policy_Tests;
