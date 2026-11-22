pragma Ada_2022;
with AML_Execute;
with AML_Names;
package body ACPI_Passive_Members with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type V.Value_Kind;
   use type N.Object_Kind;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   Names : constant array (Member_Name) of AML_Names.Segment :=
     ["_HID", "_CID", "_UID", "_ADR", "_CRS", "_STA"];

   procedure Read
     (Service : in out Core.State; Device : Core.Node_Descriptor; Member : Member_Name;
      Data : out Firmware_Tables.Bytes; Items : out Atoms; Result : out Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Root, Child : V.Value_Handle := V.No_Value;
      Found : N.Lookup_Result;
      Description : Core.Node_Descriptor;
      Value : V.Result;
      Shape : V.Value_Description;
      Copied : Natural := 0;
      Good : Boolean := True;

      procedure Release (Handle : in out V.Value_Handle) is
         Status : V.Access_Status;
      begin
         if Handle = V.No_Value then return; end if;
         Core.Release_Result (Service, Handle, Status);
         if Status /= V.Available or else Handle /= V.No_Value then
            Result.Cleanup_Result := Status; Result.Status := Cleanup_Failed; Good := False;
         end if;
      end Release;

      procedure Atom_Value (Handle : V.Value_Handle; Slot : Positive; Copying : Boolean) is
         Info : V.Value_Description;
         Count : Natural;
      begin
         Core.Describe_Result (Service, Handle, Info, Result.Access_Result);
         if Result.Access_Result /= V.Available then
            Result.Status := Read_Failed; Good := False; return;
         end if;
         case Info.Kind is
            when V.Integer_Description =>
               if Copying then
                  Items (Items'First + (Slot - 1)) :=
                    (Kind => Integer_Atom, Number => Info.Number, others => <>);
               end if;
            when V.String_Description =>
               if Member in Address | Device_Status then Result.Status := Unsupported; Good := False; return; end if;
               if not Copying then
                  if Info.Length > Natural'Last - Result.Required_Bytes then
                     Result.Status := Size_Overflow; Good := False; return;
                  end if;
                  Result.Required_Bytes := Result.Required_Bytes + Info.Length;
               else
                  -- No callback, mutation or safe point intervenes between the
                  -- sizing and copy passes; the retained parent owns each leaf.
                  if Copied > Data'Length or else Info.Length > Data'Length - Copied then
                     Result.Status := Read_Failed; Good := False; return;
                  end if;
                  if Info.Length > 0 then
                     Core.Read_Result_Bytes (Service, Handle, 0,
                       Data (Data'First + Copied .. Data'First + Copied + (Info.Length - 1)),
                       Count, Result.Access_Result);
                     if Result.Access_Result /= V.Available or else Count /= Info.Length then
                        Result.Status := Read_Failed; Good := False; return;
                     end if;
                  end if;
                  Items (Items'First + (Slot - 1)) :=
                    (Text_Atom, 0, Copied, Info.Length);
                  Copied := Copied + Info.Length;
               end if;
            when others => Result.Status := Unsupported; Good := False;
         end case;
      end Atom_Value;

      procedure Package_Pass (Copying : Boolean) is
      begin
         for Slot in 1 .. Result.Required_Atoms loop
            Core.Read_Result_Element (Service, Root, Slot - 1, Child, Result.Access_Result);
            if Result.Access_Result /= V.Available then
               Result.Status := Read_Failed; Good := False; return;
            end if;
            Atom_Value (Child, Slot, Copying);
            Release (Child);
            if not Good then return; end if;
         end loop;
      end Package_Pass;

      procedure Run is
      begin
         if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then
            Result.Status := Invalid_Device; return;
         end if;
         Core.Find_Object (Service, V.Position (Device), Names (Member), Found, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Unavailable; return; end if;
         if Found.Status = N.Not_Found then Result.Status := Missing; return;
         elsif Found.Status /= N.Found then Result.Status := Unavailable; return; end if;
         Core.Describe_Node (Service, Found.Node, Description, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Unavailable; return; end if;
         if V.Node_Kind (Description) = N.Method_Object then
            Result.Status := Method_Valued; return;
         elsif V.Node_Kind (Description) not in N.Integer_Object | N.String_Object |
           N.Buffer_Object | N.Package_Object
         then Result.Status := Unsupported; return; end if;
         Core.Observe_Named_Value (Service, Found.Node, Value, Result.Access_Result);
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
           AML_Execute.Metadata_Returned then Root := Value.Handle; end if;
         if Result.Access_Result /= V.Available then Result.Status := Unavailable; return; end if;
         if Value.Status = AML_Execute.Returned then
            if Member = Current_Resources then Result.Status := Unsupported; return; end if;
            Result.Required_Atoms := 1; Result.Size_Known := True;
            if Items'Length = 0 then Result.Status := Capacity_Required; return; end if;
            Items (Items'First) := (Kind => Integer_Atom, Number => Value.Number, others => <>);
         elsif Value.Status = AML_Execute.Object_Returned then
            Core.Describe_Result (Service, Root, Shape, Result.Access_Result);
            if Result.Access_Result /= V.Available then Result.Status := Read_Failed; return; end if;
            if Member = Current_Resources then
               if Shape.Kind /= V.Buffer_Description then Result.Status := Unsupported; return; end if;
               Result.Required_Bytes := Shape.Length; Result.Size_Known := True;
               if Shape.Length > Data'Length then Result.Status := Capacity_Required; return; end if;
               if Shape.Length > 0 then
                  Core.Read_Result_Bytes (Service, Root, 0,
                    Data (Data'First .. Data'First + (Shape.Length - 1)), Copied, Result.Access_Result);
                  if Result.Access_Result /= V.Available or else Copied /= Shape.Length then
                     Result.Status := Read_Failed; return;
                  end if;
               end if;
            else
               if Shape.Kind /= V.String_Description and then
                 not (Member = Compatible_ID and then Shape.Kind = V.Package_Description)
               then Result.Status := Unsupported; return; end if;
               if Member in Address | Device_Status then Result.Status := Unsupported; return; end if;
               Result.Required_Atoms := (if Shape.Kind = V.Package_Description then Shape.Length else 1);
               if Shape.Kind = V.Package_Description and then Member /= Compatible_ID then
                  Result.Status := Unsupported; return;
               end if;
               if Result.Required_Atoms > Items'Length then Result.Status := Capacity_Required; return; end if;
               if Shape.Kind = V.Package_Description then Package_Pass (False);
               else Atom_Value (Root, 1, False); end if;
               if not Good then return; end if;
               Result.Size_Known := True;
               if Result.Required_Bytes > Data'Length then Result.Status := Capacity_Required; return; end if;
               if Shape.Kind = V.Package_Description then Package_Pass (True);
               else Atom_Value (Root, 1, True); end if;
               if not Good then return; end if;
            end if;
         else Result.Status := Unsupported; return;
         end if;
         if Copied /= Result.Required_Bytes then Result.Status := Read_Failed; return; end if;
         Result.Copied_Atoms := Result.Required_Atoms;
         Result.Copied_Bytes := Copied;
         Result.Status := Static_Value;
      end Run;
   begin
      Data := [others => 0]; Items := [others => (others => <>)]; Result := (others => <>);
      Run;
      Release (Child); Release (Root);
      if Result.Status /= Cleanup_Failed and then Core.Retained_Results (Service) /= Before_Roots then
         Result.Status := Cleanup_Failed;
      end if;
      if Result.Status /= Static_Value then
         Data := [others => 0]; Items := [others => (others => <>)];
         Result.Copied_Atoms := 0; Result.Copied_Bytes := 0;
      end if;
   end Read;
end ACPI_Passive_Members;
