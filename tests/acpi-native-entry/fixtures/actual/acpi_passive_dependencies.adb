pragma Ada_2022;
with AML_Names;
with AML_Execute;
package body ACPI_Passive_Dependencies with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status, V.Value_Handle, V.Value_Kind;
   use type N.Object_Kind, N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   Dependency_Member : constant AML_Names.Segment := "_DEP";
   procedure Read
     (Service : in out Core.State; Device : Core.Node_Descriptor;
      Maximum_Elements : Element_Count; Items : out Device_Array; Result : out Report) is
      Before : constant Natural := Core.Retained_Results (Service);
      Root, Element : V.Value_Handle := V.No_Value;
      Found : N.Lookup_Result;
      Member : Core.Node_Descriptor;
      Value : V.Result;
      Shape : V.Value_Description;
      procedure Release (Handle : in out V.Value_Handle) is
         Status : V.Access_Status;
      begin
         if Handle = V.No_Value then return; end if;
         Core.Release_Result (Service, Handle, Status);
         if Status /= V.Available or else Handle /= V.No_Value then
            Result.Status := Cleanup_Failed; Result.Cleanup_Result := Status;
         end if;
      end Release;
      procedure Observe is
      begin
         if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then
            Result.Status := Invalid_Device; return;
         end if;
         Core.Find_Object (Service, V.Position (Device), Dependency_Member, Found, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Unavailable; return; end if;
         if Found.Status = N.Not_Found then Result.Status := Missing; return; end if;
         if Found.Status /= N.Found then Result.Status := Unavailable; return; end if;
         Core.Describe_Node (Service, Found.Node, Member, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Unavailable; return; end if;
         if V.Node_Kind (Member) = N.Method_Object then Result.Status := Method_Valued; return; end if;
         if V.Node_Kind (Member) /= N.Package_Object then Result.Status := Unsupported; return; end if;
         Core.Observe_Named_Value (Service, Found.Node, Value, Result.Access_Result);
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
            Root := Value.Handle;
         end if;
         if Result.Access_Result /= V.Available then Result.Status := Unavailable; return; end if;
         if Value.Status /= AML_Execute.Object_Returned then Result.Status := Unsupported; return; end if;
         Core.Describe_Result (Service, Root, Shape, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Read_Failed; return; end if;
         if Shape.Kind /= V.Package_Description then Result.Status := Unsupported; return; end if;
         Result.Required := Element_Count (Shape.Length); Result.Size_Known := True;
         if Result.Required > Maximum_Elements then Result.Status := Work_Limit; return; end if;
         if Shape.Length > Items'Length then Result.Status := Capacity_Required; return; end if;
         for Ordinal in 1 .. Shape.Length loop
            Core.Read_Result_Element (Service, Root, Ordinal - 1, Element, Result.Access_Result);
            if Result.Access_Result /= V.Available then Result.Status := Read_Failed; return; end if;
            Core.Read_Result_Device (Service, Element, Items (Items'First + (Ordinal - 1)), Result.Access_Result);
            if Result.Access_Result /= V.Available then Result.Status := Unsupported; return; end if;
            Release (Element);
            if Result.Status = Cleanup_Failed then return; end if;
         end loop;
         Result.Copied := Result.Required; Result.Status := Static_List;
      end Observe;
   begin
      Items := [others => V.No_Node]; Result := (others => <>);
      Observe; Release (Element); Release (Root);
      if Result.Status /= Cleanup_Failed and then Core.Retained_Results (Service) /= Before then
         Result.Status := Cleanup_Failed;
      end if;
      if Result.Status /= Static_List then Items := [others => V.No_Node]; Result.Copied := 0; end if;
   end Read;
end ACPI_Passive_Dependencies;
