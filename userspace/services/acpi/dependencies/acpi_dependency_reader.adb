pragma Ada_2022;
package body ACPI_Dependency_Reader with SPARK_Mode is
   package V renames Core.Values;
   use type V.Access_Status, V.Value_Handle, V.Value_Kind;
   procedure Copy
     (Service : in out Core.State; Handle : V.Value_Handle;
      Maximum_Elements : Element_Count; Items : out Device_Array; Result : out Report) is
      Before : constant Natural := Core.Retained_Results (Service);
      Element : V.Value_Handle := V.No_Value;
      Shape : V.Value_Description;
      procedure Release is
         Status : V.Access_Status;
      begin
         if Element = V.No_Value then return; end if;
         Core.Release_Result (Service, Element, Status);
         if Status /= V.Available or else Element /= V.No_Value then
            Result.Status := Cleanup_Failed; Result.Cleanup_Result := Status;
         end if;
      end Release;
      procedure Read is
      begin
         Core.Describe_Result (Service, Handle, Shape, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Invalid_Result; return; end if;
         if Shape.Kind /= V.Package_Description then Result.Status := Not_A_Package; return; end if;
         Result.Required := Element_Count (Shape.Length); Result.Size_Known := True;
         if Result.Required > Maximum_Elements then Result.Status := Work_Limit; return; end if;
         if Shape.Length > Items'Length then Result.Status := Capacity_Required; return; end if;
         for Ordinal in 1 .. Shape.Length loop
            Core.Read_Result_Element (Service, Handle, Ordinal - 1, Element, Result.Access_Result);
            if Result.Access_Result /= V.Available then Result.Status := Read_Failed; return; end if;
            Core.Read_Result_Device (Service, Element, Items (Items'First + (Ordinal - 1)), Result.Access_Result);
            if Result.Access_Result /= V.Available then Result.Status := Unsupported_Element; return; end if;
            Release; if Result.Status = Cleanup_Failed then return; end if;
         end loop;
         Result.Copied := Result.Required; Result.Status := Copied_List;
      end Read;
   begin
      Items := [others => V.No_Node]; Result := (others => <>); Read; Release;
      if Result.Status /= Cleanup_Failed and then Core.Retained_Results (Service) /= Before then Result.Status := Cleanup_Failed; end if;
      if Result.Status /= Copied_List then Items := [others => V.No_Node]; Result.Copied := 0; end if;
   end Copy;
end ACPI_Dependency_Reader;
