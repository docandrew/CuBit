pragma Ada_2022;
package body ACPI_Retained_Requests with SPARK_Mode is
   use type Paths.Selection_Status, Values.Argument_Kind;
   use type AML_Execute.Execution_Status;
   procedure Append_With_Store (Service : in out Core.State;
      Requested : Firmware_Tables.Identifiers.Selection;
      Caller : Values.Node_Descriptor; Root_Path, Parameter_Path : String;
      Item : Values.Argument; Budget : Natural; Result : out Report;
      Status : out Values.Access_Status)
   is
      Selection : Paths.Selection;
      Live_Root : Values.Node_Descriptor;
      Source_Description : Values.Value_Description;
      Store_Charge : Natural := 0;
   begin
      Result := (others => <>); Status := Values.Wrong_Phase;
      if Core.Current_Import_Phase (Service) /= Core.Catalog_Executed then return; end if;
      Core.Describe_Node (Service, Core.Namespace.Root, Live_Root, Status);
      if Status /= Values.Available then return; end if;
      if not Core.Current_Node (Service, Live_Root) then Status := Values.Invalid_Value; return; end if;
      Result.Index := Core.Find_Table (Service, Requested);
      if Result.Index = 0 then Result.Outcome := Table_Not_Found; return; end if;
      Paths.Select_Paths (Service, Caller, Root_Path, Parameter_Path, Selection, Status);
      Result.Paths_Checked := True; Result.Path_Status := Selection.Status;
      if Status /= Values.Available or else Selection.Status /= Paths.Selected then
         Result.Outcome := Paths_Refused;
         case Selection.Status is
            when Paths.Root_Syntax_Error | Paths.Parameter_Syntax_Error => Result.Syntax := Selection.Syntax;
            when Paths.Root_Lookup_Error | Paths.Parameter_Lookup_Error => Result.Lookup := Selection.Lookup;
            when others => null;
         end case;
         return;
      end if;
      if Selection.Has_Parameter and then Item.Kind = Values.Retained_Argument then
         Core.Describe_Result (Service, Item.Handle, Source_Description, Status);
         if Status /= Values.Available then Result.Outcome := Source_Refused; return; end if;
      end if;
      Core.Append_Retained_Module (Service, Positive (Result.Index), Budget,
        Result.Load, Status, (Values.Selected_Root, Selection.Root));
      Result.Charged := Result.Load.Append.Charged;
      case Result.Load.Outcome is
         when Core.Retained_Load_Not_Started => return;
         when Core.Retained_Load_Already_Present => Result.Outcome := Already_Present; return;
         when Core.Retained_Load_Failed => Result.Outcome := Module_Failed; return;
         when Core.Retained_Load_Completed => null;
      end case;
      if not Selection.Has_Parameter then Result.Outcome := Completed; return; end if;
      Result.Store_Attempted := True;
      Core.Store_Parameter_Data (Service, Selection.Parameter, Item,
        Budget - Result.Charged, Store_Charge, Result.Store_Outcome, Status);
      Result.Store_Access := Status; Result.Charged := Result.Charged + Store_Charge;
      if Status = Values.Available and then Result.Store_Outcome = AML_Execute.Returned then
         Result.Outcome := Completed; return;
      end if;
      Core.Require_Load_Recovery (Service, Positive (Result.Index), Result.Recovery_Access);
      Result.Outcome := (if Result.Recovery_Access = Values.Available then Parameter_Store_Failed else Recovery_Failed);
   end Append_With_Store;
end ACPI_Retained_Requests;
