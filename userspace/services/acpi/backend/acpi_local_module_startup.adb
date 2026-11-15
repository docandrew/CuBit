pragma Ada_2022;
package body ACPI_Local_Module_Startup is
   use type Core.Import_Phase, Core.Values.Access_Status;
   use type Core.Execution_Phase_Result, Core.Values.Module_Outcome;
   function Observe return Report is (Info);
   procedure Fail (Why : Reason) is
   begin
      Info.Stage := Failed; Info.Status := Why;
      Info.Close_Attempted := True;
      begin
         Close_Local;
         Info.Close_Completed := True;
      exception
         when others => null;
         -- The failed phase and incomplete close remain sticky. Do not retry
         -- a callback whose effects are unknown, or release retained backing.
      end;
   end Fail;
   procedure Execute_Module_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status) is
      Good : Boolean := False;
   begin
      Result := (others => <>); Status := Core.Values.Wrong_Phase;
      if Info.Stage /= Fresh then return; end if;
      Info.Stage := Starting;
      if not Core.Valid (Service) then Fail (Invalid_Core); return; end if;
      if Core.Current_Import_Phase (Service) /= Core.Catalog_Frozen then
         Fail (Catalog_Not_Frozen); return;
      end if;
      if Budget = 0 then Fail (No_Work_Budget); return; end if;
      for Index in 1 .. Core.Table_Count (Service) loop
         if Core.Table_Info (Service,Index).Signature = "FACP" then
            if Info.FADT_Index /= 0 then Fail (Duplicate_FADT); return; end if;
            Info.FADT_Index := Index;
         end if;
      end loop;
      if Info.FADT_Index = 0 then Fail (Missing_FADT); return; end if;
      declare
         Fixed : constant ACPI_FADT.Result := Core.Fixed_Description (Service,Info.FADT_Index);
      begin
         if not Fixed.Valid then Fail (Malformed_FADT); return; end if;
         Info.Local_Attempted := True;
         begin
            Initialize_Local (Fixed.Value,Good);
         exception
            when others =>
               Info.Initialization_Raised := True;
               Fail (Local_Initialization_Refused); return;
         end;
      end;
      if not Good then Fail (Local_Initialization_Refused); return; end if;
      Info.Local_Ready := True; Info.Module_Attempted := True;
      begin
         Execute_Fields (Service,Budget,Result,Status);
      exception
         when others =>
            Info.Execution_Raised := True;
            Result := (others => <>); Status := Core.Values.Wrong_Phase;
            Fail (Module_Execution_Refused); return;
      end;
      if not Core.Valid (Service) or else Core.Current_Import_Phase (Service) /= Core.Catalog_Executed
        or else Status /= Core.Values.Available or else Result.Phase /= Core.Module_Execution_Reported
        or else Result.Modules.Outcome /= Core.Values.Modules_Completed then
         Status := Core.Values.Wrong_Phase;
         Fail (Module_Execution_Refused); return;
      end if;
      Info.Stage := Completed; Info.Status := Modules_Completed;
   end Execute_Module_Startup;
end ACPI_Local_Module_Startup;
