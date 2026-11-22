pragma Ada_2022;
package body ACPI_Field_Module_Startup is
   procedure Execute_Frozen
     (Service : in out Core.State; IO : in out Provider.Context;
      Budget : Natural; Report : out Core.Execution_Report;
      Status : out Core.Values.Access_Status)
   is
      Targets : Fields.Module_Target_Set := [others => AML_Field_Access.No_Target];
      Count : constant Natural := Provider.Register_Count (IO);
      Accepted : Boolean;
   begin
      Report := (others => <>); Status := Core.Values.Wrong_Phase;
      if not Provider.Ready (IO) then return; end if;
      if Provider.Pending (IO) then Status := Core.Values.Busy; return; end if;
      if Count > Maximum_Bindings then Status := Core.Values.Out_Of_Bounds; return; end if;
      for Index in 1 .. Count loop
         Provider.Export_Target (IO, Index, Targets (Index), Accepted);
         if not Accepted then Status := Core.Values.Invalid_Value; return; end if;
      end loop;
      Fields.Execute_Frozen
        (Service, IO, Targets, Fields.Module_Target_Count (Count), Budget, Report, Status);
   end Execute_Frozen;
end ACPI_Field_Module_Startup;
