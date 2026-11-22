pragma Ada_2022;
with AML_Execute;
package body ACPI_Field_Module_Startup is
   use type Interfaces.Unsigned_64, System.Address;
   Minimum_Heap_Alignment : constant Interfaces.Unsigned_64 := 16;
   type Startup_Operation is (Module_Code, Static_Members);
   procedure Run
     (Operation : Startup_Operation; Service : in out Core.State;
      IO : in out Provider.Context; Budget, Count : Natural;
      Modules : out Core.Execution_Report;
      Members : out Core.Namespace.Field_Initialization_Report;
      Status : out Core.Values.Access_Status) is
      procedure Invoke (Targets : in out Fields.Module_Target_Set) is
         Accepted : Boolean;
      begin
         Targets := [others => AML_Field_Access.No_Target];
         for Index in Targets'Range loop
            Provider.Export_Target (IO, Index, Targets (Index), Accepted);
            if not Accepted then Status := Core.Values.Invalid_Value; return; end if;
         end loop;
         case Operation is
            when Module_Code =>
               Fields.Execute_Frozen (Service, IO, Targets, Fields.Module_Target_Count (Count), Budget, Modules, Status);
            when Static_Members =>
               Fields.Initialize_Members (Service, IO, Targets, Fields.Module_Target_Count (Count), Budget, Members, Status);
         end case;
      end Invoke;
   begin
      Modules := (others => <>); Members := (others => <>);
      Status := Core.Values.Root_Limit;
      if Count = 0 then
         declare Empty : Fields.Module_Target_Set (1 .. 0); begin Invoke (Empty); end;
         return;
      end if;
      declare
         subtype Shape is Fields.Module_Target_Set (1 .. Count);
         Bits : constant Interfaces.Unsigned_64 := Shape'Object_Size;
         Bytes : constant Interfaces.Unsigned_64 := Bits / System.Storage_Unit +
           (if Bits mod System.Storage_Unit = 0 then 0 else 1);
         Address : constant System.Address := Reserve_Targets (Bytes, Interfaces.Unsigned_64'Max (Minimum_Heap_Alignment, Shape'Alignment));
      begin
         if Address = System.Null_Address then return; end if;
         begin
            declare
               -- This exact input-sized extent came only from the trusted
               -- private-heap allocator. The vector cannot escape this call.
               pragma Warnings (Off, "default initialization of * may modify overlaid storage");
               Targets : Shape with Address => Address;
               pragma Warnings (On, "default initialization of * may modify overlaid storage");
            begin
               Invoke (Targets);
            end;
         exception
            when others => Release_Targets (Address); raise;
         end;
         -- Requests/retained state copy individual Target values; disposal
         -- releases only temporary metadata, never a hardware reservation.
         Release_Targets (Address);
      end;
   end Run;
   procedure Execute_Frozen
     (Service : in out Core.State; IO : in out Provider.Context;
      Budget : Natural; Report : out Core.Execution_Report;
      Status : out Core.Values.Access_Status) is
      Members : Core.Namespace.Field_Initialization_Report;
   begin
      Report := (others => <>); Status := Core.Values.Wrong_Phase;
      if not Provider.Ready (IO) then return; end if;
      if Provider.Pending (IO) then Status := Core.Values.Busy; return; end if;
      Run (Module_Code, Service, IO, Budget, Provider.Register_Count (IO), Report, Members, Status);
   end Execute_Frozen;
   procedure Initialize_Members
     (Service : in out Core.State; IO : in out Provider.Context;
      Budget : Natural; Report : out Core.Namespace.Field_Initialization_Report;
      Status : out Core.Values.Access_Status) is
      Modules : Core.Execution_Report;
   begin
      Report := (others => <>); Status := Core.Values.Wrong_Phase;
      if Provider.Pending (IO) then
         Status := Core.Values.Busy;
         Report.Failure := AML_Execute.Field_Indeterminate;
         Report.Reconciliation_Required := True; return;
      end if;
      if not Provider.Ready (IO) then return; end if;
      Run (Static_Members, Service, IO, Budget, Provider.Register_Count (IO), Modules, Report, Status);
   end Initialize_Members;
end ACPI_Field_Module_Startup;
