pragma Ada_2022;
with AML_Execute;
package body ACPI_Field_Startup_Owner is
   use type Wire.Time_Instant, Interfaces.Unsigned_64;
   use type Plain.Startup_State;
   Lease_Microseconds : constant Interfaces.Unsigned_64 := 100_000;
   Lease_IPC_Milliseconds : constant Wire.Time_Instant := 100;
   Cleanup_IPC_Milliseconds : constant Wire.Time_Instant := 25;
   function Plain_Ready (IO : Plain.Context) return Boolean is
     (Plain.Status (IO) = Plain.Ready);
   use type Provider.Startup_State;
   use type Core.Values.Access_Status, Core.Execution_Phase_Result, Core.Values.Module_Outcome;
   Provider_Symbol : constant AML_Field_Access.Endpoint_ID := 1;
   function Provider_Ready (IO : Provider.Context) return Boolean is
     (Provider.Status (IO) = Provider.Ready);
   procedure Record_Capacity_Failure (Item : ACPI_Capacity_Diagnostics.Report) is
   begin
      if not ACPI_Capacity_Diagnostics.Present (Last_Capacity) then Last_Capacity := Item; end if;
   end Record_Capacity_Failure;
   function Observe return Report is
     ((Stage, Module_Attempted, Module_Completed, Provider.Pending (IO), Provider.Register_Count (IO), Last_Capacity));
   procedure Read_Window (Now, Deadline : out Wire.Time_Instant) is
      Available : Boolean;
   begin
      Now := 0; Deadline := Wire.No_Finite_Deadline;
      if Stage /= Ready then return; end if;
      Read_Clock (Now, Available);
      if not Available or else Now < Last_Sample or else Now >= Until_Time then
         Stage := Closed; return;
      end if;
      Last_Sample := Now; Deadline := Until_Time;
   end Read_Window;
   procedure Read_NVS_Deadline (Deadline : out Wire.Time_Instant) is
      Ignored : Wire.Time_Instant;
   begin Read_Window (Ignored, Deadline); end Read_NVS_Deadline;
   procedure Read_Lease_Window
     (Lease_Deadline : out Interfaces.Unsigned_64;
      IPC_Deadline : out Wire.Time_Instant; Available : out Boolean) is
      Microseconds : Interfaces.Unsigned_64 := 0;
      Now, Until_IPC : Wire.Time_Instant;
   begin
      Lease_Deadline := 0; IPC_Deadline := Wire.No_Finite_Deadline;
      Available := False;
      Read_Window (Now, Until_IPC);
      if Until_IPC = Wire.No_Finite_Deadline then return; end if;
      Read_Lease_Clock (Microseconds, Available);
      if not Available or else Microseconds >= Interfaces.Unsigned_64'Last - Lease_Microseconds
        or else Now >= Wire.No_Finite_Deadline - Lease_IPC_Milliseconds then
         Available := False; return;
      end if;
      Lease_Deadline := Microseconds + Lease_Microseconds;
      IPC_Deadline := Wire.Time_Instant'Min (Until_IPC, Now + Lease_IPC_Milliseconds);
   end Read_Lease_Window;
   procedure Read_Cleanup_Deadline (Deadline : out Wire.Time_Instant) is
      Now : Wire.Time_Instant := 0;
      Available : Boolean := False;
   begin
      -- Cleanup remains possible after startup/data expiry or Stage=Closed.
      Deadline := Wire.No_Finite_Deadline;
      Read_Clock (Now, Available);
      if Available and then Now < Wire.No_Finite_Deadline - Cleanup_IPC_Milliseconds then
         Deadline := Now + Cleanup_IPC_Milliseconds;
      end if;
   end Read_Cleanup_Deadline;
   function Usable return Boolean is
      Now, Deadline : Wire.Time_Instant;
   begin
      if Stage /= Ready or else Provider.Status (IO) /= Provider.Ready or else Provider.Pending (IO) then
         return False;
      end if;
      Read_Window (Now, Deadline);
      return Deadline /= Wire.No_Finite_Deadline;
   end Usable;
   procedure Begin_Startup
     (Destination : Endpoint; Deadline : Wire.Time_Instant; Accepted : out Boolean)
   is
      Now : Wire.Time_Instant;
      Available : Boolean;
   begin
      Accepted := False;
      if Stage /= Fresh or else Module_Attempted then return; end if;
      Stage := Provisioning;
      Read_Clock (Now, Available);
      if Destination = No_Endpoint or else not Available or else Deadline = Wire.No_Finite_Deadline
        or else Now >= Deadline then Stage := Failed; return; end if;
      Last_Sample := Now; Until_Time := Deadline;
      Provider.Initialize (IO, Destination, Provider_Symbol, Deadline, Accepted);
      if not Accepted then Stage := Failed; return; end if;
      Stage := Ready;
      Accepted := Usable;
   end Begin_Startup;
   procedure Close_Startup is
      Completed : Boolean;
   begin
      Stage := Closed;
      Provider.Cleanup (IO, Completed);
   end Close_Startup;
   procedure Execute_Module_Startup
     (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status) is
   begin
      Result := (others => <>); Status := Core.Values.Wrong_Phase;
      if Module_Attempted then return; end if;
      Module_Attempted := True;
      if not Usable then return; end if;
      Last_Capacity := ACPI_Capacity_Diagnostics.No_Report;
      Evaluation.Bindings.Startup.Execute_Frozen (Service, IO, Budget, Result, Status);
      Module_Completed := Status = Core.Values.Available
        and then Result.Phase = Core.Module_Execution_Reported
        and then Result.Modules.Outcome = Core.Values.Modules_Completed;
      if not Module_Completed then Stage := Failed; end if;
   end Execute_Module_Startup;
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Module_Completed or else not Usable then return; end if;
      Last_Capacity := ACPI_Capacity_Diagnostics.No_Report;
      Evaluation.Prepare (Cache, Service, IO, Accepted);
   end Prepare_Fields;
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value, 0); Status := Core.Values.Wrong_Phase;
      if not Module_Completed or else not Usable then return; end if;
      Last_Capacity := ACPI_Capacity_Diagnostics.No_Report;
      Evaluation.Evaluate_Named (Cache, Service, IO, Item, Budget, Result, Status);
   end Evaluate_Named;
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value, 0); Status := Core.Values.Wrong_Phase;
      if not Module_Completed or else not Usable then return; end if;
      Last_Capacity := ACPI_Capacity_Diagnostics.No_Report;
      Evaluation.Invoke_With_Arguments
        (Cache, Service, IO, Node, Args, Argument_Count, Budget, Result, Status);
   end Invoke_With_Arguments;
   function Uncertain return AML_Field_Access.Observation is (Provider.Uncertain (IO));
end ACPI_Field_Startup_Owner;
