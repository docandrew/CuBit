pragma Ada_2022;
with AML_Decode;
package body ACPI_Field_Startup_Owner is
   use type Wire.Time_Instant, Interfaces.Unsigned_64;
   use type AML_Decode.Byte;
   package F renames ACPI_Field_Diagnostics;
   function First_Failure return F.Report is
     (if F.Present (First) then First else Provider.First_Failure (IO.Physical));
   procedure Remember_Window (Reason : F.Cause; At_Point : F.Point) is
   begin
      if not F.Present (Provider.First_Failure (IO.Physical)) then
         F.Remember (First, (Reason => Reason, At_Point => At_Point, others => <>));
      end if;
   end Remember_Window;
   use type Plain.Startup_State;
   Lease_Microseconds : constant Interfaces.Unsigned_64 := 100_000;
   Lease_IPC_Milliseconds : constant Wire.Time_Instant := 100;
   Cleanup_IPC_Milliseconds : constant Wire.Time_Instant := 25;
   function Plain_Ready (IO : Plain.Context) return Boolean is
     (Plain.Status (IO) = Plain.Ready);
   use type Provider.Startup_State;
   use type Core.Values.Access_Status, Core.Execution_Phase_Result, Core.Values.Module_Outcome;
   Provider_Symbol : constant AML_Field_Access.Endpoint_ID := 1;
   procedure Record_Capacity_Failure (Item : ACPI_Capacity_Diagnostics.Report) is
   begin
      if not ACPI_Capacity_Diagnostics.Present (Last_Capacity) then Last_Capacity := Item; end if;
   end Record_Capacity_Failure;
   function Observe return Report is
     ((Stage, Module_Attempted, Module_Completed, Root_Pending (IO), Root_Count (IO), Current_Capacity));
   procedure Read_Window (Now, Deadline : out Wire.Time_Instant) is
      Available : Boolean;
   begin
      Now := 0; Deadline := Wire.No_Finite_Deadline;
      if Stage /= Ready then return; end if;
      Read_Clock (Now, Available);
      if not Available or else Now < Last_Sample or else Now >= Until_Time then
         Remember_Window ((if not Available then F.Startup_Clock_Unavailable
           elsif Now < Last_Sample then F.Startup_Clock_Reversed else F.Startup_Deadline), F.Startup_Window);
         Stage := Closed; return;
      end if;
      Last_Sample := Now; Deadline := Until_Time;
   end Read_Window;
   procedure Read_NVS_Deadline (Deadline : out Wire.Time_Instant) is
      Ignored : Wire.Time_Instant;
   begin Read_Window (Ignored, Deadline); end Read_NVS_Deadline;
   procedure Read_PCI_Clock
     (Now : out ACPI_PCI_Protocol.Microsecond_Deadline; Available : out Boolean) is
      Value : Interfaces.Unsigned_64;
   begin
      Read_Lease_Clock (Value, Available);
      Now := ACPI_PCI_Protocol.Microsecond_Deadline (Value);
   end Read_PCI_Clock;
   procedure Read_PCI_Deadline
     (IPC_Deadline : out Wire.Time_Instant;
      Data_Deadline : out ACPI_PCI_Protocol.Microsecond_Deadline) is
      Ignored : Wire.Time_Instant;
   begin
      Read_Window (Ignored, IPC_Deadline);
      Data_Deadline := ACPI_PCI_Protocol.Data_Deadline_For (IPC_Deadline);
   end Read_PCI_Deadline;
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
         Remember_Window ((if not Available then F.Lease_Clock_Unavailable else F.Deadline_Overflow), F.Lease_Window);
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
      if Stage /= Ready or else not Root_Ready (IO) then
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
      Initialize_Root (Destination, Deadline, Accepted);
      if not Accepted then Stage := Failed; return; end if;
      Stage := Ready;
      Accepted := Usable;
   end Begin_Startup;
   procedure Close_Startup is
      Completed : Boolean;
   begin
      Stage := Closed;
      Hooks.Cleanup (IO.Transactions, IO.Physical, Completed);
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
   procedure Invoke_Retained
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value, 0); Status := Core.Values.Wrong_Phase;
      if not Module_Completed or else not Usable then return; end if;
      Last_Capacity := ACPI_Capacity_Diagnostics.No_Report;
      Evaluation.Invoke_Retained
        (Cache, Service, IO, Node, Args, Argument_Count, Budget, Result, Status);
   end Invoke_Retained;
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
   function Uncertain return AML_Field_Access.Observation is
      Route_Item : constant AML_Field_Access.Observation := Routes.Uncertain (IO.Routing);
      Legacy_Item : constant AML_Field_Access.Observation := Provider.Uncertain (IO.Physical);
      FW_Ready, FW_Pending : Boolean := False;
      FW_Item : AML_Field_Access.Observation;
      Capacity : ACPI_Capacity_Diagnostics.Report := ACPI_Capacity_Diagnostics.No_Report;
   begin
      if Route_Item.Present then return Route_Item; elsif Legacy_Item.Present then return Legacy_Item; end if;
      if Enable_Firmware_Data then Inspect_Firmware (FW_Ready,FW_Pending,FW_Item,Capacity); end if;
      return FW_Item;
   end Uncertain;
   type Firmware_Report is record
      Is_Ready, Is_Pending : Boolean := False;
      Item : AML_Field_Access.Observation;
      Capacity : ACPI_Capacity_Diagnostics.Report := ACPI_Capacity_Diagnostics.No_Report;
   end record;
   function Firmware_State return Firmware_Report is
      Value : Firmware_Report;
   begin
      if Enable_Firmware_Data then
         Inspect_Firmware (Value.Is_Ready,Value.Is_Pending,Value.Item,Value.Capacity);
      end if;
      return Value;
   end Firmware_State;
   function Current_Capacity return ACPI_Capacity_Diagnostics.Report is
     (if ACPI_Capacity_Diagnostics.Present (Last_Capacity) then Last_Capacity
      else Firmware_State.Capacity);
   function Root_Pending (IO : Root_Context) return Boolean is
     (Routes.Pending (IO.Routing) or else Hooks.Pending (IO.Transactions)
      or else Provider.Pending (IO.Physical) or else Firmware_State.Is_Pending);
   function Root_Ready (IO : Root_Context) return Boolean is
     (Provider.Status (IO.Physical) = Provider.Ready and then Routes.Ready (IO.Routing)
      and then not Root_Pending (IO)
      and then (not Enable_Firmware_Data or else Firmware_State.Is_Ready));
   function Root_Identity (IO : Root_Context) return AML_Identity.Identity is (Routes.Identity (IO.Routing));
   function Root_Count (IO : Root_Context) return Natural is
      (if Root_Ready (IO) then Routes.Register_Count (IO.Routing) else 0);
   procedure Lane_Describe (IO : Provider.Context; Path : Lane; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := False;
      case Path is
         when Existing_Lane => Provider.Describe (IO,Target,Geometry,Accepted);
         when Firmware_Lane =>
            if Enable_Firmware_Data then Describe_Firmware (Target,Geometry,Accepted); end if;
      end case;
   end Lane_Describe;
   procedure Lane_Authorize (IO : Provider.Context; Path : Lane; Target : AML_Field_Access.Target;
      For_Write : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      case Path is
         when Existing_Lane => Provider.Authorize (IO,Target,For_Write,Accepted);
         when Firmware_Lane =>
            if Enable_Firmware_Data then Authorize_Firmware (Target,For_Write,Accepted); end if;
      end case;
   end Lane_Authorize;
   procedure Lane_Dispatch (IO : in out Provider.Context; Path : Lane;
      Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response) is
   begin
      Reply := (others => <>);
      case Path is
         when Existing_Lane => Provider.Dispatch (IO,Item,Reply);
         when Firmware_Lane =>
            if Enable_Firmware_Data then Dispatch_Firmware (Item,Reply); end if;
      end case;
   end Lane_Dispatch;
   procedure Data_Begin (IO : in out Provider.Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
      Deadline : Wire.Time_Instant;
      Ticket : ACPI_Global_Lock_Protocol.Ticket;
   begin
      Accepted := not Enable_Firmware_Data;
      if not Enable_Firmware_Data then return; end if;
      Provider.Read_Field_Context (IO,Requires_Lock,For_Write,Deadline,Ticket,Accepted);
      if Accepted then
         Accepted := False;
         Begin_Firmware (Requires_Lock,For_Write,Deadline,Ticket,Accepted);
      end if;
   end Data_Begin;
   procedure Data_End (IO : in out Provider.Context; Accepted : in out Boolean) is
      pragma Unreferenced (IO);
   begin
      Accepted := not Enable_Firmware_Data;
      if Enable_Firmware_Data then End_Firmware (Accepted); end if;
   end Data_End;
   procedure Data_Cleanup (IO : in out Provider.Context; Accepted : out Boolean) is
      pragma Unreferenced (IO);
   begin
      Accepted := not Enable_Firmware_Data;
      if Enable_Firmware_Data then Cleanup_Firmware (Accepted); end if;
   end Data_Cleanup;
   procedure Adopt_Existing (IO : in out Root_Context; Inner : AML_Field_Access.Target;
      Target : out AML_Field_Access.Target; Accepted : out Boolean) is
      Canonical : AML_Field_Layout.Register_Geometry;
   begin
      Target := AML_Field_Access.No_Target;
      Provider.Describe (IO.Physical,Inner,Canonical,Accepted);
      if Accepted then
         Routes.Adopt (IO.Routing,IO.Physical,Existing_Lane,Inner,Canonical,Target,Accepted);
         if not Accepted then Record_Capacity_Failure (Routes.Observe_Capacity (IO.Routing)); end if;
      end if;
   end Adopt_Existing;
   procedure Initialize_Root (Destination : Endpoint; Deadline : Wire.Time_Instant; Accepted : out Boolean) is
      Inner, Target : AML_Field_Access.Target;
   begin
      Provider.Initialize (IO.Physical,Destination,Provider_Symbol,Deadline,Accepted);
      if not Accepted then return; end if;
      if Enable_Firmware_Data then
         Accepted := False; Initialize_Firmware (Destination,Provider_Symbol,Deadline,Accepted);
         if not Accepted then return; end if;
      end if;
      Routes.Initialize (IO.Routing,Provider_Symbol,Accepted);
      if not Accepted then return; end if;
      for I in 1 .. Provider.Register_Count (IO.Physical) loop
         Provider.Export_Target (IO.Physical,I,Inner,Accepted);
         if not Accepted then return; end if;
         Adopt_Existing (IO,Inner,Target,Accepted);
         if not Accepted then return; end if;
      end loop;
   end Initialize_Root;
   procedure Root_Export (IO : Root_Context; Ordinal : Positive; Target : out AML_Field_Access.Target; Accepted : out Boolean) is
   begin
      Target := AML_Field_Access.No_Target; Accepted := False;
      if Root_Ready (IO) then Routes.Export_Target (IO.Routing,Ordinal,Target,Accepted); end if;
   end Root_Export;
   procedure Root_Describe (IO : Root_Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := False;
      if Root_Ready (IO) then Routes.Describe (IO.Routing,IO.Physical,Target,Geometry,Accepted); end if;
   end Root_Describe;
   procedure Root_Authorize (IO : Root_Context; Target : AML_Field_Access.Target;
      For_Write : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Root_Ready (IO) then Routes.Authorize (IO.Routing,IO.Physical,Target,For_Write,Accepted); end if;
   end Root_Authorize;
   procedure Root_Dispatch (IO : in out Root_Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response) is
   begin
      Reply := (others => <>);
      if Root_Ready (IO) then Routes.Dispatch (IO.Routing,IO.Physical,Item,Reply); end if;
   end Root_Dispatch;
   procedure Root_Confirmed (IO : Root_Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean) is
   begin Routes.Reconciled (IO.Routing,Expected,Accepted); end Root_Confirmed;
   procedure Root_Select (IO : in out Root_Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Target : out AML_Field_Access.Target; Accepted : out Boolean) is
      use type AML_Field_Layout.Register_Geometry;
      Inner, Candidate : AML_Field_Access.Target;
      Canonical : AML_Field_Layout.Register_Geometry;
      Good : Boolean;
   begin
      Target := AML_Field_Access.No_Target; Accepted := False;
      if not Root_Ready (IO) then return; end if;
      Provider.Select_Target (IO.Physical,Geometry,For_Write,Inner,Good);
      if not Good and then not Root_Pending (IO) and then Enable_PCI and then not For_Write
        and then (Geometry.Region.Space = AML_Field_Layout.Memory_Space
          or else (Geometry.Region.Space = AML_Field_Layout.PCI_Config_Space
            and then Geometry.Region.PCI.Known)) then
         declare Now, Due : Wire.Time_Instant; Prepared : Boolean; begin
            Read_Window (Now, Due);
            if Due /= Wire.No_Finite_Deadline and then Now < Due then
               Provider.Prepare_Target (IO.Physical, Geometry, For_Write, Due, Prepared);
               if Prepared then Provider.Select_Target (IO.Physical,Geometry,For_Write,Inner,Good); end if;
            end if;
         end;
      end if;
      if Good then Adopt_Existing (IO,Inner,Target,Accepted); return; end if;
      if Root_Pending (IO) then return; end if;
      -- Reuse only an already issued association, never publish from geometry.
      for I in 1 .. Routes.Register_Count (IO.Routing) loop
         Routes.Export_Target (IO.Routing,I,Candidate,Good);
         if not Good then return; end if;
         Routes.Describe (IO.Routing,IO.Physical,Candidate,Canonical,Good);
         if Good and then Canonical = Geometry then
            Routes.Authorize (IO.Routing,IO.Physical,Candidate,For_Write,Good);
            if Good then Target := Candidate; Accepted := True; end if;
            return;
         end if;
      end loop;
   end Root_Select;
   procedure Root_Resolve_Module (IO : in out Root_Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is
      Inner : AML_Field_Access.Target;
   begin
      Target := AML_Field_Access.No_Target; Accepted := False;
      if not Root_Ready (IO) then return; end if;
      Provider.Resolve_Module_Target (IO.Physical,Geometry,Inner,Accepted);
      if not Accepted and then not Root_Pending (IO) and then Enable_PCI
        and then (Geometry.Region.Space = AML_Field_Layout.Memory_Space
          or else (Geometry.Region.Space = AML_Field_Layout.PCI_Config_Space
            and then Geometry.Region.PCI.Known)) then
         declare Now, Due : Wire.Time_Instant; Prepared : Boolean; begin
            Read_Window (Now, Due);
            if Due /= Wire.No_Finite_Deadline and then Now < Due then
               Provider.Prepare_Target (IO.Physical, Geometry, False, Due, Prepared);
               if Prepared then Provider.Resolve_Module_Target (IO.Physical,Geometry,Inner,Accepted); end if;
            end if;
         end;
      end if;
      if Accepted then Adopt_Existing (IO,Inner,Target,Accepted); end if;
   end Root_Resolve_Module;
   procedure Root_Resolve_Firmware (IO : in out Root_Context; Declaration : Core.Namespace.Operation_Region_Attributes;
      Geometry : AML_Field_Layout.Register_Geometry; Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is
      Inner : AML_Field_Access.Target := AML_Field_Access.No_Target;
   begin
      Target := AML_Field_Access.No_Target; Accepted := False;
      if not Enable_Firmware_Data or else not Root_Ready (IO) then return; end if;
      Resolve_Firmware (Declaration,Geometry,Inner,Accepted);
      if Accepted then
         Routes.Adopt (IO.Routing,IO.Physical,Firmware_Lane,Inner,Geometry,Target,Accepted);
         if not Accepted then Record_Capacity_Failure (Routes.Observe_Capacity (IO.Routing)); end if;
      end if;
   end Root_Resolve_Firmware;
   procedure Root_Begin (IO : in out Root_Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
   begin Hooks.Begin_Field (IO.Transactions,IO.Physical,Requires_Lock,For_Write,Accepted); end Root_Begin;
   procedure Root_End (IO : in out Root_Context; Accepted : in out Boolean) is
   begin Hooks.End_Field (IO.Transactions,IO.Physical,Accepted); end Root_End;

end ACPI_Field_Startup_Owner;
