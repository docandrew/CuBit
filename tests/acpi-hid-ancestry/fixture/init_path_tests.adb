pragma Ada_2022;
with ACPI_Service; with ACPI_Requests; with ACPI_Requests.With_Driver;
with ACPI_Device_Initialization; with ACPI_Device_Policy; with ACPI_Hosted_Heap;
with ACPI_Native_HID_Detection; with ACPI_Requests.HID_Offers;
with ACPI_Native_Log;
with ACPI_HID_Offer_Builder; with ACPI_HID_Offer_Provenance;
with ACPI_PCI_Observation_Reader; with GPIO_Bootstrap_Payload;
with AML_Execute; with AML_Decode;
with ACPI_HID_Initialization;
with ACPI_HID_I2C_Discovery; with ACPI_HID_I2C_Candidate;
with ACPI_HID_I2C_Resolved; with ACPI_Requests.HID_Devices;
with AML_Objects; with Firmware_Tables; with Interfaces;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
pragma Elaborate_All (ACPI_Hosted_Heap);
procedure Init_Path_Tests is
   pragma Unevaluated_Use_Of_Old (Allow);
   package C renames ACPI_Service; package Q renames ACPI_Requests;
   package IO renames Ada.Streams.Stream_IO;
   procedure Regions (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced (Service, Budget);
   begin Charged := 0; Outcome := ACPI_Device_Policy.No_Regions_Required; end;
   type Fault_Mode is (None, Policy_Exception, Retained_Exception, Policy_Overcharge,
      Policy_Recovery, Policy_Refusal, Status_Fatal, Status_Indeterminate,
      Status_Overcharge, Status_Retained, Status_Released_Result, Status_Exception, Budget_Stop,
      Status_Uses_Budget, Reset_Refused, Foreign_History, Status_Changes_In_Probe);
   Fault : Fault_Mode := None;
   Evaluations, Fault_Calls, Status_Reads, Invocations : Natural := 0;
   use type AML_Execute.Execution_Status;
   procedure Retain_String (Service : aliased in out C.State;
      Scope : C.Namespace.Node_ID; Budget : Natural) is
      Found : C.Namespace.Lookup_Result; Item : C.Node_Descriptor;
      Status : C.Values.Access_Status; Value : C.Values.Result;
   begin
      C.Find_Object (Service, Scope, "_CID", Found, Status);
      C.Describe_Node (Service, Found.Node, Item, Status);
      C.Evaluate_Named (Service, Item, Budget, Value, Status);
      if Value.Status /= AML_Execute.Object_Returned then raise Program_Error with "fixture must retain CID string"; end if;
      if C.Retained_Results (Service) = 0 then raise Program_Error with "fixture root not retained"; end if;
   end Retain_String;
   use type C.Node_Descriptor, C.Values.Value_Handle, C.Values.Access_Status, C.Namespace.Lookup_Status;
   Original_Data : C.Node_Descriptor := C.Values.No_Node;
   procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      if Original_Data = C.Values.No_Node then
         declare Found : C.Namespace.Lookup_Result; Access_Result : C.Values.Access_Status; begin
            C.Find_Object (Service, C.Namespace.Root, "DATA", Found, Access_Result);
            if Access_Result = C.Values.Available and then Found.Status = C.Namespace.Found then
               C.Describe_Node (Service, Found.Node, Original_Data, Access_Result);
            end if;
         end;
      end if;
      Evaluations := Evaluations + 1;
      if Fault /= None and then C.Values.Node_Name (Item) = "_STA" then
         Status := C.Values.Available;
         case Fault is
            when Status_Fatal =>
               Result := (Status => AML_Execute.Fatal_Requested, Charged => 1,
                 Fatal => (Literal => (Kind => 0, Code => 1), Evaluated_Argument => 2, Width => AML_Decode.Bits_64)); return;
            when Status_Indeterminate => Result := (Status => AML_Execute.Field_Indeterminate, Charged => 1); return;
            when Status_Changes_In_Probe =>
               Status_Reads := Status_Reads + 1;
               Result := (Status => AML_Execute.Returned, Charged => 1,
                 Number => (if Status_Reads = 1 then 15 else 0), Origin => AML_Decode.Ordinary_Integer); return;
            when Status_Uses_Budget =>
               Result := (Status => AML_Execute.Returned, Charged => Budget,
                 Number => 15, Origin => AML_Decode.Ordinary_Integer); return;
            when Status_Overcharge =>
               Result := (Status => AML_Execute.Returned, Charged => Budget + 1,
                 Number => 15, Origin => AML_Decode.Ordinary_Integer); return;
            when Status_Retained =>
               Retain_String (Service, C.Values.Parent_Position (Item), Budget);
               Result := (Status => AML_Execute.No_Return, Charged => 1); return;
            when Status_Released_Result =>
               declare Found : C.Namespace.Lookup_Result; Value : C.Values.Result;
                  Cleanup : C.Values.Access_Status; Saved : C.Values.Value_Handle;
                  Member : C.Node_Descriptor;
               begin
                  C.Find_Object (Service, C.Values.Parent_Position (Item), "_CID", Found, Status);
                  if Status /= C.Values.Available or else Found.Status /= C.Namespace.Found then
                     raise Program_Error with "release fixture requires CID";
                  end if;
                  C.Describe_Node (Service, Found.Node, Member, Status);
                  if Status /= C.Values.Available then raise Program_Error with "release fixture CID descriptor"; end if;
                  C.Evaluate_Named (Service, Member, Budget, Value, Status);
                  if Status /= C.Values.Available or else Value.Status /= AML_Execute.Object_Returned then
                     raise Program_Error with "release fixture requires actual retained string";
                  end if;
                  Saved := Value.Handle;
                  C.Release_Result (Service, Value.Handle, Cleanup);
                  if Cleanup /= C.Values.Available or else Value.Handle /= C.Values.No_Value then
                     raise Program_Error with "controlled early release must succeed";
                  end if;
                  Result := (Status => AML_Execute.Object_Returned, Charged => 1, Handle => Saved);
                  return;
               end;
            when Status_Exception => raise Program_Error with "controlled status callback interruption";
            when others => null;
         end case;
      end if;
      C.Evaluate_Named (Service, Item, Budget, Result, Status);
   end;
   package Driver is new ACPI_Device_Initialization (C, Regions, Evaluate);
   package Controlled is new Q.With_Driver (Driver);
   procedure Invoke (Service : aliased in out C.State; Node : C.Namespace.Node_ID;
      Args : C.Values.Arguments; Argument_Count : C.Values.Argument_Count;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Invocations := Invocations + 1;
      C.Invoke_With_Arguments (Service, Node, Args, Argument_Count, Budget, Result, Status);
   end Invoke;
   package Discovery is new ACPI_HID_I2C_Discovery (C, Invoke, Evaluate);
   package Candidates is new ACPI_HID_I2C_Candidate (256);
   package Resolver is new ACPI_HID_I2C_Resolved (C, Candidates);
   package Owner is new Q.HID_Devices (Discovery, Resolver);
   package H renames Owner.Controller;
   package Paths is new ACPI_HID_Initialization (C, Driver, Discovery.Queries);
   use type Paths.Outcome;
   use type Discovery.Queries.Status_Outcome;
   Path_Result : Paths.Report;
   package Native renames ACPI_Native_HID_Detection;
   package Builder is new ACPI_HID_Offer_Builder (Candidates);
   package Provenance is new ACPI_HID_Offer_Provenance (C, Candidates, Resolver, Builder);
   package Reader is new ACPI_PCI_Observation_Reader (C, Evaluate);
   package Offers is new Q.HID_Offers (Candidates, Discovery, Resolver, Builder, Provenance, Reader);
   use type Native.Outcome, Offers.Outcome, Offers.Payload_Array, Offers.HID.Controller.Policy_Outcome;
   use type Native.Progress_Step;
   Use_Unstarted, Use_Foreign : Boolean := False;
   use type H.Policy_Outcome, H.Search_Outcome, H.Search_Error_Kind;
   Mode : H.Policy_Outcome := H.Excluded;
   Overcharge : Boolean := False;
   Policy_Calls : Natural := 0;
   use type Driver.Path_Outcome;
   use type Q.Outcome, Q.Phase, Q.Revision_Number;
   function Read_Table return Firmware_Tables.Bytes is
      F : IO.File_Type;
   begin
      IO.Open (F, IO.In_File, Ada.Command_Line.Argument (1));
      declare Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (F)));
         Last : Ada.Streams.Stream_Element_Offset; Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin IO.Read (F, Raw, Last); IO.Close (F);
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Server, Foreign_Server : Q.State (1, Table'Length, Table'Length, 1, 0,
      Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes,
      Value_Element_Capacity => AML_Objects.Max_Elements);
   Initialization, Foreign_Initialization, Unstarted : Driver.Session;
   Descriptor : C.Node_Descriptor := C.Values.No_Node;
   Result : Driver.Path_Outcome; Calls, Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end;
   procedure Load (Target : in out Q.State; Run : in out Driver.Session) is
      Reply : Q.Response;
      procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
      begin Controlled.Handle (Target, Run, Q.Snapshot_Provider,
         (Label => Label, Data => [Q.Revision (Target), Value, 0, 0], others => <>), Reply);
         Check (Reply.Status = Q.OK, "controlled snapshot protocol"); end;
   begin
      Send (Q.Start_Snapshot, 1);
      Q.Import_Block (Target, Q.Snapshot_Provider, Q.Revision (Target), 1, C.DSDT, Table, Reply);
      Check (Reply.Status = Q.OK, "table imported"); Send (Q.Finish_Snapshot);
      Check (Q.Current (Target) = Q.Complete, "controlled initialization complete");
   end;
   procedure Inspect (Service : C.State; Done : out Boolean) is
      Before : constant Natural := Evaluations;
      Node : C.Namespace.Node_ID := C.Namespace.Root;
      Found : C.Namespace.Lookup_Result; Status : C.Values.Access_Status;
      procedure Child (Name : String) is
      begin C.Find_Object (Service, Node, Name, Found, Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "path member"); Node := Found.Node; end;
   begin
      Child ("_SB_"); Child ("PC00");
      if Ada.Command_Line.Argument (2) in "nested-ready" | "nested-excluded" | "nested-pruned" then Child ("BRG0"); end if;
      Child ("I2C1"); Child ("TPD0");
      C.Describe_Node (Service, Node, Descriptor, Status);
      Check (Status = C.Values.Available, "device description");
      Check (Driver.Initialization_Path (Unstarted, Service, Descriptor) = Driver.History_Unknown, "unstarted history rejected");
      Check (Driver.Initialization_Path (Foreign_Initialization, Service, Descriptor) = Driver.Stale_History,
        "foreign initialization run cannot certify this Core");
      Result := Driver.Initialization_Path (Initialization, Service, Descriptor);
      Check (C.Retained_Results (Service) = 0, "metadata observation does not retain results");
      Check (Evaluations = Before, "initialization witness executes no AML");
      Calls := Calls + 1; Done := True;
   end;
   package Metadata is new Q.With_Metadata_Continuation (Inspect);
   procedure Policy
     (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Answer : out H.Policy_Report) is
   begin
      Policy_Calls := Policy_Calls + 1;
      Answer := (H.Excluded, 0);
      if Item /= Descriptor then return; end if;
      Check (Driver.Initialization_Path (Initialization, Service, Item) = Result,
        "policy receives actual initialized private Core");
      Answer := (Mode, (if Overcharge then Budget + 1 else 7));
   end Policy;
   procedure Filtered_Find is new Owner.Find_With_Policy (Policy);
   Kind_Boundaries_Checked : Boolean := False;
   procedure Check_Kind_Boundaries (Service : aliased in out C.State) is
      package Queries renames Discovery.Queries;
      use type Queries.Identification_Outcome;
      use type C.Namespace.Object_Kind;
      Before_Roots : constant Natural := C.Retained_Results (Service);
      Found : C.Namespace.Lookup_Result;
      Status : C.Values.Access_Status;
      Item, Cursor : C.Node_Descriptor;
      Path_Answer : Paths.Report;
      Identity : Queries.Identification_Report;
      procedure Invalid_Status (Target : C.Node_Descriptor) is
         Before : constant Natural := Evaluations;
         Answer : Queries.Status_Report;
      begin
         Queries.Observe_Status (Service, Target, 100_000, Answer);
         Check (Answer.Outcome = Queries.Invalid_Device and then Answer.Charged = 0
           and then Answer.Flags = 0 and then not Answer.Defaulted,
           "invalid status object cannot acquire default device presence");
         Check (Evaluations = Before, "invalid status object executes no AML");
      end Invalid_Status;
   begin
      Invalid_Status (C.Values.No_Node);
      C.Find_Object (Service, C.Values.Position (Descriptor), "_CID", Found, Status);
      if Status /= C.Values.Available or else Found.Status /= C.Namespace.Found then
         raise Program_Error with "boundary fixture needs direct CID";
      end if;
      C.Describe_Node (Service, Found.Node, Item, Status);
      Check (Status = C.Values.Available and then C.Current_Node (Service, Item),
        "ordinary named CID is a valid current descriptor");
      Invalid_Status (Item);
      Cursor := Descriptor;
      while C.Values.Position (Cursor) /= C.Namespace.Root loop
         C.Describe_Node (Service, C.Values.Parent_Position (Cursor), Item, Status);
         if Status /= C.Values.Available then raise Program_Error with "boundary ancestor descriptor"; end if;
         if C.Values.Node_Kind (Item) in C.Namespace.Processor_Object | C.Namespace.Thermal_Zone_Object then
            declare Before : constant Natural := Evaluations; begin
               Paths.Check_Path (Initialization, Service, Item, 100_000, Path_Answer);
               Check (Path_Answer.Result /= Paths.Eligible and then Path_Answer.Charged = 0,
                 "status-bearing non-Device cannot become HID leaf");
               Check (Evaluations = Before, "non-Device leaf refusal precedes AML");
               Queries.Identify (Service, Item, 100_000, Identity);
               Check (Identity.Outcome = Queries.Invalid_Device and then Identity.Charged = 0
                 and then Identity.IDs_Examined = 0,
                 "compatible CID cannot promote Processor or ThermalZone to HID Device");
               Check (Evaluations = Before, "non-Device identification executes no CID AML");
            end;
         end if;
         Cursor := Item;
      end loop;
      C.Find_Object (Service, C.Namespace.Root, "DATA", Found, Status);
      if Status = C.Values.Available and then Found.Status = C.Namespace.Found then
         Check (Original_Data /= C.Values.No_Node, "captured value descriptor before real AML mutation");
         C.Describe_Node (Service, Found.Node, Item, Status);
         Check (Status = C.Values.Available and then C.Current_Binding (Service, Original_Data)
           and then not C.Current_Node (Service, Original_Data),
           "CopyObject changes value kind while preserving binding identity");
         Check (C.Values.Node_Kind (Item) in C.Namespace.Buffer_Object | C.Namespace.String_Object,
           "real AML changed integer binding into buffer or string");
      end if;
      Check (C.Retained_Results (Service) = Before_Roots, "kind refusals preserve retained roots");
   end Check_Kind_Boundaries;
   procedure Actual_Policy
     (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Answer : out H.Policy_Report) is
      Saved : constant Paths.Report := Path_Result;
   begin
      Answer := (H.Excluded, 0);
      if Item /= Descriptor and then Ada.Command_Line.Argument (2) not in "all-devices" | "all-probes" then return; end if;
      if not Kind_Boundaries_Checked then
         Check_Kind_Boundaries (Service); Kind_Boundaries_Checked := True;
      end if;
      if Use_Unstarted then Paths.Check_Path (Unstarted, Service, Item, Budget, Path_Result);
      elsif Use_Foreign then Paths.Check_Path (Foreign_Initialization, Service, Item, Budget, Path_Result);
      else Paths.Check_Path (Initialization, Service, Item, Budget, Path_Result); end if;
      if Use_Unstarted or else Use_Foreign then
         Check (Path_Result.Last_Node = Item and then not Paths.Status_Known (Path_Result),
           "historical refusal preserves candidate identity and unknown status");
      end if;
      Answer := (Outcome => (case Path_Result.Result is
        when Paths.Eligible => H.Eligible,
        when Paths.Excluded => H.Excluded,
        when Paths.Status_Failed => H.Node_Error,
        when Paths.Budget_Exhausted | Paths.Initialization_Incomplete | Paths.History_Unknown => H.Policy_Incomplete,
        when Paths.Recovery_Required => H.Recovery_Required,
        when others => H.Policy_Failed), Charged => Path_Result.Charged);
      -- Preserve the selected leaf observation for existing assertions while
      -- exercising the actual path policy for every Device in these cases.
      if Item /= Descriptor then Path_Result := Saved; end if;
   end Actual_Policy;
   procedure Actual_Find is new Owner.Find_With_Policy (Actual_Policy);
   procedure Check_Actual_Path is
      Found : C.Namespace.Node_ID;
      Search : H.Search_Report;
   begin
      Actual_Find (Server, C.Namespace.Root, 100_000, Found, Search);
      if Ada.Command_Line.Argument (2) = "all-probes" then
         Check (Search.Outcome = H.Found and then Found /= C.Values.Position (Descriptor),
           "identity search exposes first compatible device before resource validation");
         Actual_Find (Server, Found, 100_000, Found, Search);
      end if;
      if Result = Driver.Pruned_Path then
         Check (Path_Result.Result = Paths.Excluded and then Search.Outcome = H.No_Match,
           "pruned history blocks actual identity search");
      elsif Ada.Command_Line.Argument (2) = "skipped-now-active" then
         Check (Result = Driver.Initialization_Not_Complete and then
           Path_Result.Result = Paths.Initialization_Incomplete and then Search.Outcome = H.Incomplete,
           "reactivated leaf cannot bypass its skipped initializer");
      elsif Result = Driver.Initialization_Not_Complete then
         Check (Path_Result.Result = Paths.Excluded and then Search.Outcome = H.No_Match,
           "current inactive leaf excluded despite intentionally skipped initializer");
      elsif Ada.Command_Line.Argument (2) in "current-excluded" | "nested-excluded" then
         Check (Result = Driver.Initialization_Ready, "history alone would admit current-excluded path");
         Check (Path_Result.Result = Paths.Excluded and then Search.Outcome = H.No_Match,
           "current ancestor or leaf status excludes initialized path");
      else
         Check (Path_Result.Result = Paths.Eligible and then Search.Outcome = H.Found,
           "actual initialized current path reaches identity search");
         Check (Found = C.Values.Position (Descriptor), "same leaf selected");
         Check (Path_Result.Last_Node = Descriptor, "root-to-leaf status ends at candidate");
      end if;
      Check (Search.Charged <= 100_000, "shared path and identity allowance");
      Check (Natural (Search.Captured_Errors) + Search.Omitted_Errors = Search.Node_Errors,
        "bounded search diagnostics preserve exact total across later candidates");
      Check (Natural (Search.Captured_Errors) = Natural'Min (Search.Node_Errors, H.Maximum_Captured_Search_Errors),
        "search keeps exactly the first bounded prefix");
      for I in H.Search_Error_Index loop
         exit when I > Search.Captured_Errors;
         Check (C.Values.Position (Search.Errors (I).Device) /= C.Namespace.Root,
           "search error retains the failed candidate identity");
         if Search.Errors (I).Kind = H.Identity_Error then
            Check (Search.Errors (I).Identity.Outcome in Discovery.Queries.Evaluation_Failed | Discovery.Queries.Wrong_Result_Type,
              "copied identity failure survives later successful identity query");
         else
            Check (Search.Errors (I).Policy.Outcome = H.Node_Error,
              "copied ordinary policy error survives later candidates");
         end if;
      end loop;
      Check (not Q.Retained_Results_Pending (Server), "actual path cleanup complete");
   end Check_Actual_Path;
   procedure Native_Policy
     (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Answer : out Native.Owner.Controller.Policy_Report) is
      Common : H.Policy_Report;
   begin
      Actual_Policy (Service, Item, Budget, Common);
      Answer := (Outcome => (case Common.Outcome is
         when H.Eligible => Native.Owner.Controller.Eligible,
         when H.Excluded => Native.Owner.Controller.Excluded,
         when H.Node_Error => Native.Owner.Controller.Node_Error,
         when H.Policy_Incomplete => Native.Owner.Controller.Policy_Incomplete,
         when H.Policy_Failed => Native.Owner.Controller.Policy_Failed,
         when H.Recovery_Required => Native.Owner.Controller.Recovery_Required), Charged => Common.Charged);
   end Native_Policy;
   procedure Offer_Policy
     (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Answer : out Offers.HID.Controller.Policy_Report) is
      Common : H.Policy_Report;
   begin
      Actual_Policy (Service, Item, Budget, Common);
      Answer := (Outcome => (case Common.Outcome is
         when H.Eligible => Offers.HID.Controller.Eligible,
         when H.Excluded => Offers.HID.Controller.Excluded,
         when H.Node_Error => Offers.HID.Controller.Node_Error,
         when H.Policy_Incomplete => Offers.HID.Controller.Policy_Incomplete,
         when H.Policy_Failed => Offers.HID.Controller.Policy_Failed,
         when H.Recovery_Required => Offers.HID.Controller.Recovery_Required), Charged => Common.Charged);
   end Offer_Policy;
   procedure Native_Run is new Native.Run_With_Policy (Native_Policy);
   procedure Collect_Offers is new Offers.Collect_With_Policy (Offer_Policy);
   procedure Check_Bound is
      Detection : Native.Report;
      Rows : Offers.Device_Array := [others => C.Values.No_Node];
      Admission : GPIO_Bootstrap_Payload.Request;
      Stamp : Provenance.Stamp;
      Work, Unknown_Work, Foreign_Work : Offers.Run;
      Report : Offers.Report;
      Before : Natural;
   begin
      Native_Run (Server, Detection, (Fuel => 100_000, others => <>));
      Check (Natural (Detection.Captured_Errors) = Natural'Min (Detection.Node_Errors, Native.Maximum_Captured_Errors),
        "native report retains exactly the bounded diagnostic prefix");
      Check (ACPI_Native_Log.Error_Rows = Natural (Detection.Captured_Errors),
        "native finish emits each captured error once outside borrowed owner calls");
      for I in Native.Error_Index loop
         exit when I > Detection.Captured_Errors;
         Check (Detection.Errors (I).Path_Complete
           and then C.Values.Position (Detection.Errors (I).Device) /= C.Namespace.Root,
           "native error has copied current namespace path and failed device");
      end loop;
      if Detection.Node_Errors > Native.Maximum_Captured_Errors then
         Check (Detection.Node_Errors = 9 and then Detection.Captured_Errors = 8
           and then ACPI_Native_Log.Error_Summary = "acpi: stage hid-node-summary errors= 9 captured= 8 omitted= 1",
           "nine actual identity failures emit eight rows and exact one-omitted summary");
      end if;
      if Ada.Command_Line.Argument (2) = "all-probes" then
         Check (Detection.Status = Native.Finished and then Detection.Candidates = 2 and then Detection.Observed = 1
           and then Detection.Failed_Candidates = 1 and then Detection.Node_Errors = 1,
           "ordinary resource error preserves later good device and exact failed count");
         Check (Detection.Items (1).Device = Descriptor, "only later complete observation is retained");
      end if;
      if Result = Driver.Pruned_Path or else Ada.Command_Line.Argument (2) in "current-excluded" | "nested-excluded" then
         Check (Detection.Status = Native.No_Devices and then Detection.Observed = 0,
           "bound native search excludes pruned/current-absent ancestry");
      elsif Ada.Command_Line.Argument (2) = "skipped-now-active" then
         Check (Detection.Status = Native.Policy_Unresolved and then Detection.Observed = 0,
           "bound native detector refuses reactivated uninitialized leaf");
      elsif Result = Driver.Initialization_Not_Complete then
         Check (Detection.Status = Native.No_Devices and then Detection.Observed = 0,
           "bound native search skips an inactive uninitialized alternative");
      else
         Check (Detection.Status not in Native.Search_Failed | Native.Not_Ready | Native.Recovery_Required,
           "bound native search reaches eligible leaf probe");
      end if;
      Check (not Detection.Recovery_Needed and then not Detection.Cleanup_Required,
        "bound native search cleaned up");
      Rows (1) := Descriptor;
      Collect_Offers (Work, Server, Rows, 1, Descriptor, Admission, Stamp, 100_000, 512, Report);
      if Result = Driver.Pruned_Path or else Ada.Command_Line.Argument (2) in "current-excluded" | "nested-excluded" then
         Check (Report.Status = Offers.Device_Unavailable and then Report.Count = 0,
           "fresh offer collection excludes unavailable ancestry before probe");
      elsif Ada.Command_Line.Argument (2) = "skipped-now-active" then
         Check (Report.Status = Offers.Policy_Unresolved and then Report.Count = 0,
           "fresh collector refuses reactivated uninitialized leaf");
      elsif Result = Driver.Initialization_Not_Complete then
         Check (Report.Status = Offers.Device_Unavailable and then Report.Count = 0,
           "fresh offer excludes currently inactive skipped initializer");
      else Check (Report.Last_Policy.Outcome = Offers.HID.Controller.Eligible,
         "fresh offer gate admits initialized current path"); end if;
      Check (Report.Count = 0 and then Report.Values = Offers.Payload_Array'[others => Builder.Wire.Empty_Request],
        "invalid modeled admission never exposes partial payload");
      Check (not Report.Recovery_Needed, "fresh offer policy leaves no recovery debt");
      Use_Unstarted := True; Before := Evaluations;
      Collect_Offers (Unknown_Work, Server, Rows, 1, Descriptor, Admission, Stamp, 100, 512, Report);
      Use_Unstarted := False;
      Check (Report.Status = Offers.Policy_Unresolved and then Report.Charged = 0,
        "unstarted actual run refuses fresh offer without default history fallback");
      Check (Evaluations = Before, "unknown history precedes fresh probe AML");
      Use_Foreign := True; Before := Evaluations;
      Collect_Offers (Foreign_Work, Server, Rows, 1, Descriptor, Admission, Stamp, 100, 512, Report);
      Use_Foreign := False;
      Check (Report.Status = Offers.Probe_Refused and then Report.Charged = 0,
        "foreign actual run refuses fresh offer");
      Check (Evaluations = Before, "foreign history precedes fresh probe AML");
      Check (not Q.Retained_Results_Pending (Server), "bound owners retain no results");
   end Check_Bound;
   procedure Fault_Policy
     (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Answer : out Native.Owner.Controller.Policy_Report) is
   begin
      Fault_Calls := Fault_Calls + 1;
      Answer := (Native.Owner.Controller.Excluded, 0);
      if Item /= Descriptor then return; end if;
      case Fault is
         when Policy_Exception => raise Program_Error with "controlled policy interruption";
         when Retained_Exception =>
            Retain_String (Service, C.Values.Position (Item), Budget);
            raise Program_Error with "controlled interrupted retained callback";
         when Policy_Overcharge => Answer := (Native.Owner.Controller.Eligible, Budget + 1);
         when Policy_Recovery => Answer := (Native.Owner.Controller.Recovery_Required, 1);
         when Policy_Refusal | Reset_Refused => Answer := (Native.Owner.Controller.Policy_Failed, 0);
         when Budget_Stop => Answer := (Native.Owner.Controller.Policy_Incomplete, Budget);
         when others => Native_Policy (Service, Item, Budget, Answer);
      end case;
   end Fault_Policy;
   procedure Fault_Run is new Native.Run_With_Policy (Fault_Policy);
   procedure Check_Recovery is
      First, Again : Native.Report;
      Before_Calls, Before_Evaluations : Natural;
      Original : constant Fault_Mode := Fault;
      Reply : Q.Response;
      Version_Before : constant Q.Revision_Number := Q.Revision (Server);
      Work : Offers.Run;
      Rows : Offers.Device_Array := [others => C.Values.No_Node];
      Admission : GPIO_Bootstrap_Payload.Request;
      Stamp : Provenance.Stamp;
      Collection : Offers.Report;
   begin
      if Original = Reset_Refused then
         Q.Handle (Server, Q.Snapshot_Provider,
           (Label => Q.Start_Snapshot, Data => [Q.Revision (Server), 1, 0, 0], others => <>), Reply);
         Check (Reply.Status = Q.Wrong_Order and then Q.Revision (Server) = Version_Before,
           "one-shot snapshot refuses reset without changing owner revision");
      elsif Original = Foreign_History then Use_Foreign := True;
      end if;
      Fault_Run (Server, First, (Fuel => 100_000, others => <>));
      Check (First.Charged <= 100_000, "refusal never overflows caller budget");
      case Original is
         when Policy_Exception | Retained_Exception | Status_Exception =>
            Check (First.Status = Native.Owner_Error and then First.Recovery_Needed,
              "interrupted callback latches owner error and recovery");
            Check (First.Exceptional_Exit and then First.Last_Step = Native.Search_Candidates
              and then ACPI_Native_Log.Exception_Reports = 1
              and then ACPI_Native_Log.Compact_Result_Length in 1 .. 512,
              "interrupted search emits one compact typed exception result within native logstore limit");
         when Reset_Refused =>
            Check (First.Status = Native.Search_Failed and then Fault_Calls > 0
              and then not First.Recovery_Needed, "rejected reset preserves completed owner for ordinary policy refusal");
         when Status_Uses_Budget =>
            Check (First.Status = Native.Budget_Exhausted and then First.Charged = 100_000
              and then not First.Recovery_Needed, "status and identity share original native allowance");
            Rows (1) := Descriptor;
            Collect_Offers (Work, Server, Rows, 1, Descriptor, Admission, Stamp, 100_000, 512, Collection);
            Check (Collection.Status = Offers.Budget_Exhausted and then Collection.Charged = 100_000
              and then Collection.Count = 0 and then not Collection.Recovery_Needed,
              "fresh collection cannot renew fuel after path status");
         when Policy_Refusal | Foreign_History =>
            Check (First.Status = Native.Search_Failed and then not First.Recovery_Needed,
              "ordinary policy refusal remains distinct from recovery");
         when Budget_Stop =>
            Check (First.Status = Native.Policy_Unresolved and then First.Charged = 100_000
              and then not First.Recovery_Needed, "policy consumes original budget once");
         when others =>
            Check (First.Status = Native.Recovery_Required and then First.Recovery_Needed,
              "provider uncertainty cannot become absence or a clean refusal");
      end case;
      if Original = Status_Released_Result then
         Check (Path_Result.Last_Status.Outcome = Discovery.Queries.Cleanup_Failed
           and then Path_Result.Last_Status.Cleanup_Result = C.Values.Invalid_Value,
           "real Core release refusal is reported by status reader");
         Check (First.Recovery_Needed and then not First.Cleanup_Required
           and then not Q.Retained_Results_Pending (Server),
           "release failure needs recovery even when no retained roots remain");
      end if;
      Check (First.Observed = 0 and then First.Candidates = 0, "uncertain search starts no leaf probe");
      if Original in Retained_Exception | Status_Retained then
         Check (Q.Retained_Results_Pending (Server), "actual retained root survives interrupted callback");
      end if;
      if Original in Retained_Exception | Status_Retained then
         Check (First.Cleanup_Required, "all recovery reports include pending Core roots");
      end if;
      Before_Calls := Fault_Calls; Before_Evaluations := Evaluations; Fault := None; Use_Foreign := False;
      Fault_Run (Server, Again, (Fuel => 100_000, others => <>));
      Check (Again.Status = First.Status and then Again.Recovery_Needed = First.Recovery_Needed
        and then Again.Cleanup_Required = First.Cleanup_Required and then Again.Charged = First.Charged,
        "one-shot result survives changed provider and renewed caller budget");
      Check (Fault_Calls = Before_Calls and then Evaluations = Before_Evaluations,
        "repeated call performs no policy callback or AML");
      Fault := Original;
   end Check_Recovery;
   procedure Check_Inactive_Probe is
      Work : Offers.Run;
      Rows : Offers.Device_Array := [others => C.Values.No_Node];
      Admission : GPIO_Bootstrap_Payload.Request;
      Stamp : Provenance.Stamp;
      Report : Offers.Report;
      Before : constant Natural := Invocations;
   begin
      Rows (1) := Descriptor; Status_Reads := 0;
      Collect_Offers (Work, Server, Rows, 1, Descriptor, Admission, Stamp, 100_000, 512, Report);
      Check (Report.Status = Offers.Device_Unavailable and then Report.Status_Known
        and then Report.Status_Flags = 0, "fresh status became absent after eligible path gate");
      Check (Status_Reads = 2, "policy and leaf probe observe distinct current status");
      Check (Invocations = Before, "known absent leaf must not query DSM after probe status");
      Check (Report.Count = 0 and then Report.Values = Offers.Payload_Array'[others => Builder.Wire.Empty_Request],
        "newly inactive leaf exposes no partial offer");
      Check (not Report.Recovery_Needed and then not Q.Retained_Results_Pending (Server),
        "newly inactive leaf releases retained CRS without copy or DSM");
   end Check_Inactive_Probe;
   procedure Check_Filter is
      Found : C.Namespace.Node_ID;
      Search : H.Search_Report;
      Before, Previous_Calls : Natural;
   begin
      for M in H.Excluded .. H.Recovery_Required loop
         Mode := M; Before := Evaluations; Previous_Calls := Policy_Calls;
         Filtered_Find (Server, C.Namespace.Root, 100, Found, Search);
         Check (Policy_Calls > Previous_Calls, "policy ran");
         Check (Evaluations = Before, "refusal precedes identity AML");
         Check (Found = C.Namespace.Root, "refusal publishes no device");
         Check (Search.Charged = 7 and then Search.Last_Policy.Outcome = M,
           "exact policy charge and typed reason");
         Check (Search.Outcome = (case M is
           when H.Excluded => H.No_Match,
           when H.Policy_Incomplete => H.Incomplete,
           when others => H.Failed), "search policy disposition");
      end loop;
      Mode := H.Node_Error; Before := Evaluations;
      Filtered_Find (Server, C.Namespace.Root, 100, Found, Search);
      Check (Search.Outcome = H.No_Match and then Search.Node_Errors = 1 and then Found = C.Namespace.Root,
        "ordinary node error is counted and enumeration continues");
      Check (Search.Charged = 7 and then Evaluations = Before,
        "node-error policy preserves total fuel and does not execute identity AML");
      Check (Search.Captured_Errors = 1 and then Search.Omitted_Errors = 0
        and then Search.Errors (1).Device = Descriptor and then Search.Errors (1).Kind = H.Policy_Error
        and then Search.Errors (1).Policy.Outcome = H.Node_Error and then Search.Errors (1).Policy.Charged = 7,
        "policy diagnostic is copied without inventing an identity evaluation");
      Mode := H.Eligible; Before := Evaluations;
      Filtered_Find (Server, C.Namespace.Root, 7, Found, Search);
      Check (Search.Outcome = H.Incomplete and then Search.Charged = 7,
        "policy and identity share original allowance");
      Check (Evaluations = Before, "exhausted policy leaves no AML allowance");
      Filtered_Find (Server, C.Namespace.Root, 100, Found, Search);
      Check (Search.Outcome = H.Found and then Found = C.Values.Position (Descriptor),
        "eligible policy reaches same identity");
      Check (Search.Charged > 7 and then Search.Charged <= 100, "identity charge follows policy charge");
      Overcharge := True; Before := Evaluations;
      Filtered_Find (Server, C.Namespace.Root, 100, Found, Search);
      Check (Search.Outcome = H.Failed and then Search.Charged = 0,
        "overcharge refused before addition");
      Check (Evaluations = Before, "overcharge precedes identity AML");
      Overcharge := False; Previous_Calls := Policy_Calls;
      Filtered_Find (Server, C.Namespace.Root, 0, Found, Search);
      Check (Search.Outcome = H.Incomplete and then Search.Charged = 0
        and then Policy_Calls = Previous_Calls, "zero budget invokes no callback");
      Check (not Q.Retained_Results_Pending (Server), "policy tests retain no result");
   end Check_Filter;
   Completed : Boolean;
begin
   Load (Server, Initialization); Load (Foreign_Server, Foreign_Initialization);
   Metadata.Advance (Server, Completed); Check (Completed and then Calls = 1, "read-only witness callback");
   Ada.Text_IO.Put_Line ("history=" & Result'Image);
   if Ada.Command_Line.Argument (2) in "pruned" | "nested-pruned" then Check (Result = Driver.Pruned_Path, "pruned history rejected");
   elsif Ada.Command_Line.Argument (2) in "skipped-ini" | "skipped-now-active" then Check (Result = Driver.Initialization_Not_Complete, "uncompleted declared initializer rejected");
   else Check (Result = Driver.Initialization_Ready, "initialized/no-required-init path accepted"); end if;
   Check_Filter;
   Check_Actual_Path;
   if Ada.Command_Line.Argument_Count = 3 then
      Fault := Fault_Mode'Value (Ada.Command_Line.Argument (3));
      if Fault = Status_Changes_In_Probe then Check_Inactive_Probe; else Check_Recovery; end if;
   else Check_Bound; end if;
   if Fault not in Retained_Exception | Status_Retained then
      Check (not Q.Retained_Results_Pending (Server), "no retained-result debt");
   end if;
   Ada.Text_IO.Put_Line ("Initialization path checks" & Checks'Image);
end Init_Path_Tests;
