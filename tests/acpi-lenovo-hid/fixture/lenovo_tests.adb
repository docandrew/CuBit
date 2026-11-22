pragma Ada_2022;
with ACPI_Native_HID_Detection;
with ACPI_Device_Initialization; with ACPI_Device_Policy; with ACPI_HID_Initialization;
with ACPI_Requests.With_Driver;
with ACPI_Service; with ACPI_Requests; with ACPI_Bootstrap;
with ACPI_Requests.HID_Devices; with ACPI_Requests.HID_Offers;
with ACPI_HID_I2C_Discovery; with ACPI_HID_I2C_Candidate; with ACPI_HID_I2C_Resolved;
with ACPI_HID_Offer_Builder; with ACPI_HID_Offer_Provenance; with ACPI_PCI_Observation_Reader;
with ACPI_Hosted_Heap; with AML_Objects; with Firmware_Tables; with GPIO_Bootstrap_Payload;
with Interfaces; with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
pragma Elaborate_All (ACPI_Hosted_Heap);
procedure Lenovo_Tests is
   pragma Unevaluated_Use_Of_Old (Allow);
   package C renames ACPI_Service; package Q renames ACPI_Requests;
   package IO renames Ada.Streams.Stream_IO; package G renames GPIO_Bootstrap_Payload;
   package Discovery is new ACPI_HID_I2C_Discovery (C);
   package Candidates is new ACPI_HID_I2C_Candidate;
   package Resolver is new ACPI_HID_I2C_Resolved (C, Candidates);
   package Owner is new Q.HID_Devices (Discovery, Resolver);
   package Builder is new ACPI_HID_Offer_Builder (Candidates);
   package Provenance is new ACPI_HID_Offer_Provenance (C, Candidates, Resolver, Builder);
   package Reader is new ACPI_PCI_Observation_Reader (C);
   package Collector is new Q.HID_Offers (Candidates, Discovery, Resolver, Builder, Provenance, Reader);
   package W renames Builder.Wire;
   use type Q.Outcome, Q.Phase, C.Values.Access_Status, C.Namespace.Lookup_Status;
   use type ACPI_Bootstrap.Device_Phase, Owner.Controller.Admission;
   use type Discovery.Action_Result, Discovery.Outcome, Resolver.Outcome, Collector.Outcome, W.Request;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_16;
   use type Candidates.S.Rate;
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
   end Read_Table;
   package Native renames ACPI_Native_HID_Detection;
   use type Native.Outcome, C.Node_Descriptor;
   procedure Regions (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced (Service, Budget);
   begin Charged := 0; Outcome := ACPI_Device_Policy.No_Regions_Required; end Regions;
   package Driver is new ACPI_Device_Initialization (C, Regions);
   package Controlled is new Q.With_Driver (Driver);
   Initialization : Driver.Session;
   package Paths is new ACPI_HID_Initialization (C, Driver, Native.Discovery.Queries);
   procedure Native_Policy
     (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out Native.Owner.Controller.Policy_Report) is
      View : Paths.Report;
   begin
      Paths.Check_Path (Initialization, Service, Item, Budget, View);
      Result := (Outcome => (case View.Result is
         when Paths.Eligible => Native.Owner.Controller.Eligible,
         when Paths.Excluded => Native.Owner.Controller.Excluded,
         when Paths.History_Unknown | Paths.Initialization_Incomplete | Paths.Budget_Exhausted =>
           Native.Owner.Controller.Policy_Incomplete,
         when Paths.Recovery_Required => Native.Owner.Controller.Recovery_Required,
         when others => Native.Owner.Controller.Policy_Failed), Charged => View.Charged);
   end Native_Policy;
   procedure Native_Run is new Native.Run_With_Policy (Native_Policy);
   Detection, Saved_Detection : Native.Report;
   Limits : Native.Policy;
   Expected_Inactive : constant Natural := Natural'Value (Ada.Command_Line.Argument (7));
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Address : constant Natural := Natural'Value (Ada.Command_Line.Argument (2));
   Descriptor : constant Natural := Natural'Value (Ada.Command_Line.Argument (3));
   Kind : constant String := Ada.Command_Line.Argument (4);
   Expected_Flags : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Value (Ada.Command_Line.Argument (6));
   Server : Q.State (1, Table'Length, Table'Length, 1, 0,
      Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes,
      Value_Element_Capacity => AML_Objects.Max_Elements);
   Reply : Q.Response; Probe : Owner.Controller.Probe;
   Allowed : Owner.Controller.Admission; Action : Discovery.Action_Result;
   Info : Owner.Controller.Report; Data : Firmware_Tables.Bytes (1 .. 512);
   Touchpad, GPIO : C.Node_Descriptor := C.Values.No_Node;
   Run : Collector.Run; Report : Collector.Report;
   Devices : Collector.Device_Array := [others => C.Values.No_Node];
   Stamp : constant Provenance.Stamp := (Epoch => 16#F123_4567_0000_0007#,
     Catalog_Revision => 16#FFFF_FFFF_0000_0001#, Observation_Sequence => 1, others => <>);
   Admission : G.Request;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end;
   procedure Describe (Service : C.State; Done : out Boolean) is
      Node : C.Namespace.Node_ID := C.Namespace.Root;
      Found : C.Namespace.Lookup_Result; Status : C.Values.Access_Status;
      procedure Child (Name : String) is
      begin
         C.Find_Object (Service, Node, Name, Found, Status);
         Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "captured namespace child " & Name);
         Node := Found.Node;
      end Child;
   begin
      Child ("_SB_"); Child ("GPI0"); C.Describe_Node (Service, Node, GPIO, Status);
      Check (Status = C.Values.Available, "modeled GPIO binding");
      Node := C.Namespace.Root;
      Child ("_SB_"); Child ("PC00"); Child ("I2C1"); Child ("TPD0");
      C.Describe_Node (Service, Node, Touchpad, Status); Done := Status = C.Values.Available;
   end Describe;
   package Metadata is new Q.With_Metadata_Continuation (Describe);
   procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
   begin Controlled.Handle (Server, Initialization, Q.Snapshot_Provider,
      (Label => Label, Data => [Q.Revision (Server), Value, 0, 0], others => <>), Reply);
      Check (Reply.Status = Q.OK and then Q.Valid (Server), "captured table protocol");
   end Send;
   Completed : Boolean;
begin
   Send (Q.Start_Snapshot, 1);
   Q.Import_Block (Server, Q.Snapshot_Provider, Q.Revision (Server), 1, C.DSDT, Table, Reply);
   Check (Reply.Status = Q.OK, "captured table import");
   Send (Q.Finish_Snapshot);
   Check (Q.Current (Server) = Q.Complete, "captured snapshot complete");
   Check (Natural (Q.Module_Errors (Server).Count) = 0, "no captured module execution errors");
   Check (Q.Device_Completion (Server).Stage = ACPI_Bootstrap.Devices_Completed
     and then Q.Device_Completion (Server).Errors = 0, "captured parent and touchpad INI complete");
   Limits.Candidates := 2; Limits.Fuel := Native.Maximum_AML_Fuel;
   Native_Run (Server, Detection, Limits);
   Check (not Detection.Recovery_Needed and then not Detection.Cleanup_Required
     and then not Q.Retained_Results_Pending (Server), "native enumeration leaves no ownership debt");
   Check (Natural (Detection.Policy_Excluded) >= Expected_Inactive, "inactive paths excluded before identity without consuming active capacity");
   if Kind = "pruned" then
      Ada.Text_IO.Put_Line ("native=" & Detection.Status'Image & " active=" & Detection.Observed'Image & " inactive=" & Detection.Unavailable'Image);
      Check (Detection.Status = Native.No_Devices and then Detection.Candidates = 0 and then Detection.Observed = 0,
        "pruned ancestor must exclude its uninitialized child from native discovery");
      Ada.Text_IO.Put_Line ("Lenovo HID checks" & Checks'Image); return;
   elsif Kind = "gpio" then
      Check (Detection.Status = Native.Finished and then Detection.Candidates = 1 and then Detection.Observed = 1,
        "native startup finds the active captured touchpad past inactive alternatives");
   elsif Kind = "disabled" then
      Check (Detection.Status = Native.No_Devices and then Detection.Candidates = 0 and then Detection.Observed = 0,
        "native startup reports no active devices after all inactive alternatives");
   else
      Check (Detection.Status = Native.Finished and then Detection.Candidates = 1
        and then Detection.Observed = 0 and then Detection.Failed_Candidates = 1
        and then Detection.Node_Errors = 1 and then Detection.Captured_Errors = 1
        and then Detection.Errors (1).Path_Complete,
        "unsupported GSI/legacy is retained as a device diagnostic without stopping startup");
   end if;
   Saved_Detection := Detection; Native_Run (Server, Detection, Limits);
   Check (Detection.Status = Saved_Detection.Status and then Detection.Charged = Saved_Detection.Charged
     and then Detection.Unavailable = Saved_Detection.Unavailable, "native enumeration keeps its one-shot outcome");
   Metadata.Advance (Server, Completed); Check (Completed, "same-Core captured bindings");
   Owner.Begin_Probe (Probe, Server, C.Values.Position (Touchpad), 100_000, Allowed, Action);
   Check (Allowed = Owner.Controller.Admitted and then Action = Discovery.Accepted, "captured probe admitted");
   if Owner.Controller.Pending (Probe) then Owner.Copy_And_Resolve (Probe, Server, Data, Allowed, Action); end if;
   Info := Owner.Controller.Observe (Probe);
   Check (Info.Firmware.Status.Flags = Expected_Flags, "complete observed status word preserves AML width");
   Ada.Text_IO.Put_Line ("probe=" & Info.Firmware.Result'Image & " step=" & Info.Firmware.Step'Image &
     " exec=" & Info.Firmware.Execution'Image & " descriptor=" & Info.Firmware.Descriptor.Execution'Image);
   Check (not Owner.Controller.Pending (Probe) and then not Q.Retained_Results_Pending (Server), "captured probe releases all results");
   if Kind = "gpio" then
      Check (Info.Firmware.Result = Discovery.Observed and then Info.Sources_Attempted
        and then Info.Sources.Status = Resolver.Resolved, "captured dynamic resources and shared HIDD observed");
      declare Facts : constant Candidates.Candidate := Resolver.Facts (Info.Sources.Value); begin
         Check (Natural (Facts.I2C.Address) = Address and then Natural (Facts.Descriptor_Register) = Descriptor
           and then Facts.I2C.Connection_Speed = 400000 and then Natural (Facts.Pin) = 160,
           "captured INI mutations and DSM descriptor propagate");
      end;
   elsif Kind = "disabled" then
      Check (Info.Firmware.Result = Discovery.Observed and then Info.Sources_Attempted
        and then (not Info.Firmware.Status.Present or else not Info.Firmware.Status.Enabled),
        "disabled captured touchpad remains a descriptive observation, not admission");
   else
      Check (Info.Firmware.Result = Discovery.Analysis_Failed and then not Info.Sources_Attempted,
        "captured GSI/legacy path remains explicitly unsupported, never converted to a GPIO pin");
   end if;
   declare F : IO.File_Type;
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Info.Firmware.Copy.Copied));
   begin
      for I in Raw'Range loop Raw (I) := Ada.Streams.Stream_Element (Data (Positive (I))); end loop;
      IO.Create (F, IO.Out_File, Ada.Command_Line.Argument (5)); IO.Write (F, Raw); IO.Close (F);
   end;
   Admission := (Spans => 1, Segments => 2, HID => "INTC1057", STA => 15,
     Table => "DSDT", Table_ID => "LENTPD00", Epoch => Stamp.Epoch, others => <>);
   Admission.Path (1) := "_SB_"; Admission.Path (2) := "GPI0";
   Admission.Items (1) := (Base => 16#FD6E0000#, Length => 65536, Writable => True, others => <>);
   Devices (1) := Touchpad;
   Collector.Collect (Run, Server, Devices, 1, GPIO, Admission, Stamp, 100_000, 512, Report);
   Check (not Report.Recovery_Needed and then not Q.Retained_Results_Pending (Server), "captured fresh collection has no recovery debt");
   if Kind = "gpio" then
      Check (Report.Status_Known and then Report.Status_Flags = Expected_Flags, "complete handoff status word preserved");
      Check (Report.Status = Collector.Prepared and then Report.Count = 1 and then W.Valid (Report.Values (1)), "captured offer prepared");
      Check (Report.Values (1).Device_Path.Count = 4 and then Report.Values (1).Device_Path.Names (4) = "TPD0"
        and then Report.Values (1).I2C_Path.Count = 3 and then Report.Values (1).GPIO_Path.Count = 2,
        "actual captured controller-source paths resolved");
      Check (Report.Values (1).PCI.Has_SEG and then Report.Values (1).PCI.Has_BBN and then Report.Values (1).PCI.Has_ADR
        and then Report.Values (1).PCI.SEG = 0 and then Report.Values (1).PCI.BBN = 0
        and then Report.Values (1).PCI.ADR = 16#150001#, "captured BBN helper and conditional ADR observed");
      Check (Natural (Report.Values (1).Address) = Address and then Natural (Report.Values (1).Descriptor_Register) = Descriptor
        and then Report.Values (1).Pin = 160, "fresh captured handoff preserves vendor-specific values");
   elsif Kind = "disabled" then
      Check (Report.Status = Collector.Device_Unavailable and then Report.Count = 0
        and then Report.Status_Known and then Report.Status_Flags = Expected_Flags, "disabled captured status prevents handoff");
      for V of Report.Values loop Check (V = W.Empty_Request, "disabled device exports no partial payload"); end loop;
   else
      Check (Report.Status = Collector.Prepared and then Report.Count = 0
        and then Report.Node_Errors = 1 and then Report.Captured_Errors = 1
        and then Report.Errors (1).Firmware.Result = Discovery.Analysis_Failed
        and then Report.Errors (1).Device = Touchpad and then Report.Errors (1).Path_Complete,
        "unsupported captured path yields one copied diagnostic and no offer");
      for V of Report.Values loop Check (V = W.Empty_Request, "no partial captured payload"); end loop;
   end if;
   Ada.Text_IO.Put_Line ("Lenovo HID checks" & Checks'Image);
end Lenovo_Tests;
