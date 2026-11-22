pragma Ada_2022;
with ACPI_PCI_Observation_Reader; with ACPI_Requests.HID_Offers;
with ACPI_Service; with ACPI_Requests; with ACPI_Requests.HID_Devices;
with ACPI_HID_I2C_Discovery; with ACPI_HID_I2C_Candidate; with ACPI_HID_I2C_Resolved;
with ACPI_HID_Offer_Builder; with ACPI_HID_Offer_Provenance;
with GPIO_Bootstrap_Payload; with AML_Objects;
with ACPI_Hosted_Heap; with Firmware_Tables; with Interfaces;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
pragma Elaborate_All (ACPI_Hosted_Heap);
procedure Collection_Tests is
   package C renames ACPI_Service; package Q renames ACPI_Requests;
   package IO renames Ada.Streams.Stream_IO; package G renames GPIO_Bootstrap_Payload;
   Evaluations, Invocations, Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end;
   procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Evaluations := Evaluations + 1; C.Evaluate_Named (Service, Item, Budget, Result, Status); end;
   procedure Invoke (Service : aliased in out C.State; Node : C.Namespace.Node_ID;
      Args : C.Values.Arguments; Argument_Count : C.Values.Argument_Count;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Invocations := Invocations + 1;
      C.Invoke_With_Arguments (Service, Node, Args, Argument_Count, Budget, Result, Status); end;
   package Discovery is new ACPI_HID_I2C_Discovery (C, Invoke, Evaluate);
   package Candidates is new ACPI_HID_I2C_Candidate;
   package Resolver is new ACPI_HID_I2C_Resolved (C, Candidates);
   package Owner is new Q.HID_Devices (Discovery, Resolver);
   package Builder is new ACPI_HID_Offer_Builder (Candidates);
   package P is new ACPI_HID_Offer_Provenance (C, Candidates, Resolver, Builder);
   package Reader is new ACPI_PCI_Observation_Reader (C, Evaluate);
   package Collector is new Q.HID_Offers (Candidates, Discovery, Resolver, Builder, P, Reader);
   procedure Node_Error_Policy (Service : aliased in out C.State; Device : C.Node_Descriptor;
      Budget : Natural; Result : out Collector.HID.Controller.Policy_Report) is
      pragma Unreferenced (Service, Budget);
   begin Result := (Outcome => (if C.Values.Node_Name (Device) = "TPD0" then Collector.HID.Controller.Node_Error
      else Collector.HID.Controller.Eligible), Charged => 0); end;
   procedure Collect_Node_Error is new Collector.Collect_With_Policy (Node_Error_Policy);
   procedure All_Node_Errors (Service : aliased in out C.State; Device : C.Node_Descriptor;
      Budget : Natural; Result : out Collector.HID.Controller.Policy_Report) is
      pragma Unreferenced (Service, Device, Budget);
   begin Result := (Collector.HID.Controller.Node_Error, 0); end;
   procedure Collect_All_Errors is new Collector.Collect_With_Policy (All_Node_Errors);
   type Run_Array is array (Positive range 1 .. 16) of Collector.Run;
   Collections : Run_Array;
   Collected, Again : Collector.Report;
   Devices : Collector.Device_Array := [others => C.Values.No_Node];
   Diagnostic_Devices : Collector.Device_Array := [others => C.Values.No_Node];
   Second, Unresolved, Zero_Address : C.Node_Descriptor := C.Values.No_Node;
   use type Collector.Outcome, Collector.Payload_Array, Collector.Error_Array, Collector.Error_Kind;
   use type Collector.HID.Controller.Policy_Outcome, Reader.Topology_Outcome;
   package W renames Builder.Wire;
   use type Q.Outcome, Q.Phase, Q.Revision_Number, C.Values.Access_Status, C.Node_Descriptor;
   use type C.Namespace.Lookup_Status, Resolver.Outcome, Discovery.Outcome;
   use type Owner.Controller.Admission, Discovery.Action_Result;
   use type P.Outcome, Builder.Outcome, W.Request, W.UID_Kind;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      declare Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset; Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File);
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Server, Foreign_Server : Q.State (1, Table'Length, Table'Length, 1, 0,
      Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes,
      Value_Element_Capacity => AML_Objects.Max_Elements);
   Reply : Q.Response; Run, Pending : Owner.Controller.Probe;
   Allowed : Owner.Controller.Admission; Action : Discovery.Action_Result;
   Info : Owner.Controller.Report; Data : Firmware_Tables.Bytes (1 .. 512);
   Touchpad, Other : C.Node_Descriptor := C.Values.No_Node;
   Admission_Device : C.Node_Descriptor := C.Values.No_Node;
   Admission : G.Request;
   Observation : Resolver.Observation := Resolver.Empty_Observation;
   History : P.Stamp := (Epoch => 16#F123_4567_0000_0007#, Catalog_Revision => 16#FFFF_FFFF_0000_0001#,
      Observation_Sequence => 16#FFFF_FFFF_FFFF_FFFF#, Initializer_Count => 17,
      Initializer_Digest => 16#F111_2222_3333_4444#);
   PCI : W.PCI_Observation := (True, True, True, 0, 0, 16#150001#);
   Built : Builder.Report; Status : P.Outcome;
   Expected : P.Outcome := P.Prepared;
   procedure Describe (Service : C.State; Completed : out Boolean) is
      Found : C.Namespace.Lookup_Result; Access_Result : C.Values.Access_Status;
   begin
      C.Find_Object (Service, C.Namespace.Root, "TPD0", Found, Access_Result);
      Completed := Access_Result = C.Values.Available and then Found.Status = C.Namespace.Found;
      if Completed then C.Describe_Node (Service, Found.Node, Touchpad, Access_Result);
         Completed := Access_Result = C.Values.Available; end if;
   end Describe;
   package Metadata is new Q.With_Metadata_Continuation (Describe);
   procedure Assemble (Service : C.State; Completed : out Boolean) is
      Before_Evaluations : constant Natural := Evaluations;
      Before_Invocations : constant Natural := Invocations;
   begin
      P.Build (Service, Observation, Admission_Device, Admission, PCI, History, Built, Status);
      Check (Status = Expected, "provenance outcome");
      Check (Evaluations = Before_Evaluations and then Invocations = Before_Invocations, "metadata assembly executes no AML");
      if Expected = P.Prepared then Check (Built.Status = Builder.Built and then W.Valid (Built.Value), "canonical offer");
      else Check (Built.Value = W.Empty_Request, "no partial offer on refusal"); end if;
      Completed := True;
   end Assemble;
   package Assembly is new Q.With_Metadata_Continuation (Assemble);
   procedure Load (Target : in out Q.State) is
      procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
      begin Q.Handle (Target, Q.Snapshot_Provider,
         (Label => Label, Data => [Q.Revision (Target), Value, 0, 0], others => <>), Reply);
         Check (Reply.Status = Q.OK and then Q.Valid (Target), "snapshot protocol"); end;
   begin
      Send (Q.Start_Snapshot, 1);
      Q.Import_Block (Target, Q.Snapshot_Provider, Q.Revision (Target), 1, C.DSDT, Table, Reply);
      Check (Reply.Status = Q.OK, "table imported");
      Send (Q.Finish_Snapshot); Check (Q.Current (Target) = Q.Complete, "snapshot completed");
   end Load;
   procedure Describe_Others (Service : C.State; Completed : out Boolean) is
      Found : C.Namespace.Lookup_Result; Access_Result : C.Values.Access_Status;
   begin
      C.Find_Object (Service, C.Namespace.Root, "TPD1", Found, Access_Result);
      C.Describe_Node (Service, Found.Node, Second, Access_Result);
      Check (Access_Result = C.Values.Available, "second actual candidate");
      C.Find_Object (Service, C.Namespace.Root, "TPD2", Found, Access_Result);
      C.Describe_Node (Service, Found.Node, Unresolved, Access_Result);
      Completed := Access_Result = C.Values.Available;
      C.Find_Object (Service, C.Namespace.Root, "ZPAD", Found, Access_Result);
      if Access_Result /= C.Values.Available or else Found.Status /= C.Namespace.Found then Completed := False; return; end if;
      C.Describe_Node (Service, Found.Node, Zero_Address, Access_Result);
      if Access_Result /= C.Values.Available then Completed := False; return; end if;
      for I in 1 .. 9 loop
         declare Name : constant String := "DG0" & Character'Val (Character'Pos ('0') + I - 1);
         begin
            C.Find_Object (Service, C.Namespace.Root, Name, Found, Access_Result);
            if Access_Result /= C.Values.Available or else Found.Status /= C.Namespace.Found then
               Completed := False; return;
            end if;
            C.Describe_Node (Service, Found.Node, Diagnostic_Devices (I), Access_Result);
            if Access_Result /= C.Values.Available then Completed := False; return; end if;
         end;
      end loop;
   end Describe_Others;
   package More_Metadata is new Q.With_Metadata_Continuation (Describe_Others);
   Completed : Boolean;
   procedure Test (Outcome : P.Outcome := P.Prepared) is
      Revision : constant Q.Revision_Number := Q.Revision (Server);
   begin Expected := Outcome; Assembly.Advance (Server, Completed);
      Check (Completed and then Q.Valid (Server) and then Q.Revision (Server) = Revision, "readonly metadata continuation"); end;
begin
   Load (Server); Load (Foreign_Server);
   Metadata.Advance (Foreign_Server, Completed); Check (Completed, "foreign metadata"); Other := Touchpad;
   Metadata.Advance (Server, Completed); Check (Completed, "same Core metadata");
   Owner.Begin_Probe (Run, Server, C.Values.Position (Touchpad), 4000, Allowed, Action);
   Check (Allowed = Owner.Controller.Admitted and then Action = Discovery.Accepted, "HID probe");
   Owner.Copy_And_Resolve (Run, Server, Data, Allowed, Action); Info := Owner.Controller.Observe (Run);
   Check (Info.Firmware.Result = Discovery.Observed and then Info.Sources.Status = Resolver.Resolved
     and then Owner.Current (Run, Server) and then not Q.Retained_Results_Pending (Server), "actual completed same-Core HID observation");
   Observation := Info.Sources.Value; Admission_Device := Resolver.GPIO_Controller (Observation);
   Admission := (Spans => 1, Segments => 1, HID => "INTC1057", STA => 15,
      UID_Type => G.Integer_UID, UID => 16#FFFF_FFFF_FFFF_FFFE#, Table => "DSDT",
      Table_ID => "HIDOWNER", Epoch => History.Epoch, others => <>);
   Admission.Path (1) := "GPIO";
   Admission.Items (1) := (Base => 16#FD6E0000#, Length => 65_536, Writable => True, others => <>);
   Check (G.Valid (Admission), "modeled prior GPIO request is canonical, not an admission claim");
   Test;
   Check (Built.Value.Device_Path.Count = 1 and then Built.Value.Device_Path.Names (1) = "TPD0"
     and then Built.Value.I2C_Path.Count = 2 and then Built.Value.I2C_Path.Names (1) = "PCI0"
     and then Built.Value.I2C_Path.Names (2) = "I2C0"
     and then Built.Value.GPIO_Path.Count = 1 and then Built.Value.GPIO_Path.Names (1) = "GPIO", "paths from actual Core");
   Check (Built.Value.Table = "DSDT" and then Built.Value.Table_ID = "HIDOWNER", "actual device defining table");
   Check (Built.Value.GPIO_UID = Admission.UID and then Built.Value.GPIO_UID_Kind = W.Integer_UID
     and then Built.Value.Epoch = History.Epoch and then Built.Value.Catalog_Revision = History.Catalog_Revision
     and then Built.Value.Observation_Sequence = History.Observation_Sequence, "full-width provenance without narrowing");
   Admission.UID_Type := G.String_UID; Admission.UID := 16#FF80_FE81_FD82_FC83#; Test;
   Check (Built.Value.GPIO_UID = Admission.UID and then Built.Value.GPIO_UID_Kind = W.String_UID, "opaque UID bytes preserved");
   PCI.Has_BBN := False; Test;
   Check (not Built.Value.PCI.Has_BBN and then Built.Value.PCI.BBN = 0, "unresolved bus stays unresolved");
   PCI.BBN := 1; Test (P.Builder_Refused);
   PCI.BBN := 0; PCI.Has_BBN := True;
   Admission.Path (1) := "FAKE"; Test (P.GPIO_Binding_Mismatch); Admission.Path (1) := "GPIO";
   Admission.Epoch := History.Epoch - 1; Test (P.Invalid_Admission); Admission.Epoch := History.Epoch;
   Admission.Spans := 0; Test (P.Invalid_Admission); Admission.Spans := 1;
   Admission_Device := Other; Test (P.Stale_Observation);
   Admission_Device := Resolver.I2C_Controller (Observation); Test (P.GPIO_Binding_Mismatch);
   Admission_Device := Resolver.GPIO_Controller (Observation);
   Observation := Resolver.Empty_Observation; Test (P.Stale_Observation); Observation := Info.Sources.Value;
   History.Epoch := 0; Admission.Epoch := 0; Test (P.Builder_Refused);
   History.Epoch := 16#F123_4567_0000_0007#; Admission.Epoch := History.Epoch;
   Owner.Begin_Probe (Pending, Server, C.Values.Position (Touchpad), 4000, Allowed, Action);
   Check (Owner.Controller.Pending (Pending), "sibling retains CRS"); Test (P.Owner_Busy);
   Owner.Cancel (Pending, Server, Action); Check (not Q.Retained_Results_Pending (Server), "sibling cleanup");
   Test;
   More_Metadata.Advance (Server, Completed); Check (Completed, "additional descriptors");
   Devices (1) := Touchpad; Devices (2) := Second;
   declare
      Revision : constant Q.Revision_Number := Q.Revision (Server);
      Before : Natural := Evaluations + Invocations;
      Full_Charge : Natural;
      procedure Empty_Offers is
      begin
         Check (Collected.Count = 0, "failure publishes no row");
         for V of Collected.Values loop Check (V = W.Empty_Request, "failure erases all payload storage"); end loop;
         Check (not Q.Retained_Results_Pending (Server), "failed collection releases roots");
         Check (Q.Valid (Server) and then Q.Revision (Server) = Revision, "collection preserves owner revision");
      end Empty_Offers;
   begin
      Collector.Collect (Collections (1), Server, Devices, 2, Admission_Device, Admission, History,
        10000, 512, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Count = 2
        and then Collected.Collected = 2 and then not Collected.Working
        and then not Collected.Recovery_Needed, "all candidates prepared together");
      Check (Collected.Charged <= 10000 and then not Q.Retained_Results_Pending (Server), "shared fuel and released roots");
      Full_Charge := Collected.Charged;
      Check (Collected.Captured_Errors = 0 and then Collected.Omitted_Errors = 0,
        "clean collection invents no error records");
      Check (Full_Charge > 0, "fixture exercises charged AML methods");
      Check (Evaluations + Invocations > Before, "fresh probes execute actual AML");
      for I in 1 .. 2 loop
         Check (W.Valid (Collected.Values (I)) and then Collected.Values (I).PCI.Has_SEG
           and then Collected.Values (I).PCI.Has_BBN and then Collected.Values (I).PCI.Has_ADR
           and then Collected.Values (I).PCI.ADR = 16#150001#,
           "wire descriptions contain actual observed PCI methods");
      end loop;
      Check (Collected.Values (1).Device_Path.Names (1) = "TPD0"
        and then Collected.Values (2).Device_Path.Names (1) = "TPD1", "two immutable distinct rows");
      Before := Evaluations + Invocations;
      Collector.Collect (Collections (1), Foreign_Server, Devices, 0, Other, Admission, History, 0, 1, Again);
      Check (Again.Status = Collected.Status and then Again.Count = Collected.Count
        and then Again.Values = Collected.Values and then Again.Charged = Collected.Charged
        and then Again.Errors = Collected.Errors
        and then Evaluations + Invocations = Before, "one-shot latch does not replay or refresh on another Core");
      Devices (2) := Touchpad;
      Collector.Collect (Collections (2), Server, Devices, 2, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Duplicate_Device and then Evaluations + Invocations = Before,
        "duplicate rejection before AML"); Empty_Offers;
      Devices (2) := Other;
      Collector.Collect (Collections (3), Server, Devices, 2, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Stale_Device and then Collected.Collected = 1,
        "later foreign candidate discards earlier prepared offer"); Empty_Offers;
      Collector.Collect (Collections (4), Server, Devices, 1, Admission_Device, Admission, History, 0, 512, Collected);
      Check (Collected.Status = Collector.Budget_Exhausted and then Collected.Charged = 0, "zero fuel never replenished"); Empty_Offers;
      Collector.Collect (Collections (5), Server, Devices, 1, Admission_Device, Admission, History, 10000, 1, Collected);
      Check (Collected.Status = Collector.Capacity_Refused and then not Collected.Recovery_Needed,
        "small buffer releases retained CRS without retrying AML"); Empty_Offers;
      Admission.Path (1) := "FAKE";
      Collector.Collect (Collections (6), Server, Devices, 1, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Description_Refused, "mismatched admitted GPIO facts refused"); Empty_Offers;
      Admission.Path (1) := "GPIO";
      Before := Evaluations + Invocations;
      Collector.Collect (Collections (7), Server, Devices, 0, Admission_Device, Admission, History, 0, 1, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Count = 0
        and then Evaluations + Invocations = Before, "empty batch executes no AML");
      Owner.Begin_Probe (Pending, Server, C.Values.Position (Touchpad), 4000, Allowed, Action);
      Check (Owner.Controller.Pending (Pending), "pending sibling before collection");
      Before := Evaluations + Invocations;
      Collector.Collect (Collections (8), Server, Devices, 1, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Owner_Busy and then Evaluations + Invocations = Before
        and then Owner.Controller.Pending (Pending), "sibling result neither replaced nor evaluated over");
      Owner.Cancel (Pending, Server, Action); Empty_Offers;
      Devices (2) := Unresolved;
      Collector.Collect (Collections (9), Server, Devices, 2, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Count = 1
        and then Collected.Collected = 1 and then Collected.Node_Errors = 1,
        "ordinary later topology error preserves earlier complete row");
      Check (Collected.Captured_Errors = 1 and then Collected.Omitted_Errors = 0
        and then Collected.Errors (1).Input_Index = 2 and then Collected.Errors (1).Device = Unresolved
        and then Collected.Errors (1).Path_Complete and then Collected.Errors (1).Kind = Collector.Topology_Error
        and then Collected.Errors (1).Topology.Status /= Reader.Topology_Observed
        and then Collected.Errors (1).Charged > 0,
        "later error retains its own input identity, copied path, topology reason and charge");
      Check (W.Valid (Collected.Values (1)) and then Collected.Values (1).Device_Path.Names (1) = "TPD0", "earlier row preserved exactly");
      for I in 2 .. Collector.Candidate_Index'Last loop Check (Collected.Values (I) = W.Empty_Request, "skipped row publishes no partial payload"); end loop;
      Check (not Q.Retained_Results_Pending (Server) and then Q.Revision (Server) = Revision, "partial success preserves owner and clears roots");
      Devices (2) := Second;
      Collector.Collect (Collections (10), Server, Devices, 2, Admission_Device, Admission, History,
        Full_Charge - 1, 512, Collected);
      Check (Collected.Status = Collector.Budget_Exhausted and then Collected.Charged <= Full_Charge - 1
        and then not Collected.Recovery_Needed, "budget shared across candidates and topology, never renewed");
      Empty_Offers;
      Devices (1) := Unresolved; Devices (2) := Second;
      Collector.Collect (Collections (11), Server, Devices, 2, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Count = 1 and then Collected.Node_Errors = 1
        and then not Collected.Recovery_Needed, "ordinary first topology error permits later device");
      Check (Collected.Captured_Errors = 1 and then Collected.Errors (1).Input_Index = 1
        and then Collected.Errors (1).Device = Unresolved and then Collected.Errors (1).Path_Complete,
        "compaction does not relabel the failed input as the later successful device");
      Check (W.Valid (Collected.Values (1)) and then Collected.Values (1).Device_Path.Names (1) = "TPD1", "later row compacted to first output slot");
      for I in 2 .. Collector.Candidate_Index'Last loop Check (Collected.Values (I) = W.Empty_Request, "compacted output tail is empty"); end loop;
      Check (not Q.Retained_Results_Pending (Server) and then Q.Revision (Server) = Revision, "later row has final owner/root validation");
      Collector.Collect (Collections (12), Server, Devices, 1, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Node_Errors = 1 and then not Collected.Recovery_Needed,
        "all ordinary errors are completed known failure, not owner uncertainty"); Empty_Offers;
      Check (Collected.Captured_Errors = 1 and then Collected.Errors (1).Device = Unresolved,
        "all-failed collection preserves diagnostics despite having no payloads");
      Devices (1) := Touchpad; Devices (2) := Second;
      Collect_Node_Error (Collections (13), Server, Devices, 2, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Count = 1 and then Collected.Node_Errors = 1,
        "known policy node error skips first device and continues fresh collection");
      Check (Collected.Captured_Errors = 1 and then Collected.Errors (1).Kind = Collector.Policy_Error
        and then Collected.Errors (1).Policy.Outcome = Collector.HID.Controller.Node_Error
        and then Collected.Errors (1).Firmware.Result = Discovery.Not_Attempted
        and then Collected.Errors (1).Device = Touchpad and then Collected.Errors (1).Path_Complete,
        "policy failure cannot inherit a previous candidate's probe status");
      Check (W.Valid (Collected.Values (1)) and then Collected.Values (1).Device_Path.Names (1) = "TPD1", "policy error cannot publish first device's data");
      for I in 2 .. Collector.Candidate_Index'Last loop Check (Collected.Values (I) = W.Empty_Request, "policy skip leaves output tail empty"); end loop;
      Check (not Q.Retained_Results_Pending (Server) and then Q.Revision (Server) = Revision, "policy skip retains no resources");
      Before := Evaluations + Invocations;
      Collect_All_Errors (Collections (14), Server, Diagnostic_Devices, 9,
        Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Count = 0
        and then Collected.Node_Errors = 9 and then Collected.Captured_Errors = 8
        and then Collected.Omitted_Errors = 1, "nine failed inputs retain eight rows and one omitted record");
      Check (Evaluations + Invocations = Before, "error diagnostics and paths execute no AML");
      for I in Collector.Error_Index loop
         Check (Collected.Errors (I).Input_Index = I
           and then Collected.Errors (I).Device = Diagnostic_Devices (I)
           and then Collected.Errors (I).Path_Complete and then Collected.Errors (I).Charged = 0,
           "bounded records keep original order, identity, path and exact policy-only charge");
      end loop;
      Empty_Offers;
      Devices (1) := Zero_Address; Devices (2) := Second;
      Collector.Collect (Collections (15), Server, Devices, 2, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Count = 1
        and then Collected.Node_Errors = 1 and then not Collected.Recovery_Needed,
        "known zero-address resource rejection does not hide later valid device");
      Check (W.Valid (Collected.Values (1)) and then Collected.Values (1).Device_Path.Names (1) = "TPD1",
        "only valid later device is compacted for handoff");
      Check (Collected.Captured_Errors = 1 and then Collected.Errors (1).Device = Zero_Address
        and then Collected.Errors (1).Kind = Collector.Description_Error and then Collected.Errors (1).Path_Complete,
        "unsupported description error identifies its own failed input");
      Check (not Q.Retained_Results_Pending (Server) and then Q.Revision (Server) = Revision,
        "known unsupported description retains no Core result and no allowance renewal");
      Devices (1) := Touchpad; Devices (2) := Zero_Address;
      Collector.Collect (Collections (16), Server, Devices, 2, Admission_Device, Admission, History, 10000, 512, Collected);
      Check (Collected.Status = Collector.Prepared and then Collected.Count = 1 and then Collected.Node_Errors = 1
        and then Collected.Values (1).Device_Path.Names (1) = "TPD0", "later zero-address description preserves earlier valid offer");
      Check (Collected.Errors (1).Input_Index = 2 and then Collected.Errors (1).Device = Zero_Address,
        "compacted offers keep original failed-input order in diagnostics");
   end;
   Ada.Text_IO.Put_Line ("HID collection checks" & Checks'Image);
end Collection_Tests;
