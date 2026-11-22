pragma Ada_2022;
with ACPI_Service; with ACPI_Device_Initialization; with ACPI_Device_Policy;
with ACPI_Hosted_Heap; with Firmware_Tables; with AML_Execute; with AML_Names;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
pragma Elaborate_All (ACPI_Hosted_Heap);
procedure Node_Errors_Tests is
   package C renames ACPI_Service;
   package IO renames Ada.Streams.Stream_IO;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status;
   use type C.Namespace.Lookup_Status, AML_Execute.Execution_Status;
   Service : aliased C.State (1, 4096, 4096, 1, 256, 4096, 256);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   procedure Regions (S : aliased in out C.State; Budget : Natural;
     Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced (S, Budget);
   begin Charged := 0; Outcome := ACPI_Device_Policy.No_Regions_Required; end Regions;
   package Driver is new ACPI_Device_Initialization (C, Regions);
   use type Driver.Phase, Driver.Error_Count, Driver.Step_Result, Driver.Path_Outcome;
   Run : Driver.Session;
   Accepted : Boolean;
   function Child (Parent : C.Namespace.Node_ID; Name : AML_Names.Segment) return C.Namespace.Node_ID is
      Found : C.Namespace.Lookup_Result; Status : C.Values.Access_Status;
   begin
      C.Find_Object (Service, Parent, Name, Found, Status);
      Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "known fixture member");
      return Found.Node;
   end Child;
   function Counter (Name : AML_Names.Segment) return Natural is
      Item : C.Node_Descriptor; Status : C.Values.Access_Status; Value : C.Values.Result;
   begin
      C.Describe_Node (Service, Child (C.Namespace.Root, Name), Item, Status);
      Check (Status = C.Values.Available, "counter metadata");
      C.Evaluate_Named (Service, Item, 100, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned, "copied integer counter");
      return Natural (Value.Number);
   end Counter;
   function Describe (Node : C.Namespace.Node_ID) return C.Node_Descriptor is
      Item : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, "node description"); return Item; end Describe;
   F : IO.File_Type; Raw : Ada.Streams.Stream_Element_Array (1 .. 4096);
   Last : Ada.Streams.Stream_Element_Offset; Data : Firmware_Tables.Bytes (1 .. 4096);
   Stage : C.Stage_Status; Freeze : C.Freeze_Status; Execution : C.Execution_Report;
   Members : C.Namespace.Initialization_Report; Status : C.Values.Access_Status;
   Bus, Failed_INI, Failed_STA : C.Namespace.Node_ID;
begin
   IO.Open (F, IO.In_File, Ada.Command_Line.Argument (1)); IO.Read (F, Raw, Last); IO.Close (F);
   for I in 1 .. Natural (Last) loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
   C.Stage_Table (Service, 1, C.DSDT, Data (1 .. Natural (Last)), Stage); Check (Stage = C.Staged, "fixture table staged");
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "fixture catalog frozen");
   C.Execute_Frozen (Service, 100_000, Execution, Status); Check (Status = C.Values.Available, "real modules execute");
   C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "real members initialized");
   Bus := Child (C.Namespace.Root, "_SB_"); Failed_INI := Child (Bus, "BAD0"); Failed_STA := Child (Bus, "STAF");
   Driver.Begin_Initialization (Run, Service, 10_000, Accepted); Check (Accepted, "actual driver begins once");
   Driver.Complete_Initialization (Run, Service, Accepted); Check (Accepted, "actual driver completes once");
   Check (Driver.Observe (Run).Stage = Driver.Completed_With_Errors and then Driver.Observe (Run).Errors = 2,
     "ordinary INI and STA errors complete the walk with errors");
   Check (Counter ("BEFC") = 1 and then Counter ("ECNT") = 1 and then Counter ("CCNT") = 1
     and then Counter ("SCNT") = 1 and then Counter ("ISTA") = 1 and then Counter ("SSTA") = 1 and then Counter ("FCNT") = 1
     and then Counter ("ACNT") = 1 and then Counter ("POIS") = 0,
     "failed INI and STA preserve later children/siblings; failed STA skips own INI");
   Check (Driver.Observe_Node (Run, Failed_INI).Initialization = Driver.Evaluation_Failed
     and then Driver.Observe_Node (Run, Failed_STA).Status_Evaluation = Driver.Evaluation_Failed
     and then Driver.Observe_Node (Run, Failed_STA).Initialization = Driver.Not_Attempted,
     "known failures are distinct from successful or unattempted methods");
   Check (Driver.Initialization_Path (Run, Service, Describe (Child (Failed_INI, "CHD0"))) = Driver.Initialization_With_Errors
     and then Driver.Initialization_Path (Run, Service, Describe (Child (Failed_STA, "KID0"))) = Driver.Initialization_With_Errors
     and then Driver.Initialization_Path (Run, Service, Describe (Child (Bus, "AFT0"))) = Driver.Initialization_Ready,
     "known failed ancestor attempts are distinguished from unaffected complete paths");
   Check (Driver.Captured_Errors (Run) = 2 and then Driver.Omitted_Errors (Run) = 0, "exact bounded error accounting");
   declare A : constant Driver.Error_Report := Driver.Error_At (Run, 1);
      B : constant Driver.Error_Report := Driver.Error_At (Run, 2);
   begin
      Check (A.Scope = Failed_INI and then A.Member = "_INI" and then A.Method = Child (Failed_INI, "_INI")
        and then B.Scope = Failed_STA and then B.Member = "_STA" and then B.Method = Child (Failed_STA, "_STA"),
        "failure records identify actual scope and member in traversal order");
      Check (A.Failure = AML_Execute.Division_By_Zero and then B.Failure = AML_Execute.Division_By_Zero
        and then A.Evaluation = AML_Execute.Division_By_Zero and then B.Evaluation = AML_Execute.Division_By_Zero
        and then A.Access_Result = C.Values.Available and then B.Access_Result = C.Values.Available
        and then A.Reported_Charge > 0 and then B.Reported_Charge > 0,
        "failed method status/access/fuel are copied without pretending success");
   end;
   Check (C.Retained_Results (Service) = 0, "ordinary error completion has no retained-result debt");
   Driver.Complete_Initialization (Run, Service, Accepted); Check (not Accepted, "completed-with-errors cannot replay startup");
   Ada.Text_IO.Put_Line ("Node error checks" & Checks'Image);
end Node_Errors_Tests;
