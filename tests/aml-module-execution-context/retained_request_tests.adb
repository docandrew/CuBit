pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with ACPI_Service;
with ACPI_Retained_Requests;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
with Firmware_Tables.Identifiers;
procedure Retained_Request_Tests is
   package C renames ACPI_Service;
   package V renames C.Values;
   package N renames C.Namespace;
   package Requests is new ACPI_Retained_Requests (C);
   use type Ada.Streams.Stream_Element_Offset;
   use type AML_Decode.Integer_Value, AML_Decode.Byte;
   use type AML_Execute.Execution_Status;
   use type V.Access_Status, V.Module_Outcome, V.Append_Outcome;
   use type V.Audit_Value, V.Value_Handle;
   use type C.Stage_Status, C.Freeze_Status, C.Import_Phase;
   use type C.Retained_Load_Outcome, C.Retained_Table_Phase;
   use type Requests.Request_Outcome, Requests.Paths.Selection_Status;
   use type N.Lookup_Status;
   use type N.Definition_Identity;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Read_Table (Argument : Positive) return Firmware_Tables.Bytes is
      package IO renames Ada.Streams.Stream_IO;
      F : IO.File_Type;
   begin
      IO.Open (F, IO.In_File, Ada.Command_Line.Argument (Argument));
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (F)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (F, Raw, Last); IO.Close (F); Check (Last = Raw'Last, "whole copied fixture");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Boot : constant Firmware_Tables.Bytes := Read_Table (1);
   Added : constant Firmware_Tables.Bytes := Read_Table (2);
   Mode : constant String := Ada.Command_Line.Argument (3);
   Service, Other : aliased C.State (3, Boot'Length + 2 * Added'Length, Positive'Max (Boot'Length, Added'Length), 3);
   Wanted : constant Firmware_Tables.Identifiers.Selection := (Name => "OEM1", others => <>);
   Missing : constant Firmware_Tables.Identifiers.Selection := (Name => "oem1", others => <>);
   Status : V.Access_Status;
   Report : Requests.Report;
   Root, Caller, Device, Destination, Target, Getter, Foreign : C.Node_Descriptor;
   Counter, Observed : N.Node_ID;
   Held, Foreign_Held : V.Value_Handle;
   Result : V.Result;
   Budget : Natural := 10000;
   Source : V.Argument := (V.Immediate_Argument, 16#99#);
   Parameter_Path : constant String := (if Mode = "no-parameter" then ""
      elsif Mode = "store-fail" then "\PDST" else "PRM0");
   procedure Initialize (S : in out C.State) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Executed : C.Execution_Report;
      Initial : N.Initialization_Report;
   begin
      C.Stage_Table (S, 11, C.DSDT, Boot, Staged); Check (Staged = C.Staged, "stage boot");
      C.Stage_Table (S, 22, C.Description, Added, Staged); Check (Staged = C.Staged, "stage definition");
      C.Stage_Table (S, 33, C.Description, Added, Staged); Check (Staged = C.Staged, "stage duplicate ID");
      C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (S, 10000, Executed, Status);
      Check (Status = V.Available and then Executed.Modules.Outcome = V.Modules_Completed, "boot execution");
      C.Initialize_Members (S, Initial, Status); Check (Status = V.Available, "ready owner");
   end Initialize;
   procedure Child (S : C.State; Parent : N.Node_ID; Name : String; Item : out C.Node_Descriptor) is
      Node : N.Node_ID;
   begin
      C.Find_Child (S, Parent, Name, Node, Status);
      Check (Status = V.Available and then Node /= N.Root, "find " & Name);
      C.Describe_Node (S, Node, Item, Status); Check (Status = V.Available, "describe " & Name);
   end Child;
   procedure Number_Is (Node : N.Node_ID; Value : AML_Decode.Integer_Value) is
   begin
      C.Observe_Named_Value (Service, Node, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned
        and then Result.Number = Value, "committed integer value");
   end Number_Is;
   procedure Run (Request : Firmware_Tables.Identifiers.Selection; From : C.Node_Descriptor;
      Root_Path, Parameter : String; Item : V.Argument; Fuel : Natural;
      Expected : Requests.Request_Outcome; Access_Result : V.Access_Status := V.Available) is
      Before : constant V.Audit_Value := C.Audit (Service);
      Pins : constant Natural := C.Retained_Results (Service);
   begin
      Requests.Append_With_Store (Service, Request, From, Root_Path, Parameter, Item, Fuel, Report, Status);
      Check (Status = Access_Result and then Report.Outcome = Expected and then Report.Charged <= Fuel,
        "bounded request outcome " & Report.Outcome'Image);
      Check (C.Valid (Service) and then C.Retained_Results (Service) = Pins, "request preserves caller pins");
      if Expected in Requests.Completed | Requests.Already_Present | Requests.Module_Failed | Requests.Parameter_Store_Failed then
         Check (N.Has_Definition (Report.Load.Definition) and then Report.Load.Definition = C.Retained_Definition (Service, 2),
           "request retains canonical definition identity across completion/recovery");
      else Check (Report.Load.Definition = N.No_Definition, "request pre-effect refusal has no definition identity");
      end if;
      if Expected in Requests.Not_Started | Requests.Table_Not_Found | Requests.Paths_Refused |
        Requests.Source_Refused | Requests.Already_Present then
         Check (C.Audit (Service) = Before, "pre-effect refusal/duplicate preserves namespace audit");
      end if;
   end Run;
begin
   Run (Wanted, V.No_Node, "", "", Source, Budget, Requests.Not_Started, V.Wrong_Phase);
   Initialize (Service); Initialize (Other);
   C.Describe_Node (Service, N.Root, Root, Status); Check (Status = V.Available, "root");
   C.Describe_Node (Other, N.Root, Foreign, Status); Check (Status = V.Available, "foreign root");
   Child (Service, N.Root, "DEV0", Device); Child (Service, V.Position (Device), "CALL", Caller);
   Child (Service, N.Root, "DST0", Destination); Child (Service, V.Position (Destination), "PRM0", Target);
   Child (Service, N.Root, "GET0", Getter);
   C.Find_Child (Service, N.Root, "CNT0", Counter, Status); Check (Status = V.Available, "counter");
   C.Find_Child (Service, N.Root, "OBS0", Observed, Status); Check (Status = V.Available, "observation");
   declare Charged : Natural; Outcome : AML_Execute.Execution_Status; begin
      C.Store_Parameter_Data (Service, Target, (V.Immediate_Argument, 16#30#), 1, Charged, Outcome, Status);
      Check (Status = V.Available and then Outcome = AML_Execute.Returned and then Charged = V.Parameter_Store_Charge,
        "seed parameter before module effects");
   end;
   C.Require_Load_Recovery (Service, 2, Status); Check (Status = V.Wrong_Phase, "unloaded source cannot enter post-load recovery");
   C.Require_Load_Recovery (Service, 4, Status); Check (Status = V.Invalid_Value, "invalid source cannot enter recovery");
   C.Evaluate_Named (Service, Getter, 10000, Result, Status);
   Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "hold existing buffer"); Held := Result.Handle;
   Run (Missing, Root, "BAD..X", "BAD..X", Source, Budget, Requests.Table_Not_Found);
   Check (not Report.Paths_Checked and then Report.Index = 0, "table search precedes user path parsing");
   Run (Wanted, Caller, "BAD..X", "PRM0", Source, Budget, Requests.Paths_Refused);
   Run (Wanted, Foreign, "DST0", "PRM0", Source, Budget, Requests.Paths_Refused, V.Invalid_Value);
   Run (Wanted, Caller, "DST0", "BAD..X", Source, Budget, Requests.Paths_Refused);
   Check (Report.Path_Status = Requests.Paths.Parameter_Syntax_Error
     and then Report.Charged = 0 and then not Report.Store_Attempted,
     "malformed parameter rejected before module effects");
   Run (Wanted, Caller, "DST0", "MISS", Source, Budget, Requests.Paths_Refused);
   Check (Report.Path_Status = Requests.Paths.Parameter_Lookup_Error
     and then Report.Lookup = N.Not_Found and then Report.Charged = 0
     and then not Report.Store_Attempted,
     "absent parameter rejected before module effects");
   Run (Wanted, Caller, "DST0", "NEWV", Source, Budget, Requests.Paths_Refused);
   Check (Report.Path_Status = Requests.Paths.Parameter_Lookup_Error
     and then Report.Lookup = N.Not_Found and then Report.Charged = 0
     and then C.Retained_Load_State (Service, 2) = C.Retained,
     "parameter declared only by added table is not bound speculatively");
   declare Other_Getter : C.Node_Descriptor; begin
      Child (Other, N.Root, "GET0", Other_Getter);
      C.Evaluate_Named (Other, Other_Getter, 10000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "foreign held data"); Foreign_Held := Result.Handle;
   end;
   Run (Wanted, Caller, "DST0", "PRM0", (V.Retained_Argument, Foreign_Held), Budget,
     Requests.Source_Refused, V.Invalid_Value);
   C.Release_Result (Other, Foreign_Held, Status); Check (Status = V.Available, "release foreign source");
   Run (Wanted, Caller, "DST0", "PRM0", Source, 0, Requests.Not_Started);
   Check (Report.Load.Append.Outcome = V.Append_Budget_Refused and then not Report.Store_Attempted,
     "zero fuel does not consume source or Store");
   C.Configure_Notifications (Service, True, Status); Check (Status = V.Available, "enable interest");
   Run (Wanted, Caller, "DST0", "PRM0", Source, Budget, Requests.Not_Started, V.Invalid_Value);
   C.Configure_Notifications (Service, False, Status); Check (Status = V.Available, "disable interest");
   Check (C.Retained_Load_State (Service, 2) = C.Retained, "all refusals leave source retained");
   Number_Is (Counter, 0); Number_Is (V.Position (Target), 16#30#);
   if Mode = "budget" then
      declare Other_Root : C.Node_Descriptor; Probe : C.Retained_Load_Report; begin
         Child (Other, N.Root, "DST0", Other_Root);
         C.Append_Retained_Module (Other, 2, 10000, Probe, Status, (V.Selected_Root, Other_Root));
         Check (Status = V.Available and then Probe.Outcome = C.Retained_Load_Completed
           and then Probe.Append.Charged > 0, "measure actual module charge on independent owner");
         Budget := Probe.Append.Charged;
      end;
   end if;
   if Mode = "no-parameter" then Source := (V.Retained_Argument, V.No_Value); end if;
   Run (Wanted, Caller, "DST0", Parameter_Path, Source, Budget,
     (if Mode in "store-fail" | "retype-fail" | "budget" then Requests.Parameter_Store_Failed
      elsif Mode in "module-fail" | "fatal" then Requests.Module_Failed else Requests.Completed));
   Check (Report.Index = 2, "selected retained index"); Number_Is (Counter, 1);
   if Mode in "store-fail" | "retype-fail" | "budget" then
      Check (Report.Store_Attempted and then Report.Recovery_Access = V.Available
        and then C.Retained_Load_State (Service, 2) = C.Failed_Definition
        and then C.Current_Import_Phase (Service) = C.Catalog_Failed,
        "post-effect Store failure closes serving and source state");
      Check (Report.Store_Outcome = (if Mode = "budget" then AML_Execute.Budget_Exceeded else AML_Execute.Unsupported_Value),
        "original Store failure retained");
      Number_Is (Observed, 16#30#);
      if Mode = "budget" then Number_Is (V.Position (Target), 16#30#); end if;
   elsif Mode in "module-fail" | "fatal" then
      Check (not Report.Store_Attempted and then Report.Load.Append.Failure =
        (if Mode = "fatal" then AML_Execute.Fatal_Requested else AML_Execute.Division_By_Zero),
        "module failure prevents parameter Store");
      Number_Is (V.Position (Target), 16#30#);
   else
      Check (C.Retained_Load_State (Service, 2) = C.Loaded_Definition, "completed source state");
      Number_Is (Observed, 16#30#);
      if Mode = "retype-success" then
         Check (Report.Store_Attempted and then C.Current_Binding (Service, Target)
           and then not C.Current_Node (Service, Target), "Store uses live binding after type change");
         C.Observe_Named_Value (Service, V.Position (Target), Result, Status);
         Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned, "converted String result");
         declare
            Text : AML_Decode.Bytes (1 .. 16); Copied : Natural;
            Handle : V.Value_Handle := Result.Handle;
            Length : constant Positive := (if Boot (9) < 2 then 8 else 16);
         begin
            C.Read_Result_Bytes (Service, Handle, 0, Text, Copied, Status);
            Check (Status = V.Available and then Copied = Length and then Text (Length - 1) = Character'Pos ('9')
              and then Text (Length) = Character'Pos ('9'), "ordinary integer-to-string conversion");
            C.Release_Result (Service, Handle, Status); Check (Status = V.Available, "release String result");
         end;
      else
         Number_Is (V.Position (Target), (if Mode = "no-parameter" then 16#30# else 16#99#));
         Check (Report.Store_Attempted = (Mode /= "no-parameter"), "optional Store honored");
      end if;
      Run (Wanted, Caller, "DST0", Parameter_Path, (V.Immediate_Argument, 7), 10000, Requests.Already_Present);
      Check (not Report.Store_Attempted and then Report.Load.Existing_Index = 2 and then Report.Charged = 0,
        "duplicate applies neither module effects nor another Store"); Number_Is (Counter, 1);
      if Mode = "normal" then Number_Is (V.Position (Target), 16#99#); end if;
      C.Release_Result (Service, Held, Status); Check (Status = V.Available and then C.Retained_Results (Service) = 0,
        "release preserved caller pin");
   end if;
   if Mode in "store-fail" | "retype-fail" | "budget" | "module-fail" | "fatal" then
      C.Evaluate_Named (Service, Getter, 10000, Result, Status); Check (Status = V.Wrong_Phase, "recovery blocks further AML");
      Run (Wanted, Caller, "DST0", "PRM0", (V.Immediate_Argument, 7), 10000, Requests.Not_Started, V.Wrong_Phase);
      declare Load : C.Retained_Load_Report; begin
         C.Append_Retained_Module (Service, 3, 10000, Load, Status); Check (Status = V.Wrong_Phase, "alternate ID cannot retry recovery owner");
      end;
      Number_Is (Counter, 1);
      C.Release_Result (Service, Held, Status); Check (Status = V.Available and then Held = V.No_Value and then C.Retained_Results (Service) = 0,
        "terminal recovery permits authenticated caller pin cleanup");
      C.Evaluate_Named (Service, Getter, 10000, Result, Status);
      Check (Status = V.Wrong_Phase and then C.Current_Import_Phase (Service) = C.Catalog_Failed,
        "cleanup preserves the closed owner phase");
      C.Release_Result (Service, Held, Status);
      Check (Status = V.Invalid_Value and then Held = V.No_Value and then C.Retained_Results (Service) = 0,
        "empty cleanup handle cannot release twice");
   end if;
   Ada.Text_IO.Put_Line ("Retained request checks" & Checks'Image);
end Retained_Request_Tests;
