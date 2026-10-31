pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Requests.HID_Devices;
with ACPI_HID_I2C_Discovery;
with ACPI_HID_I2C_Candidate;
with ACPI_HID_I2C_Resolved;
with ACPI_HID_I2C_Channels;
with HID_Channel_Policy_Tests;
with I2C_Channel_Badges;
with GPIO_Channel_Badges;
with Firmware_Tables;
with HID_I2C_Descriptors;
with Interfaces;
procedure HID_Owner_Tests is
   package Core renames ACPI_Service;
   package Q renames ACPI_Requests;
   package IO renames Ada.Streams.Stream_IO;
   Evaluations, Invocations, Callback_Charges : Natural := 0;
   procedure Evaluate (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Evaluations := Evaluations + 1; Core.Evaluate_Named (Service, Item, Budget, Result, Status);
      Callback_Charges := Callback_Charges + Result.Charged;
   end Evaluate;
   procedure Invoke (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Invocations := Invocations + 1; Core.Invoke_With_Arguments (Service, Node, Args, Argument_Count, Budget, Result, Status);
      Callback_Charges := Callback_Charges + Result.Charged;
   end Invoke;
   package Discovery is new ACPI_HID_I2C_Discovery (Core, Invoke, Evaluate);
   package Candidates is new ACPI_HID_I2C_Candidate;
   package Resolver is new ACPI_HID_I2C_Resolved (Core, Candidates);
   package Channels is new ACPI_HID_I2C_Channels (Candidates);
   use type Channels.Outcome;
   use type I2C_Channel_Badges.Hertz, I2C_Channel_Badges.Address_Value;
   use type GPIO_Channel_Badges.Pin_Number, GPIO_Channel_Badges.Trigger_Mode, GPIO_Channel_Badges.Polarity;
   package Owner is new Q.HID_Devices (Discovery, Resolver);
   use type Ada.Streams.Stream_Element_Offset;
   use type Q.Outcome, Q.Phase, Q.Revision_Number;
   use type Owner.Controller.Report;
   use type Owner.Controller.Admission, Discovery.Action_Result, Discovery.Outcome;
   use type Resolver.Outcome, Resolver.Observation;
   use type Firmware_Tables.Byte, HID_I2C_Descriptors.Register_Number;
   use type Owner.Controller.Search_Outcome, Discovery.Queries.Identification_Outcome;
   Checks : Natural := 0;
   Fixture_Limit : constant := 4096;
   Budget : constant := 4000;
   -- First fixture device follows seven predefined root children.
   Touchpad_Node : constant Core.Namespace.Node_ID := 8;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Read_Table return Firmware_Tables.Bytes is
      File : IO.File_Type;
   begin
      Check (Ada.Command_Line.Argument_Count = 2, "fixture and lifecycle");
      IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
      Check (IO.Size (File) in IO.Count (Firmware_Tables.Table_Header_Size) .. Fixture_Limit, "bounded table");
      declare
         Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (IO.Size (File)));
         Last : Ada.Streams.Stream_Element_Offset;
         Data : Firmware_Tables.Bytes (1 .. Raw'Length);
      begin
         IO.Read (File, Raw, Last); IO.Close (File); Check (Last = Raw'Last, "complete table");
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Raw (Ada.Streams.Stream_Element_Offset (I))); end loop;
         return Data;
      end;
   end Read_Table;
   Table : constant Firmware_Tables.Bytes := Read_Table;
   Server, Foreign_Server : Q.State (1, Table'Length, Table'Length, 1, 0);
   Run, Cancelled, Empty_Budget : Owner.Controller.Probe;
   Allowed : Owner.Controller.Admission;
   Action : Discovery.Action_Result;
   Reply : Q.Response;
   Small : Firmware_Tables.Bytes (10 .. 10);
   Data : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
   Info : Owner.Controller.Report;
   Search : Owner.Controller.Search_Report;
   Found_Node, Second_Node : Core.Namespace.Node_ID;
   Selected_Node : Core.Namespace.Node_ID := Touchpad_Node;
   procedure Find (After : Core.Namespace.Node_ID; Fuel : Natural; Node : out Core.Namespace.Node_ID) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Owner.Find (Server, After, Fuel, Node, Search);
      Check (Q.Valid (Server) and then Q.Revision (Server) = Before
        and then Search.Charged <= Fuel, "search preserves revision and shared AML budget");
   end Find;
   procedure Load (Target : in out Q.State) is
      procedure Send (Label : Interfaces.Unsigned_32; Value : Q.Revision_Number := 0) is
      begin
         Q.Handle (Target, Q.Snapshot_Provider,
           (Label => Label, Data => [Q.Revision (Target), Value, 0, 0], others => <>), Reply);
         Check (Reply.Status = Q.OK and then Q.Valid (Target), "service snapshot protocol");
      end Send;
   begin
      Send (Q.Start_Snapshot, 1);
      Q.Import_Block (Target, Q.Snapshot_Provider, Q.Revision (Target), 1, Core.DSDT, Table, Reply);
      Check (Reply.Status = Q.OK, "actual HID AML imported");
      Send (Q.Finish_Snapshot); Check (Q.Current (Target) = Q.Complete, "catalog complete");
   end Load;
   procedure Begin_Main (Item : in out Owner.Controller.Probe; Fuel : Natural := Budget) is
      Before : constant Q.Revision_Number := Q.Revision (Server);
   begin
      Owner.Begin_Probe (Item, Server, Selected_Node, Fuel, Allowed, Action);
      Check (Q.Valid (Server) and then Q.Revision (Server) = Before, "begin preserves service revision");
   end Begin_Main;
begin
   declare Count : Natural; begin
      HID_Channel_Policy_Tests (Count); Checks := Checks + Count;
   end;
   Find (Core.Namespace.Root, Budget, Found_Node);
   Check (Search.Outcome = Owner.Controller.Not_Ready and then Evaluations = 0,
     "unready enumeration cannot execute AML");
   Begin_Main (Run);
   Check (Allowed = Owner.Controller.Not_Ready and then not Owner.Controller.Pending (Run)
     and then Evaluations = 0 and then Invocations = 0, "unstarted service cannot discover HID");
   Data := [others => 16#AA#];
   Owner.Copy_And_Resolve (Run, Server, Data, Allowed, Action);
   Check (Allowed = Owner.Controller.Not_Ready and then (for all B of Data => B = 0), "unready copy zeros all output");
   Owner.Cancel (Run, Server, Action);
   Check (Action = Discovery.Not_Pending, "cleanup available before readiness");
   declare
      Next_Run : Owner.Controller.Probe;
      Next_Device : Core.Namespace.Node_ID;
      Next_Search : Owner.Controller.Search_Report;
   begin
      Owner.Begin_Next_Probe (Next_Run, Server, Core.Namespace.Root, Budget,
        Next_Device, Next_Search, Allowed, Action);
      Check (Allowed = Owner.Controller.Not_Ready and then Action = Discovery.Not_Pending
        and then Next_Device = Core.Namespace.Root and then Evaluations = 0,
        "combined selection refuses unready owner without AML");
   end;
   Load (Server); Load (Foreign_Server);
   declare
      Next_Run : Owner.Controller.Probe;
      Next_Device : Core.Namespace.Node_ID;
      Next_Search : Owner.Controller.Search_Report;
      Before : constant Q.Revision_Number := Q.Revision (Server);
      Eval_Before, Invoke_Before, Charges_Before : Natural;
   begin
      Eval_Before := Evaluations;
      Owner.Begin_Next_Probe (Next_Run, Server, Core.Namespace.Root, 0,
        Next_Device, Next_Search, Allowed, Action);
      Check (Action = Discovery.Not_Pending and then Next_Device = Core.Namespace.Root
        and then Evaluations = Eval_Before and then not Owner.Controller.Pending (Next_Run),
        "zero-budget combined selection cannot start a probe");
      Owner.Begin_Next_Probe (Next_Run, Server, Core.Namespace.Node_ID'Last, Budget,
        Next_Device, Next_Search, Allowed, Action);
      Check (Action = Discovery.Not_Pending and then Next_Device = Core.Namespace.Root
        and then Evaluations = Eval_Before, "invalid combined cursor does no AML");
      if Ada.Command_Line.Argument (2) /= "hardware" then
         Charges_Before := Callback_Charges;
         Owner.Begin_Next_Probe (Next_Run, Server, Core.Namespace.Root, Budget,
           Next_Device, Next_Search, Allowed, Action);
         Check (Allowed = Owner.Controller.Admitted and then Action = Discovery.Accepted
           and then Next_Search.Outcome = Owner.Controller.Found
           and then Next_Device = Touchpad_Node and then Owner.Controller.Pending (Next_Run),
           "combined path selects and starts exact same-Core device");
         Eval_Before := Evaluations; Invoke_Before := Invocations;
         declare
            Original : constant Owner.Controller.Report := Owner.Controller.Observe (Next_Run);
            Search_Charge : constant Natural := Next_Search.Charged;
         begin
            Owner.Begin_Next_Probe (Next_Run, Server, Next_Device, Budget,
              Next_Device, Next_Search, Allowed, Action);
            Check (Action = Discovery.Busy and then Next_Device = Core.Namespace.Root
              and then Evaluations = Eval_Before and then Invocations = Invoke_Before
              and then Owner.Controller.Observe (Next_Run) = Original,
              "pending combined call does not search or replace retained CRS");
            Owner.Copy_And_Resolve (Next_Run, Server, Small, Allowed, Action);
            Check (Owner.Controller.Pending (Next_Run) and then Evaluations = Eval_Before
              and then Invocations = Invoke_Before, "combined sizing retry never reruns AML");
            Owner.Copy_And_Resolve (Next_Run, Server, Data, Allowed, Action);
            Check (Action = Discovery.Accepted and then not Owner.Controller.Pending (Next_Run)
              and then Owner.Controller.Observe (Next_Run).Firmware.Result = Discovery.Observed,
              "combined owner completes discovery with retained copy");
            Check (Search_Charge + Owner.Controller.Observe (Next_Run).Firmware.Charged <= Budget
              and then Callback_Charges - Charges_Before <= Budget,
              "search plus complete probe share one original AML budget");
            if Ada.Command_Line.Argument (2) = "budget" then
               declare
                  Tight_Run : Owner.Controller.Probe;
                  Full_Charge : constant Natural := Search_Charge +
                    Owner.Controller.Observe (Next_Run).Firmware.Charged;
                  Tight_Budget : constant Natural := Full_Charge - 1;
                  Charge_Start : constant Natural := Callback_Charges;
               begin
                  Check (Search_Charge > 0, "method CID makes search consume actual AML fuel");
                  Owner.Begin_Next_Probe (Tight_Run, Server, Core.Namespace.Root, Tight_Budget,
                    Next_Device, Next_Search, Allowed, Action);
                  if Owner.Controller.Pending (Tight_Run) then
                     Owner.Copy_And_Resolve (Tight_Run, Server, Data, Allowed, Action);
                  end if;
                  Check (Action = Discovery.Accepted and then
                    Owner.Controller.Observe (Tight_Run).Firmware.Result = Discovery.Budget_Exhausted
                    and then not Owner.Controller.Observe (Tight_Run).Sources_Attempted,
                    "one-short shared budget cannot complete descriptor query or source resolution");
                  Check (Callback_Charges - Charge_Start <= Tight_Budget and then
                    Next_Search.Charged + Owner.Controller.Observe (Tight_Run).Firmware.Charged <= Tight_Budget,
                    "search fuel is deducted before beginning tight-budget probe");
                  Check (not Owner.Controller.Pending (Tight_Run), "exhausted combined probe releases retained CRS");
               end;
            end if;
         end;
         Check (Q.Revision (Server) = Before, "combined discovery keeps observation revision");
      end if;
      Evaluations := 0; Invocations := 0;
   end;
   if Ada.Command_Line.Argument (2) /= "hardware" then
      Find (Core.Namespace.Root, 0, Found_Node);
      Check (Search.Outcome = Owner.Controller.Incomplete and then Evaluations = 0,
        "zero budget performs no identification");
      Find (Core.Namespace.Node_ID'Last, Budget, Found_Node);
      Check (Search.Outcome = Owner.Controller.Invalid_Cursor and then Evaluations = 0,
        "out-of-snapshot cursor performs no identification");
      Find (Core.Namespace.Root, Budget, Found_Node);
      Check (Search.Outcome = Owner.Controller.Found and then Found_Node = Touchpad_Node,
        "first actual PNP0C50 discovered in service Core");
      Selected_Node := Found_Node;
      Find (Found_Node, Budget, Second_Node);
      Check (Search.Outcome = Owner.Controller.Found and then Second_Node > Found_Node,
        "second package CID match found past unrelated controllers");
      Find (Second_Node, Budget, Found_Node);
      if Ada.Command_Line.Argument (2) = "missing" then
         Check (Search.Outcome = Owner.Controller.Failed and then Found_Node = Core.Namespace.Root
           and then Search.Last_Identity.Outcome = Discovery.Queries.Wrong_Result_Type,
           "malformed package member after compatible CID fails search without payload");
      else
         Check (Search.Outcome = Owner.Controller.No_Match and then Found_Node = Core.Namespace.Root,
           "enumeration reaches end");
      end if;
      Evaluations := 0;
   else
      Find (Core.Namespace.Root, Budget, Found_Node);
      Check (Search.Outcome = Owner.Controller.Not_Ready and then Evaluations = 0,
        "unconfigured region bars enumeration");
   end if;
   Begin_Main (Run);
   if Ada.Command_Line.Argument (2) = "hardware" then
      Check (Allowed = Owner.Controller.Not_Ready and then Evaluations = 0
        and then Invocations = 0 and then not Owner.Current (Run, Server), "unconfigured region blocks HID AML");
   else
      Check (Allowed = Owner.Controller.Admitted and then Action = Discovery.Accepted
        and then Owner.Controller.Pending (Run), "service-owned discovery retains CRS");
      Info := Owner.Controller.Observe (Run);
      Check (Info.Firmware.Result = Discovery.Awaiting_Buffer and then not Info.Sources_Attempted
        and then Evaluations = 3 and then Invocations = 0, "CID STA CRS queried once before sizing");
      Begin_Main (Run);
      Check (Action = Discovery.Busy and then Evaluations = 3, "pending probe cannot replace its owner");
      Owner.Copy_And_Resolve (Run, Foreign_Server, Data, Allowed, Action);
      Check (Action = Discovery.Wrong_Context and then Owner.Controller.Pending (Run)
        and then (for all B of Data => B = 0), "foreign server cannot consume retained CRS");
      Owner.Cancel (Run, Foreign_Server, Action);
      Check (Action = Discovery.Wrong_Context and then Owner.Controller.Pending (Run), "foreign cancellation preserves owner");
      Owner.Copy_And_Resolve (Run, Server, Small, Allowed, Action);
      Info := Owner.Controller.Observe (Run);
      Check (Action = Discovery.Accepted and then Info.Firmware.Result = Discovery.Awaiting_Buffer
        and then Owner.Controller.Pending (Run) and then not Info.Sources_Attempted
        and then Evaluations = 3 and then Invocations = 0, "capacity retry does not rerun AML or resolve partial data");
      declare Before : constant Q.Revision_Number := Q.Revision (Server); begin
         Owner.Copy_And_Resolve (Run, Server, Data, Allowed, Action);
         Check (Q.Valid (Server) and then Q.Revision (Server) = Before, "finish preserves service revision");
      end;
      Info := Owner.Controller.Observe (Run);
      Check (Action = Discovery.Accepted and then not Owner.Controller.Pending (Run)
        and then Info.Firmware.Result = Discovery.Observed and then Info.Sources_Attempted
        and then Evaluations = 3 and then Invocations = 2, "full copy releases CRS and queries DSM exactly twice");
      if Ada.Command_Line.Argument (2) = "missing" then
         Check (Info.Sources.Status = Resolver.GPIO_Source_Failed
           and then Info.Sources.Value = Resolver.Empty_Observation
           and then not Owner.Current (Run, Server), "missing GPIO source never yields partial resolved payload");
      else
         Check (Info.Sources.Status = Resolver.Resolved and then Owner.Current (Run, Server)
           and then not Owner.Current (Run, Foreign_Server), "both sources resolve only in original service Core");
         declare
            Prepared : constant Channels.Preparation := Channels.Prepare (Resolver.Facts (Info.Sources.Value));
         begin
            Check (Prepared.Status = Channels.Prepared, "actual AML resource facts prepare channel descriptions");
            Check (Prepared.I2C.Address = 16#15# and then Prepared.I2C.Maximum_Hz = 400_000,
              "actual I2C target and speed retained");
            Check (Prepared.GPIO.Pin = 16#2A# and then Prepared.GPIO.Trigger = GPIO_Channel_Badges.Level_Triggered
              and then Prepared.GPIO.Active = GPIO_Channel_Badges.Active_Low,
              "actual GPIO pin and signaling retained");
            Check (Prepared.Facts.Descriptor_Register = 16#20#,
              "actual descriptor register reaches prepared metadata");
         end;
         Check (Resolver.Facts (Info.Sources.Value).Descriptor_Register = 16#20#,
           "resolver uses actual observed DSM register");
         Check (Core.Values.Position (Resolver.Device (Info.Sources.Value)) = Touchpad_Node
           and then Core.Values.Position (Resolver.I2C_Controller (Info.Sources.Value)) = 14
           and then Core.Values.Position (Resolver.GPIO_Controller (Info.Sources.Value)) = 16,
           "same device and exact rooted I2C/GPIO controllers selected");
      end if;
      Owner.Copy_And_Resolve (Run, Server, Data, Allowed, Action);
      Check (Action = Discovery.Not_Pending and then Evaluations = 3 and then Invocations = 2,
        "completed probe never repeats copy/DSM");
      Begin_Main (Cancelled);
      Check (Owner.Controller.Pending (Cancelled), "new explicit probe has its own retained CRS");
      Owner.Cancel (Cancelled, Server, Action);
      Check (Action = Discovery.Accepted and then not Owner.Controller.Pending (Cancelled)
        and then Owner.Controller.Observe (Cancelled).Firmware.Result = Discovery.Cancelled,
        "owner cancellation releases retained result without descriptor queries");
      Begin_Main (Empty_Budget, 0);
      Check (Action = Discovery.Accepted and then not Owner.Controller.Pending (Empty_Budget)
        and then not Owner.Controller.Observe (Empty_Budget).Sources_Attempted,
        "zero fuel cannot publish controller claims");
   end if;
   Ada.Text_IO.Put_Line ("ACPI HID owner checks" & Natural'Image (Checks));
end HID_Owner_Tests;
