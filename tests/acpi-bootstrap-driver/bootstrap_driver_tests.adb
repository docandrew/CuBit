pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Service;
with ACPI_Bootstrap;
with ACPI_Requests;
with ACPI_Endpoint;
with ACPI_Requests.With_Driver;
with ACPI_Bootstrap.Lids;
with ACPI_Lid;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with ACPI_Region_Initialization;
with ACPI_FADT.Transactions;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with Bootstrap_Driver_Oracle;
procedure Bootstrap_Driver_Tests is
   package C renames ACPI_Service;
   package B renames ACPI_Bootstrap;
   package A renames AML_Field_Access;
   use type C.Values.Access_Status;
   use type C.Install_Status;
   use type C.Node_Descriptor;
   use type B.Phase;
   use type B.Device_Phase;
   use type B.Device_Report;
   use type AML_Identity.Identity;
   use type AML_Execute.Execution_Status;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type ACPI_Device_Policy.Region_Outcome;
   Register_ID : constant A.Register_ID := 16#E000_0000_0000_0011#;
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Region : C.Node_Descriptor := C.Values.No_Node;
      Allow, Ready : Boolean := False;
      Value : Unsigned_64 := 0;
      Calls, Preparations : Natural := 0;
      Unknown, Outstanding : Boolean := False;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Owner = AML_Identity.No_Identity or else IO.Outstanding);
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 1),
        Byte_Offset => 0, Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := IO.Ready and then Target.Provider = IO.Owner and then Target.Endpoint = 7
        and then Target.Epoch = 1 and then Target.Register = Register_ID;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; Writing : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (Writing);
      Geometry : AML_Field_Layout.Register_Geometry;
   begin Describe (IO, Target, Geometry, Accepted); Accepted := Accepted and then IO.Allow; end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Allowed : Boolean;
   begin
      Reply := (others => <>);
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      if not Allowed or else Item.Value > 255 then return; end if;
      IO.Calls := IO.Calls + 1;
      if Item.For_Write then IO.Value := Item.Value; end if;
      if IO.Unknown then
         IO.Outstanding := True;
         Reply := (A.Indeterminate, IO.Owner, 0, 1);
         return;
      end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Value), A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Confirmed;
   package Fields is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize);
   IO : Context;
   Items : Fields.Binding_Set;
   function Handler_Ready (Ctx : Context; Region : C.Node_Descriptor;
     Data : C.Namespace.Operation_Region_Attributes) return Boolean is
      pragma Unreferenced (Data);
   begin return Ctx.Ready and then Ctx.Allow and then Region = Ctx.Region; end Handler_Ready;
   function PCI_Root (Ctx : Context; Region : C.Node_Descriptor) return Boolean is
      pragma Unreferenced (Ctx, Region);
   begin return False; end PCI_Root;
   procedure Invoke (Service : aliased in out C.State; Ctx : in out Context;
     Node : C.Namespace.Node_ID; Args : AML_Execute.Arguments; Argument_Count : Natural;
     Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Fields.Invoke_Retained (Service, Ctx, Items, Node, Args, Argument_Count, Budget, Result, Status); end Invoke;
   package Regions is new ACPI_Region_Initialization (C, Context, Handler_Ready, PCI_Root, Invoke);
   Region_Run : Regions.Session;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   procedure Prepare_Regions (Service : aliased in out C.State; Budget : Natural;
     Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      Node : C.Namespace.Node_ID;
      Description : C.Values.Field_Description;
      Key : C.Values.Field_Key;
      Status : C.Values.Access_Status;
      Accepted : Boolean;
   begin
      IO.Preparations := IO.Preparations + 1; Fields.Clear (Items);
      if IO.Allow then
         C.Find_Child (Service, C.Namespace.Root, "REG0", Node, Status);
         C.Describe_Node (Service, Node, IO.Region, Status); Check (Status = C.Values.Available, "region identity");
         C.Find_Child (Service, C.Namespace.Root, "VAL0", Node, Status);
         C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
         Check (Status = C.Values.Available, "field descriptor"); IO.Ready := True;
         Fields.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, Register_ID), Items, Status);
         Check (Status = C.Values.Available, "trusted named-register binding");
      end if;
      Regions.Connect (Region_Run, Service, IO, Budget, Accepted);
      Charged := Regions.Observe (Region_Run).Charged; Outcome := Regions.Outcome (Region_Run);
      Check (Accepted, "one-shot region connection attempt admitted");
      Check ((Outcome = ACPI_Device_Policy.Regions_Connected) = IO.Allow,
        "region readiness matches installed handler");
   end Prepare_Regions;
   procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor;
     Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Fields.Evaluate_Named (Service, IO, Items, Item, Budget, Result, Status); end Evaluate;
   package Lid_Driver is new ACPI_Lid (C, Evaluate);
   package Lids is new B.Lids (Lid_Driver);
   use type Lids.Discovery_Outcome;
   use type Lids.Query_Admission;
   use type Lid_Driver.Query_Outcome;
   use type Lid_Driver.Lid_State;
   package Driver is new ACPI_Device_Initialization (C, Prepare_Regions, Evaluate);
   package Request_Driver is new ACPI_Requests.With_Driver (Driver);
   package Controlled is new B.With_Driver (Driver);
   procedure Finish_Controlled
     (Session : in out B.State; Run : in out Driver.Session;
      Budget : B.Startup_Budget := B.Default_Startup_Budget)
     renames Controlled.Finish;
   Run : Driver.Session;
   Raw : constant Bootstrap_Driver_Oracle.Bytes := Bootstrap_Driver_Oracle.Raw_Table;
   Session : B.State (1, Raw'Length, Raw'Length);
   Issued : Boolean;
   procedure Load (Item : in out B.State) is
      Accepted : Boolean;
      Status : C.Install_Status;
   begin
      B.Start (Item, 1, Accepted); Check (Accepted, "start snapshot");
      B.Import_Table (Item, 1, C.DSDT, Raw, Status); Check (Status = C.Installed, "import snapshot");
   end Load;
   procedure Test_Requests (Mode : String) is
      package R renames ACPI_Requests;
      use type R.Outcome;
      use type R.Phase;
      use type R.Response_Words;
      Server : R.State (1, Raw'Length, Raw'Length, 0);
      Packet : R.Packet;
      Reply : R.Response;
      Config : constant ACPI_Endpoint.Configuration :=
        (Observer_Tag => 11, Provider_Tag => 22);
      procedure Selected_Handle
        (Item : in out R.State; Origin : R.Authority;
         Request : R.Packet; Result : out R.Response) is
      begin
         if Mode = "request-default" and then Request.Label = R.Finish_Snapshot then
            R.Handle (Item, Origin, Request, Result);
         else Request_Driver.Handle (Item, Run, Origin, Request, Result); end if;
         Reply := Result;
      end Selected_Handle;
      package Endpoint is new ACPI_Endpoint.With_Handler (Selected_Handle);
      procedure Send (Origin : R.Authority; Label : Unsigned_32;
        Data : R.Words; Expected : R.Outcome) is
         Wire_Reply : R.Packet;
         Stamp : constant Unsigned_64 :=
           (case Origin is when R.No_Authority => 33,
            when R.Observer => Config.Observer_Tag,
            when R.Snapshot_Provider => Config.Provider_Tag);
         use type R.Packet;
      begin
         Packet.Label := Label; Packet.Data := Data;
         Endpoint.Route (Server, Config, Stamp, Packet, Wire_Reply);
         Check (Wire_Reply = ACPI_Endpoint.Encode (Reply), "endpoint reply encoding");
         Check (Reply.Status = Expected, "request status " & Label'Image & Reply.Status'Image);
      end Send;
      Expected_Device : constant R.Device_Phase_Code :=
        (if Mode = "request-default" then R.Device_Incomplete_Code
         elsif Mode in "request-deny" | "request-unknown" then R.Device_Failed_Code
         else R.Device_Completed_Code);
      Expected_Region : constant R.Region_Status_Code :=
        (if Mode = "request-default" then R.Regions_Unavailable_Code
         elsif Mode = "request-deny" then R.Region_Failed_Code
         else R.Regions_Connected_Code);
   begin
      Send (R.Observer, R.Read_Metrics, [R.Metrics_Region_Attempt_Page,0,0,0], R.OK);
      Check (Reply.Data = [0,0,Unsigned_64 (R.Region_Status_Code'Enum_Rep (R.Regions_Unavailable_Code)),0]
        and then IO.Preparations = 0, "region observation before startup is not attempted");
      Send (R.No_Authority, R.Finish_Snapshot, [0,0,0,0], R.Denied);
      Check (Reply.Data = [0,0,0,0] and then R.Revision (Server) = 0 and then IO.Calls = 0,
        "unauthorized finish is zeroed and inert");
      Send (R.Observer, R.Finish_Snapshot, [0,0,0,0], R.Denied);
      Packet.Reserved := 1;
      Send (R.Snapshot_Provider, R.Finish_Snapshot, [0,0,0,0], R.Malformed);
      Packet.Reserved := 0;
      Send (R.Snapshot_Provider, R.Finish_Snapshot, [1,0,0,0], R.Stale);
      Send (R.Snapshot_Provider, R.Finish_Snapshot, [0,0,0,0], R.Wrong_Order);
      Send (R.Snapshot_Provider, R.Start_Snapshot, [0,1,0,0], R.OK);
      Check (R.Revision (Server) = 1 and then R.Current (Server) = R.Receiving
        and then IO.Calls = 0, "non-finish request delegates without consuming driver");
      Send (R.Snapshot_Provider, R.Finish_Snapshot, [1,1,0,0], R.Malformed);
      R.Import_Block (Server, R.Snapshot_Provider, 1, 1, C.DSDT, Raw, Reply);
      Check (Reply.Status = R.OK and then R.Revision (Server) = 2 and then IO.Calls = 0,
        "typed table import retains copied snapshot");
      Send (R.Snapshot_Provider, R.Finish_Snapshot, [1,0,0,0], R.Stale);
      Send (R.No_Authority, R.Finish_Snapshot, [2,0,0,0], R.Denied);
      Check (Reply.Data = [0,0,0,0] and then R.Revision (Server) = 2 and then IO.Calls = 0,
        "unauthorized finish does not expose active revision");
      Send (R.Snapshot_Provider, R.Finish_Snapshot, [2,0,0,0], R.OK);
      Check (R.Current (Server) = R.Complete and then R.Revision (Server) = 3,
        "wire finish completes snapshot and advances revision once");
      Check ((if Mode = "request-default" then IO.Calls = 0 and then IO.Preparations = 0
         elsif Mode = "request-deny" then IO.Calls = 0 and then IO.Preparations = 1
         elsif Mode = "request-unknown" then IO.Calls = 1 and then IO.Preparations = 1 and then IO.Outstanding
         else IO.Calls = 1 and then IO.Preparations = 1 and then IO.Value = Bootstrap_Driver_Oracle.Expected),
         "wire finish uses only service-selected region and field context");
      Send (R.Observer, R.Read_Metrics, [R.Metrics_Region_Attempt_Page,0,0,0], R.OK);
      Check (Reply.Data = [3,1,Unsigned_64 (R.Region_Status_Code'Enum_Rep (Expected_Region)),0],
        "region callback attempt is recorded without changing revision");
      Send (R.No_Authority, R.Read_Metrics, [R.Metrics_Region_Attempt_Page,0,0,0], R.Denied);
      Check (Reply.Data = [0,0,0,0], "unknown caller cannot observe region diagnostics");
      Send (R.Observer, R.Read_Metrics, [R.Metrics_Device_State_Page,0,0,0], R.OK);
      Check (Reply.Data (1) = Unsigned_64 (R.Device_Phase_Code'Enum_Rep (Expected_Device)),
        "observer sees device readiness separately from completed snapshot");
      declare Calls : constant Natural := IO.Calls; Prep : constant Natural := IO.Preparations; begin
         Send (R.Snapshot_Provider, R.Finish_Snapshot, [3,0,0,0], R.Wrong_Order);
         Check (R.Revision (Server) = 3 and then IO.Calls = Calls and then IO.Preparations = Prep,
           "repeated wire finish never replays driver effects");
         declare Exhausted : R.State (1, Raw'Length, Raw'Length, R.Max_Revision); begin
            Packet := (Label => R.Finish_Snapshot, Data => [R.Max_Revision,0,0,0], others => <>);
            Request_Driver.Handle (Exhausted, Run, R.Snapshot_Provider, Packet, Reply);
            Check (Reply.Status = R.Resource_Limit and then R.Revision (Exhausted) = R.Max_Revision
              and then IO.Calls = Calls and then IO.Preparations = Prep, "revision exhaustion has no effects");
         end;
      end;
      if Mode = "request" then
         declare Other : R.State (1, Raw'Length, Raw'Length, 0); begin
            Packet := (Label => R.Start_Snapshot, Data => [0,1,0,0], others => <>);
            Request_Driver.Handle (Other, Run, R.Snapshot_Provider, Packet, Reply);
            Check (Reply.Status = R.OK, "reused run fresh request snapshot start");
            R.Import_Block (Other, R.Snapshot_Provider, 1, 1, C.DSDT, Raw, Reply);
            Check (Reply.Status = R.OK, "reused run fresh request snapshot import");
            Packet := (Label => R.Finish_Snapshot, Data => [2,0,0,0], others => <>);
            Request_Driver.Handle (Other, Run, R.Snapshot_Provider, Packet, Reply);
            Check (Reply.Status = R.OK and then R.Current (Other) = R.Complete
              and then IO.Calls = 1 and then IO.Preparations = 1, "consumed wire driver run never replays");
            Packet := (Label => R.Read_Metrics, Data => [R.Metrics_Device_State_Page,0,0,0], others => <>);
            Request_Driver.Handle (Other, Run, R.Observer, Packet, Reply);
            Check (Reply.Status = R.OK and then Reply.Data (1) =
              Unsigned_64 (R.Device_Phase_Code'Enum_Rep (R.Device_Failed_Code)),
              "consumed driver cannot report fresh request readiness");
         end;
      end if;
   end Test_Requests;

begin
   Check (Ada.Command_Line.Argument_Count = 1, "mode");
   declare Mode : constant String := Ada.Command_Line.Argument (1); begin
      Check (Mode in "allow" | "deny" | "reuse" | "default" | "zero" | "unknown" | "request" | "request-deny" | "request-unknown" | "request-default", "known mode");
      IO.Allow := Mode not in "deny" | "request-deny";
      IO.Unknown := Mode in "unknown" | "request-unknown";
      AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "provider identity");
      if Mode in "request" | "request-deny" | "request-unknown" | "request-default" then
         Test_Requests (Mode);
         Ada.Text_IO.Put_Line ("Bootstrap driver checks" & Checks'Image);
         return;
      end if;
      Load (Session);
      if Mode = "default" then B.Finish (Session);
      else Finish_Controlled (Session, Run, (if Mode = "zero" then 0 else B.Default_Startup_Budget)); end if;
      if Mode = "zero" then
         Check (not B.Device_Completion (Session).Regions_Attempted,
           "zero budget leaves region attempt false");
         Check (B.Current (Session) = B.Failed and then B.Startup_Charged (Session) = 0,
           "zero budget fails before provider/device phase");
         Check (IO.Calls = 0 and then IO.Preparations = 0, "zero budget no hardware/barrier");
      else
         Check (B.Device_Completion (Session).Regions_Attempted,
           "region attempt retained after callback outcome");
         Check (B.Current (Session) = B.Complete and then B.Pending_Members (Session) = 0,
           "snapshot completion independent of device readiness");
         Check (B.Startup_Charged (Session) <= B.Default_Startup_Budget, "shared fuel bound");
         if Mode = "default" then
            Check (B.Device_Completion (Session).Stage = B.Devices_Incomplete and then IO.Preparations = 0,
              "default remains unconfigured");
            Check (IO.Calls = 0 and then IO.Value = 0, "default no hardware");
         elsif Mode = "deny" then
            Check (B.Device_Completion (Session).Stage = B.Devices_Failed and then
              B.Device_Completion (Session).Region_Status = ACPI_Device_Policy.Region_Initialization_Failed,
              "missing handler prevents device walk");
            Check (IO.Calls = 0 and then IO.Value = 0 and then IO.Preparations = 1, "denied no effects");
         elsif Mode = "unknown" then
            Check (B.Device_Completion (Session).Stage = B.Devices_Failed and then
              B.Device_Completion (Session).First_Error = AML_Execute.Field_Indeterminate,
              "unknown Field completion is retained in device report");
            Check (IO.Outstanding and then IO.Calls = 1 and then IO.Value = Bootstrap_Driver_Oracle.Expected,
              "unknown completed effect retained without replay");
         else
            Check (B.Device_Completion (Session).Stage = B.Devices_Completed and then
              B.Device_Completion (Session).Region_Status = ACPI_Device_Policy.Regions_Connected,
              "actual provisioned driver completes");
            Check (IO.Calls = 1 and then IO.Value = Bootstrap_Driver_Oracle.Expected and then IO.Preparations = 1,
              "actual Field initializer effect matches ACPICA");
            Check (B.Device_Completion (Session).Initialization_Methods = 1 and then
              B.Device_Completion (Session).Status_Methods = 1, "device STA/INI executed once");
         end if;
         Check (B.Table_Byte (Session, 1, 0) = Character'Pos ('D'), "immutable copied table remains");
      end if;
      declare Saved : constant B.Device_Report := B.Device_Completion (Session);
         Calls : constant Natural := IO.Calls; Prep : constant Natural := IO.Preparations;
         Charge : constant Natural := B.Startup_Charged (Session);
      begin
         Finish_Controlled (Session, Run, 0);
         Check (B.Device_Completion (Session) = Saved and then IO.Calls = Calls
           and then IO.Preparations = Prep and then B.Startup_Charged (Session) = Charge,
           "repeated finish never replays or recharges");
      end;
      if Mode = "reuse" then
         declare Other : B.State (1, Raw'Length, Raw'Length); begin
            Load (Other); Finish_Controlled (Other, Run);
            Check (B.Current (Other) = B.Complete and then B.Device_Completion (Other).Stage = B.Devices_Failed,
              "consumed controller run cannot report fresh readiness");
            Check (IO.Calls = 1 and then IO.Preparations = 1, "consumed controller run causes no new device effects");
         end;
      end if;
      declare
         Item : Lids.Binding;
         Search : Lids.Discovery_Report;
         Result : Lid_Driver.Query_Report;
         Admission : Lids.Query_Admission;
         Calls : constant Natural := IO.Calls;
         Charge : constant Natural := B.Startup_Charged (Session);
         Saved : constant B.Device_Report := B.Device_Completion (Session);
      begin
         Lids.Find (Session, 0, Item, Search);
         Check (Search.Outcome = (if Mode in "allow" | "reuse" then Lids.Incomplete else Lids.Not_Ready)
           and then IO.Calls = Calls and then Search.Charged = 0, "lid search zero budget/readiness no effects");
         Lids.Query (Session, Lids.No_Binding, 1000, Admission, Result);
         Check (Admission = (if Mode in "allow" | "reuse" then Lids.Invalid_Binding else Lids.Not_Ready)
           and then Result.State = Lid_Driver.Unknown and then IO.Calls = Calls,
           "lid absent binding never yields state");
         Lids.Find (Session, 1000, Item, Search);
         if Mode in "allow" | "reuse" then
            Check (Search.Outcome = Lids.Found and then Search.Candidates >= 2
              and then Search.Errors = 0 and then Search.Charged <= 1000 and then IO.Calls = Calls,
              "bootstrap discovers actual PNP0C0D after other device: " & Search.Outcome'Image & Search.Scanned'Image & Search.Candidates'Image & Search.Last_Identity.Outcome'Image);
            Lids.Query (Session, Item, 0, Admission, Result);
            Check (Admission = Lids.Admitted and then Result.Outcome = Lid_Driver.Budget_Exhausted
              and then IO.Calls = Calls, "registered lid zero budget no effects");
            Lids.Query (Session, Item, 1000, Admission, Result);
            Check (Admission = Lids.Admitted and then Result.Outcome = Lid_Driver.Sampled
              and then Result.State = Lid_Driver.Open and then Result.Raw = Bootstrap_Driver_Oracle.Expected
              and then IO.Calls = Calls + 1, "bootstrap-owned lid query uses provisioned Field register");
            declare Other : B.State (1, Raw'Length, Raw'Length); begin
               Lids.Query (Other, Item, 1000, Admission, Result);
               Check (Admission = Lids.Not_Ready and then Result.State = Lid_Driver.Unknown
                 and then IO.Calls = Calls + 1, "binding does not bypass another session readiness");
            end;
         else
            Check (Search.Outcome = Lids.Not_Ready and then Search.Charged = 0
              and then IO.Calls = Calls, "failed/unconfigured startup never discovers devices");
         end if;
         Check (B.Device_Completion (Session) = Saved and then B.Startup_Charged (Session) = Charge,
           "runtime lid work does not rewrite startup results or charge");
      end;

   end;
   Ada.Text_IO.Put_Line ("Bootstrap driver checks" & Checks'Image);
end Bootstrap_Driver_Tests;
