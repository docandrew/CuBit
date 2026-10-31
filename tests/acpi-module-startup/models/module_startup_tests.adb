pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Snapshot_Startup;
with ACPI_Snapshot_Startup.With_Driver;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with ACPI_Bootstrap;
with ACPI_FADT.Transactions;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with Root_Test_Support; use Root_Test_Support;
procedure Module_Startup_Tests is
   package C renames ACPI_Service;
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Observation;
   First_ID : constant A.Register_ID := 16#8000_0000_0000_0041#;
   Second_ID : constant A.Register_ID := 16#FFFF_FFFF_FFFF_FF97#;
   -- Explicit synthetic provider, not the real backend catalog fixture.
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      First, Second : Unsigned_64 := 0;
      Deny_Writes : Boolean := False;
      Calls : Natural := 0;
      Arm_Unknown_Second_Write : Boolean := False;
      Receipt, Confirmed : A.Observation;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 3, Base => 0, Length => 2),
         Byte_Offset => (if Target.Register = First_ID then 0 else 1),
         Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := not Pending (IO) and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then Target.Register in First_ID | Second_ID;
   end Describe;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Geometry : AML_Field_Layout.Register_Geometry;
      OK : Boolean;
   begin
      Reply := (others => <>);
      Describe (IO, Item.Destination, Geometry, OK);
      if not OK then return; end if;
      IO.Calls := IO.Calls + 1;
      if Item.For_Write then
         if Item.Destination.Register = First_ID then IO.First := Item.Value;
         else IO.Second := Item.Value; end if;
      end if;
      if Item.For_Write and then IO.Arm_Unknown_Second_Write
        and then Item.Destination.Register = Second_ID then
         IO.Arm_Unknown_Second_Write := False;
         IO.Receipt := (True, IO.Owner, Item, 1);
         Reply := (A.Indeterminate, IO.Owner, 0, 1);
      else
         Reply := (A.Completed, IO.Owner,
           (if Item.For_Write then 0 elsif Item.Destination.Register = First_ID
            then IO.First else IO.Second), A.No_Operation);
      end if;
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then IO.Confirmed = Expected; end Confirmed;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then not (For_Write and then IO.Deny_Writes);
   end Authorize;
   package Q renames ACPI_Requests;
   package L renames ACPI_Snapshot_Startup;
   package R renames L.R;
   use type L.Outcome, L.State_Access, R.S.Table_ID;
   use type ACPI_Bootstrap.Device_Phase;
   use type ACPI_Bootstrap.Finish_Stage, ACPI_Bootstrap.Phase;
   use type C.Import_Phase;
   use type C.Install_Status;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
      Authorize_Target => Authorize, Maximum_Bindings => 2);
   Checks : Natural := 0;
   Mode : constant String := Ada.Command_Line.Argument (1);
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Table (Revision : Unsigned_8) return Bytes is
      Region_Space : constant Unsigned_8 := (if Mode = "forged-space" then 0 else 3);
      Region_Base : constant Bytes := (if Mode = "forged-base" then [16#0A#,16#10#] else [0]);
      Prefix : constant Bytes := [16#5B#,16#80#] & Enc ("REG0") & [Region_Space] & Region_Base & [16#0A#,2]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [1] & Enc ("FLD1") & [8] & Enc ("FLD2") & [8])
        & [16#70#,16#0A#,16#33#] & Enc ("FLD1");
      Suffix : constant Bytes := (if Mode = "fatal" then [16#5B#,16#32#,1,0,0,0,0,16#0A#,7]
        elsif Mode = "read" then [16#70#] & Enc ("FLD1") & Enc ("FLD2")
          & [16#70#,16#0A#,16#44#] & Enc ("FLD1")
        else [16#70#,16#0A#,16#E1#] & Enc ("FLD2") & [16#70#,16#0A#,16#44#] & Enc ("FLD1"));
      Payload : constant Bytes := [16#08#] & Enc ("MARK") & [0] & Prefix & Suffix
        & Method ("_INI", [16#70#,16#0A#,16#5A#] & Enc ("MARK"));
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255); end loop;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Data : constant Bytes := Table (Revision);
         HW : Context;
         Targets : F.Module_Target_Set := [others => A.No_Target];
         Count : F.Module_Target_Count := 2;
         Infos, Copies, Modules, Evaluations, Barriers : Natural := 0;
         procedure Info (Item : out L.Inventory; Status : out R.Copy_Status) is
         begin
            Infos := Infos + 1;
            Item := (True, 1, L.Count (Data'Length), L.Count (Data'Length)); Status := R.Copy_OK;
         end Info;
         procedure Describe_Table (ID : Positive; Item : out L.Description; Status : out R.Copy_Status) is
         begin
            Check (ID = 1, "descriptor identity");
            Item := ("DSDT", Revision, L.Count (Data'Length)); Status := R.Copy_OK;
         end Describe_Table;
         procedure Copy (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
            Copied : out R.Byte_Count; Status : out R.Copy_Status) is
         begin
            Check (ID = 1, "copy identity"); Copies := Copies + 1;
            for I in Into'Range loop Into (I) := Data (Data'First + Offset + I - Into'First); end loop;
            Copied := R.Byte_Count (Into'Length); Status := R.Copy_OK;
         end Copy;
         procedure Clock (Now : out R.Tick; Available : out Boolean) is
         begin Now := 0; Available := True; end Clock;
         procedure Execute_Modules
           (Service : in out C.State; Budget : Natural;
            Report : out C.Execution_Report; Status : out C.Values.Access_Status) is
         begin
            Modules := Modules + 1;
            F.Execute_Frozen (Service, HW, Targets, Count, Budget, Report, Status);
            if Mode = "overbudget" then Report.Modules.Charged := Budget + 1; end if;
            if Mode = "callback-exception" then raise Program_Error with "modeled module adapter exception"; end if;
         end Execute_Modules;
         procedure Evaluate
           (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
            Result : out C.Values.Result; Status : out C.Values.Access_Status) is
         begin
            Evaluations := Evaluations + 1;
            Check (Modules = 1 and then HW.First = 16#44# and then HW.Calls >= 3,
              "module hardware effects precede device initializer");
            Check (C.Current_Import_Phase (Service) = C.Catalog_Executed, "initializer follows module completion");
            C.Evaluate_Named (Service, Item, Budget, Result, Status);
         end Evaluate;
         procedure Regions
           (Service : aliased in out C.State; Budget : Natural;
            Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
            Node : C.Namespace.Node_ID;
            Item : C.Node_Descriptor;
            Result : C.Values.Result;
            Status : C.Values.Access_Status;
         begin
            Barriers := Barriers + 1;
            Check (Evaluations = 1, "root initializer precedes barrier");
            C.Find_Child (Service, C.Namespace.Root, "MARK", Node, Status);
            Check (Status = C.Values.Available, "marker present");
            C.Describe_Node (Service, Node, Item, Status);
            C.Evaluate_Named (Service, Item, Budget, Result, Status);
            Check (Status = C.Values.Available and then Result.Status = E.Returned
              and then Result.Number = 16#5A#, "actual root initializer effect");
            Charged := Result.Charged;
            Outcome := ACPI_Device_Policy.Regions_Connected;
         end Regions;
         package Driver is new ACPI_Device_Initialization (C, Regions, Evaluate);
         package Controlled is new L.With_Driver
           (Driver, Info, Describe_Table, 4096, Copy, Clock, Execute_Modules);
         package Unselected is new L.With_Driver
           (Driver, Info, Describe_Table, 4096, Copy, Clock);
         procedure Default_Load is new L.Load (Info, Describe_Table, 4096, Copy, Clock);
         Run : Driver.Session;
         Session : Controlled.Session;
         Fresh : Controlled.Session;
         Other : Unselected.Session;
         Owner, Again : L.State_Access;
         Report, Repeated : L.Report;
         Good : Boolean;
      begin
         AML_Identity.Issuer.Issue (HW.Owner, Good); Check (Good, "provider identity");
         Targets (1) := (Provider => HW.Owner, Endpoint => 7, Register => First_ID, Epoch => 1);
         Targets (2) := (Provider => HW.Owner, Endpoint => 7, Register => Second_ID, Epoch => 1);
         if Mode = "absent" then Targets := [others => A.No_Target]; Count := 0;
         elsif Mode = "ambiguous" then Targets (2) := Targets (1);
         elsif Mode = "unknown" then HW.Arm_Unknown_Second_Write := True;
         elsif Mode = "denied" then HW.Deny_Writes := True;
         elsif Mode = "stale" then Targets (1).Epoch := 2; Targets (2).Epoch := 2;
         end if;
         if Mode = "default" then Default_Load (10, Owner, Report);
         elsif Mode = "unselected" then Unselected.Load (Other, Run, 10, Owner, Report);
         else Controlled.Load (Session, Run, 10, Owner, Report);
         end if;
         if Mode in "normal" | "read" then
            Check (Report.Status = L.Loaded and then Owner /= null and then not Report.Owner_Retained
              and then Report.Completion.Execution.Modules.Outcome = C.Values.Modules_Completed, "snapshot completed");
            Check (Modules = 1 and then Evaluations = 1 and then Barriers = 1, "selected ordered startup");
            Check (Q.Device_Completion (Owner.all).Stage = ACPI_Bootstrap.Devices_Completed, "device completion separate and complete");
            Check (HW.First = 16#44# and then HW.Second = (if Mode = "read" then 16#33# else 16#E1#), "exact module values");
         elsif Mode in "callback-exception" | "overbudget" then
            Check (Report.Status = L.Completion_Callback_Failed and then Owner /= null and then Report.Owner_Retained, "module exception owner retained");
            Check (Modules = 1 and then HW.Calls = 3 and then Evaluations = 0 and then Barriers = 0, "exception cannot initialize devices after effects");
         else
            Check (Report.Status = L.Startup_Refused, "failed module stops startup");
            Check (Evaluations = 0 and then Barriers = 0, "no device callbacks after module refusal");
            Check ((Owner = null) = (Mode = "default"), "only default hardware-free failure releases owner");
            Check (Report.Owner_Retained = (Mode /= "default"), "controlled failure retention");
            if Mode = "unknown" then
               Check (HW.Calls = 2 and then Pending (HW) and then HW.First = 16#33#,
                 "uncertain operation and earlier effect retained");
               Check (Report.Completion.Execution.Modules.Failure = E.Field_Indeterminate, "module uncertainty reported");
            elsif Mode = "fatal" then
               Check (HW.Calls = 1 and then HW.First = 16#33# and then Report.Fatal.Present,
                 "Fatal preserves earlier write");
            else Check (HW.Calls = 0, "default or unprovisioned geometry cannot dispatch");
            end if;
         end if;
         if Mode not in "default" | "unselected" then
            declare
               Prior_Infos : constant Natural := Infos;
               Prior_Copies : constant Natural := Copies;
               Prior_Calls : constant Natural := HW.Calls;
            begin
               Controlled.Load (Session, Run, 10, Again, Repeated);
               Check (Repeated.Status = L.Startup_Already_Consumed and then Again = Owner, "same attempt cannot replay");
               Check (Infos = Prior_Infos and then Copies = Prior_Copies and then HW.Calls = Prior_Calls and then Modules = 1,
                 "no source or hardware replay");
            end;
         end if;
         if Mode /= "default" then
            declare
               Prior_Infos : constant Natural := Infos;
               Prior_Copies : constant Natural := Copies;
               Prior_Calls : constant Natural := HW.Calls;
               Prior_Modules : constant Natural := Modules;
               Another : ACPI_Bootstrap.State (1, 4096, 4096, Load_Capacity => 1);
               Installed : C.Install_Status;
               Accepted : Boolean;
               package Direct is new ACPI_Bootstrap.With_Driver (Driver, Execute_Modules);
            begin
               Check (Driver.Startup_Claimed (Run), "module attempt claimed driver before device phase");
               Controlled.Load (Fresh, Run, 10, Again, Repeated);
               Check (Repeated.Status = L.Startup_Already_Consumed and then Again = null,
                 "fresh controller cannot replay a failed module attempt");
               Check (Infos = Prior_Infos and then Copies = Prior_Copies and then HW.Calls = Prior_Calls
                 and then Modules = Prior_Modules, "fresh controller rejected before source or effects");
               ACPI_Bootstrap.Start (Another, 1, Accepted); Check (Accepted, "direct bootstrap admitted");
               ACPI_Bootstrap.Import_Table (Another, 1, C.DSDT, Data, Installed);
               Check (Installed = C.Installed, "direct bootstrap copied table");
               Direct.Finish (Another, Run);
               Check (ACPI_Bootstrap.Current (Another) = ACPI_Bootstrap.Failed
                 and then ACPI_Bootstrap.Completion (Another).Stage = ACPI_Bootstrap.Driver_Reused,
                 "direct bootstrap refuses reused driver before modules");
               Check (HW.Calls = Prior_Calls and then Modules = Prior_Modules, "direct entry cannot replay effects");
            end;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Module startup checks" & Checks'Image);
end Module_Startup_Tests;
