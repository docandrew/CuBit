pragma Ada_2022;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with ACPI_Service;
with ACPI_Snapshot_Startup;
with ACPI_Native_Snapshot_Load;
with ACPI_Native_Snapshot_Read;
with ACPI_Native_Snapshot_Load.With_Driver;
with Native_Test_Control;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with ACPI_FADT.Transactions;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with Root_Test_Support; use Root_Test_Support;
procedure Native_Controlled_Tests is
   package C renames ACPI_Service;
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
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
      Refuse_After_First_Write : Boolean := False;
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
      if Item.For_Write and then IO.Refuse_After_First_Write then
         IO.Refuse_After_First_Write := False;
         IO.Deny_Writes := True;
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
   package L renames ACPI_Snapshot_Startup;
   package P renames ACPI_Native_Snapshot_Load;
   package K renames ACPI_Native_Snapshot_Read.K;

   package T renames Native_Test_Control;
   use type L.Outcome, L.State_Access;
   use type K.Outcome, K.Byte_Count, K.Table_ID;
   use type ACPI_Native_Snapshot_Read.Kernel_Observation;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
      Authorize_Target => Authorize, Maximum_Bindings => 2);
   HW : Context;
   Targets : F.Module_Target_Set := [others => A.No_Target];
   Checks, Modules, Evaluations, Barriers : Natural := 0;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Revision : constant Unsigned_8 := Unsigned_8'Value (Ada.Command_Line.Argument (2));
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Table return Bytes is
      Padding : constant Bytes (1 .. K.Chunk_Max) := [others => 16#A3#];
      Payload : constant Bytes := [16#08#] & Enc ("MARK") & [0]
        & [16#5B#,16#80#] & Enc ("REG0") & [3,0,16#0A#,2]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [1] & Enc ("FLD1") & [8] & Enc ("FLD2") & [8])
        & [16#70#,16#0A#,16#33#] & Enc ("FLD1")
        & [16#70#,16#0A#,16#E1#] & Enc ("FLD2")
        & [16#70#,16#0A#,16#44#] & Enc ("FLD1") & Padding
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
   Data : constant Bytes := Table;
   procedure Evaluate
     (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Evaluations := Evaluations + 1;
      Check (Modules = 1 and then HW.Calls = 3, "native source modules precede _INI");
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
      C.Find_Child (Service, C.Namespace.Root, "MARK", Node, Status);
      Check (Status = C.Values.Available and then Evaluations = 1, "native root marker identity");
      C.Describe_Node (Service, Node, Item, Status);
      C.Evaluate_Named (Service, Item, Budget, Result, Status);
      Check (Status = C.Values.Available and then Result.Status = E.Returned
        and then Result.Number = 16#5A#, "native source real initializer effect");
      Charged := Result.Charged; Outcome := ACPI_Device_Policy.Regions_Connected;
   end Regions;
   package Driver is new ACPI_Device_Initialization (C, Regions, Evaluate);
   Run : Driver.Session;
   procedure Execute_Modules
     (Service : in out C.State; Budget : Natural;
      Result : out C.Execution_Report; Status : out C.Values.Access_Status);
   package Facade is new P.With_Driver (Driver, Execute_Modules);
   package Other_Facade is new P.With_Driver (Driver, Execute_Modules);
   procedure Execute_Modules
     (Service : in out C.State; Budget : Natural;
      Result : out C.Execution_Report; Status : out C.Values.Access_Status) is
   begin
      Modules := Modules + 1;
      F.Execute_Frozen (Service, HW, Targets, 2, Budget, Result, Status);
      T.Complete_Seen := True;
      if Mode = "reentry" then
         for I in 1 .. 2 loop
            declare Nested : P.Report; Borrowed : L.State_Access; begin
               Facade.Load (Run, 10, Borrowed, Nested);
               Check (Nested.Startup.Status = L.Startup_Already_Consumed and then Borrowed = null,
                 "nested load has an explicit refusal without clearing outer active guard");
            end;
         end loop;
      end if;
      if Mode = "module-exception" then raise Program_Error with "modeled module callback exception"; end if;
   end Execute_Modules;
   Owner, Again : L.State_Access;
   Report, Repeated : P.Report;
   Good : Boolean;
   Fault_Seen : Boolean := False;
   Fault_Expected : constant Boolean := Mode in "info-exception" | "module-exception";
begin
   T.Length := Data'Length;
   for I in Data'Range loop T.Data (I) := Data (I); end loop;
   AML_Identity.Issuer.Issue (HW.Owner, Good); Check (Good, "provider identity");
   HW.Refuse_After_First_Write := Mode = "module-refused";
   Targets (1) := (Provider => HW.Owner, Endpoint => 7, Register => First_ID, Epoch => 1);
   Targets (2) := (Provider => HW.Owner, Endpoint => 7, Register => Second_ID, Epoch => 1);
   Check (not Facade.Consumed, "fresh native facade");
   -- Fault catches belong to this hosted test only. The real runtime stops.
   begin
      if Mode = "default" then P.Load (10, Owner, Report);
      else Facade.Load (Run, 10, Owner, Report);
      end if;
   exception
      when Error : Program_Error =>
         Fault_Seen := True;
         Check (Fault_Expected and then Ada.Exceptions.Exception_Message (Error) =
           (if Mode = "info-exception" then "modeled source exception"
            else "modeled module callback exception"), "exact contract violation propagates");
   end;
   Check (Fault_Seen = Fault_Expected, "only modeled contract violations propagate");
   if Mode /= "default" then Check (Facade.Consumed, "native attempt consumed"); end if;
   if Fault_Expected then
      if Mode = "info-exception" then
         Check (T.Copies = 0 and then Modules = 0 and then HW.Calls = 0,
           "source violation precedes completion and effects");
      else
         Check (T.Copies = 2 and then Modules = 1 and then HW.Calls = 3 and then
           HW.First = 16#44# and then HW.Second = 16#E1# and then Evaluations = 0 and then Barriers = 0,
           "module violation records real prior effects and stops before device work");
      end if;
      declare
         Infos : constant Natural := T.Infos;
         Copies : constant Natural := T.Copies;
         Calls : constant Natural := HW.Calls;
      begin
         Facade.Load (Run, 10, Again, Repeated);
         Check (Repeated.Startup.Status = L.Startup_Already_Consumed and then Again = null,
           "fatal-contract facade cannot hand out or replay a partial owner");
         Check (T.Infos = Infos and then T.Copies = Copies and then HW.Calls = Calls,
           "host-only second call performs no source or hardware work");
      end;
      Ada.Text_IO.Put_Line ("Native controlled loader checks" & Checks'Image);
      return;
   end if;
   if Mode = "info-denied" then
      Check (Report.Startup.Status = L.Source_Refused and then Report.Catalog_Status = K.Not_Authorized, "catalog refusal translation");

   elsif Mode = "not-ready" then Check (Report.Startup.Status = L.Inventory_Not_Ready, "native inventory not ready");
   elsif Mode = "describe-denied" then
      Check (Report.Startup.Status = L.Source_Refused and then Report.Descriptor_Status = K.Not_Authorized, "descriptor refusal translation");
   elsif Mode = "clock-unavailable" then Check (Report.Startup.Status = L.Clock_Unavailable, "clock unavailable");
   elsif Mode = "early-deadline" then Check (Report.Startup.Status = L.Deadline_Expired and then Owner = null, "native deadline precedes copy");
   elsif Mode = "copy-refused" then
      Check (Report.Startup.Status = L.Copy_Refused and then Report.Last_Copy.Status = K.Copy_Failed
        and then Report.Last_Copy.Offset = K.Chunk_Max and then Owner = null, "last actual kernel-copy refusal preserved");
   elsif Mode = "default" then
      Check (Report.Startup.Status = L.Startup_Refused and then Owner = null and then Modules = 0 and then HW.Calls = 0, "native default keeps hardware-free path");
   elsif Mode = "module-refused" then
      Check (Report.Startup.Status = L.Startup_Refused and then Report.Startup.Owner_Retained and then Owner /= null,
        "typed module refusal retains the owner after completion entered");
      Check (HW.Calls = 1 and then HW.First = 16#33# and then HW.Second = 0 and then Evaluations = 0 and then Barriers = 0,
        "typed refusal preserves the first acknowledged effect and stops later work");
   elsif Mode = "late-deadline" then
      Check (Report.Startup.Status = L.Deadline_Expired and then Report.Startup.Owner_Retained and then Owner /= null,
        "native late completion retains owner");
   else
      Check (Report.Startup.Status = L.Loaded and then Owner /= null and then Modules = 1, "controlled native source completed");
      Check (HW.First = 16#44# and then HW.Second = 16#E1# and then HW.Calls = 3 and then Evaluations = 1 and then Barriers = 1,
        "native source correct module and device effects");
   end if;
   if Mode not in "info-denied" | "info-exception" | "not-ready" | "describe-denied" | "clock-unavailable" | "early-deadline" then
      Check (T.Copies = 2 and then Report.Last_Copy.Available and then Report.Last_Copy.Table = 1
        and then Report.Last_Copy.Offset = K.Chunk_Max and then Report.Last_Copy.Capacity = K.Chunk_Max,
        "native multi-chunk observation has exact identity/offset/capacity");
   end if;
   if Mode /= "default" then
      declare
         Prior_Infos : constant Natural := T.Infos;
         Prior_Copies : constant Natural := T.Copies;
         Prior_Clocks : constant Natural := T.Clock_Reads;
         Prior_Calls : constant Natural := HW.Calls;
      begin
         Facade.Load (Run, 10, Again, Repeated);
         Check (Repeated.Startup.Status = L.Startup_Already_Consumed and then Again = Owner, "native same facade cannot replay");
         Check (T.Infos = Prior_Infos and then T.Copies = Prior_Copies and then T.Clock_Reads = Prior_Clocks and then HW.Calls = Prior_Calls,
           "native repeat does not read kernel, clock or provider");
         Check (Repeated.Last_Copy = Report.Last_Copy and then Repeated.Catalog_Status = Report.Catalog_Status
           and then Repeated.Descriptor_Status = Report.Descriptor_Status, "native diagnostics survive repeat");
         if Mode in "normal" | "reentry" | "late-deadline" | "module-refused" then
            Other_Facade.Load (Run, 10, Again, Repeated);
            Check (Repeated.Startup.Status = L.Startup_Already_Consumed and then Again = null, "fresh native facade rejects claimed driver");
            Check (T.Infos = Prior_Infos and then T.Copies = Prior_Copies and then HW.Calls = Prior_Calls,
              "fresh native facade rejected before source/effects");
         end if;
      end;
   end if;
   Ada.Text_IO.Put_Line ("Native controlled loader checks" & Checks'Image);
end Native_Controlled_Tests;
