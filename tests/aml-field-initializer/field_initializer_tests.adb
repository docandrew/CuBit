pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with ACPI_Service_Core;
with ACPI_Field_Hosted_Provider;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with ACPI_Device_Initialization;
with ACPI_Device_Policy;
with ACPI_Lid;
with Root_Test_Support; use Root_Test_Support;
with Region_Initialization_Tests;
procedure Field_Initializer_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 1);
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   subtype Bytes is AML_Decode.Bytes;
   use type AML_Decode.Bytes;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Phase;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type A.Observation;
   use type A.Target;
   type Register_Index is range 1 .. 3;
   type Register_Values is array (Register_Index) of Unsigned_64;
   type Register_Counts is array (Register_Index) of Natural;
   type Hardware is record
      Values : Register_Values := [15, 0, 0];
      Reads, Writes : Register_Counts := [others => 0];
   end record;
   type Controller_Mode is (Ordinary, Unknown_Second_Write);
   Mode : Controller_Mode := Ordinary;
   procedure Transact (H : in out Hardware;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : T.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      use type ACPI_Region_Policy.Address_Space;
      Slot : Register_Index;
   begin
      if Space /= ACPI_Region_Policy.Memory_Space or else T.Octets (Width) /= 1 then
         raise Program_Error with "fixture bus geometry";
      end if;
      case Address is
         when 16#4000# => Slot := 1;
         when 16#4001# => Slot := 2;
         when 16#4002# => Slot := 3;
         when others => raise Program_Error with "fixture bus address";
      end case;
      Output := 0;
      if For_Write then
         H.Values (Slot) := Input; H.Writes (Slot) := H.Writes (Slot) + 1;
      else
         Output := H.Values (Slot); H.Reads (Slot) := H.Reads (Slot) + 1;
      end if;
      Completed := True;
      if Mode = Unknown_Second_Write and then For_Write and then Slot = 2 then
         Mode := Ordinary; Completed := False;
      end if;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Hardware, Transact,
      Maximum_Registers => 3, Maximum_Confirmations => 2);
   package F is new C.Field_IO
     (P.Context, P.Pending, P.Describe, P.Dispatch, P.Reconciled, Authorize_Target => P.Authorize,
      Maximum_Bindings => 4);
   IDs : constant array (Register_Index) of A.Register_ID :=
     [16#8000_0000_0000_0041#, A.Register_ID'Last, 16#C000_0000_0000_0097#];
   type Field_Names is array (Register_Index) of String (1 .. 4);
   Names : constant Field_Names := ["F001", "F002", "F003"];
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
   function Table (Revision : Unsigned_8; Payload : Bytes) return Bytes is
      Raw : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Raw (1 .. 4) := Enc ("DSDT");
      for I in 1 .. 4 loop Raw (4 + I) := Unsigned_8 ((Raw'Length / 256 ** (I - 1)) mod 256); end loop;
      Raw (9) := Revision; Raw (11 .. 16) := Enc ("HOSTED"); Raw (17 .. 24) := Enc ("INITTEST");
      Raw (37 .. Raw'Last) := Payload;
      for Item of Raw loop Sum := Sum + Item; end loop;
      Raw (10) := 0 - Sum; return Raw;
   end Table;
   function Store_Byte (Value : Unsigned_8; Name : String) return Bytes is
     ([16#70#,16#0A#,Value,16#5C#] & Enc (Name));
   Compound : constant Bytes := [16#A4#] & Block ([16#12#], [1,1]);
   Fields : constant Bytes := [16#5B#,16#80#] & Enc ("RGN0") &
     [0,16#0B#,0,16#40#,16#0A#,16] &
     Block ([16#5B#,16#81#], Enc ("RGN0") & [1] &
       Enc ("F001") & [8] & Enc ("F002") & [8] & Enc ("F003") & [8]);
   -- These are constructed AML regression cases, not new ACPICA observations.
   Code : constant Bytes := Fields &
     Method ("PACK", Compound) &
     Method ("_INI", Store_Byte (15, "F001") & Compound) &
     Block ([16#5B#,16#82#], Enc ("DEV0") &
       Block ([16#5B#,16#81#], [16#5C#] & Enc ("RGN0") & [1] & Enc ("_STA") & [8]) &
       Method ("_INI", Store_Byte (16#11#, "F001") & Store_Byte (16#22#, "F002") &
         Store_Byte (16#33#, "F003") & Compound)) &
     Block ([16#5B#,16#82#], Enc ("DEV1") &
       Method ("_STA", [16#A4#,16#5C#] & Enc ("F001")) &
       Method ("_INI", Store_Byte (16#44#, "F003") & Compound));
   -- Normal ASL compiled by ACPICA; the bytes below are the complete DSDT.
   -- Register/notification behavior is hosted, not SCI/GPE/EC integration.
   procedure Test_Lid is
      use type C.Namespace.Notification_Class;
      S : aliased C.State (1, 4096, 4096);
      IO : P.Context;
      Items : F.Binding_Set;
      Status : C.Values.Access_Status;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Node, Device, Lid_Method : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Value : C.Values.Result;
      Event : C.Notification_Observation;
      Present, OK : Boolean;
      Description : C.Values.Field_Description;
      Key : C.Values.Field_Key;
      Before : Hardware;
      Configuration : ACPI_Region_Policy.Configuration :=
        (Tag => 17, Base => 16#4000#, Length => 1,
         Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
         Widths => [others => False],
         Register_Authority => (Resource_ID => Unsigned_64 (IDs (1)),
           Readable => True, Writable => True, Delegable => False, Write_Mask => 255),
         Register_Offset => 0, Register_Width => T.Byte_Access);
      procedure Find (Parent : C.Namespace.Node_ID; Name : String;
        Found : out C.Namespace.Node_ID) is
      begin
         C.Find_Child (S, Parent, [Name (Name'First), Name (Name'First + 1), Name (Name'First + 2), Name (Name'First + 3)], Found, Status);
         Check (Status = C.Values.Available, "lid find " & Name);
      end Find;
      procedure Evaluate_Lid (Service : aliased in out C.State; Item : C.Node_Descriptor;
        Budget : Natural; Result : out C.Values.Result; Access_Result : out C.Values.Access_Status) is
      begin
         F.Evaluate_Named (Service, IO, Items, Item, Budget, Result, Access_Result);
      end Evaluate_Lid;
      package L is new ACPI_Lid (C, Evaluate_Lid);
      use type L.Query_Outcome;
      use type L.Lid_State;
      Device_Description, Root_Description : C.Node_Descriptor;
      Report : L.Query_Report;
      Identity : L.Identification_Report;
      use type L.Identification_Outcome;
      procedure Read_Lid (Expected : Unsigned_64) is
      begin
         Before := P.Observe_Hardware (IO);
         L.Query (S, Device_Description, 1000, Report);
         Check (Report.Outcome = L.Sampled and then Report.Charged <= 1000,
           "lid query sampled within budget");
         Check (Report.Raw = Expected and then Report.State = (if Expected = 0 then L.Closed else L.Open),
           "lid typed state and raw ACPICA value");
         Check (P.Observe_Hardware (IO).Writes = Before.Writes
           and then P.Observe_Hardware (IO).Reads (1) = Before.Reads (1) + 1,
           "lid query reads one register without writes");
      end Read_Lid;
      Raw : constant Bytes := [

        68,83,68,84,156,0,0,0,2,125,67,85,
        66,73,84,0,76,73,68,84,69,83,84,0,
        1,0,0,0,73,78,84,76,8,4,38,32,
        91,128,82,71,78,48,0,11,0,64,1,91,
        129,11,82,71,78,48,1,76,86,65,76,8,
        91,130,27,76,73,68,48,8,95,72,73,68,
        12,65,208,12,13,20,11,95,76,73,68,0,
        164,76,86,65,76,20,21,76,67,76,79,0,
        112,0,76,86,65,76,134,76,73,68,48,10,
        128,164,0,20,21,76,79,80,78,0,112,1,
        76,86,65,76,134,76,73,68,48,10,128,164,
        0,20,22,76,72,73,48,0,112,10,128,76,
        86,65,76,134,76,73,68,48,10,128,164,0];
      Methods : constant array (Positive range 1 .. 4) of String (1 .. 4) :=
        ["LCLO", "LOPN", "LHI0", "LHI0"];
      Expected : constant array (Methods'Range) of Unsigned_64 := [0, 1, 128, 128];
   begin
      Mode := Ordinary;
      C.Stage_Table (S, 42, C.DSDT, Raw, Staged);
      Check (Staged = C.Staged, "lid stage compiled table");
      C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "lid freeze");
      C.Execute_Frozen (S, 10000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed,
        "lid module load");
      C.Initialize_Members (S, Members, Status);
      Check (Status = C.Values.Available, "lid members");
      Configuration.Widths (T.Byte_Access) := True;
      P.Install (IO, 7, Configuration, True, OK); Check (OK, "lid named register install");
      Find (C.Namespace.Root, "LVAL", Node);
      C.Read_Field_Descriptor (S, Node, Description, Key, Status);
      Check (Status = C.Values.Available, "lid field descriptor");
      F.Add_Binding (S, IO, Key, (P.Identity (IO), 7, 1, IDs (1)), Items, Status);
      Check (Status = C.Values.Available, "lid field binding");
      Find (C.Namespace.Root, "LID0", Device);
      Find (Device, "_HID", Node);
      C.Describe_Node (S, Node, Item, Status);
      Check (Status = C.Values.Available, "lid HID descriptor");
      F.Evaluate_Named (S, IO, Items, Item, 1000, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = E.Returned
        and then Value.Number = 16#0D0C_D041#, "PNP0C0D ACPICA integer encoding");
      Find (Device, "_LID", Lid_Method);
      C.Configure_Notifications (S, True, Status);
      Check (Status = C.Values.Available, "lid notification subscription");
      C.Describe_Node (S, Device, Device_Description, Status);
      Check (Status = C.Values.Available, "lid device descriptor");
      Before := P.Observe_Hardware (IO);
      L.Query (S, C.Values.No_Node, 1000, Report);
      Check (Report.Outcome = L.Invalid_Device and then Report.State = L.Unknown
        and then Report.Raw = 0 and then P.Observe_Hardware (IO) = Before,
        "absent descriptor cannot select a device");
      C.Describe_Node (S, C.Namespace.Root, Root_Description, Status);
      Check (Status = C.Values.Available, "lid root descriptor");
      L.Query (S, Root_Description, 1000, Report);
      Check (Report.Outcome = L.Invalid_Device and then Report.State = L.Unknown
        and then P.Observe_Hardware (IO) = Before, "root is not a lid device");
      L.Query (S, Device_Description, 0, Report);
      Check (Report.Outcome = L.Budget_Exhausted and then Report.State = L.Unknown
        and then Report.Charged = 0 and then P.Observe_Hardware (IO) = Before,
        "typed zero budget has no effects or false closed sample");

      L.Identify (S, Device_Description, 1000, Identity);
      Check (Identity.Outcome = L.Matched and then Identity.Charged <= 1000
        and then C.Retained_Results (S) = 0 and then P.Observe_Hardware (IO) = Before,
        "compressed integer HID identifies lid without hardware effects");
      L.Identify (S, Device_Description, 0, Identity);
      Check (Identity.Outcome = L.No_Budget and then Identity.Charged = 0
        and then P.Observe_Hardware (IO) = Before, "HID zero budget has no effects");
      L.Identify (S, C.Values.No_Node, 1000, Identity);
      Check (Identity.Outcome = L.Not_Device and then Identity.Charged = 0,
        "HID invalid descriptor rejected");

      Read_Lid (15); -- Initial simulated register state, not an ACPICA observation.
      for I in Methods'Range loop
         Find (C.Namespace.Root, Methods (I), Node);
         C.Describe_Node (S, Node, Item, Status);
         Check (Status = C.Values.Available, "lid transition descriptor");
         Before := P.Observe_Hardware (IO);
         F.Evaluate_Named (S, IO, Items, Item, 0, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = E.Budget_Exceeded
           and then P.Observe_Hardware (IO) = Before and then C.Pending_Notifications (S) = 0,
           "lid zero budget has no hardware or notification effects");
         F.Evaluate_Named (S, IO, Items, Item, 1000, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = E.Returned
           and then Value.Number = 0, "lid transition returns ACPICA zero");
         Check (P.Observe_Hardware (IO).Values (1) = Expected (I)
           and then P.Observe_Hardware (IO).Writes (1) = Before.Writes (1) + 1,
           "lid transition writes named register once");
         Check (C.Pending_Notifications (S) = 1, "one notification per AML Notify");
         C.Take_Notification (S, Event, Present, Status);
         Check (Status = C.Values.Available and then Present and then Event.Current_Target
           and then Event.Node = Device and then Event.Path_Length = 1
           and then Event.Path (1) = "LID0" and then Event.Class = C.Namespace.Device_Notify
           and then Event.Number = 16#80#, "lid copied notification identity and value");
         Read_Lid (Expected (I));
         C.Take_Notification (S, Event, Present, Status);
         Check (Status = C.Values.Available and then not Present, "lid notification consumed once");
      end loop;
      declare
         Bad : aliased C.State (1, 4096, 4096);
         Bad_Device : C.Node_Descriptor;
         Bad_Node : C.Namespace.Node_ID;
         -- Constructed negative AML, not ACPICA reference observations.
         function HID_String (Text : String) return Bytes is
           ([16#08#] & Enc ("_HID") & [16#0D#] & Enc (Text) & [0]);
         -- Additional constructed identity cases, not new ACPICA observations.
         Identity_Code : constant Bytes :=
           Block ([16#5B#,16#82#], Enc ("STR0") & HID_String ("PNP0C0D")) &
           Block ([16#5B#,16#82#], Enc ("SHOR") & HID_String ("PNP0C0")) &
           Block ([16#5B#,16#82#], Enc ("SUFX") & HID_String ("PNP0C0DX")) &
           Block ([16#5B#,16#82#], Enc ("SNUM") & [16#08#] & Enc ("_HID") &
             [16#0E#,16#41#,16#D0#,16#0C#,16#0D#,1,0,0,0]) &
           Block ([16#5B#,16#82#], Enc ("BUF0") & [16#08#] & Enc ("_HID") &
             Block ([16#11#], [16#0A#,7] & Enc ("PNP0C0D"))) &
           Block ([16#5B#,16#82#], Enc ("BAD0") & Method ("_HID", Compound));

         Bad_Code : constant Bytes := Identity_Code &
           Block ([16#5B#,16#82#], Enc ("NOL0")) &
           Block ([16#5B#,16#82#], Enc ("PKG0") & [16#08#] & Enc ("_HID") & [0] & Method ("_LID", Compound)) &
           Block ([16#5B#,16#82#], Enc ("NRET") & Method ("_HID", [16#A3#]) & Method ("_LID", [16#A3#]));
         Bad_Names : constant array (Positive range 1 .. 3) of String (1 .. 4) :=
           ["NOL0", "PKG0", "NRET"];
         Outcomes : constant array (Bad_Names'Range) of L.Query_Outcome :=
           [L.Missing_Lid, L.Wrong_Result_Type, L.Evaluation_Failed];
      begin
         C.Stage_Table (Bad, 43, C.DSDT, Table (2, Bad_Code), Staged);
         Check (Staged = C.Staged, "negative lid stage");
         C.Freeze_Catalog (Bad, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "negative lid freeze");
         C.Execute_Frozen (Bad, 10000, Execution, Status);
         Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed,
           "negative lid modules");
         C.Initialize_Members (Bad, Members, Status);
         Check (Status = C.Values.Available, "negative lid members");
         for I in Bad_Names'Range loop
            C.Find_Child (Bad, C.Namespace.Root,
              [Bad_Names (I)(1), Bad_Names (I)(2), Bad_Names (I)(3), Bad_Names (I)(4)], Bad_Node, Status);
            Check (Status = C.Values.Available, "negative lid find");
            C.Describe_Node (Bad, Bad_Node, Bad_Device, Status);
            Check (Status = C.Values.Available, "negative lid descriptor");
            L.Query (Bad, Bad_Device, 1000, Report);
            Check (Report.Outcome = Outcomes (I) and then Report.State = L.Unknown
              and then Report.Raw = 0 and then C.Retained_Results (Bad) = 0,
              "negative lid is unknown and releases compound result");
         end loop;
         L.Query (S, Bad_Device, 1000, Report);
         Check (Report.Outcome = L.Invalid_Device and then Report.State = L.Unknown,
           "foreign arena device descriptor is rejected");
         L.Identify (S, Bad_Device, 1000, Identity);
         Check (Identity.Outcome = L.Not_Device, "HID foreign descriptor rejected");
         declare
            Identity_Names : constant array (Positive range 1 .. 9) of String (1 .. 4) :=
              ["NOL0", "PKG0", "NRET", "STR0", "SHOR", "SUFX", "SNUM", "BUF0", "BAD0"];
            Identity_Outcomes : constant array (Identity_Names'Range) of L.Identification_Outcome :=
              [L.No_HID, L.Not_Lid, L.Evaluation_Error, L.Matched, L.Not_Lid,
               L.Not_Lid, L.Not_Lid, L.Wrong_HID_Type, L.Wrong_HID_Type];
            Held : C.Values.Value_Handle;
         begin
            for I in Identity_Names'Range loop
               C.Find_Child (Bad, C.Namespace.Root,
                 [Identity_Names (I)(1), Identity_Names (I)(2), Identity_Names (I)(3), Identity_Names (I)(4)],
                 Bad_Node, Status);
               Check (Status = C.Values.Available, "HID fixture device find");
               C.Describe_Node (Bad, Bad_Node, Bad_Device, Status);
               Check (Status = C.Values.Available, "HID fixture descriptor");
               L.Identify (Bad, Bad_Device, 1000, Identity);
               Check (Identity.Outcome = Identity_Outcomes (I) and then Identity.Charged <= 1000
                 and then C.Retained_Results (Bad) = 0, "HID form/type and complete result cleanup");
            end loop;
            C.Find_Child (Bad, C.Namespace.Root, "PKG0", Bad_Node, Status);
            Check (Status = C.Values.Available, "HID capacity package device");
            C.Find_Child (Bad, Bad_Node, "_LID", Bad_Node, Status);
            Check (Status = C.Values.Available, "HID capacity package method");
            C.Describe_Node (Bad, Bad_Node, Item, Status);
            Check (Status = C.Values.Available, "HID capacity method descriptor");
            F.Evaluate_Named (Bad, IO, Items, Item, 1000, Value, Status);
            Check (Status = C.Values.Available and then Value.Status = E.Object_Returned
              and then C.Retained_Results (Bad) = 1, "HID capacity fill retained root");
            Held := Value.Handle;
            C.Find_Child (Bad, C.Namespace.Root, "STR0", Bad_Node, Status);
            Check (Status = C.Values.Available, "HID capacity string device");
            C.Describe_Node (Bad, Bad_Node, Bad_Device, Status);
            Check (Status = C.Values.Available, "HID capacity string descriptor");
            L.Identify (Bad, Bad_Device, 1000, Identity);
            Check (Identity.Outcome = L.Evaluation_Error and then Identity.Access_Result = C.Values.Root_Limit
              and then C.Retained_Results (Bad) = 1, "HID capacity failure preserves caller root");
            C.Release_Result (Bad, Held, Status);
            Check (Status = C.Values.Available and then C.Retained_Results (Bad) = 0,
              "HID capacity caller releases root");
         end;

      end;

   end Test_Lid;

   type Scenario is (Connected, Unknown_Write, Unavailable, Late_Provision);
begin
   Test_Lid;
   Checks := Checks + Region_Initialization_Tests;
   for Revision in Unsigned_8 range 1 .. 2 loop
      for Case_ID in Scenario loop
         declare
            S : aliased C.State (1, 4096, 4096);
            IO, Other : P.Context;
            Items : F.Binding_Set;
            Targets : array (Register_Index) of A.Target;
            Status : C.Values.Access_Status;
            Staged : C.Stage_Status;
            Frozen : C.Freeze_Status;
            Execution : C.Execution_Report;
            Members : C.Namespace.Initialization_Report;
            Node, Device : C.Namespace.Node_ID;
            Field, Pack : C.Node_Descriptor;
            Value, Held : C.Values.Result;
            Before, After : Hardware;
            OK : Boolean;
            Barrier_Calls : Natural := 0;
            Saved : A.Observation;
            procedure Find (Parent : C.Namespace.Node_ID; Name : String;
              Result : out C.Namespace.Node_ID) is
            begin
               C.Find_Child (S, Parent,
                 [Name (Name'First), Name (Name'First + 1), Name (Name'First + 2), Name (Name'First + 3)],
                 Result, Status);
               Check (Status = C.Values.Available, "find " & Name);
            end Find;
            procedure Provision is
               Description : C.Values.Field_Description;
               Key : C.Values.Field_Key;
            begin
               for I in Register_Index loop
                  declare
                     Configuration : ACPI_Region_Policy.Configuration :=
                       (Tag => 17, Base => 16#4000#, Length => 16,
                        Space => ACPI_Region_Policy.Memory_Space, Readable => True, Writable => True,
                        Widths => [others => False],
                        Register_Authority => (Resource_ID => Unsigned_64 (IDs (I)),
                          Readable => True, Writable => True, Delegable => False, Write_Mask => 255),
                        Register_Offset => Unsigned_64 (I - Register_Index'First),
                        Register_Width => T.Byte_Access);
                  begin
                     Configuration.Widths (T.Byte_Access) := True;
                     P.Install (IO, 7, Configuration, True, OK); Check (OK, "provision actual catalog");
                     Targets (I) := (P.Identity (IO), 7, 1, IDs (I));
                     Find (C.Namespace.Root, Names (I), Node);
                     C.Read_Field_Descriptor (S, Node, Description, Key, Status);
                     Check (Status = C.Values.Available, "root descriptor");
                     F.Add_Binding (S, IO, Key, Targets (I), Items, Status);
                     Check (Status = C.Values.Available, "root binding");
                  end;
               end loop;
               Find (C.Namespace.Root, "DEV0", Device); Find (Device, "_STA", Node);
               C.Read_Field_Descriptor (S, Node, Description, Key, Status);
               Check (Status = C.Values.Available, "direct _STA descriptor");
               F.Add_Binding (S, IO, Key, Targets (1), Items, Status);
               Check (Status = C.Values.Available, "direct _STA binding");
               C.Describe_Node (S, Node, Field, Status);
               Check (Status = C.Values.Available, "named _STA descriptor");
            end Provision;
            procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor;
              Budget : Natural; Result : out C.Values.Result; Access_Result : out C.Values.Access_Status) is
            begin
               F.Evaluate_Named (Service, IO, Items, Item, Budget, Result, Access_Result);
            end Evaluate;
            procedure Regions (Service : aliased in out C.State; Budget : Natural;
              Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
            begin
               pragma Assert (C.Namespace_Load_Phase (Service) = C.Values.Ready);
               Check (Budget <= 10000, "barrier residual fuel");
               Barrier_Calls := Barrier_Calls + 1; Charged := 0;
               -- This fixture contains no _REG. This is a trusted modeled
               -- connection barrier; it does not perform AML notification.
               Outcome := (if Case_ID = Unavailable then ACPI_Device_Policy.Regions_Unavailable
                 else ACPI_Device_Policy.Regions_Connected);
            end Regions;
            package Init is new ACPI_Device_Initialization (C, Regions, Evaluate);
            use type Init.Phase;
            Run : Init.Session;
         begin
            Mode := Ordinary;
            C.Stage_Table (S, 41, C.DSDT, Table (Revision, Code), Staged);
            Check (Staged = C.Staged, "stage");
            C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
            C.Execute_Frozen (S, 10000, Execution, Status);
            Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "modules");
            C.Initialize_Members (S, Members, Status); Check (Status = C.Values.Available, "members");
            if Case_ID /= Late_Provision then
               Provision;
               Find (C.Namespace.Root, "PACK", Node); C.Describe_Node (S, Node, Pack, Status);
               Check (Status = C.Values.Available, "package method descriptor");
               F.Evaluate_Named (S, IO, Items, Pack, 1000, Held, Status);
               Check (Status = C.Values.Available and then Held.Status = E.Object_Returned and then
                 C.Retained_Results (S) = 1, "held compound fills result capacity");
               Before := P.Observe_Hardware (IO);
               F.Evaluate_Named (S, IO, Items, Field, 0, Value, Status);
               Check (Status = C.Values.Available and then Value.Status = E.Budget_Exceeded and then
                 Value.Charged = 0 and then P.Observe_Hardware (IO) = Before, "zero budget before dispatch");
               F.Evaluate_Named (S, IO, Items, Field, 1, Value, Status);
               Check (Status = C.Values.Available and then Value.Status = E.Returned and then
                 Value.Number = 15 and then Value.Charged = 1 and then C.Retained_Results (S) = 1,
                 "direct field charge without result root");
               C.Evaluate_Named (S, Field, 1, Value, Status);
               Check (Status = C.Values.Root_Limit, "default resolver retains original reservation semantics");
               C.Release_Result (S, Held.Handle, Status); Check (Status = C.Values.Available, "release pressure root");
            end if;
            Init.Begin_Initialization (Run, S, 10000, OK);
            Check (OK and then Init.Observe (Run).Stage = Init.Awaiting_Regions, "begin consumes compatibility phase");
            Check (C.Retained_Results (S) = 0, "compound root _INI released");
            if Case_ID = Late_Provision then
               Check (Init.Observe (Run).Errors = 1 and then P.Observe_Hardware (IO).Writes (1) = 0,
                 "catalog missing before Begin is visible failure");
               Provision;
            else
               Check (Init.Observe (Run).Errors = 0 and then P.Observe_Hardware (IO).Writes (1) = 1,
                 "root _INI uses provisioned context");
            end if;
            Init.Begin_Initialization (Run, S, 10000, OK); Check (not OK, "Begin one shot");
            Before := P.Observe_Hardware (IO);
            if Case_ID = Unknown_Write then Mode := Unknown_Second_Write; end if;
            Init.Complete_Initialization (Run, S, OK);
            Check (OK and then Barrier_Calls = 1, "barrier once");
            After := P.Observe_Hardware (IO);
            Check (C.Retained_Results (S) = 0, "all initializer results released");
            case Case_ID is
               when Connected | Late_Provision =>
                  Check (Init.Observe (Run).Stage =
                    (if Case_ID = Connected then Init.Completed else Init.Completed_With_Errors), "completion scope");
                  Check (After.Values = [16#11#,16#22#,16#44#] and then
                    After.Writes (3) = 2 and then After.Reads (1) = Before.Reads (1) + 2,
                    "direct and method _STA plus both device initializers");
               when Unavailable =>
                  Check (Init.Observe (Run).Stage = Init.Incomplete and then After = Before,
                    "unavailable barrier prevents device walk");
               when Unknown_Write =>
                  Check (Init.Observe (Run).Stage = Init.Failed and then
                    After.Values (1) = 16#11# and then After.Values (2) = 16#22# and then
                    After.Writes (3) = 0, "unknown preserves first effects and stops later writes/device");
                  Saved := F.Uncertain (S);
                  Check (Saved.Present and then Saved = P.Uncertain (IO) and then
                    Saved.Item.Destination = Targets (2), "selected uncertainty attribution");
                  F.Evaluate_Named (S, Other, Items, Field, 1000, Value, Status);
                  Check (Status = C.Values.Available and then Value.Status = E.Field_Indeterminate,
                    "substituted context cannot bypass arena latch");
                  C.Evaluate_Named (S, Pack, 1000, Value, Status);
                  Check (Value.Status = E.Field_Indeterminate, "default method blocked");
                  P.Confirm_Completion (IO, Saved, True, OK); Check (OK, "trusted confirmation");
                  F.Reconcile (S, IO, Saved, Status); Check (Status = C.Values.Available, "own arena reconciliation");
                  P.Retire_Confirmation (IO, Saved, True, OK); Check (OK, "trusted history retirement");
                  Check (P.Observe_Hardware (IO) = After, "recovery never replays effects");
            end case;
            Init.Complete_Initialization (Run, S, OK);
            Check (not OK and then Barrier_Calls = 1 and then P.Observe_Hardware (IO) = After,
              "consumed initialization never resumes");
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Field initializer checks" & Checks'Image);
end Field_Initializer_Tests;
