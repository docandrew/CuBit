pragma Ada_2022;
with Interfaces; use Interfaces;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with ACPI_Service_Core;
with ACPI_Device_Policy;
with ACPI_Device_Initialization;
with ACPI_Region_Initialization;
with ACPI_Field_Hosted_Provider;
with ACPI_Region_Policy;
with ACPI_FADT.Transactions;
with Root_Test_Support; use Root_Test_Support;
with Region_Reg_Oracle;
function Region_Initialization_Tests return Natural is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 2);
   package E renames AML_Execute;
   package N renames C.Namespace;
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type N.Lookup_Status;
   use type N.Region_Space_ID;
   use type N.Operation_Region_Attributes;
   use type C.Values.Value_Handle;
   use type ACPI_Device_Policy.Region_Outcome;
   type Scenario is (Ordinary, No_Method, Memory_Only, IO_Only, Table_Only,
      PCI_With_BBN, PCI_Without_BBN, No_Regions, Unavailable,
      Invalid_Method, Wrong_Arity, Compound_Return, Division_Failure,
      Fatal_Failure, Unknown_Write, Readiness_Lost, No_Budget);
   Selected : Scenario := Ordinary;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "region init " & Selected'Image & " " & Why & Checks'Image; end if;
   end Check;
   type Hardware is record
      Value : Unsigned_64 := 0;
      Writes : Natural := 0;
   end record;
   procedure Transact (H : in out Hardware; Space : ACPI_Region_Policy.Address_Space;
      Address : Unsigned_64; Width : T.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      use type ACPI_Region_Policy.Address_Space;
   begin
      Check (Space = ACPI_Region_Policy.Memory_Space and then Address = 16#4000#
         and then T.Octets (Width) = 1,"named memory handler geometry");
      Completed := True; Output := H.Value;
      if For_Write then
         H.Writes := H.Writes + 1; H.Value := Input; Output := 0;
         Completed := Selected /= Unknown_Write;
      end if;
   end Transact;
   package P is new ACPI_Field_Hosted_Provider (Hardware, Transact);
   package F is new C.Field_IO
     (P.Context,P.Pending,P.Describe,P.Dispatch,P.Reconciled,Authorize_Target => P.Authorize);
   type Controller is limited record
      IO : P.Context;
      Bindings : F.Binding_Set;
      Ready : Boolean := True;
      Calls : Natural := 0;
      Last_Space, Last_Code : Unsigned_64 := 0;
   end record;
   function Ready (Context : Controller; Region : C.Node_Descriptor;
      Data : N.Operation_Region_Attributes) return Boolean is
      pragma Unreferenced (Region);
   begin
      return Context.Ready and then not (Selected = Unavailable and then Data.Space = 3);
   end Ready;
   function PCI_Root (Context : Controller; Region : C.Node_Descriptor) return Boolean is
      pragma Unreferenced (Context, Region);
   begin return Selected in PCI_With_BBN | PCI_Without_BBN; end PCI_Root;
   procedure Invoke
     (Service : aliased in out C.State; Context : in out Controller;
      Node : N.Node_ID; Args : E.Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Check (Argument_Count = 2,"two _REG arguments");
      Context.Calls := Context.Calls + 1;
      Context.Last_Space := Args (0); Context.Last_Code := Args (1);
      F.Invoke_Retained (Service,Context.IO,Context.Bindings,Node,Args,Argument_Count,Budget,Result,Status);
      if Selected = Readiness_Lost then Context.Ready := False; end if;
   end Invoke;
   package Regions is new ACPI_Region_Initialization (C,Controller,Ready,PCI_Root,Invoke);
   use type Regions.Phase;
   function Name (Part : String; Value : Bytes) return Bytes is ([16#08#] & Enc (Part) & Value);
   function Absolute (Part : String) return Bytes is ([16#5C#] & Enc (Part));
   function Store (Value : Bytes; Part : String) return Bytes is ([16#70#] & Value & Absolute (Part));
   function Increment (Part : String) return Bytes is ([16#75#] & Absolute (Part));
   function Region (Part : String; Space : Unsigned_8) return Bytes is
     ([16#5B#,16#80#] & Enc (Part) & [Space,0,1]);
   function Payload return Bytes is
      Space : constant Unsigned_8 := (case Selected is when Memory_Only => 0,
         when IO_Only => 1,when Table_Only => 126,
         when PCI_With_BBN | PCI_Without_BBN => 2,when others => 3);
      Prefix : constant Bytes := Increment ("CALL") & Store ([16#68#],"LAST")
         & Store ([16#69#],"CONN") & Store ([16#69#],"HWFL");
      Body_Code : constant Bytes := Prefix &
        (case Selected is
           when Compound_Return => [16#A4#] & Block ([16#12#], [1] & Block ([16#11#],[16#0A#,2,16#41#,16#42#])),
           when Division_Failure => [16#78#,1,0,0,0],
           when Fatal_Failure => [16#5B#,16#32#,1,16#78#,16#56#,16#34#,16#12#,16#0A#,16#42#],
           when others => Bytes'[]);
      Reg_Method : constant Bytes :=
        (if Selected = No_Method then Bytes'[]
         elsif Selected = Invalid_Method then Name ("_REG",[0])
         else Block ([16#14#],Enc ("_REG") & Bytes'[(if Selected = Wrong_Arity then 1 else 2)] & Body_Code));
      Memory : constant Bytes := [16#5B#,16#80#] & Enc ("MEM0") & [0,16#0C#,0,16#40#,0,0,1]
         & Block ([16#5B#,16#81#],Enc ("MEM0") & [1] & Enc ("HWFL") & [8]);
   begin
      if Selected = No_Regions then return Name ("CALL",[0]); end if;
      return Name ("CALL",[0]) & Name ("LAST",[0]) & Name ("CONN",[0])
         & Name ("DONE",[0]) & Name ("ROOT",[0]) & Name ("SEQN",[0]) & Memory
         & Method ("_REG",Increment ("ROOT"))
         & Method ("_INI",Increment ("SEQN"))
         & Block ([16#10#],Enc ("_SB_") & Method ("_INI",Increment ("SEQN")))
         & Block ([16#5B#,16#82#],Enc ("DEV0") & Region ("EC00",Space) & Region ("EC01",Space)
           & (if Selected = PCI_With_BBN then Name ("_BBN",[0]) else Bytes'[])
           & Reg_Method & Method ("_STA",[16#A4#,16#0A#,15])
           & Method ("_INI",Store (Absolute ("CALL"),"DONE")));
   end Payload;
   function Table (Revision : Unsigned_8; Code : Bytes) return Bytes is
      Data : Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length),I * 8) and 255); end loop;
      Data (37 .. Data'Last) := Code;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
   procedure Prepare (S : aliased in out C.State; Revision : Unsigned_8; Image : Bytes := Bytes'[]) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Report : C.Execution_Report;
      Members : N.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (S,1,C.DSDT,(if Image'Length = 0 then Table (Revision,Payload) else Image),Staged);
      Check (Staged = C.Staged,"stage");
      C.Freeze_Catalog (S,Frozen); Check (Frozen = C.Catalog_Frozen_OK,"freeze");
      C.Execute_Frozen (S,10000,Report,Status);
      Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed,"execute");
      C.Initialize_Members (S,Members,Status); Check (Status = C.Values.Available,"members");
   end Prepare;
   procedure Expect (S : in out C.State; Part : String; Number : Unsigned_64) is
      Found : N.Lookup_Result;
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Find_Object (S,N.Root,[Part (Part'First),Part (Part'First + 1),
         Part (Part'First + 2),Part (Part'First + 3)],Found,Status);
      Check (Status = C.Values.Available and then Found.Status = N.Found,"observe counter");
      C.Observe_Named_Value (S,Found.Node,Value,Status);
      Check (Status = C.Values.Available and then Value.Status = E.Returned and then Value.Number = Number,"counter value " & Part);
   end Expect;
   procedure Provision (S : C.State; Context : in out Controller) is
      Config : ACPI_Region_Policy.Configuration :=
        (Tag => 17,Base => 16#4000#,Length => 1,Space => ACPI_Region_Policy.Memory_Space,
         Readable => True,Writable => True,Widths => [others => False],
         Register_Authority => (Resource_ID => 16#8000_0000_0000_0081#,Readable => True,
           Writable => True,Delegable => False,Write_Mask => 255),
         Register_Offset => 0,Register_Width => T.Byte_Access);
      Found : N.Lookup_Result;
      Description : C.Values.Field_Description;
      Key : C.Values.Field_Key;
      Status : C.Values.Access_Status;
      OK : Boolean;
   begin
      F.Clear (Context.Bindings); Config.Widths (T.Byte_Access) := True;
      P.Install (Context.IO,7,Config,True,OK); Check (OK,"named handler admission");
      C.Find_Object (S,N.Root,"HWFL",Found,Status); Check (Status = C.Values.Available and then Found.Status = N.Found,"hardware field");
      C.Read_Field_Descriptor (S,Found.Node,Description,Key,Status); Check (Status = C.Values.Available,"hardware descriptor");
      F.Add_Binding (S,Context.IO,Key,(P.Identity (Context.IO),7,1,A.Register_ID (Config.Register_Authority.Resource_ID)),
         Context.Bindings,Status); Check (Status = C.Values.Available,"hardware association");
   end Provision;
   procedure Check_Metadata (S : C.State; Other : aliased in out C.State; Revision : Unsigned_8) is
      Node : N.Node_ID;
      Description, Root : C.Node_Descriptor;
      Data : N.Operation_Region_Attributes;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (S,N.Root,"MEM0",Node,Status);
      Check (Status = C.Values.Available and then Node /= N.Root,"metadata selection");
      C.Describe_Node (S,Node,Description,Status); Check (Status = C.Values.Available,"metadata identity");
      C.Read_Region_Descriptor (S,Description,Data,Status);
      Check (Status = C.Values.Available and then Data.Space = 0 and then Data.Address = 16#4000#
         and then Data.Length = 1,"copied region metadata");
      C.Describe_Node (S,N.Root,Root,Status); Check (Status = C.Values.Available,"root metadata");
      C.Read_Region_Descriptor (S,Root,Data,Status);
      Check (Status = C.Values.Wrong_Kind and then Data = N.Operation_Region_Attributes'(others => <>),"wrong kind clears output");
      C.Read_Region_Descriptor (Other,Description,Data,Status);
      Check (Status = C.Values.Wrong_Phase and then Data = N.Operation_Region_Attributes'(others => <>),"wrong phase clears output");
      Prepare (Other,Revision);
      C.Read_Region_Descriptor (Other,Description,Data,Status);
      Check (Status = C.Values.Invalid_Value and then Data = N.Operation_Region_Attributes'(others => <>),"another service cannot reuse descriptor");
      C.Read_Region_Descriptor (S,C.Values.No_Node,Data,Status);
      Check (Status = C.Values.Invalid_Value and then Data = N.Operation_Region_Attributes'(others => <>),"empty descriptor clears output");
   end Check_Metadata;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      for Case_ID in Scenario loop
         Selected := Case_ID;
         declare
            S, Other : aliased C.State (1, 4096, 4096, 1);
            Context : Controller;
            Run : Regions.Session;
            OK : Boolean;
            Before : Hardware;
            Calls : Natural;
         begin
            Prepare (S,Revision);
            if Selected /= No_Regions then Provision (S,Context); end if;
            if Selected = Ordinary then
               Check_Metadata (S,Other,Revision);
               declare
                  Region_Run : Regions.Session;
                  procedure Barrier (Service : aliased in out C.State; Budget : Natural;
                     Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
                     Accepted : Boolean;
                  begin
                     Expect (Service,"SEQN",2);
                     Regions.Connect (Region_Run,Service,Context,Budget,Accepted);
                     Check (Accepted,"barrier consumes region phase");
                     Charged := Regions.Observe (Region_Run).Charged; Outcome := Regions.Outcome (Region_Run);
                  end Barrier;
                  procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor;
                     Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
                  begin F.Evaluate_Named (Service,Context.IO,Context.Bindings,Item,Budget,Result,Status); end Evaluate;
                  package Devices is new ACPI_Device_Initialization (C,Barrier,Evaluate);
                  use type Devices.Phase;
                  Device_Run : Devices.Session;
               begin
                  Devices.Begin_Initialization (Device_Run,S,10000,OK);
                  Check (OK and then Devices.Observe (Device_Run).Stage = Devices.Awaiting_Regions,"root and system bus prepass");
                  Devices.Complete_Initialization (Device_Run,S,OK);
                  Check (OK and then Devices.Observe (Device_Run).Stage = Devices.Completed,"_REG before device walk");
                  Expect (S,"DONE",2); Expect (S,"CALL",2); Expect (S,"ROOT",0);
                  Check (Context.Calls = 2 and then P.Observe_Hardware (Context.IO).Writes = 2,"two regions, real admitted Field effects");
                  Regions.Disconnect (Region_Run,S,Context,10000,OK);
                  Check (OK and then Regions.Observe (Region_Run).Stage = Regions.Disconnected,"disconnect paired with connect");
                  Expect (S,"CALL",4); Expect (S,"CONN",0);
               end;
            else
               Regions.Connect (Run,S,Context,(if Selected = No_Budget then 0 else 10000),OK);
               Check (OK,"first connect accepted");
               if Selected in Unavailable | Invalid_Method | Wrong_Arity | Division_Failure |
                 Fatal_Failure | Unknown_Write | Readiness_Lost | No_Budget
               then
                  Check (Regions.Observe (Run).Stage = Regions.Failed,"failed connection is terminal");
                  Check (C.Retained_Results (S) = 0,"no leaked returned handles");
                  if Selected in Unavailable | Invalid_Method then
                     Check (Context.Calls = 0 and then P.Observe_Hardware (Context.IO).Writes = 0,"full preflight before effects");
                  elsif Selected = Unknown_Write then
                     Check (Regions.Observe (Run).Error = E.Field_Indeterminate and then P.Pending (Context.IO)
                        and then Context.Calls = 1 and then P.Observe_Hardware (Context.IO).Writes = 1,"unknown notification retains effect");
                  elsif Selected = Fatal_Failure then
                     Check (Regions.Observe (Run).Error = E.Fatal_Requested and then Regions.Observe (Run).Fatal.Present,"Fatal propagation");
                  elsif Selected = Division_Failure then
                     Check (Regions.Observe (Run).Error = E.Division_By_Zero and then Context.Calls = 1,"ordinary error stops later notices");
                  elsif Selected = No_Budget then
                     Check (Regions.Observe (Run).Error = E.Budget_Exceeded and then Regions.Observe (Run).Charged = 0,"bounded notification fuel");
                  end if;
               else
                  Check (Regions.Observe (Run).Stage = Regions.Connected,"connected");
                  if Selected in Memory_Only | IO_Only | Table_Only | PCI_With_BBN | No_Method | No_Regions then
                     Check (Context.Calls = 0,"no forbidden or inherited _REG");
                  else
                     Check (Context.Calls = 2 and then Context.Last_Space = (if Selected = PCI_Without_BBN then 2 else 3)
                        and then Context.Last_Code = 1,"per-region arguments and repeated local method");
                  end if;
                  Check (C.Retained_Results (S) = 0,"ignored compound result released");
                  Prepare (Other,Revision);
                  Regions.Disconnect (Run,Other,Context,10000,OK);
                  Check (OK and then Regions.Observe (Run).Stage = Regions.Failed,"foreign service rejected even with identical node numbers");
               end if;
               Before := P.Observe_Hardware (Context.IO); Calls := Context.Calls;
               Regions.Connect (Run,S,Context,10000,OK); Check (not OK,"connect never replayed");
               Regions.Disconnect (Run,S,Context,10000,OK); Check (not OK,"failed direction never retried");
               Check (Context.Calls = Calls and then P.Observe_Hardware (Context.IO) = Before,"no retry effects");
            end if;
         end;
      end loop;
   end loop;
   -- Load the exact normal iASL images, rather than treating constructed AML as
   -- independent reference evidence. The acpiexec logs retain automatic startup
   -- separately; expected values come from its explicit reset/notification walk.
   Selected := Ordinary;
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         S : aliased C.State (1, 4096, 4096, 1);
         Context : Controller;
         Region_Run : Regions.Session;
         OK : Boolean;
         Getters : constant array (Positive range 1 .. 7) of String (1 .. 4) :=
           ["QCNT","QSPC","QCON","QVAL","QDON","QROT","QSEQ"];
         procedure Compare (Offset : Natural) is
            Found : N.Lookup_Result;
            Result : C.Values.Result;
            Status : C.Values.Access_Status;
         begin
            for I in Getters'Range loop
               C.Find_Object (S,N.Root,Getters (I),Found,Status);
               Check (Status = C.Values.Available and then Found.Status = N.Found,"oracle getter");
               F.Invoke_Retained (S,Context.IO,Context.Bindings,Found.Node,[others => 0],0,1000,Result,Status);
               Check (Status = C.Values.Available and then Result.Status = E.Returned and then
                  Result.Number = (if Revision = 1 then Region_Reg_Oracle.Values_R1 (Offset + I)
                     else Region_Reg_Oracle.Values_R2 (Offset + I)),"normal ACPICA returned value " & Getters (I));
            end loop;
         end Compare;
         procedure Barrier (Service : aliased in out C.State; Budget : Natural;
            Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
            Accepted : Boolean;
         begin
            Regions.Connect (Region_Run,Service,Context,Budget,Accepted);
            Check (Accepted,"oracle barrier");
            Charged := Regions.Observe (Region_Run).Charged; Outcome := Regions.Outcome (Region_Run);
         end Barrier;
         procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor;
            Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
         begin F.Evaluate_Named (Service,Context.IO,Context.Bindings,Item,Budget,Result,Status); end Evaluate;
         package Devices is new ACPI_Device_Initialization (C,Barrier,Evaluate);
         use type Devices.Phase;
         Device_Run : Devices.Session;
      begin
         Prepare (S,Revision,(if Revision = 1 then Region_Reg_Oracle.Table_R1 else Region_Reg_Oracle.Table_R2));
         Provision (S,Context);
         Devices.Begin_Initialization (Device_Run,S,10000,OK);
         Check (OK,"oracle prepass");
         Devices.Complete_Initialization (Device_Run,S,OK);
         Check (OK and then Devices.Observe (Device_Run).Stage = Devices.Completed,"oracle initialized");
         Compare (0);
         Regions.Disconnect (Region_Run,S,Context,10000,OK);
         Check (OK and then Regions.Observe (Region_Run).Stage = Regions.Disconnected,"oracle disconnected");
         Compare (7);
         Check (C.Retained_Results (S) = 0,"oracle root cleanup");
      end;
   end loop;
   return Checks;
end Region_Initialization_Tests;
