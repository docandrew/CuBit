pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with AML_Execute;
with AML_Decode;
with ACPI_Service;
with ACPI_Device_Policy;
with ACPI_Device_Initialization;
procedure Device_Traversal_Tests is
   package C renames ACPI_Service;
   subtype Bytes is Firmware_Tables.Bytes;
   use type Firmware_Tables.Bytes;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Phase;
   use type C.Namespace.Load_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   Attempt_Structural_Mutation : Boolean := False;
   Expected_Barrier_Marker : Unsigned_64 := 0;
   Region_Calls : Natural := 0;
   Region_Result : constant ACPI_Device_Policy.Region_Outcome := ACPI_Device_Policy.No_Regions_Required;
   procedure Regions (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      Node : C.Namespace.Node_ID;
      Access_Result : C.Values.Access_Status;
      Value : C.Values.Result;
      Attempt : AML_Execute.Execution_Result;
   begin
      pragma Assert (C.Namespace_Load_Phase (Service) = C.Values.Ready);
      Check (Budget <= 1000, "region shared budget");
      C.Find_Child (Service, C.Namespace.Root, "ORDR", Node, Access_Result);
      Check (Access_Result = C.Values.Available, "region marker lookup");
      C.Observe_Named_Value (Service, Node, Value, Access_Result);
      Check (Access_Result = C.Values.Available and then Value.Status = AML_Execute.Returned
        and then Value.Number = Expected_Barrier_Marker, "root then SB before barrier");
      Region_Calls := Region_Calls + 1;
      Charged := 0;
      if Attempt_Structural_Mutation then
         C.Find_Child (Service, C.Namespace.Root, "MUTA", Node, Access_Result);
         Check (Access_Result = C.Values.Available, "mutation method lookup");
         C.Invoke_Scalar (Service, Node, [others => 0], 0, Budget, Attempt);
         Check (Attempt.Status = AML_Execute.No_Return and then Attempt.Charged > 0,
           "structural CopyObject completes without return");
         Charged := Attempt.Charged;
      end if;
      Outcome := Region_Result;
   end Regions;
   package Init is new ACPI_Device_Initialization (C, Regions);
   use type Init.Phase;
   function Enc (Text : String) return Bytes is
      Result : Bytes (1 .. Text'Length);
   begin
      for I in Result'Range loop Result (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Result;
   end Enc;
   function String_Operand (Text : String) return Bytes is ([16#0D#] & Enc (Text) & [0]);
   function Pkg_Length (Value : Natural) return Bytes is
   begin
      if Value < 64 then return [Unsigned_8 (Value)]; end if;
      if Value < 4_096 then return [Unsigned_8 (16#40# + Value mod 16), Unsigned_8 (Value / 16)]; end if;
      if Value < 1_048_576 then
         return [Unsigned_8 (16#80# + Value mod 16), Unsigned_8 ((Value / 16) mod 256), Unsigned_8 (Value / 4_096)];
      end if;
      raise Program_Error with "fixture package too large";
   end Pkg_Length;
   function Block (Opcode, Payload : Bytes) return Bytes is
      Width : Positive := 1;
   begin
      loop
         declare
            Length : constant Bytes := Pkg_Length (Payload'Length + Width);
         begin
            if Length'Length = Width then return Opcode & Length & Payload; end if;
            Width := Length'Length;
         end;
      end loop;
   end Block;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Block ([16#14#], Enc (Name) & [0] & Code));
   function Table (Signature : String; Revision : Unsigned_8; Payload : Bytes;
                   OEM : String := "OEM001"; Model : String := "TABLE001") return Bytes is
      Raw : Bytes (1 .. Firmware_Tables.Table_Header_Size + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Raw (1 .. 4) := Enc (Signature);
      for I in 1 .. 4 loop Raw (4 + I) := Unsigned_8 ((Raw'Length / 256 ** (I - 1)) mod 256); end loop;
      Raw (9) := Revision;
      Raw (11 .. 16) := Enc (OEM); Raw (17 .. 24) := Enc (Model);
      Raw (37 .. Raw'Last) := Payload;
      for Item of Raw loop Sum := Sum + Item; end loop;
      Raw (10) := 0 - Sum;
      return Raw;
   end Table;

   function Name (Part : String; Value : Bytes) return Bytes is ([16#08#] & Enc (Part) & Value);

   procedure Prepare (Service : aliased in out C.State; Code : Bytes) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Access_Result : C.Values.Access_Status;
      Members : C.Namespace.Initialization_Report;
   begin
      C.Stage_Table (Service, 1, C.DSDT, Table ("DSDT", 2, Code), Staged);
      Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (Service, Frozen);
      Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 1000, Execution, Access_Result);
      Check (Access_Result = C.Values.Available and then
        Execution.Modules.Outcome = C.Values.Modules_Completed, "execute");
      C.Initialize_Members (Service, Members, Access_Result);
      Check (Access_Result = C.Values.Available, "members");
      pragma Assert (C.Namespace_Load_Phase (Service) = C.Values.Ready);
   end Prepare;
   function Device (Part : String; Code : Bytes) return Bytes is
     (Block ([16#5B#,16#82#], Enc (Part) & Code));
   function Set_Root (Part : String; Number : Unsigned_8) return Bytes is
     ([16#70#,16#0A#,Number,16#5C#] & Enc (Part));
   function Tick (Part : String) return Bytes is ([16#75#,16#5C#] & Enc (Part));
   procedure Expect (Service : in out C.State; Part : String; Number : Unsigned_64) is
      Node : C.Namespace.Node_ID;
      Status : C.Values.Access_Status;
      Value : C.Values.Result;
   begin
      C.Find_Child (Service, C.Namespace.Root, Part, Node, Status);
      Check (Status = C.Values.Available, "find " & Part);
      C.Observe_Named_Value (Service, Node, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
        and then Value.Number = Number, "value " & Part);
   end Expect;
begin
   -- Explicit _SB scope reopening: only one SB initializer, before region callback.
   declare
      Service : aliased C.State (1, 8192, 8192, 1);
      Run : Init.Session;
      Accepted : Boolean;
      Code : constant Bytes := Name ("ORDR", [0]) & Name ("ROOT", [0]) &
        Name ("SBCT", [0]) & Name ("DEVC", [0]) &
        Method ("_INI", Set_Root ("ORDR", 1) & Tick ("ROOT")) &
        Block ([16#10#], Enc ("_SB_") &
          Method ("_INI", Set_Root ("ORDR", 2) & Tick ("SBCT")) &
          Device ("DEV0", Method ("_INI", Set_Root ("ORDR", 3) & Tick ("DEVC"))));
   begin
      Prepare (Service, Code); Expected_Barrier_Marker := 2;
      Init.Begin_Initialization (Run, Service, 1000, Accepted);
      Check (Accepted and then Init.Observe (Run).Stage = Init.Awaiting_Regions, "ordered begin");
      Expect (Service, "ROOT", 1); Expect (Service, "SBCT", 1); Expect (Service, "DEVC", 0);
      Init.Complete_Initialization (Run, Service, Accepted);
      Check (Accepted and then Init.Observe (Run).Stage = Init.Completed, "ordered completion");
      Expect (Service, "ORDR", 3); Expect (Service, "ROOT", 1); Expect (Service, "SBCT", 1);
      Expect (Service, "DEVC", 1);
   end;
   -- Data/method/missing _STA and failed _STA: children continue except prune.
   for Mode in 0 .. 4 loop
      declare
         Service : aliased C.State (1, 8192, 8192, 1);
         Run : Init.Session;
         Accepted : Boolean;
         Status_Code : constant Bytes :=
           (case Mode is
            when 0 => Name ("_STA", [0]),
            when 1 => Name ("_STA", [16#0A#,8]),
            when 2 => Method ("_STA", [16#9D#] & String_Operand ("status retype") &
              [16#5C#] & Enc ("DATA") & [16#A4#,1]),
            when 3 => Bytes'[],
            when others => Method ("_STA", [16#A4#] & String_Operand ("bad")));
         Code : constant Bytes := Name ("ORDR", [0]) & Name ("DATA", [0]) & Name ("PCNT", [0]) &
           Name ("CCNT", [0]) & Name ("SKIP", [0]) &
           Device ("PAR0", Status_Code & Method ("_INI", Tick ("PCNT")) &
             Device ("CHLD", Method ("_INI", Tick ("CCNT")))) &
           Device ("NONE", Method ("_STA", Tick ("SKIP") & [16#A4#,1]));
      begin
         Prepare (Service, Code); Expected_Barrier_Marker := 0;
         Init.Begin_Initialization (Run, Service, 1000, Accepted);
         Check (Accepted, "policy begin");
         Init.Complete_Initialization (Run, Service, Accepted);
         Check (Accepted and then Init.Observe (Run).Stage =
           (if Mode = 4 then Init.Completed_With_Errors else Init.Completed), "policy final");
         Expect (Service, "PCNT", (if Mode in 2 | 3 then 1 else 0));
         Expect (Service, "CCNT", (if Mode = 0 then 0 else 1));
         Expect (Service, "SKIP", 0);
         Check (Init.Observe (Run).Errors = (if Mode = 4 then 1 else 0), "wrong type visible");
      end;
   end loop;
   -- Zero and small budgets are explicit partial initialization, never success.
   for Fuel in 0 .. 1 loop
      declare
         Service : aliased C.State (1, 4096, 4096, 1);
         Run : Init.Session;
         Accepted : Boolean;
         Calls : constant Natural := Region_Calls;
      begin
         Prepare (Service, Name ("ORDR", [0]) &
           Method ("_INI", Tick ("ORDR") & Tick ("ORDR") & Tick ("ORDR")));
         Init.Begin_Initialization (Run, Service, Fuel, Accepted);
         Check (Accepted and then Init.Observe (Run).Stage = Init.Incomplete,
           "budget incomplete");
         Check (Init.Observe (Run).Charged <= Fuel and then Init.Observe (Run).Errors = 1
           and then Init.Observe (Run).First_Error = AML_Execute.Budget_Exceeded, "budget report");
         Init.Complete_Initialization (Run, Service, Accepted);
         Check (not Accepted and then Region_Calls = Calls, "budget no region execution");
      end;
   end loop;
   -- Physical descriptor credentials cannot be replayed in another live arena.
   declare
      A, B : aliased C.State (1, 4096, 4096, 1);
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      Prepare (A, Name ("DATA", [1])); Prepare (B, Name ("DATA", [1]));
      C.Find_Child (A, C.Namespace.Root, "DATA", Node, Status);
      C.Describe_Node (A, Node, Item, Status);
      Check (Status = C.Values.Available and then C.Current_Binding (A, Item), "descriptor own arena");
      Check (not C.Current_Binding (B, Item) and then not C.Current_Node (B, Item), "foreign descriptor rejected");
   end;
   -- Real Copy_And_Attach permits Device replacement; topology must detect it.
   declare
      Service : aliased C.State (1, 4096, 4096, 1);
      Run : Init.Session;
      Accepted : Boolean;
      Node : C.Namespace.Node_ID;
      Before : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      Prepare (Service, Name ("ORDR", [0]) & Device ("DEV0", Method ("_INI", Tick ("ORDR"))) &
        Method ("MUTA", [16#9D#,1] & Enc ("DEV0")));
      C.Find_Child (Service, C.Namespace.Root, "DEV0", Node, Status);
      C.Describe_Node (Service, Node, Before, Status);
      Check (Status = C.Values.Available, "structural snapshot");
      Expected_Barrier_Marker := 0; Attempt_Structural_Mutation := True;
      Init.Begin_Initialization (Run, Service, 1000, Accepted);
      Check (Accepted, "mutation begin");
      Init.Complete_Initialization (Run, Service, Accepted);
      Attempt_Structural_Mutation := False;
      Check (Accepted and then Init.Observe (Run).Stage = Init.Failed
        and then Init.Observe (Run).Errors = 1
        and then Init.Observe (Run).First_Error = AML_Execute.Unsupported_Value,
        "post-barrier topology failure recorded");
      Check (C.Current_Binding (Service, Before) and then not C.Current_Node (Service, Before),
        "structural identity preserved but kind changed");
      Expect (Service, "ORDR", 0);
      declare
         Charge : constant Natural := Init.Observe (Run).Charged;
         Calls : constant Natural := Region_Calls;
      begin
         Check (Charge > 0 and then Charge <= 1000, "failed mutation charge retained");
         Init.Complete_Initialization (Run, Service, Accepted);
         Check (not Accepted and then Region_Calls = Calls and then Init.Observe (Run).Charged = Charge,
           "failed topology cannot replay region callback");
      end;
   end;
   -- Reset/reload at the same physical index rejects the old generation.
   declare
      A : C.Values.Arena;
      Status : C.Values.Access_Status;
      Loaded : C.Namespace.Load_Status;
      Report : C.Namespace.Initialization_Report;
      Item : C.Values.Node_Descriptor;
   begin
      C.Values.Initialize (A, Status); Check (Status = C.Values.Available, "arena initialize");
      C.Values.Load (A, Device ("DEV0", []), AML_Decode.Bits_64, Loaded, Status);
      Check (Status = C.Values.Available and then Loaded = C.Namespace.Loaded, "descriptor load");
      C.Values.Seal (A, Report, Status); Check (Status = C.Values.Available, "descriptor seal");
      C.Values.Describe_Node (A, 1, Item, Status);
      Check (Status = C.Values.Available and then C.Values.Current_Node (A, Item), "structural descriptor live");
      C.Values.Reset (A, Status); Check (Status = C.Values.Available, "descriptor reset");
      Check (not C.Values.Current_Binding (A, Item), "descriptor stale before reload");
      C.Values.Load (A, Name ("DEV0", [1]), AML_Decode.Bits_64, Loaded, Status);
      Check (Status = C.Values.Available and then Loaded = C.Namespace.Loaded, "replacement data load");
      C.Values.Seal (A, Report, Status); Check (Status = C.Values.Available, "replacement seal");
      Check (not C.Values.Current_Binding (A, Item) and then not C.Values.Current_Node (A, Item),
        "same name and index cannot revive structural descriptor");
   end;
   Ada.Text_IO.Put_Line ("device traversal checks" & Checks'Image);
end Device_Traversal_Tests;
