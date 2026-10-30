pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with AML_Execute;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Table_Backing;
with ACPI_Device_Policy;
with ACPI_Device_Initialization;
procedure Named_Evaluation_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Retained_Result_Capacity => 1);
   subtype Bytes is Firmware_Tables.Bytes;
   use type Firmware_Tables.Bytes;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Phase;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   Region_Calls : Natural := 0;
   Region_Result : constant ACPI_Device_Policy.Region_Outcome := ACPI_Device_Policy.No_Regions_Required;
   procedure Regions (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
   begin
      pragma Assert (C.Namespace_Load_Phase (Service) = C.Values.Ready);
      Check (Budget <= 1000, "region shared budget");
      Region_Calls := Region_Calls + 1;
      Charged := 0; Outcome := Region_Result;
   end Regions;
   package Init is new ACPI_Device_Initialization (C, Regions);
   use type Init.Phase;
   use type C.Values.Value_Handle;
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
   function Buffer_Code (Data : Bytes) return Bytes is
     (Block ([16#11#], [16#0A#, Unsigned_8 (Data'Length)] & Data));
   procedure Locate (Service : C.State; Scope : C.Namespace.Node_ID;
      Part : String; Item : out C.Node_Descriptor) is
      Node : C.Namespace.Node_ID;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, Scope, Part, Node, Status);
      Check (Status = C.Values.Available, "find " & Part);
      C.Describe_Node (Service, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Part);
   end Locate;
begin
   -- Admission is Ready-only, even though older observation allows scalar Loading.
   declare
      Service : aliased C.State (1, 4096, 4096);
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
      A : C.Values.Arena;
      Input : aliased AML_Table_Backing.State (1, 1);
   begin
      C.Evaluate_Named (Service, C.Values.No_Node, 1, Value, Status);
      Check (Status = C.Values.Wrong_Phase and then Value.Charged = 0, "uninitialized denied");
      C.Values.Initialize (A, Status); Check (Status = C.Values.Available, "loading initialize");
      C.Values.Evaluate_Named (A, Input, C.Values.No_Node, AML_Decode.Bits_64, 1, Value, Status);
      Check (Status = C.Values.Wrong_Phase and then Value.Charged = 0
        and then C.Values.Retained_Count (A) = 0, "loading denied before identity/read");
   end;
   for Flag of Bytes'[0, 1, 8] loop
      declare
         Service : aliased C.State (1, 4096, 4096);
         Run : Init.Session;
         Accepted : Boolean;
         Parent, Item, Counter : C.Node_Descriptor;
         Value : C.Values.Result;
         Status : C.Values.Access_Status;
         Code : constant Bytes := Name ("DONE", [0]) & Device ("DEV0",
           Name ("BUFR", Buffer_Code ([Flag])) &
           [16#8C#] & Enc ("BUFR") & [0] & Enc ("_STA") &
           Method ("_INI", [16#75#,16#5C#] & Enc ("DONE")));
      begin
         Prepare (Service, Code);
         Locate (Service, C.Namespace.Root, "DEV0", Parent);
         Locate (Service, C.Values.Position (Parent), "_STA", Item);
         Locate (Service, C.Namespace.Root, "DONE", Counter);
         C.Evaluate_Named (Service, Item, 0, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Budget_Exceeded
           and then Value.Charged = 0 and then C.Retained_Results (Service) = 0, "zero field fuel before allocation");
         C.Evaluate_Named (Service, C.Values.No_Node, 1, Value, Status);
         Check (Status = C.Values.Invalid_Value and then Value.Charged = 0, "invalid descriptor uncharged");
         C.Evaluate_Named (Service, Item, 1, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = Unsigned_64 (Flag) and then Value.Charged = 1,
           "buffer field resolved");
         Init.Begin_Initialization (Run, Service, 1000, Accepted); Check (Accepted, "field begin");
         Init.Complete_Initialization (Run, Service, Accepted);
         Check (Accepted and then Init.Observe (Run).Stage = Init.Completed, "field coordinator");
         C.Evaluate_Named (Service, Counter, 1, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = (if Flag = 1 then 1 else 0) and then Value.Charged = 1,
           "stored evaluation charged and policy applied");
         Check (C.Retained_Results (Service) = 0, "scalar field has no result pin");
      end;
   end loop;
   -- Owned table-backed field value is evaluated, not its field metadata.
   declare
      Service : aliased C.State (1, 4096, 4096);
      Parent, Item : C.Node_Descriptor;
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      Prepare (Service, Device ("DEV0",
        [16#5B#,16#88#] & Enc ("REG0") & String_Operand ("DSDT") &
          String_Operand ("") & String_Operand ("") &
        Block ([16#5B#,16#81#], Enc ("REG0") & [1,0] & Pkg_Length (80) &
          Enc ("_STA") & Pkg_Length (8))));
      Locate (Service, C.Namespace.Root, "DEV0", Parent);
      Locate (Service, C.Values.Position (Parent), "_STA", Item);
      C.Evaluate_Named (Service, Item, 1, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
        and then Value.Number = Character'Pos ('O') and then Value.Charged = 1,
        "table field reads owned OEM byte");
      Check (C.Retained_Results (Service) = 0, "table scalar cleanup");
   end;
   -- Wide _STA returns a real retained buffer; strict status typing releases it.
   declare
      Service : aliased C.State (1, 4096, 4096);
      Run : Init.Session;
      Accepted : Boolean;
      Parent, Item, Counter : C.Node_Descriptor;
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
      Handle : C.Values.Value_Handle;
      Data : Bytes (1 .. 9);
      Copied : Natural;
      Expected : constant Bytes := [1,2,3,4,5,6,7,8,9];
   begin
      Prepare (Service, Name ("DONE", [0]) & Device ("DEV0",
        Name ("BUFR", Buffer_Code (Expected)) &
        [16#5B#,16#13#] & Enc ("BUFR") & [0,16#0A#,72] & Enc ("_STA") &
        Method ("_INI", [16#75#,16#5C#] & Enc ("DONE"))));
      Locate (Service, C.Namespace.Root, "DEV0", Parent);
      Locate (Service, C.Values.Position (Parent), "_STA", Item);
      Locate (Service, C.Namespace.Root, "DONE", Counter);
      C.Evaluate_Named (Service, Item, 1, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Object_Returned
        and then C.Retained_Results (Service) = 1, "wide result pinned");
      Handle := Value.Handle;
      C.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
      Check (Status = C.Values.Available and then Copied = 9 and then Data = Expected, "full wide content");
      C.Evaluate_Named (Service, Item, 1, Value, Status);
      Check (Status = C.Values.Root_Limit and then C.Retained_Results (Service) = 1,
        "root capacity rejected without leaked reservation");
      C.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
      Check (Status = C.Values.Available and then Data = Expected, "prior handle survives failed admission");
      C.Release_Result (Service, Handle, Status);
      Check (Status = C.Values.Available and then Handle = C.Values.No_Value, "release prior wide");
      Init.Begin_Initialization (Run, Service, 1000, Accepted); Check (Accepted, "wide begin");
      Init.Complete_Initialization (Run, Service, Accepted);
      Check (Accepted and then Init.Observe (Run).Stage = Init.Completed_With_Errors
        and then Init.Observe (Run).Errors = 1 and then C.Retained_Results (Service) = 0,
        "wrong-type wide STA released");
      C.Evaluate_Named (Service, Counter, 1, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
        and then Value.Number = 0, "later evaluation proves busy cleared and no INI");
      C.Evaluate_Named (Service, Item, 1, Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Object_Returned,
        "wide evaluation reusable after cleanup");
      Handle := Value.Handle; C.Release_Result (Service, Handle, Status);
      Check (Status = C.Values.Available and then C.Retained_Results (Service) = 0, "final cleanup");
   end;
   Ada.Text_IO.Put_Line ("named evaluation checks" & Checks'Image);
end Named_Evaluation_Tests;
