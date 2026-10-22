pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with AML_Execute;
with ACPI_Service;
with ACPI_Device_Policy;
with ACPI_Device_Initialization;
procedure Device_Initialization_Tests is
   package C renames ACPI_Service;
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
   Region_Result : ACPI_Device_Policy.Region_Outcome := ACPI_Device_Policy.No_Regions_Required;
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
begin
   -- Hosted-constructed AML, not an ACPICA oracle. Every PkgLength is derived.
   for Barrier in ACPI_Device_Policy.Region_Outcome loop
      declare
         Service : aliased C.State (1, 4096, 4096);
         Run : Init.Session;
         Accepted : Boolean;
         Node : C.Namespace.Node_ID;
         Access_Result : C.Values.Access_Status;
         Before : C.Node_Descriptor;
         Value : C.Values.Result;
         Calls_Before : constant Natural := Region_Calls;
         Code : constant Bytes := Name ("DATA", [0]) & Name ("DONE", [0]) &
           Method ("_INI", [16#9D#] & String_Operand ("retyped") & Enc ("DATA") &
             [16#A4#] & Block ([16#12#], [1,1])) &
           Block ([16#5B#,16#82#], Enc ("DEV0") &
             Method ("_INI", [16#70#,1,16#5C#] & Enc ("DONE")));
      begin
         Prepare (Service, Code);
         C.Find_Child (Service, C.Namespace.Root, "DATA", Node, Access_Result);
         Check (Access_Result = C.Values.Available, "data lookup");
         C.Describe_Node (Service, Node, Before, Access_Result);
         Check (Access_Result = C.Values.Available, "data descriptor");
         Region_Result := Barrier;
         Init.Begin_Initialization (Run, Service, 1000, Accepted);
         Check (Accepted and then Init.Observe (Run).Stage = Init.Awaiting_Regions,
           "root compound return ignored and data retype allowed");
         Check (C.Current_Binding (Service, Before), "binding survives retype");
         Check (not C.Current_Node (Service, Before), "kind-sensitive descriptor remains strict");
         Init.Begin_Initialization (Run, Service, 1000, Accepted);
         Check (not Accepted, "begin replay rejected");
         Init.Complete_Initialization (Run, Service, Accepted);
         Check (Accepted and then Region_Calls = Calls_Before + 1, "barrier once");
         case Barrier is
            when ACPI_Device_Policy.Regions_Connected | ACPI_Device_Policy.No_Regions_Required =>
               Check (Init.Observe (Run).Stage = Init.Completed and then
                 Init.Observe (Run).Errors = 0, "completed without false topology error");
            when ACPI_Device_Policy.Regions_Unavailable =>
               Check (Init.Observe (Run).Stage = Init.Incomplete, "unavailable incomplete");
            when ACPI_Device_Policy.Region_Initialization_Failed =>
               Check (Init.Observe (Run).Stage = Init.Failed, "region failure explicit");
         end case;
         C.Find_Child (Service, C.Namespace.Root, "DONE", Node, Access_Result);
         C.Observe_Named_Value (Service, Node, Value, Access_Result);
         Check (Access_Result = C.Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = (if Barrier in ACPI_Device_Policy.Regions_Connected |
             ACPI_Device_Policy.No_Regions_Required then 1 else 0), "device only after region barrier");
         Init.Complete_Initialization (Run, Service, Accepted);
         Check (not Accepted and then Region_Calls = Calls_Before + 1, "complete replay rejected");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("device initialization checks" & Checks'Image);
end Device_Initialization_Tests;
