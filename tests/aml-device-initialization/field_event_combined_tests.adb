pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with AML_Execute;
with ACPI_Service_Core;
with AML_Delays;
with ACPI_Device_Policy;
with ACPI_Device_Initialization;
procedure Field_Event_Combined_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Retained_Result_Capacity => 2);
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

   procedure Prepare (Service : aliased in out C.State; Code : Bytes; Revision : Unsigned_8) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Access_Result : C.Values.Access_Status;
      Members : C.Namespace.Initialization_Report;
   begin
      C.Stage_Table (Service, 1, C.DSDT, Table ("DSDT", Revision, Code), Staged);
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
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Service : aliased C.State (1, 4096, 4096);
         Run : Init.Session;
         Accepted : Boolean;
         Parent, Status_Field, Wide_Field, Event_Item, Poll_Item, Counter : C.Node_Descriptor;
         Value : C.Values.Result;
         Status : C.Values.Access_Status;
         Saved : C.Values.Value_Handle;
         Expected : constant Bytes := [1,2,3,4,5,6,7,8,9];
         Data : Bytes (1 .. 9);
         Copied : Natural;
         -- Hand-assembled AML; assertions are integration regressions, not
         -- an additional ACPICA differential observation.
         Event_Code : constant Bytes := [16#5B#,16#02#] & Enc ("EV00");
         Poll_Code : constant Bytes := [16#5B#,16#25#] & Enc ("EV00") & [0];
         Init_Code : constant Bytes :=
           [16#5B#,16#02#] & Enc ("TEMP") &
           [16#5B#,16#24#] & Enc ("EV00") &
           [16#70#] & Poll_Code & [16#5C#] & Enc ("DONE") &
           [16#5B#,16#24#] & Enc ("EV00") &
           [16#5B#,16#26#] & Enc ("EV00");
      begin
         Prepare (Service, Name ("DONE", [16#01#]) & Device ("DEV0",
           Name ("BUFR", Buffer_Code (Expected)) & Event_Code &
           [16#8C#] & Enc ("BUFR") & [0] & Enc ("_STA") &
           [16#5B#,16#13#] & Enc ("BUFR") & [0,16#0A#,72] & Enc ("WIDE") &
           Method ("_INI", Init_Code) & Method ("POLL", [16#A4#] & Poll_Code)), Revision);
         Locate (Service, C.Namespace.Root, "DEV0", Parent);
         Locate (Service, C.Values.Position (Parent), "_STA", Status_Field);
         Locate (Service, C.Values.Position (Parent), "WIDE", Wide_Field);
         Locate (Service, C.Values.Position (Parent), "EV00", Event_Item);
         Locate (Service, C.Values.Position (Parent), "POLL", Poll_Item);
         Locate (Service, C.Namespace.Root, "DONE", Counter);
         C.Evaluate_Named (Service, Status_Field, 1, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = 1 and then Value.Charged = 1, "field STA value");
         -- Direct Event evaluation is deliberately unsupported; method lookup
         -- must nevertheless resolve this same Event through Named_Readers.
         C.Evaluate_Named (Service, Event_Item, 1, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Unsupported_Value
           and then Value.Charged = 1 and then C.Retained_Results (Service) = 0,
           "direct Event evaluation policy");
         C.Evaluate_Named (Service, Wide_Field, 1, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Object_Returned
           and then C.Retained_Results (Service) = 1, "wide field retained before Event allocation");
         Saved := Value.Handle;
         Init.Begin_Initialization (Run, Service, 1000, Accepted);
         Check (Accepted, "combined initialization begin");
         Init.Complete_Initialization (Run, Service, Accepted);
         Check (Accepted and then Init.Observe (Run).Stage = Init.Completed
           and then Init.Observe (Run).Errors = 0
           and then Init.Observe (Run).Status_Methods = 0
           and then Init.Observe (Run).Initialization_Methods = 1
           and then Init.Observe (Run).Charged > 1
           and then Init.Observe (Run).Charged <= 1000, "field and Event shared initialization budget");
         C.Read_Result_Bytes (Service, Saved, 0, Data, Copied, Status);
         Check (Status = C.Values.Available and then Copied = Expected'Length
           and then Data = Expected and then C.Retained_Results (Service) = 1,
           "full field result survives method Event allocation collection hook");
         C.Release_Result (Service, Saved, Status);
         Check (Status = C.Values.Available and then Saved = C.Values.No_Value
           and then C.Retained_Results (Service) = 0, "wide result released");
         C.Evaluate_Named (Service, Counter, 1, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = 0, "INI Signal then successful Wait");
         C.Evaluate_Named (Service, Poll_Item, 1000, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
           and then Value.Number = (if Revision = 1 then 16#FFFF_FFFF# else 16#FFFF_FFFF_FFFF_FFFF#),
           "INI Reset then poll timeout");
         C.Evaluate_Named (Service, Event_Item, 1, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Unsupported_Value
           and then C.Retained_Results (Service) = 0, "Event direct rejection remains reusable");
         Check (C.Current_Binding (Service, Status_Field) and then C.Current_Binding (Service, Event_Item),
           "field and Event binding identities preserved");
      end;
   end loop;
   Check (Region_Calls = 2, "one barrier per revision");
   Ada.Text_IO.Put_Line ("field Event combined checks" & Checks'Image);
end Field_Event_Combined_Tests;
