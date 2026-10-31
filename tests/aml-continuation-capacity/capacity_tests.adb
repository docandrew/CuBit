with Ada.Text_IO;
with Ada.Command_Line;
with ACPI_Service_Core;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with Capacity_Data;
procedure Capacity_Tests is
   package Default_Core is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 256);
   package Larger_Core is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 256,
      Block_Capacity => 128);
   use type AML_Decode.Integer_Value;
   use type AML_Execute.Execution_Status;
   Checks : Natural := 0;
   Selection : constant String :=
     (if Ada.Command_Line.Argument_Count = 0 then "all" else Ada.Command_Line.Argument (1));
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   generic
      with package C is new ACPI_Service_Core (<>);
      Larger : Boolean;
   procedure Exercise;
   procedure Exercise is
      use type C.Stage_Status;
      use type C.Freeze_Status;
      use type C.Execution_Phase_Result;
      use type C.Values.Access_Status;
      use type C.Values.Module_Outcome;
      procedure Run (Table, Supplemental : AML_Decode.Bytes) is
         A : aliased C.State (2, Table'Length + Supplemental'Length, Table'Length + Supplemental'Length, 2);
         Staged : C.Stage_Status;
         Frozen : C.Freeze_Status;
         Report : C.Execution_Report;
         Status : C.Values.Access_Status;
         Init : C.Namespace.Initialization_Report;
         Node : C.Namespace.Node_ID;
         Result : C.Values.Result;
         Before_Nodes : Natural;
         procedure Invoke (Name : String; Expected : AML_Decode.Integer_Value; Rejected : Boolean := False) is
         begin
            C.Find_Child (A, C.Namespace.Root, Name, Node, Status);
            Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "find " & Name);
            Before_Nodes := C.Observe (A).Objects;
            C.Invoke_Retained (A, Node, [others => 0], 0, 100000, Result, Status);
            Check (Status = C.Values.Available, "invoke " & Name);
            Check (Result.Status = (if Rejected then AML_Execute.Block_Limit else AML_Execute.Returned),
              Name & " status " & Result.Status'Image);
            if not Rejected then Check (Result.Number = Expected, Name & " number" & Result.Number'Image); end if;
            Check (C.Retained_Results (A) = 0, Name & " roots");
            Check (C.Observe (A).Objects = Before_Nodes, Name & " temporary node cleanup");
         end Invoke;
      begin
         Ada.Text_IO.Put_Line ("Actual Core state bits" & A'Size'Image);
         C.Stage_Table (A, 1, C.DSDT, Table, Staged); Check (Staged = C.Staged, "stage oracle");
         C.Stage_Table (A, 2, C.SSDT, Supplemental, Staged); Check (Staged = C.Staged, "stage supplement");
         C.Freeze_Catalog (A, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
         C.Execute_Frozen (A, 100000, Report, Status);
         Check (Status = C.Values.Available and then Report.Phase = C.Module_Execution_Reported
           and then Report.Modules.Outcome = C.Values.Modules_Completed, "modules");
         C.Initialize_Members (A, Init, Status); Check (Status = C.Values.Available, "initialize");
         for Repeat in 1 .. 2 loop
            Invoke ("W032", 1);
            Invoke ("W033", 1, not Larger); Invoke ("R200", 200);
            Invoke ("S032", 1);
            Invoke ("S033", 1, not Larger); Invoke ("R200", 200);
            Invoke ("CALL", 1, not Larger); Invoke ("R200", 200);
            Invoke ("L064", 1);
            Invoke ("L065", 1, True); Invoke ("R200", 200);
         end loop;
      end Run;
   begin
      if Selection = "all" or else Selection = (if Larger then "large32" else "default32") then
         Run (Capacity_Data.Oracle_1, Capacity_Data.Supplement_1);
      end if;
      if Selection = "all" or else Selection = (if Larger then "large64" else "default64") then
         Run (Capacity_Data.Oracle_2, Capacity_Data.Supplement_2);
      end if;
   end Exercise;
   procedure Default_Run is new Exercise (Default_Core, False);
   procedure Larger_Run is new Exercise (Larger_Core, True);
begin
   if Ada.Command_Line.Argument_Count > 1 or else
     (Selection /= "all" and then Selection /= "default32" and then Selection /= "default64"
      and then Selection /= "large32" and then Selection /= "large64")
   then raise Program_Error with "invalid complete partition selection"; end if;
   Default_Run; Larger_Run;
   Ada.Text_IO.Put_Line ("Continuation capacity checks" & Checks'Image);
end Capacity_Tests;
