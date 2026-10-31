with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with Package_Excess_Data;
procedure Package_Excess_Tests is
   package C is new ACPI_Service_Core (Perform_Delay => AML_Delays.Unavailable_Provider);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Execution_Phase_Result;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   Checks : Natural := 0;
   Fuel : constant Positive := 10_000;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Run (Label : String; Table : AML_Decode.Bytes;
                  Expected_Status : AML_Execute.Execution_Status;
                  Expected_Value, Expected_Mark : AML_Decode.Integer_Value) is
      A : aliased C.State (1, Table'Length, Table'Length, 1);
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Report : C.Execution_Report;
      Status : C.Values.Access_Status;
      Init : C.Namespace.Initialization_Report;
      Node : C.Namespace.Node_ID;
      Result : C.Values.Result;
      procedure Invoke (Name : String; Expected : AML_Execute.Execution_Status;
                        Number : AML_Decode.Integer_Value) is
         Nodes : constant Natural := C.Observe (A).Objects;
      begin
         C.Find_Child (A, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Label & " find " & Name);
         C.Invoke_Retained (A, Node, [others => 0], 0, Fuel, Result, Status);
         Check (Status = C.Values.Available, Label & " access " & Name);
         Check (Result.Status = Expected, Label & " " & Name & " expected " & Expected'Image & " actual " & Result.Status'Image);
         Check (Result.Charged > 0 and then Result.Charged <= Fuel, Label & " charged " & Name);
         if Expected = AML_Execute.Returned then
            Check (Result.Number = Number, Label & " " & Name & " expected" & Number'Image & " actual" & Result.Number'Image);
         end if;
         Check (C.Retained_Results (A) = 0, Label & " retained cleanup");
         Check (C.Observe (A).Objects = Nodes, Label & " temporary name cleanup");
      end Invoke;
   begin
      C.Stage_Table (A, 1, C.DSDT, Table, Staged); Check (Staged = C.Staged, Label & " stage");
      C.Freeze_Catalog (A, Frozen); Check (Frozen = C.Catalog_Frozen_OK, Label & " freeze");
      C.Execute_Frozen (A, Fuel, Report, Status);
      Check (Status = C.Values.Available and then Report.Phase = C.Module_Execution_Reported
        and then Report.Modules.Outcome = C.Values.Modules_Completed, Label & " module");
      C.Initialize_Members (A, Init, Status); Check (Status = C.Values.Available, Label & " initialize");
      for Repeat in 1 .. 2 loop
         Invoke ("TEST", Expected_Status, Expected_Value);
         Invoke ("OBS0", AML_Execute.Returned, Expected_Mark);
      end loop;
   end Run;
begin
   Run ("good-count0-r1", Package_Excess_Data.good_count0_r1, AML_Execute.Returned, 0, 1);
   Run ("good-count1-r1", Package_Excess_Data.good_count1_r1, AML_Execute.Returned, 1, 1);
   Run ("miss-count0-r1", Package_Excess_Data.miss_count0_r1, AML_Execute.Returned, 0, 1);
   Run ("miss-count1-r1", Package_Excess_Data.miss_count1_r1, AML_Execute.Returned, 1, 1);
   Run ("p000-count0-r1", Package_Excess_Data.p000_count0_r1, AML_Execute.Returned, 0, 1);
   Run ("p000-count1-r1", Package_Excess_Data.p000_count1_r1, AML_Execute.Returned, 1, 1);
   Run ("nested-count0-r1", Package_Excess_Data.nested_count0_r1, AML_Execute.Returned, 0, 1);
   Run ("nested-count1-r1", Package_Excess_Data.nested_count1_r1, AML_Execute.Returned, 1, 1);
   Run ("good-count0-r2", Package_Excess_Data.good_count0_r2, AML_Execute.Returned, 0, 1);
   Run ("good-count1-r2", Package_Excess_Data.good_count1_r2, AML_Execute.Returned, 1, 1);
   Run ("miss-count0-r2", Package_Excess_Data.miss_count0_r2, AML_Execute.Returned, 0, 1);
   Run ("miss-count1-r2", Package_Excess_Data.miss_count1_r2, AML_Execute.Returned, 1, 1);
   Run ("p000-count0-r2", Package_Excess_Data.p000_count0_r2, AML_Execute.Returned, 0, 1);
   Run ("p000-count1-r2", Package_Excess_Data.p000_count1_r2, AML_Execute.Returned, 1, 1);
   Run ("nested-count0-r2", Package_Excess_Data.nested_count0_r2, AML_Execute.Returned, 0, 1);
   Run ("nested-count1-r2", Package_Excess_Data.nested_count1_r2, AML_Execute.Returned, 1, 1);
   Ada.Text_IO.Put_Line ("Runtime package excess checks" & Checks'Image);
end Package_Excess_Tests;
