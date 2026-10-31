with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with Nested_Data;
procedure Nested_Buffer_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Block_Capacity => 128);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Execution_Phase_Result;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   procedure Run (Table : AML_Decode.Bytes) is
      A : aliased C.State (1, Table'Length, Table'Length, 1);
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Report : C.Execution_Report;
      Status : C.Values.Access_Status;
      Init : C.Namespace.Initialization_Report;
      Node : C.Namespace.Node_ID;
      Result : C.Values.Result;
      procedure Invoke (Name : String; Expected : AML_Decode.Integer_Value) is
         Nodes : constant Natural := C.Observe (A).Objects;
      begin
         C.Find_Child (A, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "find " & Name);
         C.Invoke_Retained (A, Node, [others => 0], 0, 10000, Result, Status);
         Check (Status = C.Values.Available, "invoke " & Name);
         Check (Result.Status = AML_Execute.Returned, Name & " actual " & Result.Status'Image);
         Check (Result.Number = Expected, Name & " expected" & Expected'Image & " actual" & Result.Number'Image);
         Check (C.Retained_Results (A) = 0 and then C.Observe (A).Objects = Nodes, "cleanup");
      end Invoke;
   begin
      C.Stage_Table (A, 1, C.DSDT, Table, Staged); Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (A, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (A, 10000, Report, Status);
      Check (Status = C.Values.Available and then Report.Phase = C.Module_Execution_Reported
        and then Report.Modules.Outcome = C.Values.Modules_Completed, "module");
      C.Initialize_Members (A, Init, Status); Check (Status = C.Values.Available, "initialize");
      for Repeat in 1 .. 2 loop Invoke ("DRCT", 51); end loop;
      for Repeat in 1 .. 2 loop Invoke ("TEST", 153); end loop;
   end Run;
begin
   Run (Nested_Data.Revision_1); Run (Nested_Data.Revision_2);
   Ada.Text_IO.Put_Line ("Nested buffer checks" & Checks'Image);
end Nested_Buffer_Tests;
