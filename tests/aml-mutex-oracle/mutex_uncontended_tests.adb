with Ada.Text_IO;
with Interfaces;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with Mutex_Oracle_Data;
procedure Mutex_Uncontended_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64);
   use type Interfaces.Unsigned_64;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   Service : aliased C.State (1, 4096, 4096);
   Stage : C.Stage_Status;
   Frozen : C.Freeze_Status;
   Report : C.Execution_Report;
   Status : C.Values.Access_Status;
   Members : C.Namespace.Initialization_Report;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Evaluate (Name : String; Expected : Interfaces.Unsigned_64) is
      Node : C.Namespace.Node_ID;
      Descriptor : C.Node_Descriptor;
      Value : C.Values.Result;
      Actual : AML_Execute.Execution_Status;
      Number : Interfaces.Unsigned_64;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "find " & Name);
      C.Describe_Node (Service, Node, Descriptor, Status);
      Check (Status = C.Values.Available, "describe " & Name);
      C.Evaluate_Named (Service, Descriptor, 10000, Value, Status);
      Actual := Value.Status;
      Ada.Text_IO.Put_Line (Name & " access=" & Status'Image &
        " status=" & Value.Status'Image & " charged=" & Value.Charged'Image);
      if Value.Status = AML_Execute.Returned then
         Ada.Text_IO.Put_Line ("number=" & Value.Number'Image);
      end if;
      Check (Status = C.Values.Available, "evaluate " & Name);
      if Actual in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned
      then
         C.Release_Result (Service, Value.Handle, Status);
         Check (Status = C.Values.Available, "release unexpected result");
      end if;
      Check (C.Retained_Results (Service) = 0, "retained cleanup " & Name);
      Check (Actual = AML_Execute.Returned,
        Name & " expected RETURNED, actual " & Actual'Image);
      Number := Value.Number;
      Check (Number = Expected, Name & " expected" & Expected'Image & " actual" & Number'Image);
   end Evaluate;
begin
   C.Stage_Table (Service, 1, C.DSDT, Mutex_Oracle_Data.Table, Stage);
   Check (Stage = C.Staged, "stage exact DSDT");
   C.Freeze_Catalog (Service, Frozen);
   Check (Frozen = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (Service, 10000, Report, Status);
   Check (Status = C.Values.Available and then
     Report.Modules.Outcome = C.Values.Modules_Completed, "module admission");
   C.Initialize_Members (Service, Members, Status);
   Check (Status = C.Values.Available, "initialize members");
   for Repeat in 1 .. 2 loop
      Evaluate ("TEST", 0);
      Evaluate ("MARK", 1);
      Evaluate ("PROB", 0);
   end loop;
   Ada.Text_IO.Put_Line ("Mutex uncontended checks" & Checks'Image);
end Mutex_Uncontended_Tests;
