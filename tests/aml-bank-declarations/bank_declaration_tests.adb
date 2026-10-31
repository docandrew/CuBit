pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Service; use ACPI_Service;
with AML_Decode;
with AML_Execute;
with Bank_Declaration_Oracle;
procedure Bank_Declaration_Tests is
   use type Values.Access_Status;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   Service : aliased State (1, Bank_Declaration_Oracle.Raw_Table'Length, Bank_Declaration_Oracle.Raw_Table'Length, Load_Capacity => 1);
   Staged_Result : Stage_Status;
   Frozen : Freeze_Status;
   Status : Values.Access_Status;
   Execution : Execution_Report;
   Members : Namespace.Initialization_Report;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   procedure Expect_Integer (Name : String; Expected : AML_Decode.Integer_Value) is
      Node : Namespace.Node_ID;
      Item : Node_Descriptor;
      Result : Values.Result;
      Before : constant Namespace.Owned.Retained_Root_Count := Retained_Results (Service);
   begin
      Find_Child (Service, Namespace.Root, Name, Node, Status);
      Check (Status = Values.Available and then Node /= Namespace.Root, Name & " lookup");
      Describe_Node (Service, Node, Item, Status);
      Check (Status = Values.Available, Name & " descriptor");
      Evaluate_Named (Service, Item, 10_000, Result, Status);
      Check (Status = Values.Available and then Result.Status = AML_Execute.Returned,
        Name & " evaluation: " & Values.Access_Status'Image (Status) & "/" &
        AML_Execute.Execution_Status'Image (Result.Status));
      Check (Result.Number = Expected, Name & " ACPICA integer");
      Check (Retained_Results (Service) = Before, Name & " no retained roots");
   end Expect_Integer;
   procedure Expect_Failure (Name : String; Wanted : AML_Execute.Execution_Status) is
      Node : Namespace.Node_ID;
      Item : Node_Descriptor;
      Result : Values.Result;
   begin
      Find_Child (Service, Namespace.Root, Name, Node, Status);
      Check (Status = Values.Available and then Node /= Namespace.Root, Name & " lookup");
      Describe_Node (Service, Node, Item, Status);
      Check (Status = Values.Available, Name & " descriptor");
      Evaluate_Named (Service, Item, 10_000, Result, Status);
      Check (Status = Values.Available and then Result.Status = Wanted,
        Name & " expected failure: " & Result.Status'Image);
      Check (Retained_Results (Service) = 0, Name & " no retained roots");
      Find_Child (Service, Node, "TMP0", Node, Status);
      Check (Status = Values.Available and then Node = Namespace.Root, Name & " temporary field retired");
   end Expect_Failure;
begin
   Stage_Table (Service, 1, DSDT, Bank_Declaration_Oracle.Raw_Table, Staged_Result);
   Check (Staged_Result = Staged, "stage exact normal iASL table");
   Freeze_Catalog (Service, Frozen);
   Check (Frozen = Catalog_Frozen_OK, "freeze");
   Execute_Frozen (Service, 100_000, Execution, Status);
   Check (Status = Values.Available and then Execution.Phase = Module_Execution_Reported,
     "firmware module execution");
   Initialize_Members (Service, Members, Status);
   Check (Status = Values.Available, "initialize members");
   Expect_Integer ("CNT0", 0);
   Expect_Integer ("DYNB", 5);
   Expect_Integer ("CNT0", 1);
   Expect_Integer ("MOVE", 1);
   Expect_Integer ("DYNB", 5);
   Expect_Integer ("CNT0", 2);
   declare
      Method, Child : Namespace.Node_ID;
   begin
      Find_Child (Service, Namespace.Root, "DYNB", Method, Status);
      Check (Status = Values.Available and then Method /= Namespace.Root, "method scope");
      Find_Child (Service, Method, "TMP0", Child, Status);
      Check (Status = Values.Available and then Child = Namespace.Root, "first banked field retired");
      Find_Child (Service, Method, "TMP1", Child, Status);
      Check (Status = Values.Available and then Child = Namespace.Root, "second banked field retired");
   end;
   Expect_Failure ("RERR", AML_Execute.Unknown_Name);
   Expect_Integer ("CNT0", 3);
   Expect_Failure ("BERR", AML_Execute.Unknown_Name);
   Expect_Integer ("CNT0", 4);
   Expect_Integer ("DUPL", 0);
   Expect_Integer ("CNT0", 6);
   Expect_Failure ("FAIL", AML_Execute.Division_By_Zero);
   Expect_Integer ("CNT0", 7);
   Ada.Text_IO.Put_Line ("Bank declaration checks" & Natural'Image (Checks));
end Bank_Declaration_Tests;
