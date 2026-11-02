pragma Ada_2022;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with ACPI_Bootstrap;
with ACPI_Service;
with ACPI_Startup_Diagnostics;
with ACPI_Capacity_Diagnostics;
with AML_Execute;
procedure Field_Capacity_Format_Tests is
   package D renames ACPI_Capacity_Diagnostics;
   Item : ACPI_Bootstrap.Finish_Report;
   Capacity : constant D.Report := (D.NVS_Targets, D.Process_Heap, 33, 32, 0, 3_584, 0);
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Expect (Status : AML_Execute.Execution_Failure; Reason : String) is
   begin
      Item.Execution.Modules.Field_Failure.Failure := Status;
      declare Line : constant String := ACPI_Startup_Diagnostics.Format_Field_Failure (Item, Capacity); begin
         Check (Ada.Strings.Fixed.Index (Line, "fail " & Reason & " name=TEST") > 0);
         Check (Ada.Strings.Fixed.Index (Line, "demand= 33 capacity= 32") > 0);
      end;
   end Expect;
begin
   Check (ACPI_Startup_Diagnostics.Format_Field_Failure (Item, Capacity) = "acpi: stage aml-field ok NONE");
   Item.Stage := ACPI_Bootstrap.Execution_Failed;
   Item.Execution.Phase := ACPI_Service.Module_Execution_Reported;
   Item.Execution.Modules.Field_Failure :=
     (Present => True, Region_Known => True, For_Write => False, Name => "TEST", Region => "SANV",
      Space => 0, Failure => AML_Execute.Unsupported_Value);
   Expect (AML_Execute.Unsupported_Value, "CAPACITY");
   Expect (AML_Execute.Field_Indeterminate, "FIELD_INDETERMINATE");
   Expect (AML_Execute.Budget_Exceeded, "BUDGET_EXCEEDED");
   Expect (AML_Execute.Value_Limit, "VALUE_LIMIT");
   Item.Execution.Modules.Field_Failure.Failure := AML_Execute.Unsupported_Value;
   Check (Ada.Strings.Fixed.Index (ACPI_Startup_Diagnostics.Format_Field_Failure (Item), "fail UNSUPPORTED_VALUE") > 0);
   Item.Execution.Modules.Field_Failure.Present := False;
   Check (ACPI_Startup_Diagnostics.Format_Field_Failure (Item, Capacity) = "acpi: stage aml-field ok NONE");
   Ada.Text_IO.Put_Line ("PASS field capacity diagnostic priority checks=" & Checks'Image);
end Field_Capacity_Format_Tests;
