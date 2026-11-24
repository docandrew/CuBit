with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Missing_Member_Data;
procedure Missing_Member_Tests is
   package C is new ACPI_Service_Core (Perform_Delay => AML_Delays.Unavailable_Provider);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Execution_Phase_Result;
   use type C.Values.Module_Outcome;
   use type C.Values.Access_Status;
   use type C.Values.Value_Kind;
   use type C.Values.Value_Handle;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   Table : AML_Decode.Bytes renames Missing_Member_Data.Table;
   A : aliased C.State (1, Table'Length, Table'Length, 1,
     Value_Object_Capacity => 64, Value_Byte_Capacity => 512, Value_Element_Capacity => 64);
   Staged : C.Stage_Status;
   Frozen : C.Freeze_Status;
   Report : C.Execution_Report;
   Init : C.Namespace.Initialization_Report;
   Status : C.Values.Access_Status;
   R : C.Values.Result;
   Node : C.Namespace.Node_ID;
   Description : C.Values.Value_Description;
   Parent, Child : C.Values.Value_Handle := C.Values.No_Value;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   procedure Invoke (Name : String) is
   begin
      C.Find_Child (A, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "lookup " & Name);
      C.Invoke_Retained (A, Node, [others => 0], 0, 10_000, R, Status);
      Check (Status = C.Values.Available and then R.Charged > 0, "invoke " & Name);
      Check (C.Valid (A), "valid after " & Name);
   end Invoke;
begin
   C.Stage_Table (A, 1, C.DSDT, Table, Staged); Check (Staged = C.Staged, "stage");
   C.Freeze_Catalog (A, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (A, 10_000, Report, Status);
   Check (Status = C.Values.Available and then Report.Phase = C.Module_Execution_Reported
     and then Report.Modules.Outcome = C.Values.Modules_Completed, "module");
   C.Initialize_Members (A, Init, Status); Check (Status = C.Values.Available, "initialize");
   for Repeat in 1 .. 2 loop
      Invoke ("BAD0");
      Check (R.Status = AML_Execute.Returned and then R.Number = 1, "missing member must not abort");
      Invoke ("READ");
      Check (R.Status = AML_Execute.Returned and then R.Number = AML_Decode.Integer_Value (Repeat), "initializer effect retained once");
      Invoke ("PKGR");
      if R.Status /= AML_Execute.Object_Returned then raise Program_Error with "package result"; end if;
      Parent := R.Handle;
      C.Describe_Result (A, Parent, Description, Status);
      Check (Status = C.Values.Available and then Description.Kind = C.Values.Package_Description
        and then Description.Length = 2, "package shape");
      C.Read_Result_Element (A, Parent, 0, Child, Status);
      Check (Status = C.Values.Available, "captured field member");
      C.Describe_Result (A, Child, Description, Status);
      Check (Status = C.Values.Available and then Description.Kind = C.Values.Integer_Description
        and then Description.Number = 65, "captured field value");
      C.Release_Result (A, Child, Status); Check (Status = C.Values.Available, "release child");
      C.Read_Result_Element (A, Parent, 1, Child, Status);
      Check (Status = C.Values.Uninitialized_Element and then Child = C.Values.No_Value, "missing member is uninitialized");
      C.Release_Result (A, Parent, Status); Check (Status = C.Values.Available, "release parent");
      Check (C.Retained_Results (A) = 0 and then C.Valid (A), "valid and roots balanced");
   end loop;
   Ada.Text_IO.Put_Line ("missing member checks" & Checks'Image);
end Missing_Member_Tests;
