with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with ACPI_Service_Core;
with ACPI_Service;
with Firmware_Tables;
with Capacity_Fixture; use Capacity_Fixture;
procedure Core_Capacity_Tests is
   use type Integer_Value;
   package S is new ACPI_Service_Core (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 4,
      Aggregate_Method_Capacity => 70000, Retained_Result_Capacity => 1);
   use type S.Install_Status;
   use type S.Namespace.Initialization_Report;
   use type S.Values.Access_Status;
   use type S.Values.Audit_Value;
   use type Firmware_Tables.Byte;
   type State_Access is access S.State;
   Tree : constant State_Access := new S.State (3, 150000, 100000);
   Install_Status : S.Install_Status;
   Report : S.Namespace.Initialization_Report;
   Status : S.Values.Access_Status;
   Result : S.Values.Result;
   Root : S.Values.Value_Handle;
   Before : S.Values.Audit_Value;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Table (Code : Bytes) return Firmware_Tables.Bytes is
      R : Firmware_Tables.Bytes (1 .. 36 + Code'Length) := [others => 0];
      Size : Natural := R'Length;
      Sum : Firmware_Tables.Byte := 0;
   begin
      for I in 1 .. 4 loop R (I) := Firmware_Tables.Byte (Text ("DSDT") (I)); end loop;
      for I in 5 .. 8 loop R (I) := Firmware_Tables.Byte (Size mod 256); Size := Size / 256; end loop;
      R (9) := 2;
      for I in Code'Range loop R (37 + I - Code'First) := Firmware_Tables.Byte (Code (I)); end loop;
      for B of R loop Sum := Sum + B; end loop; R (10) := 0 - Sum;
      return R;
   end Table;
   -- Both methods are legal separately; pool use is exactly 70000.
   Compound : Bytes := Body_Of_Size (30000);
begin
   Check (ACPI_Service.Method_Storage_Capacity = 65536,
      "standard defaults preserved");
   Check (S.Method_Storage_Capacity = 70000, "independent provisioned capacities");
   Compound (1 .. 6) := [16#A4#,16#11#,4,16#0A#,1,16#A5#];
   S.Install (Tree.all, 1, S.DSDT,
      Table (Method ("AAAA", Body_Of_Size (40000)) & Method ("BUFF", Compound)), Install_Status);
   Check (Install_Status = S.Installed and then S.Observe (Tree.all).Method_Bytes = 70000,
      "core aggregate metrics above 65536");
   S.Initialize_Members (Tree.all, Report, Status);
   Check (Status = S.Values.Available and then Report = (0,0,0), "seal");
   S.Invoke_Retained (Tree.all, 2, [others => 0], 0, 100, Result, Status);
   Check (Status = S.Values.Available and then Result.Status = Object_Returned
      and then S.Retained_Results (Tree.all) = 1, "first independent root available");
   Root := Result.Handle; Before := S.Audit (Tree.all);
   S.Invoke_Retained (Tree.all, 2, [others => 0], 0, 100, Result, Status);
   Check (Status = S.Values.Root_Limit and then Result.Charged = 0
      and then S.Retained_Results (Tree.all) = 1 and then S.Audit (Tree.all) = Before,
      "root budget one despite four namespace nodes; before effects");
   S.Release_Result (Tree.all, Root, Status);
   Check (Status = S.Values.Available and then S.Retained_Results (Tree.all) = 0, "release root");
   S.Invoke_Retained (Tree.all, 1, [others => 0], 0, 100, Result, Status);
   Check (Status = S.Values.Available and then Result.Status = Returned and then Result.Number = 1,
      "scalar invocation with provisioned store");
   Ada.Text_IO.Put_Line ("CAPACITY-CORE: PASS" & Checks'Image);
end Core_Capacity_Tests;
