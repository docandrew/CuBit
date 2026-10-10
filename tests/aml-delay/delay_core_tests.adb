with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with ACPI_Service_Core;
with Firmware_Tables;
procedure Delay_Core_Tests is
   use type AML_Delays.Outcome;
   use type Firmware_Tables.Byte;
   Selected : AML_Delays.Outcome := AML_Delays.Completed;
   Calls : Natural := 0;
   procedure Provider (Item : AML_Delays.Request; Result : out AML_Delays.Outcome) is
      pragma Unreferenced (Item);
   begin Calls := Calls + 1; Result := Selected; end Provider;
   package S is new ACPI_Service_Core (Provider);
   use type S.Install_Status;
   use type S.Values.Access_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with "delay Core" & Checks'Image; end if; end Check;
   function Table (Code : Bytes) return Firmware_Tables.Bytes is
      R : Firmware_Tables.Bytes (1 .. 36 + Code'Length) := [others => 0];
      Size : Natural := R'Length;
      Sum : Firmware_Tables.Byte := 0;
   begin
      R (1 .. 4) := [16#44#,16#53#,16#44#,16#54#];
      for I in 5 .. 8 loop R (I) := Firmware_Tables.Byte (Size mod 256); Size := Size / 256; end loop;
      R (9) := 2;
      for I in Code'Range loop R (37 + I - Code'First) := Firmware_Tables.Byte (Code (I)); end loop;
      for B of R loop Sum := Sum + B; end loop; R (10) := 0 - Sum;
      return R;
   end Table;
   Data : constant Firmware_Tables.Bytes := Table
     ([16#14#,12,16#54#,16#45#,16#53#,16#54#,0,16#5B#,16#22#,1,16#A4#,16#0A#,7,
       16#14#,14,16#42#,16#41#,16#44#,16#30#,0,16#5B#,16#21#,16#0B#,0,1,16#A4#,16#0A#,7]);
   Tree : aliased S.State (1, 512, 512);
   Installed : S.Install_Status;
   Report : S.Namespace.Initialization_Report;
   Access_Result : S.Values.Access_Status;
   Result : S.Values.Result;
   Scalar : Execution_Result;
begin
   S.Install (Tree, 1, S.DSDT, Data, Installed); Check (Installed = S.Installed);
   S.Initialize_Members (Tree, Report, Access_Result); Check (Access_Result = S.Values.Available);
   for Choice in AML_Delays.Outcome loop
      Selected := Choice; Calls := 0;
      S.Invoke_Retained (Tree, 1, [others => 0], 0, 100, Result, Access_Result);
      Check (Access_Result = S.Values.Available);
      Check (Result.Status = (case Choice is when AML_Delays.Completed => Returned,
          when AML_Delays.Unavailable => Unsupported, when AML_Delays.Failed => Delay_Failed));
      Check (Calls = 1 and S.Retained_Results (Tree) = 0);
      S.Invoke_Scalar (Tree, 1, [others => 0], 0, 100, Scalar);
      Check (Scalar.Status = Result.Status); Check (Calls = 2);
   end loop;
   Calls := 0;
   S.Invoke_Retained (Tree, 2, [others => 0], 0, 100, Result, Access_Result);
   Check (Result.Status = Invalid_Delay and Calls = 0);
   S.Invoke_Scalar (Tree, 2, [others => 0], 0, 100, Scalar);
   Check (Scalar.Status = Invalid_Delay and Calls = 0);
   Ada.Text_IO.Put_Line ("DELAY CORE PASS" & Checks'Image);
end Delay_Core_Tests;
