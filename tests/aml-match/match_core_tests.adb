with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with ACPI_Service_Core;
with Firmware_Tables;
procedure Match_Core_Tests is
   use type Firmware_Tables.Byte;
   use type AML_Decode.Integer_Value;
   package S is new ACPI_Service_Core (AML_Delays.Unavailable_Provider);
   use type S.Install_Status;
   use type S.Values.Access_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   function Name (Text : String) return Bytes is
      Data : Bytes (1 .. Text'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Data;
   end Name;
   function Method (Text : String; Code : Bytes) return Bytes is
     ([16#14#,Byte (Code'Length + 6)] & Name (Text) & Bytes'(1 => 0) & Code);
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
   Code : constant Bytes :=
     Bytes'(16#08#,77,65,82,75,0) &
     Method ("SET0", [16#70#,1] & Name ("MARK") & [16#A4#,0]) &
     Method ("OBJ0", [16#A4#,16#12#,2,0]) &
     Method ("INV0", [16#A4#,16#89#,16#12#,3,1,1,6,0,0,0] & Name ("SET0")) &
     Method ("TYPE", [16#A4#,16#89#,16#12#,3,1,1,6] & Name ("OBJ0") &
       [0,0] & Name ("SET0")) &
     Method ("EMPT", [16#70#,1] & Name ("MARK") &
       [16#A4#,16#89#,16#12#,3,1,1,6] & Name ("OBJ0") & [0,0,16#11#,2,0]) &
     Method ("BND0", [16#A4#,16#89#,16#12#,3,1,1,0,0,0,0,1]) &
     Method ("CLRM", [16#70#,0] & Name ("MARK") & [16#A4#,0]);
   Data : constant Firmware_Tables.Bytes := Table (Code);
   Tree : aliased S.State (1, 1024, 1024);
   Installed : S.Install_Status;
   Report : S.Namespace.Initialization_Report;
   Access_Result : S.Values.Access_Status;
   Result : S.Values.Result;
   Scalar : Execution_Result;
   Expected : constant array (Positive range 4 .. 7) of Execution_Status :=
     [Invalid_Match_Operation, Unsupported_Value, Empty_Buffer, Package_Limit];
begin
   S.Install (Tree, 1, S.DSDT, Data, Installed); Check (Installed = S.Installed);
   S.Initialize_Members (Tree, Report, Access_Result);
   Check (Access_Result = S.Values.Available and then Report.Missing = 0 and then Report.Unsupported = 0);
   for Node in Expected'Range loop
      S.Invoke_Scalar (Tree, 8, [others => 0], 0, 100, Scalar);
      Check (Scalar.Status = Returned and then Scalar.Value = 0);
      S.Invoke_Retained (Tree, Node, [others => 0], 0, 100, Result, Access_Result);
      Check (Access_Result = S.Values.Available and then Result.Status = Expected (Node));
      Check (S.Retained_Results (Tree) = 0);
      S.Observe_Named_Value (Tree, 1, Result, Access_Result);
      Check (Access_Result = S.Values.Available and then Result.Status = Returned and then Result.Number = (if Node = 7 then 0 else 1));
      S.Invoke_Scalar (Tree, Node, [others => 0], 0, 100, Scalar);
      Check (Scalar.Status = Expected (Node));
   end loop;
   Ada.Text_IO.Put_Line ("Match Core checks" & Checks'Image);
end Match_Core_Tests;
