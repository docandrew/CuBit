with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Mutex_Module_Tests is
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Byte;
   package E renames AML_Execute;
   package N is new AML_Namespace (Capacity => 24, Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type E.Execution_Status;
   use type C.Access_Status;
   use type C.Module_Outcome;
   A : C.Arena;
   Status : C.Access_Status;
   Report : C.Module_Report;
   Init : N.Initialization_Report;
   Result : C.Result;
   Node : N.Node_ID;
   Checks : Natural := 0;
   Declare_Mutex : constant Bytes := [16#5B#, 1] & Enc ("M000") & [0];
   Acquire_Mutex : constant Bytes := [16#5B#, 16#23#] & Enc ("M000") & [0, 0];
   Release_Mutex : constant Bytes := [16#5B#, 16#27#] & Enc ("M000");
   Bad : constant Bytes := Declare_Mutex & Acquire_Mutex
     & [16#08#] & Enc ("ZVAL") & [0, 16#78#, 1] & Enc ("ZVAL") & [0, 0];
   Good : constant Bytes := Declare_Mutex & Acquire_Mutex & Release_Mutex
     & Method ("PROB", Acquire_Mutex & Release_Mutex & [16#A4#, 0]);
   function Table (Payload : Bytes; Revision : Byte) return Bytes is
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Byte := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT");
      for I in 1 .. 4 loop Data (4 + I) := Byte ((Data'Length / 256 ** (I - 1)) mod 256); end loop;
      Data (9) := Revision; Data (11 .. 16) := Enc ("CUBIT "); Data (17 .. 24) := Enc ("MUTEXMOD");
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Table;
   procedure Check (Value : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Label; end if; end Check;
begin
   C.Initialize (A, Status); Check (Status = C.Available, "initialize");
   for Width in Integer_Width loop
      declare
         Revision : constant Byte := (if Width = Bits_32 then 1 else 2);
         Bad_Table : constant Bytes := Table (Bad, Revision);
         Good_Table : constant Bytes := Table (Good, Revision);
         Bad_Input : aliased AML_Table_Backing.State (1, Bad_Table'Length);
         Good_Input : aliased AML_Table_Backing.State (1, Good_Table'Length);
      begin
         Bad_Input.Count := 1; Bad_Input.Tables (1) := (0, Bad_Table'Length); Bad_Input.Data.Bytes := Bad_Table;
         Good_Input.Count := 1; Good_Input.Tables (1) := (0, Good_Table'Length); Good_Input.Data.Bytes := Good_Table;
         C.Reset (A, Status); Check (Status = C.Available, "reset for failed module");
         C.Execute_Modules (A, Bad_Input, 1000, Report, Status);
         Check (Status = C.Available and then Report.Outcome = C.Module_Rejected
           and then Report.Failure = E.Division_By_Zero and then Report.Completed = 0,
           "module fails only after successful acquisition");
         Check (C.Node_Count (A) = 0 and then C.Retained_Count (A) = 0, "failed module namespace rolled back");
         C.Find_Child (A, N.Root, "M000", Node, Status);
         Check (Status = C.Available and then Node = N.Root, "failed mutex binding removed");
         C.Reset (A, Status); Check (Status = C.Available, "failed invocation drained before reset");
         C.Execute_Modules (A, Good_Input, 1000, Report, Status);
         Check (Status = C.Available and then Report.Outcome = C.Modules_Completed, "new owner module succeeds");
         C.Seal (A, Init, Status); Check (Status = C.Available, "seal new owner");
         C.Find_Child (A, N.Root, "PROB", Node, Status);
         Check (Status = C.Available and then Node /= N.Root, "find probe");
         for Repeat in 1 .. 2 loop
            C.Invoke (A, Good_Input, Node, [others => (C.Immediate_Argument, 0)], 0, 1000, Result, Status);
            Check (Status = C.Available and then Result.Status = E.Returned, "reentry after rollback");
            Check (Result.Number = 0, "reentry acquired and released");
            Check (C.Retained_Count (A) = 0, "no retained leaks");
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Mutex module checks" & Checks'Image);
end Mutex_Module_Tests;
