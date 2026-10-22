with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Breakpoint_Tests is
   use type AML_Decode.Bytes;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   package E renames AML_Execute;
   package N is new AML_Namespace
     (Capacity => 24, Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type E.Execution_Status;
   use type C.Access_Status;
   use type C.Module_Outcome;
   A : C.Arena;
   Access_Result : C.Access_Status;
   Report : C.Module_Report;
   Init : N.Initialization_Report;
   Result : C.Result;
   Node : N.Node_ID;
   Checks : Natural := 0;
   BP : constant Bytes := [16#CC#];
   Ret_One : constant Bytes := [16#A4#, 1];
   Acquire : constant Bytes := [16#5B#, 16#23#] & Enc ("M000") & [0, 0];
   Release_Mutex : constant Bytes := [16#5B#, 16#27#] & Enc ("M000");
   Payload : constant Bytes := BP & [16#5B#, 1] & Enc ("M000") & [0]
     & Method ("BASE", BP & Ret_One)
     & Method ("NEST", BP & [16#A4#] & Enc ("BASE"))
     & Method ("SKIP", Block ([16#A0#], Bytes'(1 => 0) & BP) & Ret_One)
     & Method ("LOCK", Acquire & BP & Release_Mutex & Ret_One)
     & Method ("FAIL", Acquire & BP & [16#A4#, 16#0A#]);
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Label; end if;
   end Check;
   function Table (Code : Bytes; Width : Integer_Width) return Bytes is
      Data : Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Byte := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT");
      for I in 1 .. 4 loop Data (4 + I) := Byte ((Data'Length / 256 ** (I - 1)) mod 256); end loop;
      Data (9) := (if Width = Bits_32 then 1 else 2);
      Data (11 .. 16) := Enc ("CUBIT "); Data (17 .. 24) := Enc ("BRKNOOP ");
      Data (37 .. Data'Last) := Code;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Table;
   procedure Direct
     (Code : Bytes; Width : Integer_Width; Budget : Natural;
      Expected : E.Execution_Status; Charge : Natural) is
      R : constant E.Execution_Result := E.Run (Code, Width, [others => 0], 0, Budget);
   begin
      Check (R.Status = Expected, "direct status" & R.Status'Image);
      Check (R.Charged = Charge, "direct exact instruction charge");
      if Expected = E.Returned then Check (R.Value = 1, "following return"); end if;
   end Direct;
   procedure Invoke
     (Input : aliased AML_Table_Backing.State; Name : String;
      Expected : E.Execution_Status; Budget : Natural := 100) is
   begin
      C.Find_Child (A, N.Root, Name, Node, Access_Result);
      Check (Access_Result = C.Available and then Node /= N.Root, "find method");
      -- Actual owner session; no synthetic debug observer or lock callbacks.
      C.Invoke (A, Input, Node, [others => (C.Immediate_Argument, 0)], 0,
        Budget, Result, Access_Result);
      Check (Access_Result = C.Available and then Result.Status = Expected,
        Name & " owned status" & Result.Status'Image);
      Check (Result.Charged <= Budget, "owned fuel bounded");
      if Expected = E.Returned then Check (Result.Number = 1, "owned following return"); end if;
      Check (C.Retained_Count (A) = 0, "no retained results");
   end Invoke;
begin
   C.Initialize (A, Access_Result); Check (Access_Result = C.Available, "initialize");
   for Width in Integer_Width loop
      Direct (BP, Width, 0, E.Budget_Exceeded, 0);
      Direct (BP, Width, 1, E.No_Return, 1);
      Direct (BP & Ret_One, Width, 2, E.Budget_Exceeded, 2);
      Direct (BP & Ret_One, Width, 3, E.Returned, 3);
      Direct (BP & BP & Ret_One, Width, 4, E.Returned, 4);
      Direct (BP & [16#A4#, 16#0A#], Width, 10, E.Truncated, 3);
      Direct (BP & [16#5B#], Width, 10, E.Truncated, 2);
      Direct ([16#A4#, 16#CC#], Width, 10, E.Unsupported, 2);
      declare
         Data : constant Bytes := Table (BP, Width);
         Input : aliased AML_Table_Backing.State (1, Data'Length);
      begin
         Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
         C.Reset (A, Access_Result); Check (Access_Result = C.Available, "module reset");
         C.Execute_Modules (A, Input, 0, Report, Access_Result);
         Check (Access_Result = C.Available and then Report.Outcome = C.Module_Rejected
           and then Report.Failure = E.Budget_Exceeded and then Report.Charged = 0,
           "module zero budget");
         C.Reset (A, Access_Result); Check (Access_Result = C.Available, "module reset after budget");
         C.Execute_Modules (A, Input, 1, Report, Access_Result);
         Check (Access_Result = C.Available and then Report.Outcome = C.Modules_Completed
           and then Report.Charged = 1, "module exact one instruction");
      end;
      declare
         Data : constant Bytes := Table (Payload, Width);
         Input : aliased AML_Table_Backing.State (1, Data'Length);
      begin
         Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
         C.Reset (A, Access_Result); Check (Access_Result = C.Available, "owned module reset");
         C.Execute_Modules (A, Input, 1000, Report, Access_Result);
         Check (Access_Result = C.Available and then Report.Outcome = C.Modules_Completed,
           "module continues to mutex and method declarations");
         C.Seal (A, Init, Access_Result); Check (Access_Result = C.Available, "seal");
         for Repeat in 1 .. 2 loop
            Invoke (Input, "BASE", E.Returned, 3);
            Invoke (Input, "BASE", E.Budget_Exceeded, 0);
            Invoke (Input, "BASE", E.Returned, 3);
            Invoke (Input, "NEST", E.Returned);
            Invoke (Input, "SKIP", E.Returned, 4);
            Check (Result.Charged = 4, "skipped breakpoint not charged");
            Invoke (Input, "LOCK", E.Returned);
            Invoke (Input, "FAIL", E.Truncated);
            Invoke (Input, "LOCK", E.Returned);
         end loop;
      end;
      C.Reset (A, Access_Result); Check (Access_Result = C.Available, "final reset after lock cleanup");
   end loop;
   Ada.Text_IO.Put_Line ("Non-debug BreakPoint checks" & Checks'Image);
end Breakpoint_Tests;
