with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Operation_Region_Declaration_Tests is
   package E renames AML_Execute;
   package N is new AML_Namespace
     (Capacity => 48, Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type Bytes;
   use type Byte;
   use type Integer_Value;
   use type E.Execution_Status;
   use type C.Access_Status;
   use type C.Module_Outcome;
   use type C.Collection_Count;
   A : C.Arena;
   Access_Result : C.Access_Status;
   Report : C.Module_Report;
   Init : N.Initialization_Report;
   Result : C.Result;
   Checks : Natural := 0;
   Ret_One : constant Bytes := [16#A4#, 1];
   function Region (Address, Length : Bytes; Space : Byte := 0) return Bytes is
     ([16#5B#, 16#80#] & Enc ("REG0") & [Space] & Address & Length);
   Kind_Result : constant Bytes := [16#A4#, 16#8E#] & Enc ("REG0");
   Fixed_Fatal : constant Bytes :=
     [16#5B#,16#32#,16#A5#,16#78#,16#56#,16#34#,16#12#,16#0A#,16#33#];
   Buffer_Address : constant Bytes := Block ([16#11#], [16#0A#,2,16#34#,16#12#]);
   Garbage : constant Bytes := Block ([16#11#], [16#0A#,128]);
   Small : constant Bytes := Block ([16#11#], [1]);
   Payload : constant Bytes := [16#08#] & Enc ("MARK") & [0]
     & Method ("OFFS", [16#75#] & Enc ("MARK") & [16#A4#,16#0A#,16#20#])
     & Method ("LENG", [16#75#] & Enc ("MARK") & [16#A4#,16#0A#,16#10#])
     & Method ("READ", [16#A4#] & Enc ("MARK"))
     & Method ("CLER", [16#70#,0] & Enc ("MARK") & Ret_One)
     & Method ("GOOD", Region ([16#0A#,16#20#], [16#0A#,16#10#]) & Kind_Result)
     & Method ("UNUS", Region (Enc ("OFFS"), Enc ("LENG")) & Ret_One)
     & Block ([16#14#], Enc ("ARGS") & [2] & [16#70#,16#68#,16#60#]
         & Region ([16#60#], [16#69#]) & Kind_Result)
     & Method ("CALL", [16#A4#] & Enc ("ARGS") & [16#0A#,16#20#,16#0A#,16#10#])
     & Method ("DUPL", Region ([0], [1]) & Region (Enc ("OFFS"), Enc ("LENG")) & Ret_One)
     & Method ("DIV0", [16#75#] & Enc ("MARK") & [16#A4#,16#78#,1,0,0,0])
     & Method ("FAIL", Region (Enc ("DIV0"), Enc ("LENG")) & Ret_One)
     & Method ("FLEN", Region (Enc ("OFFS"), Enc ("DIV0")) & Ret_One)
     & Method ("FERR", Fixed_Fatal & Ret_One)
     & Method ("FATO", Region (Enc ("FERR"), Enc ("LENG")) & Ret_One)
     & Method ("FATL", Region (Enc ("OFFS"), Enc ("FERR")) & Ret_One)
     & Method ("PRES", [16#70#] & Garbage & [16#60#,16#70#,0,16#60#,16#70#]
         & Small & [16#61#,16#A4#,1])
     & Method ("GCOP", Region (Buffer_Address, Enc ("PRES")) & Kind_Result)
     & Method ("NOIO", Region ([16#0A#,16#20#], [1], 1)
         & Block ([16#5B#,16#81#], Enc ("REG0") & [1] & Enc ("FLD0") & [8]) & Ret_One)
     & Method ("PKGF", Region (Block ([16#12#], [1,0]), Enc ("LENG")) & Ret_One)
     & Method ("WIDE", Region ([0], [16#0E#,0,0,0,0,1,0,0,0]) & Kind_Result)
     & Method ("SPAC", Region (Enc ("OFFS"), Enc ("LENG"), 16#80#) & Kind_Result);
   procedure Check (OK : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Label; end if;
   end Check;
   function Table (Code : Bytes; Width : Integer_Width) return Bytes is
      Data : Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Byte := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT");
      for I in 1 .. 4 loop Data (4 + I) := Byte ((Data'Length / 256 ** (I - 1)) mod 256); end loop;
      Data (9) := (if Width = Bits_32 then 1 else 2);
      Data (11 .. 16) := Enc ("CUBIT "); Data (17 .. 24) := Enc ("OPREGION");
      Data (37 .. Data'Last) := Code;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Table;
   procedure Invoke (Input : aliased AML_Table_Backing.State; Name : String;
      Expected : E.Execution_Status; Number : Integer_Value := 1; Budget : Natural := 1000;
      Expected_Charge : Natural := Natural'Last) is
      Method_Node, Child_Node : N.Node_ID;
   begin
      C.Find_Child (A, N.Root, Name, Method_Node, Access_Result);
      Check (Access_Result = C.Available and then Method_Node /= N.Root, "find " & Name);
      C.Invoke (A, Input, Method_Node, [others => (C.Immediate_Argument, 0)], 0,
        Budget, Result, Access_Result);
      Check (Access_Result = C.Available, "invoke access");
      Check (Result.Status = Expected, Name & " status " & Result.Status'Image);
      Check (Result.Charged <= Budget, "bounded charge");
      if Expected_Charge /= Natural'Last then
         Check (Result.Charged = Expected_Charge, "exact boundary charge");
      end if;
      if Result.Status = E.Returned then Check (Result.Number = Number, "returned number"); end if;
      if Result.Status = E.Fatal_Requested then
         Check (Result.Fatal.Literal.Kind = 16#A5#, "first fatal kind");
         Check (Result.Fatal.Evaluated_Argument = 16#33#, "first fatal argument");
      end if;
      C.Find_Child (A, Method_Node, "REG0", Child_Node, Access_Result);
      Check (Access_Result = C.Available and then Child_Node = N.Root, "method region retired");
      Check (C.Retained_Count (A) = 0, "no retained roots");
   end Invoke;
begin
   C.Initialize (A, Access_Result); Check (Access_Result = C.Available, "initialize");
   for Width in Integer_Width loop
      declare
         Data : constant Bytes := Table (Payload, Width);
         Input : aliased AML_Table_Backing.State (1, Data'Length);
         Before, After : C.Collection_Statistics;
         Args_Node, Child_Node : N.Node_ID;
      begin
         Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
         C.Reset (A, Access_Result); Check (Access_Result = C.Available, "reset");
         C.Execute_Modules (A, Input, 10000, Report, Access_Result);
         Check (Access_Result = C.Available and then Report.Outcome = C.Modules_Completed, "module load");
         C.Seal (A, Init, Access_Result); Check (Access_Result = C.Available, "seal");
         -- One term charge, four NameSeg bytes, then one Space byte.
         -- Insufficient name/Space fuel must not run either operand helper.
         Invoke (Input, "CLER", E.Returned);
         for Budget in 0 .. 6 loop
            Invoke (Input, "UNUS", E.Budget_Exceeded, Budget => Budget,
              Expected_Charge => (if Budget = 0 then 0 elsif Budget < 5 then 1 else Budget));
            Invoke (Input, "READ", E.Returned, 0);
         end loop;
         for Repeat in 1 .. 2 loop
            Invoke (Input, "CLER", E.Returned);
            Invoke (Input, "UNUS", E.Returned);
            Invoke (Input, "READ", E.Returned, 2);
            Invoke (Input, "UNUS", E.Returned);
            Invoke (Input, "READ", E.Returned, 4);
            Invoke (Input, "GOOD", E.Returned, 10);
            Invoke (Input, "CALL", E.Returned, 10);
            C.Find_Child (A, N.Root, "ARGS", Args_Node, Access_Result);
            Check (Access_Result = C.Available and then Args_Node /= N.Root, "args producer");
            C.Find_Child (A, Args_Node, "REG0", Child_Node, Access_Result);
            Check (Access_Result = C.Available and then Child_Node = N.Root, "nested owner retirement");
            Invoke (Input, "CLER", E.Returned);
            Invoke (Input, "DUPL", E.Duplicate_Name);
            Invoke (Input, "READ", E.Returned, 0);
            Invoke (Input, "FAIL", E.Division_By_Zero);
            Invoke (Input, "READ", E.Returned, 1);
            Invoke (Input, "FLEN", E.Division_By_Zero);
            Invoke (Input, "READ", E.Returned, 3);
            Invoke (Input, "FATO", E.Fatal_Requested);
            Invoke (Input, "READ", E.Returned, 3);
            Invoke (Input, "FATL", E.Fatal_Requested);
            Invoke (Input, "READ", E.Returned, 4);
            Invoke (Input, "UNUS", E.Budget_Exceeded, Budget => 2);
            Invoke (Input, "GOOD", E.Returned, 10);
            Before := C.Reclamation_Metrics (A);
            Invoke (Input, "GCOP", E.Returned, 10);
            After := C.Reclamation_Metrics (A);
            Check (After.Completed > Before.Completed, "actual completed GC");
            Check (After.Freed_Bytes >= Before.Freed_Bytes + 128, "actual discarded buffer reclaimed");
            Check (After.Rejected = Before.Rejected, "no census rejection");
            Invoke (Input, "NOIO", E.Returned, 1);
            Invoke (Input, "PKGF", E.Unsupported_Value);
            Invoke (Input, "SPAC", E.Returned, 10);
            Invoke (Input, "WIDE", E.Returned, 10);
            Invoke (Input, "GOOD", E.Returned, 10);
         end loop;
      end;
      declare
         Data : constant Bytes := Table (Region ([0], [1]), Width);
         Input : aliased AML_Table_Backing.State (1, Data'Length);
      begin
         Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
         C.Reset (A, Access_Result); Check (Access_Result = C.Available, "module boundary reset");
         C.Execute_Modules (A, Input, 1000, Report, Access_Result);
         Check (Access_Result = C.Available and then Report.Outcome = C.Modules_Completed
           and then Report.Completed = 1, "simple module region completed");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Operation region declaration checks" & Checks'Image);
end Operation_Region_Declaration_Tests;
