with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Fatal;
with AML_Namespace;
with AML_Table_Backing;
with Interfaces;
with Root_Test_Support; use Root_Test_Support;
procedure Fatal_Result_Tests is
   use type AML_Decode.Bytes;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   use type Interfaces.Unsigned_32;
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
   Fixed : constant Bytes := [16#5B#, 16#32#, 16#A5#, 16#78#, 16#56#, 16#34#, 16#F2#];
   Fatal_51 : constant Bytes := Fixed & [16#0A#, 16#33#];
   Inner_Fixed : constant Bytes := [16#5B#,16#32#,16#C7#,16#BE#,16#BA#,16#FE#,16#CA#];
   Outer_Fixed : constant Bytes := [16#5B#,16#32#,16#B6#,16#78#,16#56#,16#34#,16#12#];
   Ret_One : constant Bytes := [16#A4#, 1];
   Acquire : constant Bytes := [16#5B#, 16#23#] & Enc ("M000") & [0, 0];
   Release_Mutex : constant Bytes := [16#5B#, 16#27#] & Enc ("M000");
   Payload : constant Bytes := [16#5B#, 1] & Enc ("M000") & [0]
     & [16#08#] & Enc ("MARK") & [0]
     & Method ("BASE", Fatal_51 & Ret_One)
     & Method ("NEST", [16#A4#] & Enc ("BASE"))
     & Method ("GOOD", Ret_One)
     & Method ("SIDE", [16#75#] & Enc ("MARK") & [16#A4#, 16#0A#, 16#33#])
     & Method ("ONCE", Fixed & Enc ("SIDE") & [16#75#] & Enc ("MARK"))
     & Method ("READ", [16#A4#] & Enc ("MARK"))
     & Method ("LOCK", Acquire & Fatal_51)
     & Method ("PROB", Acquire & Release_Mutex & Ret_One)
     & Block ([16#14#], Enc ("SERL") & [16#08#] & Fatal_51)
     & Method ("FAIL", Fixed & [16#78#, 1, 0, 0, 0]);
   Edge_Payload : constant Bytes := [16#08#] & Enc ("MARK") & [0]
     & Method ("READ", [16#A4#] & Enc ("MARK"))
     & Method ("GOOD", Ret_One)
     & Method ("INNR", Inner_Fixed & [16#0A#,16#66#,16#75#] & Enc ("MARK"))
     & Method ("OUTR", Outer_Fixed & Enc ("INNR") & [16#75#] & Enc ("MARK"))
     & Method ("SSTR", Fixed & [16#0D#] & Enc ("FEDCBA9876543210") & [0])
     & Method ("BUFF", Fixed & Block ([16#11#],
         [16#0A#,9,16#10#,16#32#,16#54#,16#76#,16#98#,16#BA#,16#DC#,16#FE#,16#99#]))
     & Method ("EMPT", Fixed & Block ([16#11#], [0]))
     & Method ("PKGF", Fixed & Block ([16#12#], [1,1]));
   procedure Check (Condition : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Label; end if;
   end Check;
   procedure Check_Payload (Item : AML_Fatal.Request; Width : Integer_Width; Argument : Integer_Value;
      Kind : Byte := 16#A5#; Code : Interfaces.Unsigned_32 := 16#F2345678#) is
   begin
      Check (Item.Literal.Kind = Kind, "raw fatal type");
      Check (Item.Literal.Code = Code, "literal little endian code");
      Check (Item.Evaluated_Argument = Argument, "full evaluated argument");
      Check (Item.Width = Width, "execution width");
   end Check_Payload;
   function Table (Code : Bytes; Width : Integer_Width) return Bytes is
      Data : Bytes (1 .. 36 + Code'Length) := [others => 0];
      Sum : Byte := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT");
      for I in 1 .. 4 loop Data (4 + I) := Byte ((Data'Length / 256 ** (I - 1)) mod 256); end loop;
      Data (9) := (if Width = Bits_32 then 1 else 2);
      Data (11 .. 16) := Enc ("CUBIT "); Data (17 .. 24) := Enc ("FATALREQ");
      Data (37 .. Data'Last) := Code;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum;
      return Data;
   end Table;
   procedure Direct (Code : Bytes; Width : Integer_Width; Budget : Natural;
      Expected : E.Execution_Status; Charge : Natural) is
      R : constant E.Execution_Result := E.Run (Code, Width, [others => 0], 0, Budget);
   begin
      Ada.Text_IO.Put_Line ("DIRECT length=" & Code'Length'Image & " width=" & Width'Image
        & " budget=" & Budget'Image & " actual=" & R.Status'Image & " charged=" & R.Charged'Image
        & " expected=" & Expected'Image & " charged=" & Charge'Image);
      Check (R.Status = Expected, "direct status" & R.Status'Image);
      Check (R.Charged = Charge, "direct exact charge");
      if R.Status = E.Fatal_Requested then Check_Payload (R.Fatal, Width, 16#33#); end if;
   end Direct;
   procedure Invoke (Input : aliased AML_Table_Backing.State; Name : String;
      Width : Integer_Width; Expected : E.Execution_Status; Number : Integer_Value := 1;
      Fatal_Argument : Integer_Value := 16#33#; Kind : Byte := 16#A5#;
      Code : Interfaces.Unsigned_32 := 16#F2345678#) is
   begin
      C.Find_Child (A, N.Root, Name, Node, Access_Result);
      Check (Access_Result = C.Available and then Node /= N.Root, "find method");
      C.Invoke (A, Input, Node, [others => (C.Immediate_Argument, 0)], 0,
        1000, Result, Access_Result);
      Check (Access_Result = C.Available and then Result.Status = Expected, Name & " outcome");
      Check (Result.Charged <= 1000, "fuel bounded");
      if Result.Status = E.Returned then Check (Result.Number = Number, "ordinary value");
      elsif Result.Status = E.Fatal_Requested then Check_Payload (Result.Fatal, Width, Fatal_Argument, Kind, Code); end if;
      Check (C.Retained_Count (A) = 0, "fatal owns no retained result root");
   end Invoke;
begin
   C.Initialize (A, Access_Result); Check (Access_Result = C.Available, "initialize");
   for Width in Integer_Width loop
      Direct (Fatal_51, Width, 0, E.Budget_Exceeded, 0);
      Direct (Fatal_51, Width, 6, E.Budget_Exceeded, 1);
      Direct (Fixed, Width, 100, E.Truncated, 8);
      Direct (Fatal_51 & Ret_One, Width, 100, E.Fatal_Requested, 8);
      for Length in 1 .. Fixed'Length - 1 loop
         Direct (Fixed (1 .. Length), Width, 100, E.Truncated, 1);
      end loop;
      declare
         High : constant Integer_Value := 16#FEDCBA98_76543210#;
         R : constant E.Execution_Result := E.Run (Fixed & [16#68#], Width, [0 => High, others => 0], 1, 100);
      begin
         Check (R.Status = E.Fatal_Requested, "argument fatal status");
         if R.Status = E.Fatal_Requested then
            Check_Payload (R.Fatal, Width, High);
            Check (AML_Fatal.ACPICA_Host_Argument (R.Fatal) = 16#76543210#, "explicit host low32 projection");
         end if;
      end;
      declare
         R : constant E.Execution_Result := E.Run
           (Fixed & [16#0E#,16#10#,16#32#,16#54#,16#76#,16#98#,16#BA#,16#DC#,16#FE#],
            Width, [others => 0], 0, 100);
      begin
         Check (R.Status = E.Fatal_Requested, "QWord literal fatal status");
         if R.Status = E.Fatal_Requested then
            Check_Payload (R.Fatal, Width,
              (if Width = Bits_32 then 16#76543210# else 16#FEDCBA98_76543210#));
         end if;
      end;
      declare
         Data : constant Bytes := Table (Fatal_51 & Payload, Width);
         Input : aliased AML_Table_Backing.State (1, Data'Length);
      begin
         Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
         C.Reset (A, Access_Result); Check (Access_Result = C.Available, "module reset");
         C.Execute_Modules (A, Input, 1000, Report, Access_Result);
         Check (Access_Result = C.Available and then Report.Outcome = C.Module_Rejected
           and then Report.Failure = E.Fatal_Requested, "module abort");
         Check (Report.Fatal.Present, "payload survives module rollback");
         if Report.Fatal.Present then Check_Payload (Report.Fatal.Item, Width, 16#33#); end if;
      end;
      declare
         Data : constant Bytes := Table (Payload, Width);
         Input : aliased AML_Table_Backing.State (1, Data'Length);
      begin
         Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
         C.Reset (A, Access_Result); Check (Access_Result = C.Available, "reset after fatal rollback");
         C.Execute_Modules (A, Input, 1000, Report, Access_Result);
         Check (Access_Result = C.Available and then Report.Outcome = C.Modules_Completed, "load recovery");
         Check (not Report.Fatal.Present, "fresh module report");
         C.Seal (A, Init, Access_Result); Check (Access_Result = C.Available, "seal");
         for Repeat in 1 .. 2 loop
            Invoke (Input, "BASE", Width, E.Fatal_Requested);
            Invoke (Input, "NEST", Width, E.Fatal_Requested);
            Invoke (Input, "LOCK", Width, E.Fatal_Requested);
            Invoke (Input, "PROB", Width, E.Returned);
            Invoke (Input, "SERL", Width, E.Fatal_Requested);
            Invoke (Input, "SERL", Width, E.Fatal_Requested);
            Invoke (Input, "FAIL", Width, E.Division_By_Zero);
            Invoke (Input, "ONCE", Width, E.Fatal_Requested);
            Invoke (Input, "READ", Width, E.Returned, Integer_Value (Repeat));
            Invoke (Input, "GOOD", Width, E.Returned);
         end loop;
      end;
      declare
         Data : constant Bytes := Table (Edge_Payload, Width);
         Input : aliased AML_Table_Backing.State (1, Data'Length);
      begin
         Input.Count := 1; Input.Tables (1) := (0, Data'Length); Input.Data.Bytes := Data;
         C.Reset (A, Access_Result); Check (Access_Result = C.Available, "separate edge table reset");
         C.Execute_Modules (A, Input, 1000, Report, Access_Result);
         Check (Access_Result = C.Available and then Report.Outcome = C.Modules_Completed, "load edge table");
         C.Seal (A, Init, Access_Result); Check (Access_Result = C.Available, "seal edge table");
         for Repeat in 1 .. 2 loop
            Invoke (Input, "OUTR", Width, E.Fatal_Requested,
              Fatal_Argument => 16#66#, Kind => 16#C7#, Code => 16#CAFEBABE#);
            Invoke (Input, "READ", Width, E.Returned, 0);
            Invoke (Input, "SSTR", Width, E.Fatal_Requested,
              Fatal_Argument => (if Width = Bits_32 then 16#FEDCBA98# else 16#FEDCBA9876543210#));
            Invoke (Input, "BUFF", Width, E.Fatal_Requested,
              Fatal_Argument => (if Width = Bits_32 then 16#76543210# else 16#FEDCBA9876543210#));
            Invoke (Input, "EMPT", Width, E.Empty_Buffer);
            Invoke (Input, "PKGF", Width, E.Unsupported_Value);
            Invoke (Input, "READ", Width, E.Returned, 0);
            Invoke (Input, "GOOD", Width, E.Returned);
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Fatal result checks" & Checks'Image);
end Fatal_Result_Tests;
