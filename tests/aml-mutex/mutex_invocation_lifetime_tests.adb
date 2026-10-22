with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Identity.Issuer;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Mutex_Invocation_Lifetime_Tests is
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   package E renames AML_Execute;
   procedure Read_Time (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace
     (Capacity => 32, Perform_Delay => AML_Delays.Unavailable_Provider,
      Read_Microseconds => Read_Time);
   package C is new N.Owned.Collecting;
   use type E.Execution_Status;
   use type C.Access_Status;
   use type N.Load_Status;
   use type C.Collection_Count;
   use type C.Audit_Value;
   A : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Init : N.Initialization_Report;
   Result : C.Result;
   Node, Recovery_Node : N.Node_ID;
   Checks : Natural := 0;
   Reset_Probes : Natural := 0;
   Declare_Mutex : constant Bytes := [16#5B#, 1] & Enc ("TEMP") & [0];
   Acquire_Mutex : constant Bytes := [16#5B#, 16#23#] & Enc ("TEMP") & [0, 0];
   Release_Mutex : constant Bytes := [16#5B#, 16#27#] & Enc ("TEMP");
   Allocate : constant Bytes := [16#70#] & Block ([16#11#], [16#0A#, 128, 0]) & [16#60#];
   function Pressure return Bytes is
      Value : Bytes (1 .. Allocate'Length * 12);
   begin
      for I in 0 .. 11 loop
         Value (I * Allocate'Length + 1 .. (I + 1) * Allocate'Length) := Allocate;
      end loop;
      return Value;
   end Pressure;
   Code : constant Bytes :=
     Method ("HOLD", Declare_Mutex & Acquire_Mutex & [16#A4#, 0])
     & Method ("GET0", Declare_Mutex & Acquire_Mutex & [16#A4#] & Enc ("TEMP"))
     & Method ("CHRN", Pressure & [16#A4#, 0])
     & Block ([16#14#], Enc ("PUT0") & [2, 16#5B#, 16#27#, 16#68#, 16#A4#, 0])
     & Method ("NEST", [16#A4#] & Enc ("PUT0") & Enc ("GET0") & Enc ("CHRN"))
     & Method ("GONE", Enc ("HOLD") & Enc ("CHRN") & [16#A4#, 0])
     & Method ("IPKG", Declare_Mutex & Acquire_Mutex & [16#5B#, 16#27#, 16#88#]
         & Block ([16#12#], [1, 16#71#] & Enc ("TEMP")) & [0, 0]
         & Enc ("CHRN") & [16#A4#, 0])
     & Method ("FAIL", Declare_Mutex & Acquire_Mutex & [16#78#, 1, 0, 0, 0])
     & Method ("BUDG", Declare_Mutex & Acquire_Mutex & Block ([16#A2#], [1, 16#A3#]))
     & Method ("NRET", Declare_Mutex & Acquire_Mutex)
     & Method ("RSET", Declare_Mutex & Acquire_Mutex & [16#70#, 16#5B#, 16#33#, 16#60#] & Release_Mutex & [16#A4#, 0])
     & Method ("GLOB", [16#5B#, 1, 16#5C#] & Enc ("_GL_") & [0, 16#5B#, 16#23#, 16#5C#] & Enc ("_GL_") & [0, 0, 16#A4#, 0])
     & Method ("SCOP", [16#5B#, 1] & Enc ("_GL_") & [0, 16#5B#, 16#23#] & Enc ("_GL_") & [0, 0, 16#5B#, 16#27#] & Enc ("_GL_") & [16#A4#, 0])
     & Method ("EVNT", [16#5B#, 2] & Enc ("TEMP") & Acquire_Mutex & [16#A4#, 0])
     & Method ("TRNC", Declare_Mutex & [16#5B#, 16#23#] & Enc ("TEMP") & [0])
     & Method ("WORD", Declare_Mutex & [16#5B#, 16#23#] & Enc ("TEMP") & [0, 16#0A#] & Release_Mutex & [16#A4#, 0])
     & Method ("GOOD", Declare_Mutex & Acquire_Mutex & Release_Mutex & [16#A4#, 0]);
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   procedure Read_Time (Value : out Integer_Value; Available : out Boolean) is
      Before : constant Natural := AML_Identity.Issuer.Issued_Count;
      Reset_Status : C.Access_Status;
      Nested : C.Result;
      Before_Tree : constant C.Audit_Value := C.Audit (A);
   begin
      C.Invoke (A, Input, Recovery_Node, [others => (C.Immediate_Argument, 0)], 0, 1000, Nested, Reset_Status);
      Check (Reset_Status = C.Busy, "active invocation reentry refused");
      Check (C.Audit (A) = Before_Tree and then AML_Identity.Issuer.Issued_Count = Before,
        "refused reentry changes no namespace or owner identity");
      C.Reset (A, Reset_Status);
      Check (Reset_Status = C.Busy, "active reset refused");
      Check (AML_Identity.Issuer.Issued_Count = Before, "active reset does not mint identity");
      Reset_Probes := Reset_Probes + 1;
      Value := 0; Available := True;
   end Read_Time;
   procedure Invoke (Name : String; Expected : E.Execution_Status; Budget : Natural := 1000) is
      Method_Node, Producer_Node : N.Node_ID;
   begin
      C.Find_Child (A, N.Root, Name, Node, Status);
      Check (Status = C.Available and then Node /= N.Root, "find " & Name);
      Method_Node := Node;
      C.Invoke (A, Input, Method_Node, [others => (C.Immediate_Argument, 0)], 0, Budget, Result, Status);
      Check (Status = C.Available, "invoke access " & Name);
      Check (Result.Status = Expected, "invoke status " & Name & Result.Status'Image);
      if Expected = E.Returned then Check (Result.Number = 0, "integer result " & Name); end if;
      Check (C.Retained_Count (A) = 0, "no public retained roots " & Name);
      C.Find_Child (A, Method_Node, "TEMP", Node, Status);
      Check (Status = C.Available and then Node = N.Root, "method-local name retired " & Name);
      if Name = "NEST" or else Name = "GONE" then
         C.Find_Child (A, N.Root, (if Name = "NEST" then "GET0" else "HOLD"), Producer_Node, Status);
         Check (Status = C.Available and then Producer_Node /= N.Root, "producer method remains");
         C.Find_Child (A, Producer_Node, "TEMP", Node, Status);
         Check (Status = C.Available and then Node = N.Root, "nested producer local retired");
      end if;
      if Name = "GLOB" or else Name = "SCOP" then
         C.Find_Child (A, (if Name = "GLOB" then N.Root else Method_Node), "_GL_", Node, Status);
         Check (Status = C.Available and then Node = N.Root, "global/scoped temporary retired");
      end if;
   end Invoke;
begin
   C.Initialize (A, Status); Check (Status = C.Available, "initialize");
   for Width in Integer_Width loop
      C.Reset (A, Status); Check (Status = C.Available, "reset");
      C.Load (A, Code, Width, Loaded, Status);
      Check (Status = C.Available and then Loaded = N.Loaded, "load methods");
      C.Seal (A, Init, Status); Check (Status = C.Available, "seal");
      C.Find_Child (A, N.Root, "GOOD", Recovery_Node, Status);
      Check (Status = C.Available and then Recovery_Node /= N.Root, "reentry target");
      for Repeat in 1 .. 2 loop
         declare Before : constant C.Collection_Statistics := C.Reclamation_Metrics (A); begin
            Invoke ("NEST", E.Returned);
            Check (C.Reclamation_Metrics (A).Completed > Before.Completed
              and then C.Reclamation_Metrics (A).Freed_Bytes > Before.Freed_Bytes,
              "actual collection between GET0 return and PUT0 release");
            Check (C.Reclamation_Metrics (A).Rejected = Before.Rejected, "nested root census");
         end;
         declare Before : constant C.Collection_Statistics := C.Reclamation_Metrics (A); begin
            Invoke ("GONE", E.Returned);
            Check (C.Reclamation_Metrics (A).Completed > Before.Completed
              and then C.Reclamation_Metrics (A).Freed_Bytes > Before.Freed_Bytes,
              "actual collection with expired name and invocation-only lock root");
            Check (C.Reclamation_Metrics (A).Rejected = Before.Rejected, "held-only root census");
         end;
         -- Exact original handcrafted IPKG is a negative package-expression case:
         -- ACPICA reports AE_SUPPORT before CHRN in both revisions/repeats.
         -- No successful Release-Index scratch-root reclamation claim follows.
         Invoke ("IPKG", E.Unsupported_Value);
         Invoke ("GOOD", E.Returned);
         Invoke ("GLOB", E.Unsupported_Value); Invoke ("GOOD", E.Returned);
         Invoke ("EVNT", E.Unsupported_Value); Invoke ("GOOD", E.Returned);
         Invoke ("TRNC", E.Truncated); Invoke ("GOOD", E.Returned);
         Invoke ("WORD", E.Returned); Invoke ("SCOP", E.Returned);
         Invoke ("RSET", E.Returned);
         Invoke ("FAIL", E.Division_By_Zero); Invoke ("GOOD", E.Returned);
         Invoke ("BUDG", E.Budget_Exceeded, 40); Invoke ("GOOD", E.Returned);
         Invoke ("NRET", E.No_Return); Invoke ("GOOD", E.Returned);
      end loop;
   end loop;
   Check (Reset_Probes = 4, "active reset probe count");
   Ada.Text_IO.Put_Line ("Mutex invocation lifetime checks" & Checks'Image);
end Mutex_Invocation_Lifetime_Tests;
