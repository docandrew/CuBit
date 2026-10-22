with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Mutex_Lifetime_Tests is
   use type AML_Decode.Bytes;
   use type AML_Decode.Byte;
   package E renames AML_Execute;
   package N is new AML_Namespace (Capacity => 16, Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type E.Execution_Status;
   use type C.Access_Status;
   use type N.Load_Status;
   use type C.Value_Kind;
   use type C.Collection_Count;
   use type C.Audit_Value;
   use type C.Collection_Statistics;
   use type E.Named_Metadata_Type;
   A, Foreign : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Init : N.Initialization_Report;
   Result : C.Result;
   Held, Stale : C.Value_Handle;
   Description : C.Value_Description;
   Metrics : C.Collection_Statistics;
   Node : N.Node_ID;
   Checks : Natural := 0;
   Mutex_Opcode : constant Bytes := [16#5B#,16#01#];
   Code : constant Bytes := Mutex_Opcode & Enc ("M000") & [0]
     & Method ("MAKE", Mutex_Opcode & Enc ("TEMP") & [7,16#A4#] & Enc ("TEMP"))
     & Method ("FAIL", Mutex_Opcode & Enc ("TEMP") & [0,16#A4#] & Enc ("MISS"))
     & Method ("CLSH", Mutex_Opcode & [16#5C#] & Enc ("M000") & [0,16#A4#,0])
     & Method ("BADS", Mutex_Opcode & Enc ("TEMP") & [16#80#,16#A4#,0]);
   procedure Check (Good : Boolean; Text : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Text; end if; end Check;
   procedure Invoke (Name : String) is
   begin
      C.Find_Child (A, N.Root, Name, Node, Status);
      Check (Status = C.Available and then Node /= N.Root, "method");
      C.Invoke (A, Input, Node, [others => (C.Immediate_Argument, 0)], 0, 1000, Result, Status);
      Check (Status = C.Available, "invoke");
   end Invoke;
   procedure Release_New is
   begin C.Release (A, Result.Handle, Status); Check (Status = C.Available, "release new mutex"); end Release_New;
begin
   declare
      Legacy : N.State := N.Empty;
   begin
      N.Load_Names (Legacy, Mutex_Opcode & Enc ("RAW0") & [16#80#], Bits_64, Loaded);
      Check (Loaded = N.Loaded and then N.Count (Legacy) = 1, "static metadata admission unchanged");
      Check (N.Mutex_Data (Legacy, 1).Raw_Flags = 16#80# and then
        not N.Mutex_Data (Legacy, 1).Canonical, "static reserved flags preserved without normalization");
   end;
   C.Initialize (A, Status); Check (Status = C.Available, "initialize");
   C.Initialize (Foreign, Status); Check (Status = C.Available, "foreign arena");
   for Width in Integer_Width loop
      C.Reset (A, Status); Check (Status = C.Available, "reset");
      C.Load (A, Code, Width, Loaded, Status); Check (Status = C.Available and then Loaded = N.Loaded, "legacy metadata loader");
      C.Seal (A, Init, Status); Check (Status = C.Available, "seal");
      Invoke ("MAKE"); Check (Result.Status = E.Object_Returned, "runtime real mutex result"); Held := Result.Handle;
      C.Find_Child (A, N.Root, "TEMP", Node, Status); Check (Status = C.Available and then Node = N.Root, "method name retired");
      Metrics := C.Reclamation_Metrics (A);
      for Repeat in 1 .. 8 loop
         Invoke ("MAKE"); Check (Result.Status = E.Object_Returned, "repeat real mutex"); Release_New;
      end loop;
      Check (C.Reclamation_Metrics (A).Completed > Metrics.Completed and then
        C.Reclamation_Metrics (A).Freed_Objects > Metrics.Freed_Objects, "actual collection and reclamation");
      Check (C.Reclamation_Metrics (A).Rejected = Metrics.Rejected, "root census accepted");
      C.Describe (A, Held, Description, Status);
      Check (Status = C.Available and then Description.Kind = C.Metadata_Description and then
        Description.Object_Type = E.Mutex_Metadata, "retained mutex survived collection");
      C.Describe (Foreign, Held, Description, Status); Check (Status /= C.Available, "foreign handle rejected");
      declare Before : constant C.Audit_Value := C.Audit (A); Stats : constant C.Collection_Statistics := C.Reclamation_Metrics (A); begin
         Invoke ("CLSH"); Check (Result.Status = E.Duplicate_Name, "duplicate");
         Check (C.Audit (A) = Before and then C.Reclamation_Metrics (A) = Stats, "duplicate no allocation");
         Invoke ("BADS"); Check (Result.Status = E.Unsupported_Value, "reserved flags reject");
         Check (C.Audit (A) = Before and then C.Reclamation_Metrics (A) = Stats, "invalid flags no allocation");
      end;
      Invoke ("FAIL"); Check (Result.Status = E.Unknown_Name, "failure after declaration");
      C.Find_Child (A, N.Root, "TEMP", Node, Status); Check (Status = C.Available and then Node = N.Root, "failure retires name");
      Invoke ("MAKE"); Check (Result.Status = E.Object_Returned, "same arena recovery"); Release_New;
      Stale := Held; C.Release (A, Held, Status); Check (Status = C.Available, "release retained mutex");
      C.Describe (A, Stale, Description, Status); Check (Status /= C.Available, "released handle rejected");
      Check (C.Retained_Count (A) = 0, "no retained leaks");
      Metrics := C.Reclamation_Metrics (A);
      for Repeat in 1 .. 4 loop
         Invoke ("MAKE"); Check (Result.Status = E.Object_Returned, "postrelease collection"); Release_New;
      end loop;
      Check (C.Reclamation_Metrics (A).Completed > Metrics.Completed and then
        C.Reclamation_Metrics (A).Freed_Objects > Metrics.Freed_Objects, "released products reclaimed");
   end loop;
   Ada.Text_IO.Put_Line ("Mutex lifetime checks" & Checks'Image);
end Mutex_Lifetime_Tests;
