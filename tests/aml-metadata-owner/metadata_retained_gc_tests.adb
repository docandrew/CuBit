with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Metadata_Owner_Support; use Metadata_Owner_Support;
procedure Metadata_Retained_GC_Tests is
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 8);
   package C is new N.Owned.Collecting;
   use type AML_Decode.Integer_Value;
   use type N.Load_Status;
   use type AML_Execute.Named_Metadata_Type;
   use type C.Access_Status;
   use type C.Value_Kind;
   use type C.Value_Handle;
   use type C.Collection_Count;
   use type C.Audit_Value;
   A, B : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Args : constant C.Arguments := [others => (C.Immediate_Argument, 0)];
   Result : C.Result;
   Status : C.Access_Status;
   Description : C.Value_Description;
   Capture, Explicit, Derived, Stale, Temporary : C.Value_Handle;
   Checks : Natural := 0;
   Budget : constant Natural := 100_000;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Setup (Target : in out C.Arena; Width : Integer_Width) is
      Loaded : N.Load_Status;
      Report : N.Initialization_Report;
   begin
      C.Reset (Target, Status); Check (Status = C.Available, "reset collecting arena");
      C.Load (Target, Fixture, Width, Loaded, Status);
      Check (Status = C.Available and then Loaded = N.Loaded, "load fixture");
      C.Seal (Target, Report, Status);
      Check (Status = C.Available and then Report.Missing = 0 and then Report.Unsupported = 0,
        "complete fixture seal");
   end Setup;
   procedure Call (Name : String) is
      Node : N.Node_ID;
   begin
      C.Find_Child (A, N.Root, Name, Node, Status);
      Check (Status = C.Available and then Node /= N.Root, "method exists");
      C.Invoke (A, Input, Node, Args, 0, Budget, Result, Status);
      Check (Status = C.Available, "invoke available");
   end Call;
   procedure Device_Description (Handle : C.Value_Handle) is
   begin
      C.Describe (A, Handle, Description, Status);
      Check (Status = C.Available and then Description.Kind = C.Metadata_Description
        and then Description.Object_Type = Device_Metadata, "captured device description");
   end Device_Description;
   procedure Release (Handle : in out C.Value_Handle) is
   begin
      C.Release (A, Handle, Status);
      Check (Status = C.Available and then Handle = C.No_Value, "release pin");
   end Release;
begin
   for Width in Integer_Width loop
      Setup (A, Width); Setup (B, Width);
      Call ("CAPT"); Check (Result.Status = Metadata_Returned, "capture returns metadata");
      Capture := Result.Handle; Device_Description (Capture);
      Check (C.Retained_Count (A) = 1, "one sole retained snapshot root after invocation");
      declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
         Call ("PRES");
         Check (Result.Status = Returned and then C.Reclamation_Metrics (A).Completed > Before,
           "later independent invocation really collects");
      end;
      Device_Description (Capture);
      C.Dereference (A, Capture, Derived, Status);
      Check (Status = C.Wrong_Kind and then Derived = C.No_Value and then C.Retained_Count (A) = 1,
        "snapshot is not explicit reference");
      declare Before : constant C.Audit_Value := C.Audit (B); begin
         C.Describe (B, Capture, Description, Status);
         Check (Status = C.Invalid_Value, "foreign public captured handle rejected");
         Temporary := Capture; C.Release (B, Temporary, Status);
         Check (Status = C.Invalid_Value and then Temporary = Capture
           and then C.Retained_Count (B) = 0 and then C.Audit (B) = Before,
           "foreign release preserves recipient and original handle");
      end;
      Device_Description (Capture);
      Call ("REFR"); Check (Result.Status = Reference_Returned, "explicit ref positive control");
      Explicit := Result.Handle;
      Call ("REPL"); Check (Result.Status = Returned and then Result.Number = 1, "source replacement");
      C.Dereference (A, Explicit, Derived, Status);
      Check (Status = C.Available, "explicit reference follows source replacement");
      C.Describe (A, Derived, Description, Status);
      Check (Status = C.Available and then Description.Kind = C.Integer_Description
        and then Description.Number = 42, "explicit reference sees current integer");
      Release (Derived); Release (Explicit); Device_Description (Capture);
      Stale := Capture; Release (Capture);
      C.Describe (A, Stale, Description, Status);
      Check (Status = C.Invalid_Value, "released captured handle invalid");
      declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Freed_Objects; begin
         Call ("PRES");
         Check (Result.Status = Returned and then C.Reclamation_Metrics (A).Freed_Objects > Before
           and then C.Retained_Count (A) = 0, "general reclamation after release; no public pin leak");
      end;
      Setup (A, Width);
      declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
         Call ("LOCL");
         Check (Result.Status = Returned and then Result.Number = 6
           and then C.Reclamation_Metrics (A).Completed > Before,
           "local snapshot survives nested allocating call with real GC");
      end;
      Check (C.Retained_Count (A) = 0, "scalar local test leaves no public pin");
      Call ("CAPT"); Check (Result.Status = Metadata_Returned, "fresh captured handle before reset");
      Stale := Result.Handle;
      Setup (A, Width);
      C.Describe (A, Stale, Description, Status);
      Check (Status = C.Invalid_Value and then C.Retained_Count (A) = 0, "reset rejects old captured handle");
      Call ("CAPT"); Check (Result.Status = Metadata_Returned, "positive capture in reset lifetime");
      Capture := Result.Handle; Device_Description (Capture); Release (Capture);
      Check (C.Reclamation_Metrics (A).Rejected = 0, "no rejected collection hidden by result");
   end loop;
   Ada.Text_IO.Put_Line ("Metadata retained GC checks" & Checks'Image);
end Metadata_Retained_GC_Tests;
