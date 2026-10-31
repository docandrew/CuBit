pragma Ada_2022;
with Ada.Text_IO;
with AML_Execution_Limits;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Bank_Access_Oracle;
procedure Bank_Access_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 96, Retained_Result_Capacity => 16);
   package A renames AML_Field_Access;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Operation_ID;
   use type A.Observation;
   type Slot is (Bank_Port, Data_Zero, Data_One, Extra_Port);
   type Geometry_Mode is (Matching, Wrong_Space, Wrong_Base, Wrong_Length,
     Outside_Region, Overflow_Offset, Crossing_End, Shifted_Offset);
   type Fault_Kind is (No_Fault, Unknown_Before_Effect, Unknown_After_Effect,
     Known_Failure, Malformed_Completion);
   type Values is array (Slot) of AML_Decode.Integer_Value;
   type Register_IDs is array (Slot) of A.Register_ID;
   IDs : constant Register_IDs := [16#8000_0000_0000_0011#,16#9000_0000_0000_0022#,
     16#A000_0000_0000_0033#,16#B000_0000_0000_0044#];
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Data : Values := [others => 0];
      Calls, Bank_Writes : Natural := 0;
      Trace : String (1 .. 128) := [others => ' '];
      Allow_Bank_Write, Allow_Data_Read, Allow_Selector_Write : Boolean := True;
      Revoke_Data_After_Bank : Boolean := False;
      Unknown_Bank, Unknown_Data, Fail_Data, Malformed_Data : Boolean := False;
      Receipt, Confirmed : A.Observation := A.No_Observation;
      Next_Operation : A.Operation_ID := 1;
      Geometry : Geometry_Mode := Matching;
      Change_Geometry_After_Bank : Boolean := False;
      Fault : Fault_Kind := No_Fault;
      Fault_Call, Data_Zero_Writes : Natural := 0;
      Allow_Last_Write : Boolean := True;
      Revoke_Last_After_First, Shift_Last_After_First : Boolean := False;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Locate (ID : A.Register_ID; S : out Slot; Found : out Boolean) is
   begin
      S := Bank_Port; Found := False;
      for Candidate in Slot loop
         if ID = IDs (Candidate) then S := Candidate; Found := True; return; end if;
      end loop;
   end Locate;
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
      S : Slot;
      Found : Boolean;
   begin
      Locate (Target.Register, S, Found);
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 4),
        Byte_Offset => AML_Decode.Integer_Value (Slot'Pos (S)), Width => ACPI_FADT.Transactions.Byte_Access);
      if S /= Bank_Port then
         case (if IO.Change_Geometry_After_Bank and then IO.Bank_Writes > 0
               then Shifted_Offset else IO.Geometry) is
            when Matching => null;
            when Wrong_Space => Geometry.Region.Space := 1;
            when Wrong_Base => Geometry.Region.Base := 16#2000#;
            when Wrong_Length => Geometry.Region.Length := 5;
            when Outside_Region => Geometry.Byte_Offset := 4;
            when Overflow_Offset => Geometry.Byte_Offset := AML_Decode.Integer_Value'Last;
            when Crossing_End =>
               Geometry.Byte_Offset := 3; Geometry.Width := ACPI_FADT.Transactions.Word_Access;
            when Shifted_Offset => Geometry.Byte_Offset := 2;
         end case;
      end if;
      if S = Data_One and then IO.Shift_Last_After_First and then IO.Data_Zero_Writes > 0 then
         Geometry.Byte_Offset := 3;
      end if;
      Accepted := Found and then IO.Owner /= AML_Identity.No_Identity
        and then Target.Provider = IO.Owner and then Target.Endpoint = 7 and then Target.Epoch = 1;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then
        (Target.Register /= IDs (Data_Zero) or else not For_Write or else IO.Allow_Selector_Write) and then
        (if Target.Register = IDs (Bank_Port) then not For_Write or else IO.Allow_Bank_Write
         else not (IO.Revoke_Data_After_Bank and then IO.Bank_Writes > 0)
           and then (For_Write or else IO.Allow_Data_Read))
        and then (Target.Register /= IDs (Data_One) or else
          ((not For_Write or else IO.Allow_Last_Write)
           and then not (IO.Revoke_Last_After_First and then IO.Data_Zero_Writes > 0)));
   end Authorize;
   procedure Retain_Unknown (IO : in out Context; Item : A.Request; Reply : out A.Response) is
   begin
      IO.Receipt := (True, IO.Owner, Item, IO.Next_Operation);
      Reply := (A.Indeterminate, IO.Owner, 0, IO.Next_Operation);
      IO.Next_Operation := IO.Next_Operation + 1;
   end Retain_Unknown;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      S : Slot;
      Found, Allowed : Boolean;
      Geometry : AML_Field_Layout.Register_Geometry;
      Fault : Fault_Kind := No_Fault;
   begin
      Reply := (others => <>);
      Locate (Item.Destination.Register, S, Found);
      Describe (IO, Item.Destination, Geometry, Allowed);
      if not Found or else not Allowed or else (Item.For_Write and then Item.Value > 255) then return; end if;
      IO.Calls := IO.Calls + 1;
      IO.Trace (IO.Calls) := (case S is
        when Bank_Port => (if Item.For_Write then 'B' else 'b'),
        when Data_Zero => (if Item.For_Write then 'w' else 'r'),
        when Data_One => (if Item.For_Write then 'W' else 'R'),
        when Extra_Port => (if Item.For_Write then 'x' else 'X'));
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      if not Allowed then return; end if;
      if IO.Calls = IO.Fault_Call then Fault := IO.Fault; IO.Fault := No_Fault; end if;
      if Fault = Known_Failure then
         Reply := (A.Failed_Completed, IO.Owner, 0, A.No_Operation); return;
      elsif Fault = Unknown_Before_Effect then
         Retain_Unknown (IO, Item, Reply); return;
      end if;
      if (S = Bank_Port and then IO.Unknown_Bank) or else
        (S /= Bank_Port and then IO.Unknown_Data) then
         IO.Unknown_Bank := False; IO.Unknown_Data := False;
         Retain_Unknown (IO, Item, Reply);
         return;
      end if;
      if S /= Bank_Port and then IO.Fail_Data then
         IO.Fail_Data := False;
         Reply := (A.Failed_Completed, IO.Owner, 0, A.No_Operation);
         return;
      end if;
      if Item.For_Write then
         IO.Data (S) := Item.Value;
         if S = Bank_Port then IO.Bank_Writes := IO.Bank_Writes + 1; end if;
         if S = Data_Zero then IO.Data_Zero_Writes := IO.Data_Zero_Writes + 1; end if;
      end if;
      if Fault = Unknown_After_Effect then Retain_Unknown (IO, Item, Reply); return; end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Data (S)), A.No_Operation);
      if (S /= Bank_Port and then IO.Malformed_Data) or else Fault = Malformed_Completion then
         IO.Malformed_Data := False;
         Reply.Value := (if Item.For_Write then 1 else 256);
      end if;
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then Expected = IO.Confirmed; end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Maximum_Bindings => 4, Authorize_Target => Authorize);
   Service : aliased C.State (1, Bank_Access_Oracle.Raw_Table'Length, Bank_Access_Oracle.Raw_Table'Length, 1);
   IO : Context;
   Items : F.Binding_Set;
   Stage : C.Stage_Status;
   Freeze : C.Freeze_Status;
   Status : C.Values.Access_Status;
   Modules : C.Execution_Report;
   Init : C.Namespace.Initialization_Report;
   Issued : Boolean;
   Region : C.Node_Descriptor;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   procedure Expect (Name : AML_Names.Segment; Expected : AML_Decode.Integer_Value;
     Want : AML_Execute.Execution_Status := AML_Execute.Returned) is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Result : C.Values.Result;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " lookup");
      C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available, Name & " descriptor");
      F.Evaluate_Named (Service, IO, Items, Item, 10_000, Result, Status);
      Check (Status = C.Values.Available and then Result.Status = Want, Name & " status " & Result.Status'Image);
      if Want = AML_Execute.Returned then Check (Result.Number = Expected, Name & " expected scalar"); end if;
      Check (C.Retained_Results (Service) = 0, Name & " roots");
   end Expect;
   Failures : Natural := 0;
   procedure Probe (Name : AML_Names.Segment; Expected : AML_Decode.Integer_Value) is
      Node : C.Namespace.Node_ID;
      Descriptor : C.Node_Descriptor;
      Result : C.Values.Result;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "graph lookup");
      C.Describe_Node (Service, Node, Descriptor, Status); Check (Status = C.Values.Available, "graph descriptor");
      IO.Calls := 0;
      F.Evaluate_Named (Service, IO, Items, Descriptor, 100_000, Result, Status);
      Ada.Text_IO.Put_Line (Name & " expected RETURNED actual " & Result.Status'Image &
        " requests" & Natural'Image (IO.Calls));
      if Status /= C.Values.Available or else Result.Status /= AML_Execute.Returned
        or else Result.Number /= Expected then Failures := Failures + 1; end if;
      Check (C.Retained_Results (Service) = 0, "graph result roots");
   end Probe;
begin
   AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "provider identity");
   C.Stage_Table (Service, 1, C.DSDT, Bank_Access_Oracle.Raw_Table, Stage); Check (Stage = C.Staged, "stage");
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (Service, 100_000, Modules, Status);
   Check (Status = C.Values.Available and then Modules.Modules.Outcome = C.Values.Modules_Completed, "module import");
   C.Initialize_Members (Service, Init, Status); Check (Status = C.Values.Available, "members");
   F.Clear (Items);
   for S in Slot loop
      declare
         Node : C.Namespace.Node_ID;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
         Name : constant AML_Names.Segment := (case S is when Bank_Port => "SEL0",
           when Data_Zero => "IDX0", when Data_One => "DAT0", when Extra_Port => "MEM0");
      begin
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " leaf");
         C.Read_Field_Descriptor (Service, Node, Description, Key, Status); Check (Status = C.Values.Available, Name & " key");
         F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
         Check (Status = C.Values.Available, Name & " binding");
      end;
   end loop;
   declare N : C.Namespace.Node_ID; begin
      C.Find_Child (Service, C.Namespace.Root, "REG0", N, Status);
      C.Describe_Node (Service, N, Region, Status); Check (Status = C.Values.Available, "region descriptor");
   end;
   for S in Slot range Data_Zero .. Extra_Port loop
      F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Items, Status);
      Check (Status = C.Values.Available, "explicit leaf catalog");
   end loop;
   Expect ("INIT", 0);
   Probe ("RIND", 16#A5#);
   Check (IO.Calls = 4 and then IO.Trace (1 .. 4) = "BwBR", "banked index and banked data ordering");
   Expect ("SEL0", 3); Expect ("IDX0", 4); Expect ("DAT0", 16#A5#);
   Probe ("RBAN", 16#5A#);
   Check (IO.Calls = 3 and then IO.Trace (1 .. 3) = "BwX", "banked bank-selector ordering");
   Expect ("SEL0", 3); Expect ("IDX0", 5);
   Probe ("RMIX", 16#5A#);
   Check (IO.Calls = 5 and then IO.Trace (1 .. 5) = "BwBWX", "mixed graph ordering");
   Expect ("SEL0", 3); Expect ("IDX0", 4); Expect ("DAT0", 7); Expect ("MEM0", 16#5A#);
   for Graph in 1 .. 3 loop
      declare
         Name : constant AML_Names.Segment := (case Graph is when 1 => "RIND", when 2 => "RBAN", when others => "RMIX");
         Trace : constant String := (case Graph is when 1 => "BwBR", when 2 => "BwX", when others => "BwBWX");
         Expected : constant AML_Decode.Integer_Value := (if Graph = 1 then 16#A5# else 16#5A#);
      begin
         IO.Calls := 0; IO.Allow_Selector_Write := False;
         Expect (Name, 0, AML_Execute.Unsupported_Value);
         Check (IO.Calls = 0, "deep write denial checked before any selector effect");
         IO.Allow_Selector_Write := True; IO.Allow_Data_Read := False;
         Expect (Name, 0, AML_Execute.Unsupported_Value);
         Check (IO.Calls = 0, "deep read denial checked before any selector effect");
         IO.Allow_Data_Read := True; IO.Bank_Writes := 0; IO.Revoke_Data_After_Bank := True;
         Expect (Name, 0, AML_Execute.Unsupported_Value);
         Check (IO.Calls = 1 and then IO.Trace (1) = 'B', "deep recheck after first known selector effect");
         IO.Revoke_Data_After_Bank := False;
         for Fault in Fault_Kind range Unknown_Before_Effect .. Unknown_After_Effect loop
            for At_Call in Trace'Range loop
               IO.Calls := 0; IO.Bank_Writes := 0;
               IO.Data (Data_One) := 16#A5#; IO.Data (Extra_Port) := 16#5A#;
               IO.Fault := Fault; IO.Fault_Call := At_Call; IO.Confirmed := A.No_Observation;
               Expect (Name, 0, AML_Execute.Field_Indeterminate);
               Check (IO.Calls = At_Call and then IO.Trace (1 .. At_Call) = Trace (1 .. At_Call),
                 "unknown graph preserves exact request prefix");
               Check (F.Uncertain (Service) = IO.Receipt, "deep exact unknown receipt");
               declare
                  Destination : constant Slot := (case Trace (At_Call) is
                    when 'B' => Bank_Port, when 'w' => Data_Zero, when 'R' | 'W' => Data_One,
                    when others => Extra_Port);
               begin
                  Check (IO.Receipt.Item.Destination.Register = IDs (Destination), "actual deep leaf destination");
                  Check (IO.Receipt.Item.For_Write = (Trace (At_Call) in 'B' | 'w' | 'W'), "actual deep leaf direction");
                  Check (IO.Receipt.Item.Value = (case Trace (At_Call) is
                    when 'B' => 3, when 'w' => (if Graph = 2 then 5 else 4), when 'W' => 7, when others => 0),
                    "actual deep leaf request value");
               end;
               Check (IO.Data (Data_One) = (if Graph = 3 and then
                 (At_Call > 4 or else (Fault = Unknown_After_Effect and then At_Call = 4))
                 then 7 else 16#A5#), "mixed write prefix retained " & Graph'Image & Fault'Image & At_Call'Image);
               Expect (Name, 0, AML_Execute.Field_Indeterminate);
               Check (IO.Calls = At_Call, "deep unknown operation never replayed");
               declare Saved : constant A.Observation := IO.Receipt; begin
                  IO.Receipt := A.No_Observation; IO.Confirmed := Saved;
                  F.Reconcile (Service, IO, Saved, Status); Check (Status = C.Values.Available, "deep exact reconcile");
               end;
               IO.Calls := 0;
               Expect (Name, Expected);
               Check (IO.Calls = Trace'Length and then IO.Trace (1 .. IO.Calls) = Trace,
                 "explicit invocation after deep reconcile");
            end loop;
         end loop;
      end;
   end loop;
   declare
      type Work_Case is (Exhausted_Work, Sufficient_Work);
   begin
      for Trial in Work_Case loop
         declare
            package Budgeted_IO is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
              Maximum_Bindings => 4, Authorize_Target => Authorize,
              Maximum_Work => (if Trial = Exhausted_Work then 1 else 64));
            Bindings : Budgeted_IO.Binding_Set;
            procedure Budget_Expect (Name : AML_Names.Segment; Expected : AML_Decode.Integer_Value;
              Want : AML_Execute.Execution_Status) is
               Node : C.Namespace.Node_ID;
               Descriptor : C.Node_Descriptor;
               Result : C.Values.Result;
            begin
               C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
               Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "work policy lookup");
               C.Describe_Node (Service, Node, Descriptor, Status); Check (Status = C.Values.Available, "work descriptor");
               Budgeted_IO.Evaluate_Named (Service, IO, Bindings, Descriptor, 100_000, Result, Status);
               Check (Status = C.Values.Available and then Result.Status = Want,
                 Name & " work policy " & Trial'Image & " actual " & Result.Status'Image);
               if Want = AML_Execute.Returned then Check (Result.Number = Expected, "work policy value"); end if;
               Check (C.Retained_Results (Service) = 0, "work policy roots");
            end Budget_Expect;
         begin
            Budgeted_IO.Clear (Bindings);
            for S in Slot loop
               declare
                  Node : C.Namespace.Node_ID;
                  Description : C.Values.Field_Description;
                  Key : C.Values.Field_Key;
                  Name : constant AML_Names.Segment := (case S is when Bank_Port => "SEL0",
                    when Data_Zero => "IDX0", when Data_One => "DAT0", when Extra_Port => "MEM0");
               begin
                  C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
                  C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
                  Check (Status = C.Values.Available, "work policy leaf descriptor");
                  Budgeted_IO.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Bindings, Status);
                  Check (Status = C.Values.Available, "work policy named leaf");
               end;
            end loop;
            for S in Slot range Data_Zero .. Extra_Port loop
               Budgeted_IO.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Bindings, Status);
               Check (Status = C.Values.Available, "work policy catalog");
            end loop;
            for Graph in 1 .. 3 loop
               declare
                  Name : constant AML_Names.Segment := (case Graph is when 1 => "RIND", when 2 => "RBAN", when others => "RMIX");
                  Expected : constant AML_Decode.Integer_Value := (if Graph = 1 then 16#A5# else 16#5A#);
                  Requests : constant Natural := (case Graph is when 1 => 4, when 2 => 3, when others => 5);
               begin
                  IO.Calls := 0; IO.Bank_Writes := 0; IO.Fault := No_Fault;
                  IO.Data := [Bank_Port => 0, Data_Zero => 0, Data_One => 16#A5#, Extra_Port => 16#5A#];
                  Budget_Expect (Name, Expected, (if Trial = Exhausted_Work then AML_Execute.Value_Limit else AML_Execute.Returned));
                  Check (not Budgeted_IO.Uncertain (Service).Present and then not Pending (IO), "work limit creates no uncertainty");
                  if Trial = Exhausted_Work then
                     Check (IO.Calls = 0 and then IO.Data =
                       Values'[Bank_Port => 0, Data_Zero => 0, Data_One => 16#A5#, Extra_Port => 16#5A#],
                       "work exhaustion precedes all provider effects");
                  else
                     Check (IO.Calls = Requests, "sufficient work preserves request count");
                  end if;
               end;
            end loop;
            IO.Calls := 0; IO.Data (Data_One) := 16#A5#;
            for Invocation in 1 .. 8 loop
               Budget_Expect ("RIND", 16#A5#,
                 (if Trial = Exhausted_Work then AML_Execute.Value_Limit else AML_Execute.Returned));
               Check (IO.Calls = (if Trial = Exhausted_Work then 0 else Invocation * 4),
                 "each explicit invocation starts a fresh work budget");
            end loop;
         end;
      end loop;
   end;
   declare
      type Coverage is array (Positive range 1 .. 3) of Boolean;
      Saw_Before, Saw_Prefix, Saw_Complete : Coverage := [others => False];
   begin
      for Limit in AML_Execution_Limits.Field_Work_Limit range 1 .. 64 loop
         declare
            package Budgeted_IO is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
              Maximum_Bindings => 4, Authorize_Target => Authorize, Maximum_Work => Limit);
            Bindings : Budgeted_IO.Binding_Set;
         begin
            Budgeted_IO.Clear (Bindings);
            for S in Slot loop
               declare
                  Node : C.Namespace.Node_ID;
                  Description : C.Values.Field_Description;
                  Key : C.Values.Field_Key;
                  Name : constant AML_Names.Segment := (case S is when Bank_Port => "SEL0",
                    when Data_Zero => "IDX0", when Data_One => "DAT0", when Extra_Port => "MEM0");
               begin
                  C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
                  C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
                  Check (Status = C.Values.Available, "prefix work leaf");
                  Budgeted_IO.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Bindings, Status);
                  Check (Status = C.Values.Available, "prefix work binding");
               end;
            end loop;
            for S in Slot range Data_Zero .. Extra_Port loop
               Budgeted_IO.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Bindings, Status);
               Check (Status = C.Values.Available, "prefix work catalog");
            end loop;
            for Graph in 1 .. 3 loop
               declare
                  Name : constant AML_Names.Segment := (case Graph is when 1 => "RIND", when 2 => "RBAN", when others => "RMIX");
                  Trace : constant String := (case Graph is when 1 => "BwBR", when 2 => "BwX", when others => "BwBWX");
                  Expected : constant AML_Decode.Integer_Value := (if Graph = 1 then 16#A5# else 16#5A#);
                  Node : C.Namespace.Node_ID;
                  Descriptor : C.Node_Descriptor;
                  Result : C.Values.Result;
               begin
                  C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
                  C.Describe_Node (Service, Node, Descriptor, Status); Check (Status = C.Values.Available, "prefix work method");
                  IO.Calls := 0; IO.Bank_Writes := 0; IO.Data_Zero_Writes := 0; IO.Fault := No_Fault;
                  IO.Data := [Bank_Port => 0, Data_Zero => 0, Data_One => 16#A5#, Extra_Port => 16#5A#];
                  Budgeted_IO.Evaluate_Named (Service, IO, Bindings, Descriptor, 100_000, Result, Status);
                  Check (Status = C.Values.Available and then Result.Status in AML_Execute.Returned | AML_Execute.Value_Limit,
                    "work prefix status " & Limit'Image & Name & Result.Status'Image);
                  Check (IO.Calls <= Trace'Length and then IO.Trace (1 .. IO.Calls) = Trace (1 .. IO.Calls),
                    "work limit retains exact known request prefix");
                  Check (IO.Data (Bank_Port) = (if IO.Calls = 0 then 0 else 3)
                    and then IO.Data (Data_Zero) = (if IO.Calls < 2 then 0 else (if Graph = 2 then 5 else 4))
                    and then IO.Data (Data_One) = (if Graph = 3 and then IO.Calls >= 4 then 7 else 16#A5#)
                    and then IO.Data (Extra_Port) = 16#5A#, "work limit preserves all completed effects");
                  Check (not Budgeted_IO.Uncertain (Service).Present and then not Pending (IO), "known work prefix has no unknown receipt");
                  Check (C.Retained_Results (Service) = 0, "work prefix releases roots");
                  if Result.Status = AML_Execute.Returned then
                     Check (Result.Number = Expected and then IO.Calls = Trace'Length, "completed work prefix has correct value");
                     Saw_Complete (Graph) := True;
                  elsif IO.Calls = 0 then Saw_Before (Graph) := True;
                  elsif IO.Calls < Trace'Length then Saw_Prefix (Graph) := True;
                  end if;
               end;
            end loop;
         end;
      end loop;
      for Graph in 1 .. 3 loop
         Check (Saw_Before (Graph) and then Saw_Prefix (Graph) and then Saw_Complete (Graph),
           "work sweep covers no-effect exhaustion, completed prefixes and success");
      end loop;
   end;
   if Failures /= 0 then
      raise Program_Error with "unimplemented field graphs" & Natural'Image (Failures);
   end if;
   Ada.Text_IO.Put_Line ("Bank access checks" & Natural'Image (Checks));
end Bank_Access_Tests;
