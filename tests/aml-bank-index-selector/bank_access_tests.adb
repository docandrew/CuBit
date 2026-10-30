pragma Ada_2022;
with Ada.Text_IO;
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
     Maximum_Bindings => 3, Authorize_Target => Authorize);
   Service : aliased C.State (1, Bank_Access_Oracle.Raw_Table'Length, Bank_Access_Oracle.Raw_Table'Length);
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
begin
   AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "provider identity");
   C.Stage_Table (Service, 1, C.DSDT, Bank_Access_Oracle.Raw_Table, Stage); Check (Stage = C.Staged, "stage");
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (Service, 100_000, Modules, Status);
   Check (Status = C.Values.Available and then Modules.Modules.Outcome = C.Values.Modules_Completed, "module import");
   C.Initialize_Members (Service, Init, Status); Check (Status = C.Values.Available, "members");
   F.Clear (Items);
   for S in Slot range Bank_Port .. Data_One loop
      declare
         Node : C.Namespace.Node_ID;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
         Name : constant AML_Names.Segment := (case S is when Bank_Port => "IDX0",
           when Data_Zero => "DAT0", when Data_One => "MDAT");
      begin
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " leaf");
         C.Read_Field_Descriptor (Service, Node, Description, Key, Status); Check (Status = C.Values.Available, Name & " key");
         F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
         Check (Status = C.Values.Available, Name & " binding");
      end;
   end loop;
   Expect ("READ", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "leaf authority alone grants no bank data access");
   declare N : C.Namespace.Node_ID; begin
      C.Find_Child (Service, C.Namespace.Root, "REG0", N, Status);
      C.Describe_Node (Service, N, Region, Status); Check (Status = C.Values.Available, "region descriptor");
   end;
   F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_One)), True, True, Items, Status);
   Check (Status = C.Values.Available, "explicit data catalog");
   -- Same thirteen observations, in order, as the normal ACPICA fixture.
   Expect ("IN00", 0);
   IO.Calls := 0;
   Expect ("READ", 16#A5#);
   Check (IO.Calls = 3 and then IO.Trace (1 .. 3) = "BwR", "index then bank then data");
   Expect ("IDX0", 4); Expect ("DAT0", 3);
   IO.Calls := 0;
   Expect ("WRIT", 0);
   Check (IO.Calls = 6 and then IO.Trace (1 .. 6) = "BwRBwW", "partial write selects bank before each data request");
   Expect ("MDAT", 16#A6#); Expect ("IDX0", 4); Expect ("DAT0", 3);
   IO.Calls := 0;
   Expect ("FULL", 0);
   Check (IO.Calls = 3 and then IO.Trace (1 .. 3) = "BwW", "full write does not read data");
   Expect ("MDAT", 16#5A#); Expect ("READ", 16#5A#); Expect ("IDX0", 4); Expect ("DAT0", 3);
   IO.Calls := 0; IO.Allow_Selector_Write := False;
   Expect ("READ", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "nested selector permissions checked before any effect");
   IO.Allow_Selector_Write := True; IO.Allow_Last_Write := False;
   Expect ("WRIT", 0, AML_Execute.Unsupported);
   Check (IO.Calls = 0, "parent write permission checked before selector effects");
   IO.Allow_Last_Write := True; IO.Allow_Data_Read := False;
   Expect ("READ", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "parent read permission checked before selector effects");
   IO.Allow_Data_Read := True; IO.Bank_Writes := 0; IO.Revoke_Data_After_Bank := True;
   Expect ("READ", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 1 and then IO.Data (Bank_Port) = 4, "selector revocation retains only completed index write");
   IO.Revoke_Data_After_Bank := False;
   for Fault in Fault_Kind range Unknown_Before_Effect .. Unknown_After_Effect loop
      for At_Call in 1 .. 6 loop
         IO.Calls := 0; IO.Bank_Writes := 0; IO.Data (Data_One) := 16#A5#;
         IO.Fault := Fault; IO.Fault_Call := At_Call; IO.Confirmed := A.No_Observation;
         Expect ("WRIT", 0, AML_Execute.Field_Indeterminate);
         Check (IO.Calls = At_Call and then F.Uncertain (Service) = IO.Receipt, "exact nested unknown receipt");
         Check (IO.Receipt.Item.Destination.Register = IDs
           ((case At_Call is when 1 | 4 => Bank_Port, when 2 | 5 => Data_Zero, when others => Data_One)),
           "unknown attributed to actual selector or data leaf");
         Check (IO.Data (Data_One) = (if Fault = Unknown_After_Effect and then At_Call = 6 then 16#A6# else 16#A5#),
           "only completed partial write changes memory");
         Expect ("READ", 0, AML_Execute.Field_Indeterminate);
         Check (IO.Calls = At_Call, "nested uncertainty never replays a selector");
         declare Saved : constant A.Observation := IO.Receipt; begin
            IO.Receipt := A.No_Observation; IO.Confirmed := Saved;
            F.Reconcile (Service, IO, Saved, Status); Check (Status = C.Values.Available, "exact nested reconcile");
         end;
         IO.Calls := 0;
         Expect ("READ", IO.Data (Data_One));
         Check (IO.Calls = 3, "explicit new nested read after reconciliation");
      end loop;
   end loop;
   declare
      package Limited_IO is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
        Maximum_Bindings => 3, Authorize_Target => Authorize, Maximum_Requests => 5);
      Limited_Service : aliased C.State
        (1, Bank_Access_Oracle.Raw_Table'Length, Bank_Access_Oracle.Raw_Table'Length);
      Bus : Context;
      Bindings : Limited_IO.Binding_Set;
      Region_Node : C.Node_Descriptor;
      procedure Limited_Expect (Name : AML_Names.Segment; Want : AML_Execute.Execution_Status) is
         Node : C.Namespace.Node_ID;
         Descriptor : C.Node_Descriptor;
         Result : C.Values.Result;
      begin
         C.Find_Child (Limited_Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "limited lookup");
         C.Describe_Node (Limited_Service, Node, Descriptor, Status);
         Check (Status = C.Values.Available, "limited descriptor");
         Limited_IO.Evaluate_Named (Limited_Service, Bus, Bindings, Descriptor, 100_000, Result, Status);
         Check (Status = C.Values.Available and then Result.Status = Want,
           Name & " limited status " & Result.Status'Image);
         Check (C.Retained_Results (Limited_Service) = 0, "limited result roots");
      end Limited_Expect;
   begin
      AML_Identity.Issuer.Issue (Bus.Owner, Issued); Check (Issued, "limited provider identity");
      C.Stage_Table (Limited_Service, 1, C.DSDT, Bank_Access_Oracle.Raw_Table, Stage);
      Check (Stage = C.Staged, "limited stage");
      C.Freeze_Catalog (Limited_Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "limited freeze");
      C.Execute_Frozen (Limited_Service, 100_000, Modules, Status);
      Check (Status = C.Values.Available and then Modules.Modules.Outcome = C.Values.Modules_Completed,
        "limited module import");
      C.Initialize_Members (Limited_Service, Init, Status); Check (Status = C.Values.Available, "limited members");
      Limited_IO.Clear (Bindings);
      for S in Slot range Bank_Port .. Data_One loop
         declare
            Node : C.Namespace.Node_ID;
            Description : C.Values.Field_Description;
            Key : C.Values.Field_Key;
            Name : constant AML_Names.Segment := (case S is when Bank_Port => "IDX0",
              when Data_Zero => "DAT0", when Data_One => "MDAT");
         begin
            C.Find_Child (Limited_Service, C.Namespace.Root, Name, Node, Status);
            Check (Status = C.Values.Available, "limited leaf lookup");
            C.Read_Field_Descriptor (Limited_Service, Node, Description, Key, Status);
            Check (Status = C.Values.Available, "limited key");
            Limited_IO.Add_Binding (Limited_Service, Bus, Key, (Bus.Owner, 7, 1, IDs (S)), Bindings, Status);
            Check (Status = C.Values.Available, "limited leaf binding");
         end;
      end loop;
      declare Node : C.Namespace.Node_ID; begin
         C.Find_Child (Limited_Service, C.Namespace.Root, "REG0", Node, Status);
         C.Describe_Node (Limited_Service, Node, Region_Node, Status);
         Check (Status = C.Values.Available, "limited region");
      end;
      for S in Slot range Data_One .. Data_One loop
         Limited_IO.Add_Region_Register (Limited_Service, Bus, Region_Node,
           (Bus.Owner, 7, 1, IDs (S)), True, True, Bindings, Status);
         Check (Status = C.Values.Available, "limited explicit catalog");
      end loop;
      Bus.Data (Data_One) := 16#A5#;
      Limited_Expect ("WRIT", AML_Execute.Value_Limit);
      Check (Bus.Calls = 5 and then Bus.Trace (1 .. 5) = "BwRBw"
        and then Bus.Data (Data_One) = 16#A5#, "shared limit stops before partial data write");
      Check (not Limited_IO.Uncertain (Limited_Service).Present and then not Pending (Bus),
        "known exhausted prefix creates no uncertainty");
      Limited_Expect ("READ", AML_Execute.Returned);
      Check (Bus.Calls = 8 and then Bus.Trace (6 .. 8) = "BwR", "fresh field operation gets fresh budget");
      Limited_Expect ("FULL", AML_Execute.Returned);
      Check (Bus.Calls = 11 and then Bus.Data (Data_One) = 16#5A#, "fresh write finishes in budget");
      Bus.Fault := Unknown_After_Effect; Bus.Fault_Call := 13;
      Limited_Expect ("READ", AML_Execute.Field_Indeterminate);
      Check (Bus.Calls = 13 and then Limited_IO.Uncertain (Limited_Service) = Bus.Receipt,
        "provider uncertainty takes precedence over unused request budget");
      Limited_Expect ("READ", AML_Execute.Field_Indeterminate);
      Check (Bus.Calls = 13, "uncertain request is never replayed on new invocation");
      declare Saved : constant A.Observation := Bus.Receipt; begin
         Bus.Receipt := A.No_Observation; Bus.Confirmed := Saved;
         Limited_IO.Reconcile (Limited_Service, Bus, Saved, Status);
         Check (Status = C.Values.Available, "limited exact receipt reconciliation");
      end;
      Bus.Fault := No_Fault;
      Limited_Expect ("READ", AML_Execute.Returned);
      Check (Bus.Calls = 16, "explicit invocation after reconciliation gets fresh budget");
   end;
   Ada.Text_IO.Put_Line ("Bank access checks" & Natural'Image (Checks));
end Bank_Access_Tests;
