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
      Allow_Bank_Write, Allow_Data_Read, Allow_Selector_Write, Allow_Memory_Write : Boolean := True;
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
        (Target.Register /= IDs (Extra_Port) or else not For_Write or else IO.Allow_Memory_Write) and then
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
   type Write_Case is (Indexed_Full, Banked_Full, Mixed_Full, Indexed_Partial, Banked_Partial, Mixed_Partial,
     Indexed_Ones, Banked_Ones, Mixed_Ones, Indexed_Zeros, Banked_Zeros, Mixed_Zeros);
   function Name_Of (Op : Write_Case) return AML_Names.Segment is
     (case Op is when Indexed_Full => "WIND", when Banked_Full => "WBAN", when Mixed_Full => "WMIX",
       when Indexed_Partial => "WPIN", when Banked_Partial => "WPBA", when Mixed_Partial => "WPMI",
       when Indexed_Ones => "WION", when Banked_Ones => "WBON", when Mixed_Ones => "WMON",
       when Indexed_Zeros => "WIZE", when Banked_Zeros => "WBZE", when Mixed_Zeros => "WMZE");
   function Trace_Of (Op : Write_Case) return String is
     (case Op is when Indexed_Full | Indexed_Ones | Indexed_Zeros => "BwBW",
       when Banked_Full | Banked_Ones | Banked_Zeros => "Bwx",
       when Mixed_Full | Mixed_Ones | Mixed_Zeros => "BwBWx",
       when Indexed_Partial => "BwBRBwBW", when Banked_Partial => "BwXBwx", when Mixed_Partial => "BwBWXBwBWx");
   function Request_Value (Op : Write_Case; Code : Character) return AML_Decode.Integer_Value is
     (case Code is when 'B' => 3, when 'w' => (if Op in Banked_Full | Banked_Partial | Banked_Ones | Banked_Zeros then 5 else 4),
       when 'W' => (if Op = Indexed_Full then 16#C3# elsif Op = Indexed_Partial then 16#A6# elsif Op = Indexed_Ones then 16#F2# elsif Op = Indexed_Zeros then 2 else 7),
       when 'x' => (if Op = Banked_Full then 16#A6# elsif Op = Mixed_Full then 16#C3# elsif Op in Banked_Ones | Mixed_Ones then 16#F2#
         elsif Op in Banked_Zeros | Mixed_Zeros then 2 else 16#56#),
       when others => 0);
   function Destination (Code : Character) return Slot is
     (case Code is when 'B' => Bank_Port, when 'w' => Data_Zero, when 'R' | 'W' => Data_One, when others => Extra_Port);
   function Values_After (Op : Write_Case; Completed : Natural) return Values is
      Result : Values := [Bank_Port => 0, Data_Zero => 0, Data_One => 16#A5#, Extra_Port => 16#5A#];
      Trace : constant String := Trace_Of (Op);
   begin
      for P in 1 .. Completed loop
         if Trace (P) in 'B' | 'w' | 'W' | 'x' then
            Result (Destination (Trace (P))) := Request_Value (Op, Trace (P));
         end if;
      end loop;
      return Result;
   end Values_After;
   procedure Reset_Model is
   begin
      Check (not F.Uncertain (Service).Present and then not Pending (IO), "clean completion before reset");
      IO.Calls := 0; IO.Bank_Writes := 0; IO.Data_Zero_Writes := 0;
      IO.Data := [Bank_Port => 0, Data_Zero => 0, Data_One => 16#A5#, Extra_Port => 16#5A#];
      IO.Allow_Bank_Write := True; IO.Allow_Data_Read := True; IO.Allow_Selector_Write := True;
      IO.Allow_Last_Write := True; IO.Allow_Memory_Write := True; IO.Revoke_Data_After_Bank := False;
      IO.Fault := No_Fault; IO.Fault_Call := 0; IO.Confirmed := A.No_Observation;
   end Reset_Model;
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
      Check (Status = C.Values.Available, "explicit named catalog");
   end loop;
   for Op in Write_Case loop
      declare
         Name : constant AML_Names.Segment := Name_Of (Op);
         Trace : constant String := Trace_Of (Op);
         Final : constant Values := Values_After (Op, Trace'Length);
      begin
         Reset_Model; Expect ("INIT", 0); IO.Calls := 0;
         Expect (Name, 0);
         Check (IO.Calls = Trace'Length and then IO.Trace (1 .. IO.Calls) = Trace, "write/Preserve request ordering");
         Check (IO.Data = Final, "write final register snapshot");
         -- Same six ordered observations per case as the ACPICA oracle.
         Expect ("SEL0", Final (Bank_Port)); Expect ("IDX0", Final (Data_Zero));
         Expect ("DAT0", Final (Data_One)); Expect ("MEM0", Final (Extra_Port));
         Reset_Model; IO.Allow_Data_Read := False;
         if Op not in Indexed_Partial | Banked_Partial | Mixed_Partial then
            Expect (Name, 0);
            Check (IO.Calls = Trace'Length and then IO.Trace (1 .. IO.Calls) = Trace and then IO.Data = Final,
              "non-Preserve write requires no data read authority");
         else
            Expect (Name, 0, AML_Execute.Unsupported);
            Check (IO.Calls = 0, "partial Preserve requires read authority before effects");
         end if;
         Reset_Model;
         if Op in Indexed_Full | Indexed_Partial | Indexed_Ones | Indexed_Zeros then IO.Allow_Last_Write := False;
         else IO.Allow_Memory_Write := False; end if;
         Expect (Name, 0, AML_Execute.Unsupported);
         Check (IO.Calls = 0, "data write denial precedes selector effects");
         Reset_Model; IO.Allow_Selector_Write := False;
         Expect (Name, 0, AML_Execute.Unsupported);
         Check (IO.Calls = 0, "deep selector write denial precedes effects");
         Reset_Model; IO.Revoke_Data_After_Bank := True;
         Expect (Name, 0, AML_Execute.Unsupported);
         Check (IO.Calls = 1 and then IO.Trace (1) = 'B' and then IO.Data = Values_After (Op, 1),
           "revocation retains only first completed selector");
         IO.Revoke_Data_After_Bank := False;
         for Fault in Fault_Kind range Unknown_Before_Effect .. Unknown_After_Effect loop
            for At_Call in Trace'Range loop
               Reset_Model; IO.Fault := Fault; IO.Fault_Call := At_Call;
               Expect (Name, 0, AML_Execute.Field_Indeterminate);
               Check (IO.Calls = At_Call and then IO.Trace (1 .. At_Call) = Trace (1 .. At_Call), "uncertain write request prefix");
               Check (F.Uncertain (Service) = IO.Receipt, "uncertain write exact receipt");
               Check (IO.Receipt.Item.Destination.Register = IDs (Destination (Trace (At_Call)))
                 and then IO.Receipt.Item.For_Write = (Trace (At_Call) in 'B' | 'w' | 'W' | 'x')
                 and then IO.Receipt.Item.Value = Request_Value (Op, Trace (At_Call)), "actual write leaf/value/direction");
               Check (IO.Data = Values_After (Op, At_Call - (if Fault = Unknown_Before_Effect then 1 else 0)),
                 "completed write effects survive uncertain suffix");
               Expect (Name, 0, AML_Execute.Field_Indeterminate);
               Check (IO.Calls = At_Call, "uncertain write never automatically replays");
               declare Saved : constant A.Observation := IO.Receipt; Wrong : A.Observation := Saved; begin
                  F.Reconcile (Service, IO, Saved, Status);
                  Check (Status /= C.Values.Available and then IO.Calls = At_Call, "unconfirmed write cannot reconcile");
                  IO.Receipt := A.No_Observation; IO.Confirmed := Saved;
                  Wrong.Operation := Wrong.Operation + 1;
                  F.Reconcile (Service, IO, Wrong, Status); Check (Status /= C.Values.Available, "wrong write receipt denied");
                  F.Reconcile (Service, IO, Saved, Status); Check (Status = C.Values.Available, "exact write reconcile");
               end;
               Expect (Name, 0);
               Check (IO.Calls = At_Call + Trace'Length and then IO.Trace (At_Call + 1 .. IO.Calls) = Trace
                 and then IO.Data = Final, "only explicit new invocation completes after reconcile");
            end loop;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Bank access checks" & Natural'Image (Checks));
end Bank_Access_Tests;
