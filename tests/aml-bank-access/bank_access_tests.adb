pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Field_Targets;
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
      Allow_Bank_Write, Allow_Data_Read : Boolean := True;
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
   Service : aliased C.State (1, Bank_Access_Oracle.Raw_Table'Length, Bank_Access_Oracle.Raw_Table'Length, 1);
   IO : Context;
   Items : F.Binding_Set;
   Direct_Items, Read_Items : F.Binding_Set;
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
   procedure Temporary_Gone (Name : AML_Names.Segment) is
      Parent, Child : C.Namespace.Node_ID;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Parent, Status);
      Check (Status = C.Values.Available and then Parent /= C.Namespace.Root, "dynamic method remains");
      C.Find_Child (Service, Parent, "TMP0", Child, Status);
      Check (Status = C.Values.Available and then Child = C.Namespace.Root,
        "method-local banked field retired after failed access");
   end Temporary_Gone;
   procedure Resolve is
      Saved : constant A.Observation := IO.Receipt;
      Wrong : A.Observation := Saved;
      Before : constant Natural := IO.Calls;
   begin
      Check (Saved.Present and then F.Uncertain (Service) = Saved, "exact request and operation retained");
      F.Reconcile (Service, IO, Saved, Status);
      Check (Status /= C.Values.Available and then F.Uncertain (Service) = Saved,
        "unconfirmed operation cannot be cleared");
      IO.Receipt := A.No_Observation; IO.Confirmed := Saved;
      Wrong.Operation := Wrong.Operation + 1;
      F.Reconcile (Service, IO, Wrong, Status);
      Check (Status /= C.Values.Available and then F.Uncertain (Service) = Saved,
        "different operation cannot clear original observation");
      F.Reconcile (Service, IO, Saved, Status);
      Check (Status = C.Values.Available and then not F.Uncertain (Service).Present,
        "authoritative completion reconciles after temporary field retirement");
      Check (IO.Calls = Before, "reconciliation never replays hardware operations");
   end Resolve;
   procedure Begin_Fault_Case (Fault : Fault_Kind; At_Call : Natural) is
   begin
      Check (not Pending (IO) and then not F.Uncertain (Service).Present, "previous fault case resolved");
      IO.Calls := 0; IO.Bank_Writes := 0; IO.Data_Zero_Writes := 0;
      IO.Data := [Bank_Port => 0, Data_Zero => 16#A5#, Data_One => 16#5A#, Extra_Port => 0];
      IO.Fault := Fault; IO.Fault_Call := At_Call;
      IO.Revoke_Last_After_First := False; IO.Shift_Last_After_First := False; IO.Allow_Last_Write := True;
      IO.Confirmed := A.No_Observation;
   end Begin_Fault_Case;
begin
   AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "provider identity");
   C.Stage_Table (Service, 1, C.DSDT, Bank_Access_Oracle.Raw_Table, Stage); Check (Stage = C.Staged, "stage");
   C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "freeze");
   C.Execute_Frozen (Service, 100_000, Modules, Status);
   Check (Status = C.Values.Available and then Modules.Modules.Outcome = C.Values.Modules_Completed, "module import");
   C.Initialize_Members (Service, Init, Status); Check (Status = C.Values.Available, "members");
   Ada.Text_IO.Put_Line ("Bank catalog initial nodes" & Natural'Image (C.Observe (Service).Objects));
   F.Clear (Items);
   for S in Slot range Bank_Port .. Data_One loop
      declare
         Node : C.Namespace.Node_ID;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
         Name : constant AML_Names.Segment := (case S is when Bank_Port => "BNK0",
           when Data_Zero => "DAT0", when Data_One => "DAT1");
      begin
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " leaf");
         C.Read_Field_Descriptor (Service, Node, Description, Key, Status); Check (Status = C.Values.Available, Name & " key");
         F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
         Check (Status = C.Values.Available, Name & " binding");
      end;
   end loop;
   Expect ("CNT0", 0); Expect ("INIT", 0);
   Direct_Items := Items;
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "ordinary field bindings do not imply a region register catalog");
   Expect ("CNT0", 1);
   declare N : C.Namespace.Node_ID; begin
      C.Find_Child (Service, C.Namespace.Root, "REG0", N, Status);
      Check (Status = C.Values.Available and then N /= C.Namespace.Root, "region lookup");
      C.Describe_Node (Service, N, Region, Status); Check (Status = C.Values.Available, "region descriptor");
   end;
   F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_Zero)), False, False, Items, Status);
   Check (Status /= C.Values.Available, "empty region permission rejected");
   F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 2, IDs (Data_Zero)), True, False, Items, Status);
   Check (Status /= C.Values.Available, "unrecognized target epoch rejected");
   declare N : C.Namespace.Node_ID; Not_Region : C.Node_Descriptor; begin
      C.Find_Child (Service, C.Namespace.Root, "BNK0", N, Status);
      C.Describe_Node (Service, N, Not_Region, Status);
      Check (Status = C.Values.Available, "non-region descriptor");
      F.Add_Region_Register (Service, IO, Not_Region, (IO.Owner, 7, 1, IDs (Data_Zero)), True, False, Items, Status);
      Check (Status /= C.Values.Available, "field descriptor cannot authorize a region");
   end;
   F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_Zero)), True, False, Items, Status);
   Check (Status = C.Values.Available, "explicit read-only data register");
   F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_Zero)), True, True, Items, Status);
   Check (Status /= C.Values.Available, "duplicate entry cannot silently upgrade direction");
   Read_Items := Items;
   Expect ("DYNB", 16#A5#);
   Check (IO.Calls = 2 and then IO.Bank_Writes = 1 and then IO.Data (Bank_Port) = 3, "select bank then read data");
   Expect ("CNT0", 2); Expect ("MOVE", 2); Expect ("DYNB", 16#A5#);
   Check (IO.Bank_Writes = 2 and then IO.Data (Bank_Port) = 7, "fresh selector on next invocation");
   Expect ("CNT0", 3);
   IO.Calls := 0; IO.Bank_Writes := 0; IO.Allow_Bank_Write := False;
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "banked read needs selector-write permission before effects");
   IO.Allow_Bank_Write := True; IO.Allow_Data_Read := False;
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "denied data read must not select the bank");
   IO.Allow_Data_Read := True; IO.Revoke_Data_After_Bank := True;
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 1 and then IO.Bank_Writes = 1 and then IO.Data (Bank_Port) = 7,
     "revocation after selection preserves known effect and denies data");
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 1, "revocation does not cause automatic replay");
   IO.Revoke_Data_After_Bank := False;
   IO.Calls := 0; IO.Bank_Writes := 0;
   Items := Read_Items;
   Expect ("DYNW", 0, AML_Execute.Unsupported);
   Check (IO.Calls = 0, "read-only region catalog denies writes before selection");
   Items := Direct_Items;
   F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_Zero)), False, True, Items, Status);
   Check (Status = C.Values.Available, "explicit write-only data register");
   IO.Allow_Data_Read := False;
   Expect ("DYNW", 0);
   Check (IO.Trace (1 .. IO.Calls) = "Bw" and then IO.Data (Data_Zero) = 16#C3#,
     "full write uses selector then write without a data read");
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("DYNP", 0, AML_Execute.Unsupported);
   Check (IO.Calls = 0 and then IO.Data (Data_Zero) = 16#C3#,
     "partial Preserve needs data read permission before effects");
   Expect ("DYNO", 0);
   Check (IO.Trace (1 .. IO.Calls) = "Bw" and then IO.Data (Data_Zero) = 16#F2#,
     "WriteAsOnes uses write-only authority and fills adjacent bits");
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("DYNZ", 0);
   Check (IO.Trace (1 .. IO.Calls) = "Bw" and then IO.Data (Data_Zero) = 4,
     "WriteAsZeros uses write-only authority and clears adjacent bits");
   IO.Allow_Data_Read := True;
   Items := Direct_Items;
   for S in Slot range Data_Zero .. Data_One loop
      F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Items, Status);
      Check (Status = C.Values.Available, "read/write data register");
   end loop;
   Expect ("DYNW", 0);
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("DYNP", 0);
   Check (IO.Trace (1 .. IO.Calls) = "BrBw" and then IO.Data (Data_Zero) = 16#C6#,
     "Preserve reselects before both read and write and keeps adjacent bits");
   Expect ("INIT", 0);
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("DYNM", 0);
   Check (IO.Trace (1 .. IO.Calls) = "BrBwBW" and then IO.Bank_Writes = 3
     and then IO.Data (Data_Zero) = 16#C5# and then IO.Data (Data_One) = 16#AB#,
     "unaligned multidatum Preserve selects separately for every transfer");
   IO.Calls := 0; IO.Bank_Writes := 0; IO.Unknown_Bank := True;
   Expect ("DYNB", 0, AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 1 and then Pending (IO) and then IO.Receipt.Item.For_Write
     and then IO.Receipt.Item.Destination.Register = IDs (Bank_Port),
     "uncertain bank selection retains the selector leaf request");
   Temporary_Gone ("DYNB");
   Expect ("DYNW", 0, AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 1, "uncertain selection blocks a different method without replay");
   Resolve;
   IO.Calls := 0; IO.Bank_Writes := 0; IO.Unknown_Data := True;
   Expect ("DYNB", 0, AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 2 and then IO.Bank_Writes = 1 and then Pending (IO)
     and then not IO.Receipt.Item.For_Write
     and then IO.Receipt.Item.Destination.Register = IDs (Data_Zero),
     "uncertain data read retains data request after known selector effect");
   Temporary_Gone ("DYNB");
   Expect ("DYNB", 0, AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 2, "uncertain read blocks further access");
   Resolve;
   IO.Calls := 0; IO.Bank_Writes := 0; IO.Unknown_Data := True;
   Expect ("DYNW", 0, AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 2 and then IO.Bank_Writes = 1 and then Pending (IO)
     and then IO.Receipt.Item.For_Write and then IO.Receipt.Item.Value = 16#C3#
     and then IO.Receipt.Item.Destination.Register = IDs (Data_Zero),
     "uncertain Store retains exact data write and value");
   Temporary_Gone ("DYNW");
   Expect ("DYNW", 0, AML_Execute.Field_Indeterminate);
   Check (IO.Calls = 2, "uncertain write blocks a repeated Store");
   Resolve;
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("DYNW", 0);
   Check (IO.Trace (1 .. IO.Calls) = "Bw" and then IO.Data (Data_Zero) = 16#C3#,
     "explicit new invocation after reconciliation performs one fresh pair");
   IO.Calls := 0; IO.Bank_Writes := 0; IO.Fail_Data := True;
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 2 and then IO.Bank_Writes = 1 and then not Pending (IO)
     and then not F.Uncertain (Service).Present, "known failed data read has no uncertain operation");
   IO.Calls := 0; IO.Bank_Writes := 0; IO.Malformed_Data := True;
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 2 and then not F.Uncertain (Service).Present,
     "oversized completed byte value is rejected without invented uncertainty");
   IO.Calls := 0; IO.Bank_Writes := 0; IO.Malformed_Data := True;
   Expect ("DYNW", 0, AML_Execute.Unsupported);
   Check (IO.Calls = 2 and then IO.Data (Data_Zero) = 16#C3# and then not F.Uncertain (Service).Present,
     "malformed write acknowledgement preserves known completed effect");
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("LOOP", 16#AB#);
   Check (IO.Trace (1 .. IO.Calls) = "BR" and then IO.Bank_Writes = 1
     and then IO.Data (Bank_Port) = 1,
     "reexecuted two-field declaration captures the second selector before access");
   Temporary_Gone ("LOOP");
   for Mode in Geometry_Mode range Wrong_Space .. Crossing_End loop
      declare Fresh : F.Binding_Set; begin
         F.Clear (Fresh); IO.Geometry := Mode;
         F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_Zero)), True, True, Fresh, Status);
         Check (Status /= C.Values.Available, "reject inadmissible catalog geometry " & Mode'Image);
         IO.Geometry := Matching;
         for S in Slot range Data_Zero .. Extra_Port loop
            F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Fresh, Status);
            Check (Status = C.Values.Available,
              "all catalog slots remain after failed registration " & Mode'Image & S'Image);
         end loop;
      end;
   end loop;
   IO.Calls := 0; IO.Bank_Writes := 0; IO.Geometry := Shifted_Offset;
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 0, "changed catalog geometry rejected before bank selection");
   IO.Geometry := Matching; IO.Change_Geometry_After_Bank := True;
   Expect ("DYNB", 0, AML_Execute.Unsupported_Value);
   Check (IO.Calls = 1 and then IO.Bank_Writes = 1,
     "geometry change after selection preserves bank effect and denies data");
   IO.Change_Geometry_After_Bank := False;
   IO.Calls := 0; IO.Bank_Writes := 0;
   F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Bank_Port)), True, True, Items, Status);
   Check (Status = C.Values.Available, "fill third named catalog slot");
   F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Extra_Port)), True, True, Items, Status);
   Check (Status /= C.Values.Available, "catalog capacity rejects a fourth valid register");
   Expect ("DYNB", 16#C3#);
   Check (IO.Trace (1 .. IO.Calls) = "Br", "capacity failure preserves existing catalog entries");
   declare
      Fresh : F.Binding_Set;
      Saved : constant F.Binding_Set := Items;
      Owner : constant AML_Identity.Identity := IO.Owner;
      Another : AML_Identity.Identity;
      Node : C.Namespace.Node_ID;
      Description : C.Values.Field_Description;
      Key : C.Values.Field_Key;
      Targets : AML_Field_Targets.Vector := [others => <>];
   begin
      F.Clear (Fresh);
      F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_Zero)), True, False, Fresh, Status);
      Check (Status = C.Values.Available, "catalog-only set established");
      C.Find_Child (Service, C.Namespace.Root, "BNK0", Node, Status);
      C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
      Check (Status = C.Values.Available, "selector field for provider consistency checks");
      AML_Identity.Issuer.Issue (Another, Issued); Check (Issued, "second provider identity");
      IO.Owner := Another;
      F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_One)), True, False, Fresh, Status);
      Check (Status /= C.Values.Available, "catalog-only set cannot change provider through a new catalog entry");
      F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (Bank_Port)), Fresh, Status);
      Check (Status /= C.Values.Available, "catalog-only set cannot change provider through a direct binding");
      Targets (Targets'First) := (IO.Owner, 7, 1, IDs (Bank_Port));
      F.Add_Read_Binding (Service, IO, Key, Targets, 1, Fresh, Status);
      Check (Status /= C.Values.Available, "catalog-only set cannot change provider through a read plan");
      F.Add_Write_Binding (Service, IO, Key, Targets, 1, Fresh, Status);
      Check (Status /= C.Values.Available, "catalog-only set cannot change provider through a write plan");
      IO.Owner := Owner;
      F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (Bank_Port)), Fresh, Status);
      Check (Status = C.Values.Available, "rejected provider additions preserve original catalog ownership");
      Items := Fresh; IO.Calls := 0; IO.Bank_Writes := 0;
      Expect ("DYNB", 16#C3#);
      Check (IO.Trace (1 .. IO.Calls) = "Br", "catalog-only admission failures preserve usable prior entries");
      Items := Saved;
   end;
   declare
      Other : aliased C.State (1, Bank_Access_Oracle.Raw_Table'Length, Bank_Access_Oracle.Raw_Table'Length, 1);
      Other_Region, Other_Method : C.Node_Descriptor;
      Node : C.Namespace.Node_ID;
      Outcome : C.Values.Result;
      Before : constant Natural := IO.Calls;
   begin
      C.Stage_Table (Other, 1, C.DSDT, Bank_Access_Oracle.Raw_Table, Stage); Check (Stage = C.Staged, "other arena stage");
      C.Freeze_Catalog (Other, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "other arena freeze");
      C.Execute_Frozen (Other, 100_000, Modules, Status);
      Check (Status = C.Values.Available and then Modules.Modules.Outcome = C.Values.Modules_Completed, "other arena module");
      C.Initialize_Members (Other, Init, Status); Check (Status = C.Values.Available, "other arena initialization");
      C.Find_Child (Other, C.Namespace.Root, "REG0", Node, Status);
      C.Describe_Node (Other, Node, Other_Region, Status); Check (Status = C.Values.Available, "other arena region");
      declare Catalog_Only : F.Binding_Set; begin
         F.Clear (Catalog_Only);
         F.Add_Region_Register (Service, IO, Other_Region, (IO.Owner, 7, 1, IDs (Extra_Port)), True, False, Catalog_Only, Status);
         Check (Status /= C.Values.Available, "foreign region descriptor rejected with catalog capacity available");
         F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data_Zero)), True, False, Catalog_Only, Status);
         Check (Status = C.Values.Available, "arena-specific catalog-only set");
         F.Add_Region_Register (Other, IO, Other_Region, (IO.Owner, 7, 1, IDs (Data_One)), True, False, Catalog_Only, Status);
         Check (Status /= C.Values.Available, "catalog-only set cannot cross namespace arenas");
      end;
      C.Find_Child (Other, C.Namespace.Root, "DYNB", Node, Status);
      C.Describe_Node (Other, Node, Other_Method, Status); Check (Status = C.Values.Available, "other arena method");
      F.Evaluate_Named (Other, IO, Items, Other_Method, 10_000, Outcome, Status);
      Check (Status = C.Values.Available and then Outcome.Status = AML_Execute.Unsupported_Value
        and then IO.Calls = Before, "same-address region in another arena cannot use this catalog");
   end;
   Begin_Fault_Case (No_Fault, 0); IO.Allow_Last_Write := False;
   Expect ("DYNM", 0, AML_Execute.Unsupported);
   Check (IO.Calls = 0 and then IO.Data (Data_Zero) = 16#A5# and then IO.Data (Data_One) = 16#5A#,
     "last datum permission is preflighted before any bank or first-datum effect");
   IO.Allow_Last_Write := True;
   for Timing in Fault_Kind range Unknown_Before_Effect .. Unknown_After_Effect loop
      Begin_Fault_Case (Timing, 4);
      Expect ("DYNP", 0, AML_Execute.Field_Indeterminate);
      Check (IO.Trace (1 .. IO.Calls) = "BrBw" and then IO.Bank_Writes = 2
        and then IO.Data (Data_Zero) = (if Timing = Unknown_Before_Effect then 16#A5# else 16#A6#),
        "partial Preserve write may or may not have completed " & Timing'Image);
      Check (IO.Receipt.Item.Destination.Register = IDs (Data_Zero) and then IO.Receipt.Item.For_Write
        and then IO.Receipt.Item.Value = 16#A6#, "uncertain partial write records the merged full byte");
      Temporary_Gone ("DYNP");
      Expect ("DYNW", 0, AML_Execute.Field_Indeterminate);
      Check (IO.Calls = 4, "partial write uncertainty blocks further requests");
      Resolve;
      Check (IO.Calls = 4 and then IO.Data (Data_Zero) =
        (if Timing = Unknown_Before_Effect then 16#A5# else 16#A6#),
        "confirming partial write does not replay its read or write");
   end loop;
   for Timing in Fault_Kind range Unknown_Before_Effect .. Unknown_After_Effect loop
      Begin_Fault_Case (Timing, 5);
      Expect ("DYNM", 0, AML_Execute.Field_Indeterminate);
      Check (IO.Trace (1 .. IO.Calls) = "BrBwB" and then IO.Data (Data_Zero) = 16#C5#
        and then IO.Data (Data_One) = 16#5A# and then IO.Bank_Writes =
          (if Timing = Unknown_Before_Effect then 2 else 3),
        "later unknown selector preserves the completed first datum " & Timing'Image);
      Check (IO.Receipt.Item.Destination.Register = IDs (Bank_Port) and then IO.Receipt.Item.For_Write
        and then IO.Receipt.Item.Value = 7, "later selector receipt retains its own bank request");
      Temporary_Gone ("DYNM");
      Expect ("DYNM", 0, AML_Execute.Field_Indeterminate);
      Check (IO.Calls = 5, "unknown later selector stops the next data write");
      Resolve;
      Check (IO.Calls = 5 and then IO.Data (Data_Zero) = 16#C5# and then IO.Data (Data_One) = 16#5A#,
        "reconciliation does not roll back prefix or finish remaining datum");
   end loop;
   for Timing in Fault_Kind range Unknown_Before_Effect .. Unknown_After_Effect loop
      Begin_Fault_Case (Timing, 6);
      Expect ("DYNM", 0, AML_Execute.Field_Indeterminate);
      Check (IO.Trace (1 .. IO.Calls) = "BrBwBW" and then IO.Bank_Writes = 3
        and then IO.Data (Data_Zero) = 16#C5# and then IO.Data (Data_One) =
          (if Timing = Unknown_Before_Effect then 16#5A# else 16#AB#),
        "later unknown data write preserves known prefix " & Timing'Image);
      Check (IO.Receipt.Item.Destination.Register = IDs (Data_One) and then IO.Receipt.Item.For_Write
        and then IO.Receipt.Item.Value = 16#AB#, "later data receipt is the exact last write");
      Temporary_Gone ("DYNM");
      Expect ("DYNW", 0, AML_Execute.Field_Indeterminate);
      Check (IO.Calls = 6, "unknown last write blocks a different method");
      Resolve;
      Check (IO.Calls = 6 and then IO.Data (Data_One) =
        (if Timing = Unknown_Before_Effect then 16#5A# else 16#AB#),
        "last write confirmation does not issue an automatic continuation");
      Expect ("DYNM", 0);
      Check (IO.Calls = 12 and then IO.Trace (7 .. 12) = "BrBwBW"
        and then IO.Data (Data_Zero) = 16#C5# and then IO.Data (Data_One) = 16#AB#,
        "only a new explicit invocation performs a fresh complete write");
   end loop;
   for Fault in Fault_Kind range Known_Failure .. Malformed_Completion loop
      Begin_Fault_Case (Fault, 6);
      Expect ("DYNM", 0, AML_Execute.Unsupported);
      Check (IO.Calls = 6 and then IO.Data (Data_Zero) = 16#C5# and then IO.Data (Data_One) =
        (if Fault = Known_Failure then 16#5A# else 16#AB#)
        and then not Pending (IO) and then not F.Uncertain (Service).Present,
        "known later completion preserves its actual effects " & Fault'Image);
      Temporary_Gone ("DYNM");
      Expect ("DYNM", 0);
      Check (IO.Calls = 12 and then IO.Data (Data_One) = 16#AB#,
        "known completion allows a deliberate fresh invocation without reconciliation");
   end loop;
   declare
      type Late_Change is (Revoke_Last, Shift_Last);
   begin
      for Change in Late_Change loop
         Begin_Fault_Case (No_Fault, 0);
         IO.Revoke_Last_After_First := Change = Revoke_Last;
         IO.Shift_Last_After_First := Change = Shift_Last;
         Expect ("DYNM", 0, AML_Execute.Unsupported);
         Check (IO.Trace (1 .. IO.Calls) = "BrBw" and then IO.Data (Data_Zero) = 16#C5#
           and then IO.Data (Data_One) = 16#5A# and then IO.Bank_Writes = 2,
           "late target change stops before the next selection " & Change'Image);
         Check (not Pending (IO) and then not F.Uncertain (Service).Present, "late denial is a known failure");
         Expect ("DYNM", 0, AML_Execute.Unsupported);
         Check (IO.Calls = 4, "still-denied last target prevents another first-datum write");
         IO.Revoke_Last_After_First := False; IO.Shift_Last_After_First := False;
         Expect ("DYNM", 0);
         Check (IO.Calls = 10 and then IO.Trace (5 .. 10) = "BrBwBW" and then IO.Data (Data_One) = 16#AB#,
           "restoring target permits only an explicit new invocation");
      end loop;
   end;
   declare Saved_Items : constant F.Binding_Set := Items; begin
      Begin_Fault_Case (Unknown_Before_Effect, 6);
      Expect ("DYNM", 0, AML_Execute.Field_Indeterminate);
      F.Clear (Items);
      Expect ("DYNW", 0, AML_Execute.Field_Indeterminate);
      Check (IO.Calls = 6, "clearing bindings cannot erase unresolved completion");
      Resolve;
      Expect ("DYNW", 0, AML_Execute.Unsupported);
      Check (IO.Calls = 6, "reconciliation does not restore cleared catalog authority");
      Items := Saved_Items;
      Expect ("DYNM", 0);
      Check (IO.Calls = 12 and then IO.Data (Data_Zero) = 16#C5# and then IO.Data (Data_One) = 16#AB#,
        "explicit authority restoration and invocation resume usable access");
   end;
   declare
      package Limited_IO is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
        Maximum_Bindings => 3, Authorize_Target => Authorize, Maximum_Requests => 3);
      Limited_Service : aliased C.State
        (1, Bank_Access_Oracle.Raw_Table'Length, Bank_Access_Oracle.Raw_Table'Length, 1);
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
            Name : constant AML_Names.Segment := (case S is when Bank_Port => "BNK0",
              when Data_Zero => "DAT0", when Data_One => "DAT1");
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
      for S in Slot range Data_Zero .. Data_One loop
         Limited_IO.Add_Region_Register (Limited_Service, Bus, Region_Node,
           (Bus.Owner, 7, 1, IDs (S)), True, True, Bindings, Status);
         Check (Status = C.Values.Available, "limited explicit catalog");
      end loop;
      Bus.Data (Data_Zero) := 16#A5#;
      Limited_Expect ("DYNP", AML_Execute.Value_Limit);
      Check (Bus.Calls = 3 and then Bus.Trace (1 .. 3) = "BrB"
        and then Bus.Data (Data_Zero) = 16#A5#, "shared limit stops before partial data write");
      Check (not Limited_IO.Uncertain (Limited_Service).Present and then not Pending (Bus),
        "known exhausted prefix creates no uncertainty");
      Limited_Expect ("DYNB", AML_Execute.Returned);
      Check (Bus.Calls = 5 and then Bus.Trace (4 .. 5) = "Br", "fresh field operation gets fresh budget");
      Limited_Expect ("DYNW", AML_Execute.Returned);
      Check (Bus.Calls = 7 and then Bus.Data (Data_Zero) = 16#C3#, "fresh write finishes in budget");
      Bus.Fault := Unknown_After_Effect; Bus.Fault_Call := 9;
      Limited_Expect ("DYNB", AML_Execute.Field_Indeterminate);
      Check (Bus.Calls = 9 and then Limited_IO.Uncertain (Limited_Service) = Bus.Receipt,
        "provider uncertainty takes precedence over unused request budget");
      Limited_Expect ("DYNB", AML_Execute.Field_Indeterminate);
      Check (Bus.Calls = 9, "uncertain request is never replayed on new invocation");
      declare Saved : constant A.Observation := Bus.Receipt; begin
         Bus.Receipt := A.No_Observation; Bus.Confirmed := Saved;
         Limited_IO.Reconcile (Limited_Service, Bus, Saved, Status);
         Check (Status = C.Values.Available, "limited exact receipt reconciliation");
      end;
      Bus.Fault := No_Fault;
      Limited_Expect ("DYNB", AML_Execute.Returned);
      Check (Bus.Calls = 11, "explicit invocation after reconciliation gets fresh budget");
   end;
   Ada.Text_IO.Put_Line ("Bank access checks" & Natural'Image (Checks));
end Bank_Access_Tests;

