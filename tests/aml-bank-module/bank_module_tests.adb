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
with Bank_Module_Oracle;
procedure Bank_Module_Tests is
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
           and then (For_Write or else IO.Allow_Data_Read));
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      S : Slot;
      Found, Allowed : Boolean;
      Geometry : AML_Field_Layout.Register_Geometry;
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
      if (S = Bank_Port and then IO.Unknown_Bank) or else
        (S /= Bank_Port and then IO.Unknown_Data) then
         IO.Unknown_Bank := False; IO.Unknown_Data := False;
         IO.Receipt := (True, IO.Owner, Item, IO.Next_Operation);
         Reply := (A.Indeterminate, IO.Owner, 0, IO.Next_Operation);
         IO.Next_Operation := IO.Next_Operation + 1;
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
      end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Data (S)), A.No_Operation);
      if S /= Bank_Port and then IO.Malformed_Data then
         IO.Malformed_Data := False;
         Reply.Value := (if Item.For_Write then 1 else 256);
      end if;
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then Expected = IO.Confirmed; end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Maximum_Bindings => 3, Authorize_Target => Authorize);
   Service : aliased C.State (1, Bank_Module_Oracle.Raw_Table'Length, Bank_Module_Oracle.Raw_Table'Length, 1);
   IO : Context;
   Items : F.Binding_Set;
   Staged : C.Stage_Status;
   Frozen : C.Freeze_Status;
   Status : C.Values.Access_Status;
   Modules : C.Execution_Report;
   Members : C.Namespace.Initialization_Report;
   Issued : Boolean;
   Region : C.Node_Descriptor;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
   procedure Expect (Name : AML_Names.Segment; Expected : AML_Decode.Integer_Value) is
      Node : C.Namespace.Node_ID;
      Description : C.Node_Descriptor;
      Result : C.Values.Result;
   begin
      C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " lookup");
      C.Describe_Node (Service, Node, Description, Status); Check (Status = C.Values.Available, Name & " descriptor");
      F.Evaluate_Named (Service, IO, Items, Description, 10_000, Result, Status);
      Check (Status = C.Values.Available and then Result.Status = AML_Execute.Returned,
        Name & " evaluation " & Result.Status'Image);
      Check (Result.Number = Expected, Name & " integer");
      Check (C.Retained_Results (Service) = 0, Name & " roots");
   end Expect;
begin
   AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "module provider identity");
   C.Stage_Table (Service, 1, C.DSDT, Bank_Module_Oracle.Raw_Table, Staged); Check (Staged = C.Staged, "module stage");
   C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "module freeze");
   C.Execute_Frozen (Service, 100_000, Modules, Status);
   Check (Status = C.Values.Available and then Modules.Modules.Outcome = C.Values.Modules_Completed,
     "module execution " & Modules.Modules.Failure'Image);
   Check (IO.Calls = 0, "module declaration has no provider effects");
   C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "module member initialization");
   F.Clear (Items);
   for S in Slot range Bank_Port .. Data_One loop
      declare
         Node : C.Namespace.Node_ID;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
         Name : constant AML_Names.Segment := (case S is
           when Bank_Port => "BNK0", when Data_Zero => "DAT0", when Data_One => "DAT1");
      begin
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
         Check (Status = C.Values.Available, Name & " module key");
         F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
         Check (Status = C.Values.Available, Name & " module binding");
      end;
   end loop;
   declare Node : C.Namespace.Node_ID; begin
      C.Find_Child (Service, C.Namespace.Root, "REG0", Node, Status);
      C.Describe_Node (Service, Node, Region, Status); Check (Status = C.Values.Available, "module region");
   end;
   for S in Slot range Data_Zero .. Data_One loop
      F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Items, Status);
      Check (Status = C.Values.Available, "module data register");
   end loop;
   -- Deliberate lifecycle policy: one evaluation per declaration execution.
   -- ACPICA's extra deferred passes instead produce CNT0=3 and bank=7.
   Expect ("SEEN", 1); Expect ("CNT0", 1); Expect ("SEL0", 7);
   Check (IO.Calls = 0 and then IO.Bank_Writes = 0, "initialization and scalar queries do not select a bank");
   Expect ("INIT", 0);
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("READ", 16#A5#);
   Check (IO.Trace (1 .. IO.Calls) = "Br" and then IO.Data (Bank_Port) = 3,
     "module field uses declaration-time selector even after module changes its source");
   Expect ("CNT0", 1); Expect ("MOVE", 1); Expect ("SEL0", 9);
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("READ", 16#A5#);
   Check (IO.Trace (1 .. IO.Calls) = "Br" and then IO.Data (Bank_Port) = 3,
     "later control method source change does not reevaluate module selector");
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("RDN1", 16#5A#);
   Check (IO.Trace (1 .. IO.Calls) = "BR" and then IO.Data (Bank_Port) = 3,
     "both fields in the declaration share one captured selector");
   IO.Calls := 0; IO.Bank_Writes := 0;
   Expect ("WRIT", 16#C3#);
   Check (IO.Trace (1 .. IO.Calls) = "Bwr" and then IO.Data (Bank_Port) = 3,
     "module banked write selects captured bank before data and direct readback");
   Expect ("CNT0", 1);
   C.Initialize_Members (Service, Members, Status);
   Check (Status /= C.Values.Available, "member initialization cannot be repeated");
   C.Execute_Frozen (Service, 100_000, Modules, Status);
   Check (Status /= C.Values.Available, "definition block cannot be replayed after ready");
   Expect ("CNT0", 1);
   Ada.Text_IO.Put_Line ("Bank module checks" & Natural'Image (Checks));
end Bank_Module_Tests;
