pragma Ada_2022;
with Ada.Text_IO;
with AML_Namespace;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Table_Backing;
with Bank_Access_Oracle;
procedure Bank_Catalog_Reset_Tests is
   package N is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider,
     Capacity => 96, Max_Retained_Roots => 16);
   package V is new N.Owned.Collecting;
   use type V.Access_Status;
   use type V.Module_Outcome;
   package A renames AML_Field_Access;
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

   Status : V.Access_Status;
   Issued : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
      package VF is new V.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
        Maximum_Bindings => 3, Authorize_Target => Authorize);
      Arena : V.Arena;
      Input : aliased AML_Table_Backing.State (1, Bank_Access_Oracle.Raw_Table'Length);
      Model : Context;
      Old_Items, Fresh : VF.Binding_Set;
      Old_Region, New_Region : V.Node_Descriptor;
      Description : V.Field_Description;
      Key : V.Field_Key;
      Node, Bank : N.Node_ID;
      Report : V.Module_Report;
      Members : N.Initialization_Report;
      Outcome : V.Result;
      Saved : A.Observation;
      Before : Natural;
      procedure Load is
      begin
         V.Execute_Modules (Arena, Input, 100_000, Report, Status);
         Check (Status = V.Available and then Report.Outcome = V.Modules_Completed, "reset catalog module");
         V.Seal (Arena, Members, Status); Check (Status = V.Available, "reset catalog seal");
         V.Find_Child (Arena, N.Root, "REG0", Node, Status);
         V.Describe_Node (Arena, Node, New_Region, Status);
         Check (Status = V.Available, "reset catalog region descriptor");
         V.Find_Child (Arena, N.Root, "BNK0", Bank, Status);
         V.Read_Field_Descriptor (Arena, Bank, Description, Key, Status);
         Check (Status = V.Available, "reset catalog selector key");
         V.Find_Child (Arena, N.Root, "DYNB", Node, Status);
         Check (Status = V.Available, "reset catalog method");
      end Load;
      procedure Invoke (Bindings : VF.Binding_Set; Wanted : AML_Execute.Execution_Status) is
      begin
         VF.Invoke (Arena, Model, Bindings, Input, Node, [others => (V.Immediate_Argument, 0)],
           0, 10_000, Outcome, Status);
         Check (Status = V.Available and then Outcome.Status = Wanted,
           "reset catalog invocation " & Outcome.Status'Image);
      end Invoke;
   begin
      Input.Count := 1; Input.Tables (1) := (0, Bank_Access_Oracle.Raw_Table'Length);
      Input.Data.Bytes := AML_Decode.Bytes (Bank_Access_Oracle.Raw_Table);
      AML_Identity.Issuer.Issue (Model.Owner, Issued); Check (Issued, "reset provider identity");
      V.Initialize (Arena, Status); Check (Status = V.Available, "reset catalog initialize");
      Load; Old_Region := New_Region;
      VF.Clear (Old_Items);
      VF.Add_Binding (Arena, Model, Key, (Model.Owner, 7, 1, IDs (Bank_Port)), Old_Items, Status);
      Check (Status = V.Available, "reset catalog selector binding");
      VF.Add_Region_Register (Arena, Model, Old_Region, (Model.Owner, 7, 1, IDs (Data_Zero)),
        True, False, Old_Items, Status);
      Check (Status = V.Available, "reset catalog data authority");
      Model.Unknown_Data := True;
      Invoke (Old_Items, AML_Execute.Field_Indeterminate);
      Saved := VF.Uncertain (Arena); Before := Model.Calls;
      Check (Saved = Model.Receipt and then Saved.Present and then Before = 2,
        "reset captures banked data uncertainty");
      V.Reset (Arena, Status); Check (Status = V.Available, "reset metadata while data completion unknown");
      Check (VF.Uncertain (Arena) = Saved and then not V.Current_Node (Arena, Old_Region),
        "reset preserves receipt and invalidates old region descriptor");
      VF.Reconcile (Arena, Model, Saved, Status);
      Check (Status /= V.Available and then VF.Uncertain (Arena) = Saved, "reset still needs authoritative completion");
      Model.Receipt := A.No_Observation; Model.Confirmed := Saved;
      VF.Reconcile (Arena, Model, Saved, Status);
      Check (Status = V.Available and then not VF.Uncertain (Arena).Present and then Model.Calls = Before,
        "historical banked receipt reconciles without replay after reset");
      Load;
      Check (not V.Current_Node (Arena, Old_Region) and then V.Current_Node (Arena, New_Region),
        "same-address region reload does not revive an old descriptor");
      Invoke (Old_Items, AML_Execute.Unsupported_Value);
      Check (Model.Calls = Before, "stale banked catalog cannot dispatch after reload");
      VF.Clear (Fresh);
      VF.Add_Region_Register (Arena, Model, Old_Region, (Model.Owner, 7, 1, IDs (Data_Zero)),
        True, False, Fresh, Status);
      Check (Status /= V.Available, "stale region descriptor cannot register fresh authority");
      VF.Add_Binding (Arena, Model, Key, (Model.Owner, 7, 1, IDs (Bank_Port)), Fresh, Status);
      Check (Status = V.Available, "fresh selector key after reset");
      VF.Add_Region_Register (Arena, Model, New_Region, (Model.Owner, 7, 1, IDs (Data_Zero)),
        True, False, Fresh, Status);
      Check (Status = V.Available, "fresh catalog admitted after reset");
      Invoke (Fresh, AML_Execute.Returned);
      Check (Model.Calls = Before + 2 and then Model.Trace (Before + 1 .. Model.Calls) = "Br",
        "only fresh explicit authority enables a new pair after reset");
      VF.Clear (Fresh); Before := Model.Calls;
      Invoke (Fresh, AML_Execute.Unsupported_Value);
      Check (Model.Calls = Before, "Clear removes catalog authority and selector associations");
      VF.Add_Binding (Arena, Model, Key, (Model.Owner, 7, 1, IDs (Bank_Port)), Fresh, Status);
      Check (Status = V.Available, "Clear permits fresh selector association");
      Invoke (Fresh, AML_Execute.Unsupported_Value);
      Check (Model.Calls = Before, "restoring only the selector does not restore cleared catalog authority");

   Ada.Text_IO.Put_Line ("Bank reset checks" & Natural'Image (Checks));
end Bank_Catalog_Reset_Tests;
