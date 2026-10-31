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
with Bank_Access_Oracle;
procedure Bank_Capacity_Tests is
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
   Issued : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
      Temporary_Fields : constant Positive := 2;
      Initial_Namespace_Nodes : constant Positive := 23;
      Exact_Capacity : constant Positive := Initial_Namespace_Nodes + Temporary_Fields;
      package Small is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
        Namespace_Node_Capacity => Exact_Capacity, Retained_Result_Capacity => 16);
      use type Small.Stage_Status;
      use type Small.Freeze_Status;
      use type Small.Values.Access_Status;
      use type Small.Values.Module_Outcome;
      package SF is new Small.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
        Maximum_Bindings => 3, Authorize_Target => Authorize);
      Limited_Service : aliased Small.State (1, Bank_Access_Oracle.Raw_Table'Length, Bank_Access_Oracle.Raw_Table'Length, 1);
      Model : Context;
      Bindings : SF.Binding_Set;
      Staged : Small.Stage_Status;
      Frozen : Small.Freeze_Status;
      Accessed : Small.Values.Access_Status;
      Executed : Small.Execution_Report;
      Members : Small.Namespace.Initialization_Report;
      Region_Item, Method_Item : Small.Node_Descriptor;
      Node : Small.Namespace.Node_ID;
      Description : Small.Values.Field_Description;
      Key : Small.Values.Field_Key;
      Outcome : Small.Values.Result;
   begin
      AML_Identity.Issuer.Issue (Model.Owner, Issued); Check (Issued, "capacity provider identity");
      Small.Stage_Table (Limited_Service, 1, Small.DSDT, Bank_Access_Oracle.Raw_Table, Staged);
      Check (Staged = Small.Staged, "capacity stage");
      Small.Freeze_Catalog (Limited_Service, Frozen); Check (Frozen = Small.Catalog_Frozen_OK, "capacity freeze");
      Small.Execute_Frozen (Limited_Service, 100_000, Executed, Accessed);
      Check (Accessed = Small.Values.Available and then Executed.Modules.Outcome = Small.Values.Modules_Completed,
        "capacity module");
      Small.Initialize_Members (Limited_Service, Members, Accessed);
      Check (Accessed = Small.Values.Available, "capacity initialization");
      Check (Small.Observe (Limited_Service).Objects + Temporary_Fields = Exact_Capacity,
        "exactly two namespace slots remain for the repeated two-field declaration");
      Small.Find_Child (Limited_Service, Small.Namespace.Root, "REG0", Node, Accessed);
      Small.Describe_Node (Limited_Service, Node, Region_Item, Accessed);
      Check (Accessed = Small.Values.Available, "capacity region");
      Small.Find_Child (Limited_Service, Small.Namespace.Root, "BNK0", Node, Accessed);
      Small.Read_Field_Descriptor (Limited_Service, Node, Description, Key, Accessed);
      Check (Accessed = Small.Values.Available, "capacity selector");
      SF.Clear (Bindings);
      SF.Add_Binding (Limited_Service, Model, Key, (Model.Owner, 7, 1, IDs (Bank_Port)), Bindings, Accessed);
      Check (Accessed = Small.Values.Available, "capacity selector binding");
      SF.Add_Region_Register (Limited_Service, Model, Region_Item, (Model.Owner, 7, 1, IDs (Data_One)),
        True, False, Bindings, Accessed);
      Check (Accessed = Small.Values.Available, "capacity explicit data target");
      Small.Find_Child (Limited_Service, Small.Namespace.Root, "LOOP", Node, Accessed);
      Small.Describe_Node (Limited_Service, Node, Method_Item, Accessed);
      Check (Accessed = Small.Values.Available, "capacity loop method");
      Model.Data (Data_One) := 16#AB#;
      for Attempt in 1 .. 2 loop
         Model.Calls := 0; Model.Bank_Writes := 0;
         SF.Evaluate_Named (Limited_Service, Model, Bindings, Method_Item, 10_000, Outcome, Accessed);
         Check (Accessed = Small.Values.Available and then Outcome.Status = AML_Execute.Returned
           and then Outcome.Number = 16#AB#, "repeated declaration succeeds at full namespace capacity" & Attempt'Image);
         Check (Model.Trace (1 .. Model.Calls) = "BR" and then Model.Data (Bank_Port) = 1,
           "full namespace reuses fields and captures the second selector");
         Check (Small.Observe (Limited_Service).Objects + Temporary_Fields = Exact_Capacity,
           "temporary field cleanup restores both namespace slots");
      end loop;

   Ada.Text_IO.Put_Line ("Bank capacity checks" & Natural'Image (Checks));
end Bank_Capacity_Tests;
