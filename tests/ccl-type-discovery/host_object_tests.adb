with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Catalog;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values;
with CCL.Language; use CCL.Language;
with CCL.VM;
with CCL.Compiler;

procedure Host_Object_Tests is
   use type CCL.Objects.Catalog.Publication_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.VM.Value_Kind;
   use type CCL.Compiler.Compilation_Status;
   Key : constant Schema_Key := [1, 2, 3, 4];
   Other_Key : constant Schema_Key := [5, 6, 7, 8];
   Types : Registry;
   Root, Unused : Type_Reference;
   Defined_As : Definition_Result;
   Contract, Other_Contract : Binding;
   Ok : Boolean;
   Catalog, Missing_Schema, Changed : Interface_Catalog;
   Grants, No_Grants : Granted_Bindings;
   Outcome : Interpretation_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "host object check" & Checks'Image; end if;
   end Check;
   type Fault is (None, Wrong_Key, Bad_Padding, Wrong_Kind, Failed);
   type Host is record
      Calls : Natural := 0;
      Mode : Fault := None;
      Stored : CCL.Objects.Image;
   end record;
   Context : Host;
   procedure Invoke
     (State : in out Host; Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result) is
   begin
      State.Calls := State.Calls + 1;
      if Binding = 1 then
         Check (CCL.Host_Values.Matches (Argument, Contract));
         State.Stored := Argument.Object;
      else Check (Binding = 2); end if;
      Reply := (Value => CCL.Host_Values.Object_Constant (State.Stored), Success => True, Why => <>);
      case State.Mode is
         when None => null;
         when Wrong_Key => Reply.Value.Object.Schema := Other_Key;
         when Bad_Padding => Reply.Value.Object.Padding (1) := 1;
         when Wrong_Kind => Reply.Value := CCL.Host_Values.Integer_Constant (42);
         when Failed => Reply.Success := False;
      end case;
   end Invoke;
   procedure Run is new Interpret_With_Values (Host, Invoke);
   procedure Describe_Interface (View : in out Interface_Catalog; Result_Key : Schema_Key) is
      D : Interface_Descriptor;
      O : Operation_Descriptor;
      Error : Catalog_Error;
   begin
      Define_Interface ("objects", 1, 0, [11, 12, 13, 14], D, Error); Check (Error = Catalog_Valid);
      Define_Host_Operation ("echo", 1,
        (Argument => CCL.Host_Values.Object_Value, Result => CCL.Host_Values.Object_Value,
         Argument_Schema => Key, Result_Schema => Result_Key, others => <>), O, Error);
      Check (Error = Catalog_Valid);
      Add_Operation (D, O, Error); Check (Error = Catalog_Valid);
      Define_Host_Operation ("get", 0,
        (Result => CCL.Host_Values.Object_Value, Result_Schema => Result_Key, others => <>), O, Error);
      Check (Error = Catalog_Valid);
      Add_Operation (D, O, Error); Check (Error = Catalog_Valid);
      Publish (View, D, Error); Check (Error = Catalog_Valid);
   end Describe_Interface;
   procedure Approve (View : in out Interface_Catalog; Item : CCL.Objects.Binding) is
      Result : CCL.Objects.Catalog.Publication_Result;
   begin
      Publish_Schema (View, Item, Result); Check (Result = CCL.Objects.Catalog.Published);
   end Approve;
   procedure Execute (Source : String; Member : String; Number : Integer_64 := 0;
                      Flag : Boolean := False) is
   begin
      Run (Source, 4096, Catalog, Grants, Context, Outcome);
      if Outcome.Status /= Succeeded then
         Ada.Text_IO.Put_Line (Outcome.Status'Image & " " & Outcome.Diagnostic'Image);
      end if;
      Check (Outcome.Status = Succeeded and Outcome.Has_Value);
      Check (Same (Outcome.Variant_Member_Name, Named (Member)));
      if Member = "Value" then Check (Outcome.Result_Value.Integer = Number);
      elsif Member = "Flag" then
         Check (Outcome.Result_Value.Kind = CCL.VM.Boolean_Value and then Outcome.Result_Value.Boolean = Flag);
      end if;
   end Execute;
   Resolved : Resolved_Operation;
   Found : Boolean;
   Granted : Grant_Result;
begin
   Define (Types, (Identifier => Named ("Hidden"), Form => Product, others => <>), Unused, Defined_As);
   Check (Defined_As = Defined);
   Define (Types, (Identifier => Named ("Reading"), Form => Sum, Count => 3,
     Parts => [1 => (Named ("Value"), CCL.Types.Integer_Type), 2 => (Named ("Unavailable"), CCL.Types.Unit_Type),
               3 => (Named ("Flag"), CCL.Types.Boolean_Type), others => <>]), Root, Defined_As);
   Check (Defined_As = Defined);
   Bind (Types, Root, Key, Contract, Ok); Check (Ok);
   Bind (Types, Root, Other_Key, Other_Contract, Ok); Check (Ok);
   Approve (Catalog, Contract);
   Check (Schema_Type (Catalog, Key) /= Root); -- imported IDs differ from producer
   Describe_Interface (Catalog, Key);
   Describe_Interface (Missing_Schema, Key);
   Resolve (Catalog, "objects.echo", Resolved, Found); Check (Found);
   Install (Grants, Resolved, 1, Granted); Check (Granted = Grant_Added);
   Resolve (Catalog, "objects.get", Resolved, Found); Check (Found);
   Install (Grants, Resolved, 2, Granted); Check (Granted = Grant_Added);
   Run ("(objects.get)", 4096, Missing_Schema, Grants, Context, Outcome);
   Check (Outcome.Status = Type_Check_Failed and Outcome.Diagnostic = Host_Schema_Unavailable);
   Check (Context.Calls = 0);
   Run ("(objects.echo 42)", 4096, Catalog, Grants, Context, Outcome);
   Check (Outcome.Status = Type_Check_Failed and Outcome.Diagnostic = Argument_Type_Mismatch);
   Check (Context.Calls = 0);
   Run ("(objects.echo Reading.Unavailable)", 4096, Catalog, No_Grants, Context, Outcome);
   Check (Outcome.Status = Host_Authority_Denied and Context.Calls = 0);
   Execute ("(objects.echo (Reading.Value 42))", "Value", 42);
   Execute ("(objects.get)", "Value", 42);
   Execute ("(objects.echo (Reading.Flag false))", "Flag");
   Execute ("(objects.echo (Reading.Flag true))", "Flag", Flag => True);
   Execute ("(objects.echo Reading.Unavailable)", "Unavailable");
   Execute ("(define (identity (r Reading)) Reading (objects.echo r)) (identity (Reading.Value 99))", "Value", 99);
   for Mode in Wrong_Key .. Failed loop
      Context.Mode := Mode;
      Run ("(objects.get)", 4096, Catalog, Grants, Context, Outcome);
      Check (Outcome.Status = (if Mode = Failed then Host_Call_Failed else Host_Result_Type_Mismatch));
      Check (not Outcome.Has_Value);
   end loop;
   Context.Mode := None;
   Approve (Changed, Contract); Approve (Changed, Other_Contract);
   Describe_Interface (Changed, Other_Key);
   declare Before : constant Natural := Context.Calls; begin
      Run ("(objects.get)", 4096, Changed, Grants, Context, Outcome);
      Check (Outcome.Status = Host_Authority_Denied and Context.Calls = Before);
   end;
   declare
      Analysis : Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
   begin
      Analyze ("(objects.get)", Catalog, Analysis);
      Check (Analysis_Status_Of (Analysis) = Analysis_Succeeded);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   end;
   -- A visible record is inspectable but visibility never grants access.
   -- Missing authority still rejects before invoking its host.
   declare
      View : Interface_Catalog;
      Before : constant Natural := Context.Calls;
   begin
      Bind (Types, Unused, Key, Contract, Ok); Check (Ok);
      Approve (View, Contract);
      Describe_Interface (View, Key);
      Run ("(objects.get)", 4096, View, No_Grants, Context, Outcome);
      Check (Outcome.Status = Host_Authority_Denied);
      Check (Context.Calls = Before);
   end;
   declare
      View : Interface_Catalog;
      Accesses : Granted_Bindings;
      Built : Build_Result;
      type Natural_Array is array (Positive range <>) of Natural;
   begin
      Bind (Types, CCL.Types.String_Type, Key, Contract, Ok); Check (Ok);
      Approve (View, Contract);
      Describe_Interface (View, Key);
      Resolve (View, "objects.echo", Resolved, Found); Check (Found);
      Install (Accesses, Resolved, 1, Granted); Check (Granted = Grant_Added);
      Resolve (View, "objects.get", Resolved, Found); Check (Found);
      Install (Accesses, Resolved, 2, Granted); Check (Granted = Grant_Added);
      Run ("(objects.echo ""Cubie"")", 4096, View, Accesses, Context, Outcome);
      Check (Outcome.Status = Succeeded and Outcome.Has_Text);
      Check (Outcome.Result_Text.Length = 5 and Outcome.Result_Text.Data (1 .. 5) = "Cubie");
      for Length of Natural_Array'(0, MAX_TEXT_BYTES, MAX_TEXT_BYTES + 1) loop
         Context.Stored := Empty (Contract);
         Append_Text (Context.Stored, String'(1 .. Length => 'x'), Built); Check (Built = Added);
         Run ("(objects.get)", 4096, View, Accesses, Context, Outcome);
         if Length <= MAX_TEXT_BYTES then
            Check (Outcome.Status = Succeeded and Outcome.Has_Text);
            Check (Outcome.Result_Text.Length = Length and
              Outcome.Result_Text.Data (1 .. Length) = String'(1 .. Length => 'x'));
         else
            -- The native host value is valid; only the scalar UI output
            -- buffer is too small. Do not misreport it as a hostile reply.
            Check (Outcome.Status = Evaluation_Text_Storage_Exhausted and not Outcome.Has_Text);
         end if;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Typed source host objects: PASS" & Checks'Image & " checks");
end Host_Object_Tests;
