with CCL.Evaluation;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog; use CCL.Catalog;
with CCL.Types; use CCL.Types;
with CCL.Host_Values;
with CCL.Language;
with CCL.Compiler;
with CCL.Objects;
with CCL.Objects.Values;
with CCL.VM;
with Config_Object_Outcomes;
with Config_Object_Messages;

procedure Outcome_Tests is
   package O renames Config_Object_Outcomes;
   package W renames Config_Object_Messages;
   package V renames CCL.VM;
   use type V.Value;
   use type V.Value_Kind;
   use type CCL.Host_Values.Value;
   use type O.Write_Alternative;
   use type W.Status;
   use type V.Execution_Status;
   use type V.Validation_Error;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Language.Interpretation_Status;
   Catalog, Shifted, Bad : Interface_Catalog;
   Types, Other : Registry;
   Root, Ref : Type_Reference;
   Defined : Definition_Result;
   Imported : Import_Result;
   Good : Boolean;
   Value : V.Value;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "Config outcome check" & Checks'Image; end if;
   end Check;
   type Context is record
      Valid : Boolean := True;
      Code : W.Status := W.Success;
      Revision : Unsigned_64 := 42;
      Calls : Natural := 0;
   end record;
   procedure Invoke
     (State : in out Context; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
   begin
      Check (Binding = 77 and then Argument = CCL.Host_Values.Integer_Constant (7));
      State.Calls := State.Calls + 1;
      O.To_Host (State.Valid, State.Code, State.Revision, Reply);
   end Invoke;
   procedure Evaluate is new CCL.Evaluation.Evaluate_With_Values (Context, Invoke);
   Source : constant String :=
     "(match (config-test.set 7) ((ConfigWrite.Committed revision) revision) " &
     "((ConfigWrite.InvalidRequest) 2) ((ConfigWrite.Denied) 3) ((ConfigWrite.Busy) 4) " &
     "((ConfigWrite.Unavailable) 5) ((ConfigWrite.Conflict) 6) ((ConfigWrite.Rejected) 7) " &
     "((ConfigWrite.Uncertain) 8))";
   Descriptor : Interface_Descriptor;
   Op : Operation_Descriptor;
   Error : Catalog_Error;
   Grants : Granted_Bindings;
   Resolved : Resolved_Operation;
   Granted : Grant_Result;
   Linked : Link_Result;
   Analysis : CCL.Language.Analysis_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Verified : V.Validated_Program;
   Validation : V.Validation_Error;
   Machine : V.Machine_State;
   VM_Result : V.Execution_Result;
   Interpreted : CCL.Language.Interpretation_Result;
   State : Context;
   type Revision_Array is array (Positive range <>) of Unsigned_64;
begin
   Check (CCL.Objects.Is_Bound (O.Schema));
   O.Publish (Catalog, Good); Check (Good);
   Types := Visible_Types (Catalog); Root := Schema_Type (Catalog, O.Key);
   Check (Root in Declared_Type);
   Define (Other, (Identifier => Named ("Before"), Form => Product, others => <>), Ref, Defined);
   Check (Defined = CCL.Types.Defined);
   Publish_Type (Shifted, Other, Ref, Ref, Imported); Check (Imported = CCL.Types.Imported);
   O.Publish (Shifted, Good); Check (Good and Schema_Type (Shifted, O.Key) /= Root);
   declare
      Description : CCL.Types.Description := Describe (Types, Root);
   begin
      Other := Visible_Types (Bad);
      Description.Parts (O.Write_Alternative'Enum_Rep (O.Committed)).Payload := Boolean_Type;
      Define (Other, Description, Ref, Defined); Check (Defined = CCL.Types.Defined);
      O.To_VM (Other, True, W.Success, 42, Value, Good); Check (not Good);
      Other := Visible_Types (Bad);
      O.To_VM (Other, True, W.Success, 42, Value, Good); Check (not Good);
   end;
   for Valid in Boolean loop
      for Code in W.Status loop
         for Revision of Revision_Array'[0, 1, W.Maximum_Revision, Unsigned_64'Last] loop
            declare
               Expected : constant O.Write_Alternative :=
                 (if not Valid then O.Uncertain
                  elsif Code = W.Success and Revision in 1 .. W.Maximum_Revision then O.Committed
                  elsif Revision /= 0 then O.Uncertain
                  else (case Code is when W.Invalid_Request => O.Invalid_Request, when W.Denied => O.Denied,
                    when W.Busy => O.Busy, when W.Unavailable => O.Unavailable, when W.Conflict => O.Conflict,
                    when W.Rejected => O.Rejected, when others => O.Uncertain));
               Native : CCL.Objects.Image;
            begin
               O.To_VM (Visible_Types (Shifted), Valid, Code, Revision, Value, Good);
               Check (Good and Value.Kind = V.Variant_Value and Value.Data_Type = Schema_Type (Shifted, O.Key));
               Check (Value.Alternative = O.Write_Alternative'Enum_Rep (Expected));
               Check (Value.Integer = (if Expected = O.Committed then Integer_64 (Revision) else 0));
               CCL.Objects.Values.From_VM (O.Schema, Visible_Types (Shifted), Value, Native, Good);
               Check (Good and CCL.Objects.Validate (Native, O.Schema));
            end;
         end loop;
      end loop;
   end loop;
   Define_Interface ("config-test", 1, 0, [11, 12, 13, 14], Descriptor, Error); Check (Error = Catalog_Valid);
   Define_Host_Operation ("set", 1,
     (Result => CCL.Host_Values.Object_Value, Result_Schema => O.Key, others => <>), Op, Error);
   Check (Error = Catalog_Valid);
   Add_Operation (Descriptor, Op, Error); Check (Error = Catalog_Valid);
   Publish (Catalog, Descriptor, Error); Check (Error = Catalog_Valid);
   Resolve (Catalog, "config-test.set", Resolved, Good); Check (Good);
   Install (Grants, Resolved, 77, Granted); Check (Granted = Grant_Added);
   CCL.Language.Analyze (Source, Catalog, Analysis);
   CCL.Compiler.Compile (Analysis, Compiled); Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog); Check (Linked = Link_Valid);
   V.Verify (Compiled.Program, Verified, Validation); Check (Validation = V.Valid);
   for Choice in O.Write_Alternative loop
      State := (Valid => Choice /= O.Uncertain, Code =>
        (case Choice is when O.Committed => W.Success, when O.Invalid_Request => W.Invalid_Request,
         when O.Denied => W.Denied, when O.Busy => W.Busy, when O.Unavailable => W.Unavailable,
         when O.Conflict => W.Conflict, when O.Rejected => W.Rejected, when O.Uncertain => W.Unavailable),
        Revision => (if Choice = O.Committed then 42 else 0), others => <>);
      Evaluate (Source, 128, Catalog, Grants, State, Interpreted);
      Check (State.Calls = 1 and Interpreted.Status = CCL.Language.Succeeded and Interpreted.Has_Value);
      Check (Interpreted.Result_Value = V.Integer_Constant
        ((if Choice = O.Committed then 42 else O.Write_Alternative'Enum_Rep (Choice))));
      V.Initialize (Verified, 128, Machine);
      V.Continue_Execution (Verified, Machine, VM_Result);
      Check (VM_Result.Status = V.Waiting_For_Host and VM_Result.Requested_Binding = 77);
      O.To_VM (Compiled.Program.Data_Types, State.Valid, State.Code, State.Revision, Value, Good);
      Check (Good);
      V.Complete_Host_Call (Verified, Machine, Value, True);
      V.Continue_Execution (Verified, Machine, VM_Result);
      Check (VM_Result.Status = V.Completed and VM_Result.Result_Value = Interpreted.Result_Value);
   end loop;
   Ada.Text_IO.Put_Line ("Typed Config write outcomes: PASS" & Checks'Image & " checks");
end Outcome_Tests;
