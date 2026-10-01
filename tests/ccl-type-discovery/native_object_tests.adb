with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Catalog; use CCL.Catalog;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Compiler;
with CCL.Format;
with Module_Patches;
with CCL.VM; use CCL.VM;
with CCL.VM.Native_Objects;

procedure Native_Object_Tests is
   package N renames CCL.VM.Native_Objects;
   package F renames CCL.Format;
   Types : Registry;
   Record_Type : Type_Reference;
   Declared : Definition_Result;
   Contract, Wrong : Binding;
   Catalog : Interface_Catalog;
   Grants, No_Grants : Granted_Bindings;
   Interface_Item : Interface_Descriptor;
   Op : Operation_Descriptor;
   Error : Catalog_Error;
   Published : CCL.Objects.Catalog.Publication_Result;
   Resolved : Resolved_Operation;
   Installed : Grant_Result;
   Good : Boolean;
   Analysis : CCL.Language.Analysis_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Code : Program;
   Checked : Validated_Program;
   Validity : Validation_Error;
   Linked : Link_Result;
   Machine : N.Machine;
   Scalar_Machine : Machine_State;
   Outcome : Execution_Result;
   Inspection : Inspection_Snapshot;
   Input, Output, Corrupt : CCL.Objects.Image;
   Built : Build_Result;
   Bytes : F.Byte_Array;
   Size : F.Module_Length;
   Format_Error : F.Format_Error;
   Links : Linkage_Table;
   Limits : F.Resource_Limits;
   Checks : Natural := 0;
   use type CCL.Objects.Catalog.Publication_Result;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   use type F.Format_Error;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "native object VM check" & Checks'Image; end if;
   end Check;
   procedure Compile (Source : String) is
   begin
      CCL.Language.Analyze (Source, Catalog, Analysis);
      Check (CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      F.Encode (Compiled.Program, Compiled.Linkage, (Fuel => 256, others => <>), Bytes, Size, Format_Error, Validity);
      if Format_Error /= F.Format_Valid then
         Ada.Text_IO.Put_Line (Format_Error'Image & " " & Validity'Image);
      end if;
      Check (Format_Error = F.Format_Valid);
      F.Decode (Bytes, Size, Code, Links, Limits, Format_Error, Validity);
      Check (Format_Error = F.Format_Valid and Validity = Valid);
      declare
         Changed : F.Byte_Array := Bytes;
         Changed_Size : F.Module_Length := Size;
         Patched : Boolean;
      begin
         Module_Patches.Replace
           (Changed, Changed_Size, [16#44#, 16#43#, 16#43#, 16#4C#, 16#42#, F.FORMAT_VERSION],
            [16#44#, 16#43#, 16#43#, 16#4C#, 16#42#, 5], Patched);
         Check (Patched);
         F.Decode (Changed, Changed_Size, Code, Links, Limits, Format_Error, Validity);
         Check (Format_Error = F.Unsupported_Version);
      end;
      F.Decode (Bytes, Size, Code, Links, Limits, Format_Error, Validity);
      Check (Format_Error = F.Format_Valid and Validity = Valid);
      Link_Program (No_Grants, Links, Code, Linked, Catalog);
      Check (Linked /= Link_Valid);
      Link_Program (Grants, Links, Code, Linked, Catalog); Check (Linked = Link_Valid);
      Verify (Code, Checked, Validity); Check (Validity = Valid);
   end Compile;
   procedure Advance is
   begin N.Continue_Execution_For (Checked, Machine, 256, Outcome); end Advance;
begin
   Define (Types, (Identifier => Named ("Settings"), Form => Product, Count => 2,
     Parts => [1 => (Named ("title"), CCL.Types.String_Type),
               2 => (Named ("enabled"), CCL.Types.Boolean_Type), others => <>]), Record_Type, Declared);
   Check (Declared = Defined);
   Bind (Types, Record_Type, [11, 12, 13, 14], Contract, Good); Check (Good);
   Publish_Schema (Catalog, Contract, Published); Check (Published = CCL.Objects.Catalog.Published);
   Define_Interface ("objects", 1, 0, [101, 102, 103, 104], Interface_Item, Error); Check (Error = Catalog_Valid);
   Define_Host_Operation ("get", 0,
     (Result => CCL.Host_Values.Object_Value, Result_Schema => Identity (Contract), others => <>), Op, Error);
   Check (Error = Catalog_Valid);
   Add_Operation (Interface_Item, Op, Error); Check (Error = Catalog_Valid);
   Define_Host_Operation ("echo", 1,
     (Argument => CCL.Host_Values.Object_Value, Argument_Schema => Identity (Contract),
      Result => CCL.Host_Values.Object_Value, Result_Schema => Identity (Contract), others => <>), Op, Error);
   Check (Error = Catalog_Valid);
   Add_Operation (Interface_Item, Op, Error); Check (Error = Catalog_Valid);
   Publish (Catalog, Interface_Item, Error); Check (Error = Catalog_Valid);
   Resolve (Catalog, "objects.get", Resolved, Good); Check (Good);
   Install (Grants, Resolved, 77, Installed); Check (Installed = Grant_Added);
   Resolve (Catalog, "objects.echo", Resolved, Good); Check (Good);
   Install (Grants, Resolved, 78, Installed); Check (Installed = Grant_Added);
   Input := Empty (Contract);
   Append (Input, Product_Cell (2), Built); Check (Built = Added);
   Append_Text (Input, String'(1 .. Maximum_Text_Bytes => 'x'), Built); Check (Built = Added);
   Append (Input, Boolean_Cell (True), Built); Check (Built = Added);
   Compile ("(let ((settings (objects.get))) (objects.echo settings))");
   N.Initialize (Checked, 256, Machine);
   Check (N.Snapshot (Machine).Steps = 0 and not N.Snapshot (Machine).Terminal);
   Advance; Check (Outcome.Status = Waiting_For_Host and Outcome.Requested_Binding = 77);
   N.Inspect (Checked, Machine, Inspection);
   Check (Inspection.Machine = N.Snapshot (Machine) and Inspection.Machine.Waiting);
   Check (Inspection.Waiting_Result_Kind = Object_Value and Inspection.Stack_Length = 0);
   N.Complete_Object (Checked, Machine, Contract, Input, True);
   Advance; Check (Outcome.Status = Waiting_For_Host and Outcome.Requested_Binding = 78);
   N.Inspect (Checked, Machine, Inspection);
   Check (Inspection.Waiting_Argument.Kind = Object_Value and Inspection.Waiting_Argument.Object /= 0);
   N.Export_Argument (Checked, Machine, Contract, Output, Good); Check (Good and Output = Input);
   Input.Text (1) := 'y'; -- mutate the original host buffer, not just a copy
   N.Export_Argument (Checked, Machine, Contract, Output, Good);
   Check (Good and Output.Text (1) = 'x');
   Input.Text (1) := 'x';
   N.Export_Argument (Checked, Machine, Contract, Output, Good); Check (Good and Output = Input);
   N.Complete_Object (Checked, Machine, Contract, Input, True);
   Advance; Check (Outcome.Status = Completed and Outcome.Has_Value);
   N.Export_Result (Checked, Machine, Contract, Output, Good); Check (Good and Output = Input);
   Corrupt := Input; Corrupt.Text (1) := 'y';
   N.Complete_Object (Checked, Machine, Contract, Corrupt, True);
   N.Export_Result (Checked, Machine, Contract, Output, Good); Check (Good and Output = Input);
   N.Export_Argument (Checked, Machine, Contract, Output, Good); Check (not Good);
   Bind (Types, CCL.Types.String_Type, Identity (Contract), Wrong, Good); Check (Good);
   N.Export_Result (Checked, Machine, Wrong, Output, Good); Check (not Good and Output = Empty (Wrong));
   N.Stop (Machine);
   N.Inspect (Checked, Machine, Inspection);
   Check (Inspection.Machine.Terminal and Inspection.Stack_Length = 0 and Inspection.Locals_Length = 0);
   N.Export_Result (Checked, Machine, Contract, Output, Good); Check (not Good);
   for Fault in 1 .. 3 loop
      N.Initialize (Checked, 256, Machine); Advance;
      Corrupt := Input;
      if Fault = 1 then Corrupt.Reserved := 1; end if;
      N.Complete_Object (Checked, Machine, (if Fault = 2 then Wrong else Contract), Corrupt, Fault /= 3);
      Advance; Check (Outcome.Status = Host_Call_Failed and not Outcome.Has_Value);
   end loop;
   -- The scalar API cannot manufacture references into a native store.
   Initialize (Checked, 256, Scalar_Machine);
   Continue_Execution (Checked, Scalar_Machine, Outcome); Check (Outcome.Status = Waiting_For_Host);
   Complete_Host_Call (Checked, Scalar_Machine,
     (Kind => Object_Value, Data_Type => Code.Imports (0).Result_Data_Type, Object => 1, others => <>), True);
   Continue_Execution (Checked, Scalar_Machine, Outcome); Check (Outcome.Status = Invalid_Bytecode);
   declare
      Injected : Local_Value_Array := [others => <>];
      Candidate : Program := Code;
      Initial : Validated_Program;
   begin
      Candidate.Imports_Length := 0;
      Candidate.Length := 2;
      Candidate.Code := [0 => (Op => Copy_Local, others => <>), others => (others => <>)];
      Candidate.Locals_Length := 1;
      Candidate.Dynamic_Locals_Length := 0;
      Candidate.Types_Length := 1;
      Candidate.Local_Kinds (0) := Object_Value;
      Candidate.Local_Data_Types (0) := Code.Imports (0).Result_Data_Type;
      Verify (Candidate, Initial, Validity); Check (Validity = Valid);
      Injected (0) := (Kind => Object_Value, Data_Type => Candidate.Local_Data_Types (0), Object => 1, others => <>);
      Initialize_With_Locals (Initial, 32, Injected, 1, Scalar_Machine, Good); Check (not Good);
   end;
   declare
      function Echoes (Count : Natural) return String is
        (if Count = 0 then "(objects.get)" else "(objects.echo " & Echoes (Count - 1) & ")");
      Calls : Natural := 0;
   begin
      Compile (Echoes (MAX_OBJECT_VALUES));
      N.Initialize (Checked, 256, Machine);
      loop
         Advance;
         exit when Outcome.Status /= Waiting_For_Host;
         Calls := Calls + 1;
         N.Complete_Object (Checked, Machine, Contract, Input, True);
      end loop;
      Check (Outcome.Status = Object_Storage_Exhausted and Calls = MAX_OBJECT_VALUES);
      N.Export_Result (Checked, Machine, Contract, Output, Good); Check (not Good);
      N.Stop (Machine);
   end;
   -- Projection is a normal instruction: one fuel step, no host callback and
   -- no extra snapshot. A field can be unboxed or exported as an owned subtree.
   Compile ("(field (objects.get) enabled)");
   N.Initialize (Checked, 32, Machine);
   N.Continue_Execution_For (Checked, Machine, 1, Outcome); Check (Outcome.Status = Paused and Outcome.Steps = 1);
   N.Continue_Execution_For (Checked, Machine, 1, Outcome); Check (Outcome.Status = Waiting_For_Host and Outcome.Steps = 2);
   declare
      Before : constant Machine_Snapshot := N.Snapshot (Machine);
   begin
      for Attempt in 1 .. 3 loop
         N.Inspect (Checked, Machine, Inspection);
         Check (Inspection.Machine = Before and N.Snapshot (Machine) = Before);
      end loop;
   end;
   N.Complete_Object (Checked, Machine, Contract, Input, True);
   N.Continue_Execution_For (Checked, Machine, 1, Outcome); Check (Outcome.Status = Paused and Outcome.Steps = 3);
   N.Continue_Execution_For (Checked, Machine, 1, Outcome);
   Check (Outcome.Status = Completed and Outcome.Steps = 4 and Outcome.Result_Value = Boolean_Constant (True));
   N.Initialize (Checked, 32, Machine);
   Advance; Check (Outcome.Status = Waiting_For_Host);
   N.Stop (Machine);
   Check (N.Snapshot (Machine).Status = Stopped);
   N.Inspect (Checked, Machine, Inspection);
   Check (Inspection.Stack_Length = 0 and Inspection.Locals_Length = 0);
   N.Complete_Object (Checked, Machine, Contract, Input, True);
   Check (N.Snapshot (Machine).Status = Stopped);
   N.Initialize (Checked, 32, Machine);
   Check (N.Snapshot (Machine).Steps = 0 and not N.Snapshot (Machine).Terminal);
   declare
      Original : constant Program := Code;
      Bad : Program;
      Rejected : Validated_Program;
   begin
      for Fault in 1 .. 5 loop
         Bad := Original;
         case Fault is
            when 1 => Bad.Code (2).Immediate := 0;
            when 2 => Bad.Code (2).Immediate := 3;
            when 3 => Bad.Code (2).Immediate := Integer_64'Last;
            when 4 => Bad.Code (2).Data_Type := CCL.Types.String_Type;
            when others => Bad.Code (2).Alternative := 1;
         end case;
         Verify (Bad, Rejected, Validity); Check (Validity = Invalid_Data_Type and not Is_Valid (Rejected));
      end loop;
   end;
   Compile ("(field (objects.get) title)");
   N.Initialize (Checked, 32, Machine); Advance;
   N.Complete_Object (Checked, Machine, Contract, Input, True); Advance;
   Check (Outcome.Status = Completed);
   Bind (Types, CCL.Types.String_Type, [21, 22, 23, 24], Wrong, Good); Check (Good);
   N.Export_Result (Checked, Machine, Wrong, Output, Good);
   Corrupt := Empty (Wrong);
   Append_Text (Corrupt, String'(1 .. Maximum_Text_Bytes => 'x'), Built); Check (Built = Added);
   Check (Good and Output = Corrupt);
   declare
      function Fields (Count : Positive) return String is
        (if Count = 1 then "(if (field s enabled) 1 0)"
         else "(+ (if (field s enabled) 1 0) " & Fields (Count - 1) & ")");
   begin
      Compile ("(let ((s (objects.get))) " & Fields (MAX_OBJECT_VALUES + 1) & ")");
      N.Initialize (Checked, 256, Machine); Advance;
      N.Complete_Object (Checked, Machine, Contract, Input, True); Advance;
      Check (Outcome.Status = Completed and Outcome.Result_Value = Integer_Constant (MAX_OBJECT_VALUES + 1));
   end;
   -- Nested sum payloads reuse the owner's cursor and can themselves be
   -- projected. Nullary alternatives must not leave a payload on the stack.
   declare
      Envelope : Type_Reference;
   begin
      Define (Types, (Identifier => Named ("Envelope"), Form => Sum, Count => 2,
        Parts => [1 => (Named ("Found"), Record_Type), 2 => (Named ("Missing"), CCL.Types.Unit_Type), others => <>]),
        Envelope, Declared); Check (Declared = Defined);
      Bind (Types, Envelope, [31, 32, 33, 34], Contract, Good); Check (Good);
      Catalog := Empty_Catalog;
      Initialize (Grants);
      Publish_Schema (Catalog, Contract, Published); Check (Published = CCL.Objects.Catalog.Published);
      Define_Interface ("objects", 1, 0, [101, 102, 103, 104], Interface_Item, Error); Check (Error = Catalog_Valid);
      Define_Host_Operation ("get", 0,
        (Result => CCL.Host_Values.Object_Value, Result_Schema => Identity (Contract), others => <>), Op, Error);
      Check (Error = Catalog_Valid);
      Add_Operation (Interface_Item, Op, Error); Check (Error = Catalog_Valid);
      Publish (Catalog, Interface_Item, Error); Check (Error = Catalog_Valid);
      Resolve (Catalog, "objects.get", Resolved, Good); Check (Good);
      Install (Grants, Resolved, 77, Installed); Check (Installed = Grant_Added);
      Compile ("(match (objects.get) ((Envelope.Found s) (field s enabled)) ((Envelope.Missing) false))");
      for Present in Boolean loop
         Input := Empty (Contract);
         Append (Input, Variant_Cell (if Present then 1 else 2), Built); Check (Built = Added);
         if Present then
            Append (Input, Product_Cell (2), Built); Check (Built = Added);
            Append_Text (Input, "hello", Built); Check (Built = Added);
            Append (Input, Boolean_Cell (True), Built); Check (Built = Added);
         else Append (Input, Unit_Cell, Built); Check (Built = Added);
         end if;
         N.Initialize (Checked, 64, Machine); Advance; Check (Outcome.Status = Waiting_For_Host);
         N.Complete_Object (Checked, Machine, Contract, Input, True); Advance;
         Check (Outcome.Status = Completed and Outcome.Result_Value = Boolean_Constant (Present));
      end loop;
   end;
   -- Native scalar images use the same typed completion API but do not spend
   -- aggregate snapshot slots. Preflight observes state without executing it.
   for Boolean_Result in Boolean loop
      Code := (Length => 3 * (MAX_OBJECT_VALUES + 1), Imports_Length => 1, others => <>);
      Code.Imports (0) := (Result => (if Boolean_Result then Boolean_Value else Integer_Value),
        Authority => Observe_Authority, Binding => 77, others => <>);
      for Call in 0 .. MAX_OBJECT_VALUES loop
         Code.Code (Instruction_Index (3 * Call)) := (Op => Push_Integer, others => <>);
         Code.Code (Instruction_Index (3 * Call + 1)) := (Op => Invoke_Import, others => <>);
         Code.Code (Instruction_Index (3 * Call + 2)) := (Op => (if Call = MAX_OBJECT_VALUES then Halt else Drop), others => <>);
      end loop;
      Verify (Code, Checked, Validity); Check (Validity = Valid);
      Bind (Types, (if Boolean_Result then CCL.Types.Boolean_Type else CCL.Types.Integer_Type),
        [51, 52, 53, 54], Contract, Good); Check (Good);
      Bind (Types, (if Boolean_Result then CCL.Types.Integer_Type else CCL.Types.Boolean_Type),
        [61, 62, 63, 64], Wrong, Good); Check (Good);
      Input := Empty (Contract);
      Append (Input, (if Boolean_Result then Boolean_Cell (True) else Integer_Cell (Integer_64'First)), Built);
      Check (Built = Added);
      N.Initialize (Checked, 256, Machine);
      Check (N.Pending_Call (Checked, Machine).Status = No_Result and
        not N.Accepts_Object_Result (Checked, Machine, Contract));
      for Call in 0 .. MAX_OBJECT_VALUES loop
         Advance; Check (Outcome.Status = Waiting_For_Host);
         Check (N.Pending_Call (Checked, Machine) = Outcome);
         Check (N.Accepts_Object_Result (Checked, Machine, Contract) and
           not N.Accepts_Object_Result (Checked, Machine, Wrong));
         N.Complete_Object (Checked, Machine, Contract, Input, True);
      end loop;
      Advance;
      Check (Outcome.Status = Completed and Outcome.Result_Value =
        (if Boolean_Result then Boolean_Constant (True) else Integer_Constant (Integer_64'First)));
      Check (N.Pending_Call (Checked, Machine).Status = No_Result);
      N.Stop (Machine);
   end loop;
   Ada.Text_IO.Put_Line ("Native object VM: PASS" & Checks'Image & " checks");
end Native_Object_Tests;
