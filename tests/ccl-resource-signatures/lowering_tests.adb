with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Language;
with CCL.Language.Views;
with CCL.Host_Values;
with CCL.Imports;
with CCL.Types;
with CCL.Ownership;
with CCL.Resource_Policies;
with CCL.Resources;
with CCL.VM;
with CCL.VM.Native_Objects;
with CCL.Format;

procedure Lowering_Tests is
   package C renames CCL.Catalog;
   package L renames CCL.Language;
   package T renames CCL.Types;
   package H renames CCL.Host_Values;
   package O renames CCL.Ownership;
   package R renames CCL.Resources;
   package V renames CCL.VM;
   package N renames V.Native_Objects;
   use type C.Catalog_Error;
   use type C.Resource_Publication;
   use type C.Grant_Result;
   use type C.Link_Result;
   use type T.Definition_Result;
   use type T.Type_Reference;
   use type L.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   use type R.Outcome;
   use type R.Reference;
   use type V.Program;
   use type V.Value;
   use type V.Value_Kind;
   use type V.Validation_Error;
   use type V.Execution_Status;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Integer_64;
   Catalog : C.Interface_Catalog;
   Grants, Empty : C.Granted_Bindings;
   Types : T.Registry;
   Kind, Ref : T.Type_Reference;
   Defined : T.Definition_Result;
   Published : C.Resource_Publication;
   Policy : CCL.Resource_Policies.Description := (Mode => O.Must_Handle, Count => 1, others => <>);
   Description : C.Interface_Descriptor;
   Operation : C.Operation_Descriptor;
   Error : C.Catalog_Error;
   Resolved : C.Resolved_Operation;
   Found : Boolean;
   Installed : C.Grant_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Site & " lowering check" & Checks'Image; end if;
   end Check;
   procedure Add (Name : String; Parameters : C.Parameter_Count; Import : H.Import_Declaration) is
   begin
      C.Define_Host_Operation (Name, Parameters, Import, Operation, Error); Check (Error = C.Catalog_Valid);
      C.Add_Operation (Description, Operation, Error); Check (Error = C.Catalog_Valid);
   end Add;
   procedure Grant (Name : String; Binding : Interfaces.Unsigned_32) is
   begin
      C.Resolve (Catalog, Name, Resolved, Found); Check (Found);
      C.Install (Grants, Resolved, Binding, Installed); Check (Installed = C.Grant_Added);
   end Grant;
   procedure Compile (Source : String; Valid : Boolean := True) is
      Analysis : L.Analysis_Result;
   begin
      L.Analyze (Source, Catalog, Analysis);
      Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Succeeded);
      CCL.Compiler.Compile (Analysis, Compiled);
      if Valid then Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      else Check (Compiled.Status = CCL.Compiler.Ownership_Check_Failed);
      end if;
   end Compile;
   procedure Run (Source : String; Expected : Interfaces.Integer_64; Acquisitions : Natural) is
      Program, Before : V.Program;
      Linked : C.Link_Result;
      Checked : V.Validated_Program;
      Validation : V.Validation_Error;
      Machine : N.Machine;
      Step : V.Execution_Result;
      Owner : R.Registry (994);
      Session : R.Run;
      Ticket : R.Ticket;
      Outcome : R.Outcome;
      References : array (1 .. 2) of R.Reference := [others => R.No_Reference];
      Values : array (References'Range) of Interfaces.Integer_64 := [111, 222];
      Receiver : R.Reference;
      Slot : Positive;
      Acquired : Natural := 0;
      Good : Boolean;
   begin
      Compile (Source);
      Program := Compiled.Program; Before := Program;
      C.Link_Program (Empty, Compiled.Linkage, Program, Linked, Catalog);
      Check (Linked = C.Authority_Not_Granted and Program = Before);
      C.Link_Program (Grants, Compiled.Linkage, Program, Linked, Catalog);
      Check (Linked = C.Link_Valid);
      V.Verify (Program, Checked, Validation); Check (Validation = V.Valid);
      R.Start (Owner, Types, Session, Outcome); Check (Outcome = R.Succeeded);
      N.Initialize (Checked, 256, Machine);
      for Iteration in 1 .. 32 loop
         N.Continue_Execution_For (Checked, Machine, 256, Step);
         exit when Step.Status = V.Completed;
         Check (Step.Status = V.Waiting_For_Host);
         if Step.Requested_Binding = 1 then
            Check (Acquired < References'Length);
            Acquired := Acquired + 1;
            R.Reserve (Owner, Session, Kind, Ticket, Outcome); Check (Outcome = R.Succeeded);
            R.Publish (Owner, Ticket, True, References (Acquired), Outcome); Check (Outcome = R.Succeeded);
            N.Complete_Resource (Checked, Machine, Owner, References (Acquired), Good); Check (Good);
         else
            Check (Step.Requested_Binding in 2 .. 5 and Step.Request_Owned);
            Receiver := (if Step.Requested_Binding in 4 | 5 then Step.Request_Receiver
                         else Step.Request_Argument.Resource);
            Slot := (if Receiver = References (1) then 1 else 2);
            Check (Receiver = References (Slot));
            R.Begin_Use (Owner, Receiver, Kind, Ticket, Outcome);
            Check (Outcome = R.Succeeded);
            N.Acknowledge_Host_Submission (Checked, Machine, True);
            R.Finish_Use (Owner, Ticket, Step.Requested_Binding /= 3, Outcome); Check (Outcome = R.Succeeded);
            if Step.Requested_Binding in 2 | 5 then
               N.Complete_Scalar (Checked, Machine,
                 V.Integer_Constant (Values (Slot)), True);
            elsif Step.Requested_Binding = 4 then
               Check (Step.Request_Argument.Kind = V.Integer_Value);
               Values (Slot) := Step.Request_Argument.Integer;
               N.Complete_Scalar (Checked, Machine, V.Integer_Constant (0), True);
            else
               N.Complete_Scalar (Checked, Machine, V.Integer_Constant (0), True);
               R.Reclaim (Owner, Ticket, Outcome); Check (Outcome = R.Succeeded);
            end if;
         end if;
      end loop;
      Check (Step.Status = V.Completed and Step.Has_Value and Step.Result_Value = V.Integer_Constant (Expected));
      Check (Acquired = Acquisitions and R.Empty (Owner));
   end Run;
   Single : constant String :=
     "(let ((a (collections.open))) (let ((value (collections.get a))) " &
     "(let ((closed (collections.close a))) value)))";
   Write_Read : constant String :=
     "(let ((a (collections.open))) (let ((written (collections.set a 42))) " &
     "(let ((value (collections.read a))) (let ((closed (collections.close a))) value))))";
begin
   T.Define (Types, (Identifier => T.Named ("Collection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Kind, Defined);
   Check (Defined = T.Defined);
   Policy.Dispositions (0) := (Verb => 1, Effect => O.Consume, others => <>);
   C.Publish_Resource (Catalog, Types, Kind, Policy, Ref, Published); Check (Published = C.Resource_Published);
   C.Define_Interface ("collections", 1, 0, [others => 2], Description, Error); Check (Error = C.Catalog_Valid);
   Add ("open", 0, (Result => H.Resource_Value, Result_Resource => T.Named ("Collection"), others => <>));
   Add ("get", 1, (Argument => H.Resource_Value, Argument_Resource => T.Named ("Collection"),
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RO_Argument, others => <>));
   Add ("close", 1, (Argument => H.Resource_Value, Argument_Resource => T.Named ("Collection"),
     Ownership_Argument => True, Transfer => CCL.Imports.Move_Argument,
     Success_Verb => 1, Failure_Verb => 1, others => <>));
   Add ("set", 1, (Receiver_Resource => T.Named ("Collection"),
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RW_Argument, others => <>));
   Add ("read", 0, (Receiver_Resource => T.Named ("Collection"),
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RO_Argument, others => <>));
   C.Publish (Catalog, Description, Error); Check (Error = C.Catalog_Valid);
   Grant ("collections.open", 1); Grant ("collections.get", 2); Grant ("collections.close", 3);
   Grant ("collections.set", 4); Grant ("collections.read", 5);
   Run (Single, 111, 1);
   Run ("(collections.close (collections.open))", 0, 1);
   Run ("(let ((a (collections.open))) (let ((b a)) (collections.close b)))", 0, 1);
   Run ("(let ((a (collections.open))) (let ((b (collections.open))) " &
     "(let ((value (+ (collections.get a) (collections.get b)))) " &
     "(let ((c (collections.close a))) (let ((d (collections.close b))) value)))))", 333, 2);
   Run (Write_Read, 42, 1);
   Run ("(let ((a (collections.open))) (let ((b (collections.open))) " &
     "(let ((wa (collections.set a 20))) (let ((wb (collections.set b 22))) " &
     "(let ((value (+ (collections.read a) (collections.read b)))) " &
     "(let ((ca (collections.close a))) (let ((cb (collections.close b))) value)))))))", 42, 2);
   Compile ("(collections.set (collections.open) 42)", False);
   Compile ("(let ((a (collections.open))) (collections.set a (collections.close a)))", False);
   declare
      Basic, Lisp : L.Views.Conversion;
      use type L.Views.Conversion_Status;
   begin
      L.Views.Convert (Write_Read, L.Views.Lisp, L.Views.Basic, Catalog, Basic);
      Check (Basic.Status = L.Views.Converted);
      L.Views.Convert (Basic.Rendered.Data (1 .. Basic.Rendered.Length), L.Views.Basic,
        L.Views.Lisp, Catalog, Lisp);
      Check (Lisp.Status = L.Views.Converted);
      Run (Lisp.Rendered.Data (1 .. Lisp.Rendered.Length), 42, 1);
   end;
   Compile ("(let ((a (collections.open))) 42)", False);
   Compile ("(collections.get (collections.open))", False);
   Compile ("(let ((a (collections.open))) (let ((b (collections.close a))) (collections.close a)))", False);
   Compile ("(let ((a (collections.open))) (let ((b a)) (+ (collections.close a) (collections.close b))))", False);
   Compile ("(let ((a (collections.open))) (if true (collections.close a) 0))", False);

   Compile (Single);
   declare
      Bytes : CCL.Format.Byte_Array;
      Size : CCL.Format.Module_Length;
      Error : CCL.Format.Format_Error;
      Validation : V.Validation_Error;
      use type CCL.Format.Format_Error;
   begin
      CCL.Format.Encode (Compiled.Program, Compiled.Linkage,
        (Fuel => 256, Memory => 4096, In_Flight => 1), Bytes, Size, Error, Validation);
      Check (Error = CCL.Format.Invalid_Linkage and Size = 0);
   end;
   declare
      Program, Before : V.Program;
      Linked : C.Link_Result;
      Changed : C.Interface_Catalog;
      Shifted : T.Registry;
      Dummy : T.Type_Reference;
   begin
      -- Every rejection must preserve every runtime binding, not just the
      -- import where the mismatch was noticed.
      for Attack in 1 .. 6 loop
         Program := Compiled.Program;
         case Attack is
            when 1 => Program.Types (1).Mode := O.Move_Only;
            when 2 => Program.Types (1).Dispositions_Length := 0;
            when 3 => Program.Types (1).Dispositions (0).Effect := O.Transfer;
            when 4 => Program.Imports (0).Result_Type_Tag := 0;
            when 5 => Program.Local_Types (0) := 0;
            when 6 => Program.Imports (1).Local := 1;
         end case;
         Before := Program;
         C.Link_Program (Grants, Compiled.Linkage, Program, Linked, Catalog);
         Check (Linked = C.Import_Contract_Mismatch and Program = Before);
      end loop;
      -- Catalog-local type numbers need not equal the compiler snapshot.
      T.Define (Shifted, (Identifier => T.Named ("Prefix"), Form => T.Product, others => <>), Dummy, Defined);
      Check (Defined = T.Defined);
      T.Define (Shifted, T.Describe (Types, Kind), Ref, Defined); Check (Defined = T.Defined and Ref /= Kind);
      declare
         Imported : T.Import_Result;
         use type T.Import_Result;
      begin
         C.Publish_Type (Changed, Shifted, Dummy, Dummy, Imported); Check (Imported = T.Imported);
      end;
      C.Publish_Resource (Changed, Shifted, Ref, Policy, Dummy, Published); Check (Published = C.Resource_Published);
      Program := Compiled.Program;
      C.Link_Program (Grants, Compiled.Linkage, Program, Linked, Changed);
      Check (Linked = C.Link_Valid);
   end;
   Compile (Write_Read);
   declare
      Program, Before : V.Program;
      Linked : C.Link_Result;
      Receiver_Import : V.Import_Index := 0;
   begin
      for I in 0 .. Compiled.Program.Imports_Length - 1 loop
         if V.Has_Receiver (Compiled.Program.Imports (I)) then
            Receiver_Import := I; exit;
         end if;
      end loop;
      Check (V.Has_Receiver (Compiled.Program.Imports (Receiver_Import)));
      for Attack in 1 .. 5 loop
         Program := Compiled.Program;
         case Attack is
            when 1 => Program.Imports (Receiver_Import).Receiver_Data_Type := T.Integer_Type;
            when 2 => Program.Imports (Receiver_Import).Receiver_Data_Type := T.Invalid_Type;
            when 3 => Program.Imports (Receiver_Import).Ownership_Argument := False;
            when 4 => Program.Imports (Receiver_Import).Transfer := CCL.Imports.Copy_Argument;
            when 5 => Program.Imports (Receiver_Import).Argument := V.Boolean_Value;
         end case;
         Before := Program;
         C.Link_Program (Grants, Compiled.Linkage, Program, Linked, Catalog);
         Check (Linked = C.Import_Contract_Mismatch and Program = Before);
      end loop;
      declare
         Bytes : CCL.Format.Byte_Array;
         Size : CCL.Format.Module_Length;
         Error : CCL.Format.Format_Error;
         Validation : V.Validation_Error;
         use type CCL.Format.Format_Error;
      begin
         CCL.Format.Encode (Compiled.Program, Compiled.Linkage,
           (Fuel => 256, Memory => 4096, In_Flight => 1), Bytes, Size, Error, Validation);
         Check (Error = CCL.Format.Invalid_Linkage and Size = 0);
      end;
   end;
   Ada.Text_IO.Put_Line ("Source resource ownership/linkage: PASS" & Checks'Image & " checks");
end Lowering_Tests;
