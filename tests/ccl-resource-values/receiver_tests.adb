with Interfaces;
with Ada.Text_IO;
with CCL.Types;
with CCL.Resources;
with CCL.Ownership;
with CCL.Imports;
with CCL.Objects;
with CCL.Host_Values;
with CCL.VM.Native_Objects;

procedure Receiver_Tests is
   package T renames CCL.Types;
   package R renames CCL.Resources;
   package O renames CCL.Objects;
   package V renames CCL.VM;
   package N renames V.Native_Objects;
   use V;
   use type T.Definition_Result;
   use type R.Outcome;
   use type R.Reference;
   use type O.Build_Result;
   use type O.Image;
   Types : T.Registry;
   Record_Type, Collection, Other : T.Type_Reference;
   Defined : T.Definition_Result;
   Owner : R.Registry (801);
   Session : R.Run;
   Factory, Call : R.Ticket;
   Ref : R.Reference;
   Outcome : R.Outcome;
   Candidate, Template : Program;
   Checked : Validated_Program;
   Error : Validation_Error;
   Machine : N.Machine;
   Step : Execution_Result;
   Contract : O.Binding;
   Input, Output : O.Image;
   Built : O.Build_Result;
   Good : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why; end if;
   end Check;
   procedure Advance is
   begin
      N.Continue_Execution_For (Checked, Machine, 32, Step);
   end Advance;
begin
   T.Define (Types, (Identifier => T.Named ("Setting"), Form => T.Product,
     Count => 2, Parts => [1 => (T.Named ("Count"), T.Integer_Type),
       2 => (T.Named ("Enabled"), T.Boolean_Type), others => <>]), Record_Type, Defined);
   Check (Defined = T.Defined, "define native data record");
   T.Define (Types, (Identifier => T.Named ("Collection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), Record_Type), others => <>]), Collection, Defined);
   Check (Defined = T.Defined, "define resource parameterized by record");
   T.Define (Types, (Identifier => T.Named ("Other"), Form => T.Resource, others => <>), Other, Defined);
   Check (Defined = T.Defined, "define distinct receiver type");
   O.Bind (Types, Record_Type, [5, 6, 7, 8], Contract, Good);
   Check (Good, "bind persistable data only");
   Input := O.Empty (Contract);
   O.Append (Input, O.Product_Cell (2), Built); Check (Built = O.Added, "record head");
   O.Append (Input, O.Integer_Cell (42), Built); Check (Built = O.Added, "record integer");
   O.Append (Input, O.Boolean_Cell (True), Built); Check (Built = O.Added, "record boolean");
   Check (O.Validate (Input, Contract), "valid native data image");
   R.Start (Owner, Types, Session, Outcome); Check (Outcome = R.Succeeded, "start registry");
   R.Reserve (Owner, Session, Collection, Factory, Outcome); Check (Outcome = R.Succeeded, "reserve acquisition");
   R.Publish (Owner, Factory, True, Ref, Outcome); Check (Outcome = R.Succeeded, "publish resource");

   Candidate.Data_Types := Types;
   Candidate.Types_Length := 2;
   Candidate.Types (1).Mode := CCL.Ownership.Must_Handle;
   Candidate.Types (1).Dispositions_Length := 1;
   Candidate.Types (1).Dispositions (0) := (Verb => 1, Effect => CCL.Ownership.Consume, Next_Type => 0);
   Candidate.Locals_Length := 1;
   Candidate.Dynamic_Locals_Length := 1;
   Candidate.Local_Types (0) := 1;
   Candidate.Local_Kinds (0) := Resource_Value;
   Candidate.Local_Data_Types (0) := Collection;
   Candidate.Imports_Length := 4;
   Candidate.Imports (0) := (Result => Resource_Value, Result_Data_Type => Collection,
     Result_Type_Tag => 1, Binding => 1, others => <>);
   Candidate.Imports (1) := (Receiver_Data_Type => Collection,
     Result => Object_Value, Result_Data_Type => Record_Type,
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RO_Argument,
     Binding => 2, others => <>);
   Candidate.Imports (2) := (Receiver_Data_Type => Collection,
     Argument => Object_Value, Argument_Data_Type => Record_Type,
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RW_Argument,
     Binding => 3, others => <>);
   Candidate.Imports (3) := (Argument => Resource_Value, Argument_Data_Type => Collection,
     Ownership_Argument => True, Transfer => CCL.Imports.Move_Argument,
     Success_Verb => 1, Failure_Verb => 1, Binding => 4, others => <>);
   Candidate.Length := 9;
   Candidate.Code (0) := (Op => Push_Integer, others => <>);
   Candidate.Code (1) := (Op => Invoke_Import, Import => 0, others => <>);
   Candidate.Code (2) := (Op => Initialize_Local, others => <>);
   Candidate.Code (3) := (Op => Push_Integer, others => <>);
   Candidate.Code (4) := (Op => Invoke_Import, Import => 1, others => <>);
   Candidate.Code (5) := (Op => Invoke_Import, Import => 2, others => <>);
   Candidate.Code (6) := (Op => Drop, others => <>);
   Candidate.Code (7) := (Op => Invoke_Import, Import => 3, others => <>);
   Candidate.Code (8) := (Op => Halt, others => <>);
   Template := Candidate;
   Verify (Candidate, Checked, Error); Check (Error = Valid, "admit resource get/set/close: " & Error'Image);
   N.Initialize (Checked, 32, Machine); Advance;
   Check (Step.Status = Waiting_For_Host and then not Step.Request_Owned, "factory call");
   N.Complete_Resource (Checked, Machine, Owner, Ref, Good); Check (Good, "acquire on native object machine");
   Advance;
   Check (Step.Status = Waiting_For_Host and then Step.Requested_Import = 1 and then
     Step.Request_Receiver = Ref and then Step.Request_Owned and then
     Step.Request_Argument = Integer_Constant (0), "get separates receiver from ordinary argument");
   Check (N.Pending_Call (Checked, Machine) = Step, "pending snapshot retains receiver");
   Check (N.Accepts_Object_Result (Checked, Machine, Contract), "preflight before service effect");
   Check (not N.Ready_For_Completion (Checked, Machine), "offered borrow not yet acquired");
   N.Complete_Object (Checked, Machine, Contract, Input, True);
   Check (N.Pending_Call (Checked, Machine) = Step, "premature object completion changes nothing");
   R.Begin_Use (Owner, Step.Request_Receiver, Collection, Call, Outcome);
   Check (Outcome = R.Succeeded, "host validates live get receiver");
   N.Acknowledge_Host_Submission (Checked, Machine, True);
   Check (N.Ready_For_Completion (Checked, Machine), "acknowledged borrow accepts completion");
   R.Finish_Use (Owner, Call, True, Outcome); Check (Outcome = R.Succeeded, "host completes get");
   N.Complete_Object (Checked, Machine, Contract, Input, True);
   Advance;
   Check (Step.Status = Waiting_For_Host and then Step.Requested_Import = 2 and then
     Step.Request_Receiver = Ref and then Step.Request_Argument.Kind = Object_Value,
     "set retains receiver while passing aggregate on operand stack");
   N.Export_Argument (Checked, Machine, Contract, Output, Good);
   Check (Good and then Output = Input, "set exports exactly the native record, no receiver serialization");
   R.Begin_Use (Owner, Step.Request_Receiver, Collection, Call, Outcome);
   Check (Outcome = R.Succeeded, "host validates live set receiver");
   N.Acknowledge_Host_Submission (Checked, Machine, True);
   R.Finish_Use (Owner, Call, True, Outcome); Check (Outcome = R.Succeeded, "host completes set");
   N.Complete_Scalar (Checked, Machine, Integer_Constant (0), True);
   Advance;
   Check (Step.Status = Waiting_For_Host and then Step.Requested_Import = 3 and then
     Step.Request_Receiver = R.No_Reference and then Step.Request_Argument.Resource = Ref,
     "local-only close clears previous separate receiver");
   N.Export_Argument (Checked, Machine, Contract, Output, Good);
   Check (not Good, "cannot export owned resource as data");
   R.Begin_Use (Owner, Ref, Collection, Call, Outcome); Check (Outcome = R.Succeeded, "host begins close");
   N.Acknowledge_Host_Submission (Checked, Machine, True);
   R.Finish_Use (Owner, Call, False, Outcome); Check (Outcome = R.Succeeded, "host retires collection");
   N.Complete_Scalar (Checked, Machine, Integer_Constant (0), True);
   Advance; Check (Step.Status = Completed and then not R.Current (Owner, Ref), "complete without leaked owner");
   Check (not N.Ready_For_Completion (Checked, Machine), "cannot complete twice");

   Candidate := Template; Candidate.Imports (1).Ownership_Argument := False;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Import, "receiver requires checked ownership");
   Candidate := Template; Candidate.Imports (1).Transfer := CCL.Imports.Copy_Argument;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Import, "receiver cannot copy authority");
   Candidate := Template; Candidate.Imports (1).Receiver_Data_Type := Record_Type;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Import, "data is not a receiver");
   Candidate := Template; Candidate.Imports (1).Receiver_Data_Type := Other;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Import, "receiver must match local nominal type");
   Candidate := Template; Candidate.Imports (1).Argument := Boolean_Value;
   Verify (Candidate, Checked, Error); Check (Error = Type_Mismatch, "receiver does not bypass data typing");
   Candidate := Template; Candidate.Code (3) := (Op => Jump, Target => 4, others => <>);
   Verify (Candidate, Checked, Error); Check (Error = Stack_Underflow, "receiver call must provide data operand");
   Candidate := Template; Candidate.Imports (2).Argument := Resource_Value;
   Candidate.Imports (2).Argument_Data_Type := Collection;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Import, "resource cannot masquerade as separate data");
   Candidate := Template; Candidate.Imports (1).Result := Integer_Value;
   Candidate.Imports (1).Result_Data_Type := T.Invalid_Type;
   Check (not Scalar_Import (Candidate.Imports (1)), "receiver is not a plain scalar contract");
   Check (not CCL.Host_Values.Portable_Contract
     (Candidate.Imports (1), O.No_Schema, O.No_Schema), "portable encoding must not erase receiver semantics");

   -- Exhaustion before host submission must reject the offered borrow and
   -- leave a well-formed terminal machine. It must not look like a submitted
   -- operation or replace the useful exhaustion diagnosis on the next step.
   R.Reserve (Owner, Session, Collection, Factory, Outcome);
   Check (Outcome = R.Succeeded, "reserve exhaustion fixture");
   R.Publish (Owner, Factory, True, Ref, Outcome);
   Check (Outcome = R.Succeeded, "publish exhaustion fixture");
   --  Results live in the value arena: an object import is admitted only
   --  when the arena can hold the largest value of its result type. A deep
   --  record (7 records of 16 one-field records: 120 nodes) fits four times
   --  in 512 nodes; the fifth call is refused before the host acts.
   declare
      Leaf, Middle, Top : T.Type_Reference;
      Deep_Contract : O.Binding;
      Deep : O.Image;
      Calls : constant := 5;
   begin
      T.Define (Types, (Identifier => T.Named ("Leaf"), Form => T.Product, Count => 1,
        Parts => [1 => (T.Named ("x"), T.Integer_Type), others => <>]), Leaf, Defined);
      Check (Defined = T.Defined, "define leaf record");
      T.Define (Types, (Identifier => T.Named ("Middle"), Form => T.Product, Count => 16,
        Parts => [for P in T.Component_Index => (T.Named ("m" & Character'Val (Character'Pos ('a') + P - 1)), Leaf)]),
        Middle, Defined);
      Check (Defined = T.Defined, "define middle record");
      T.Define (Types, (Identifier => T.Named ("Top"), Form => T.Product, Count => 7,
        Parts => [for P in T.Component_Index =>
                    (if P <= 7 then (T.Named ("t" & Character'Val (Character'Pos ('a') + P - 1)), Middle)
                     else (others => <>))]),
        Top, Defined);
      Check (Defined = T.Defined, "define deep record");
      O.Bind (Types, Top, [9, 10, 11, 12], Deep_Contract, Good); Check (Good, "bind deep record");
      Deep := O.Empty (Deep_Contract);
      O.Append (Deep, O.Product_Cell (7), Built);
      for M in 1 .. 7 loop
         O.Append (Deep, O.Product_Cell (16), Built);
         for L in 1 .. 16 loop
            O.Append (Deep, O.Product_Cell (1), Built);
            O.Append (Deep, O.Integer_Cell (Interfaces.Integer_64 (L)), Built);
         end loop;
      end loop;
      Check (Built = O.Added and then O.Validate (Deep, Deep_Contract), "valid deep image");
      Candidate := Template;
      Candidate.Data_Types := Types;
      Candidate.Imports (1).Result_Data_Type := Top;
      Candidate.Length := 3 + 3 * Calls + 2;
      for I in 0 .. Calls - 1 loop
         Candidate.Code (Instruction_Index (3 + 3 * I)) := (Op => Push_Integer, others => <>);
         Candidate.Code (Instruction_Index (4 + 3 * I)) := (Op => Invoke_Import, Import => 1, others => <>);
         Candidate.Code (Instruction_Index (5 + 3 * I)) := (Op => Drop, others => <>);
      end loop;
      Candidate.Code (Instruction_Index (Candidate.Length - 2)) := (Op => Invoke_Import, Import => 3, others => <>);
      Candidate.Code (Instruction_Index (Candidate.Length - 1)) := (Op => Halt, others => <>);
      Verify (Candidate, Checked, Error); Check (Error = Valid, "admit bounded object pressure");
      N.Initialize (Checked, 256, Machine); Advance;
      N.Complete_Resource (Checked, Machine, Owner, Ref, Good); Check (Good, "pressure fixture factory");
      for I in 1 .. Calls - 1 loop
         Advance;
         Check (Step.Status = Waiting_For_Host and then Step.Request_Receiver = Ref, "offer object read");
         N.Acknowledge_Host_Submission (Checked, Machine, True);
         N.Complete_Object (Checked, Machine, Deep_Contract, Deep, True);
      end loop;
      Advance;
      Check (Step.Status = Object_Storage_Exhausted, "reject capacity before host effect");
      Check (N.Pending_Call (Checked, Machine).Status = No_Result, "no pending operation at exhaustion");
      Advance;
      Check (Step.Status = Object_Storage_Exhausted, "exhausted owned call stays well formed");
   end;
   N.Stop (Machine);
   R.Begin_Use (Owner, Ref, Collection, Call, Outcome); Check (Outcome = R.Succeeded, "host can clean up after exhaustion");
   R.Finish_Use (Owner, Call, False, Outcome); Check (Outcome = R.Succeeded, "retire exhausted run resource");
   R.Reclaim (Owner, Factory, Outcome); Check (Outcome = R.Succeeded, "reclaim exhausted run resource");
   Ada.Text_IO.Put_Line ("Receiver/data VM imports: PASS" & Checks'Image & " checks");
end Receiver_Tests;
