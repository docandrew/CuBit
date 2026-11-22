with Ada.Text_IO;
with Interfaces;
with CCL.Types;
with CCL.Resources;
with CCL.Ownership;
with CCL.Imports;
with CCL.VM.Resource_Values;
with CCL.Objects.Values;
with CCL.Host_Values;

procedure Resource_Value_Tests is
   package T renames CCL.Types;
   package R renames CCL.Resources;
   package V renames CCL.VM;
   use V;
   use type T.Definition_Result;
   use type T.Type_Reference;
   use type R.Outcome;
   use type R.Reference;
   use type Interfaces.Integer_64;
   Types : T.Registry;
   Collection, Other : T.Type_Reference;
   Defined : T.Definition_Result;
   Owner : R.Registry (701);
   Foreign : R.Registry (702);
   Session, Foreign_Run : R.Run;
   Factory, Foreign_Factory, Call : R.Ticket;
   Ref, Wrong, Foreign_Ref : R.Reference;
   Result : R.Outcome;
   Candidate, Template : Program;
   Checked : Validated_Program;
   Error : Validation_Error;
   State : Machine_State;
   Step : Execution_Result;
   Image : CCL.Objects.Image;
   Contract : CCL.Objects.Binding;
   Poison : Value;
   Accepted : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Why; end if;
   end Check;
   procedure Wait_For_Factory is
   begin
      Initialize (Checked, 16, State);
      Continue_Execution (Checked, State, Step);
      Check (Step.Status = Waiting_For_Host and then Step.Requested_Import = 0,
        "wait for authorized factory");
   end Wait_For_Factory;
begin
   T.Define (Types, (Identifier => T.Named ("Collection"), Form => T.Resource,
     Count => 1, Parts => [1 => (T.Named ("Value"), T.Integer_Type), others => <>]), Collection, Defined);
   Check (Defined = T.Defined, "define collection resource");
   T.Define (Types, (Identifier => T.Named ("Other"), Form => T.Resource, others => <>), Other, Defined);
   Check (Defined = T.Defined, "define distinct resource");
   R.Start (Owner, Types, Session, Result); Check (Result = R.Succeeded, "start host registry");
   R.Reserve (Owner, Session, Collection, Factory, Result); Check (Result = R.Succeeded, "reserve before effect");
   R.Publish (Owner, Factory, True, Ref, Result); Check (Result = R.Succeeded, "publish authenticated acquisition");
   R.Reserve (Owner, Session, Other, Call, Result); Check (Result = R.Succeeded, "reserve other type");
   R.Publish (Owner, Call, True, Wrong, Result); Check (Result = R.Succeeded, "publish other type");
   R.Start (Foreign, Types, Foreign_Run, Result); Check (Result = R.Succeeded, "start foreign context");
   R.Reserve (Foreign, Foreign_Run, Collection, Foreign_Factory, Result);
   Check (Result = R.Succeeded, "reserve foreign context");
   R.Publish (Foreign, Foreign_Factory, True, Foreign_Ref, Result);
   Check (Result = R.Succeeded, "publish foreign context");

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
   Candidate.Imports_Length := 3;
   Candidate.Imports (0) := (Result => Resource_Value, Result_Data_Type => Collection,
     Result_Type_Tag => 1, Binding => 1, others => <>);
   Candidate.Imports (1) := (Argument => Resource_Value, Argument_Data_Type => Collection,
     Ownership_Argument => True, Transfer => CCL.Imports.Borrowed_RO_Argument,
     Binding => 2, others => <>);
   Candidate.Imports (2) := (Argument => Resource_Value, Argument_Data_Type => Collection,
     Ownership_Argument => True, Transfer => CCL.Imports.Move_Argument,
     Success_Verb => 1, Failure_Verb => 1, Binding => 3, others => <>);
   Candidate.Length := 7;
   Candidate.Code (0) := (Op => Push_Integer, Immediate => 0, others => <>);
   Candidate.Code (1) := (Op => Invoke_Import, Import => 0, others => <>);
   Candidate.Code (2) := (Op => Initialize_Local, Local => 0, others => <>);
   Candidate.Code (3) := (Op => Invoke_Import, Import => 1, others => <>);
   Candidate.Code (4) := (Op => Drop, others => <>);
   Candidate.Code (5) := (Op => Invoke_Import, Import => 2, others => <>);
   Candidate.Code (6) := (Op => Halt, others => <>);
   Template := Candidate;
   Verify (Candidate, Checked, Error); Check (Error = Valid, "verify factory borrow close");

   Wait_For_Factory;
   V.Resource_Values.Complete (Checked, State, Owner, Foreign_Ref, Accepted);
   Check (not Accepted, "foreign reference cannot enter machine");
   V.Resource_Values.Complete (Checked, State, Owner, Wrong, Accepted);
   Check (not Accepted, "wrong nominal resource type cannot enter machine");
   V.Resource_Values.Complete (Checked, State, Owner, R.No_Reference, Accepted);
   Check (not Accepted, "null reference cannot enter machine");
   V.Resource_Values.Complete (Checked, State, Owner, Ref, Accepted);
   Check (Accepted, "admit live opaque factory result");
   V.Resource_Values.Complete (Checked, State, Owner, Ref, Accepted);
   Check (not Accepted, "factory completion consumed once");
   Continue_Execution (Checked, State, Step);
   Check (Step.Status = Waiting_For_Host and then Step.Requested_Import = 1 and then
     Step.Request_Argument.Kind = Resource_Value and then Step.Request_Argument.Resource = Ref,
     "borrow exact opaque resource");
   R.Begin_Use (Owner, Step.Request_Argument.Resource, Collection, Call, Result);
   Check (Result = R.Succeeded, "host resolves live reference for get");
   Acknowledge_Host_Submission (Checked, State, True);
   R.Finish_Use (Owner, Call, True, Result); Check (Result = R.Succeeded, "get completion");
   Complete_Host_Call (Checked, State, Integer_Constant (42), True);
   Continue_Execution (Checked, State, Step);
   Check (Step.Status = Waiting_For_Host and then Step.Requested_Import = 2 and then
     Step.Request_Argument.Resource = Ref, "move resource to close: " & Step.Status'Image & Step.Requested_Import'Image);
   R.Begin_Use (Owner, Ref, Collection, Call, Result); Check (Result = R.Succeeded, "start close");
   Acknowledge_Host_Submission (Checked, State, True);
   R.Finish_Use (Owner, Call, False, Result); Check (Result = R.Succeeded, "close retires reference");
   Complete_Host_Call (Checked, State, Integer_Constant (0), True);
   Continue_Execution (Checked, State, Step);
   Check (Step.Status = Completed and then not R.Current (Owner, Ref), "complete without outstanding owner");

   Wait_For_Factory;
   V.Resource_Values.Complete (Checked, State, Owner, Ref, Accepted);
   Check (not Accepted, "closed reference cannot be returned again");
   Stop (State);
   V.Resource_Values.Complete (Checked, State, Owner, Wrong, Accepted);
   Check (not Accepted, "stopped machine cannot accept resources");

   -- Keep the service handle out of both the scalar completion entry point and
   -- the persistence converter, even if a trusted caller presents a bad record.
   R.Reserve (Owner, Session, Collection, Factory, Result); Check (Result = R.Succeeded, "reserve another collection");
   R.Publish (Owner, Factory, True, Ref, Result); Check (Result = R.Succeeded, "publish another collection");
   Wait_For_Factory;
   Poison := (Kind => Resource_Value, Resource => Ref, Data_Type => Collection,
     Type_Tag => 1, Copyable => False, others => <>);
   Complete_Host_Call (Checked, State, Poison, True);
   Continue_Execution (Checked, State, Step);
   Check (Step.Status = Invalid_Bytecode, "scalar completion cannot inject resources");
   CCL.Objects.Bind (Types, T.Integer_Type, [1, 2, 3, 4], Contract, Accepted);
   Check (Accepted, "bind primitive data");
   CCL.Objects.Values.From_VM (Contract, Types, Poison, Image, Accepted);
   Check (not Accepted, "resource is not a data image");
   Poison := (Kind => Integer_Value, Resource => Ref, others => <>);
   Check (not Well_Typed (Types, Poison), "resource cannot hide in primitive metadata");
   CCL.Objects.Values.From_VM (Contract, Types, Poison, Image, Accepted);
   Check (not Accepted, "primitive conversion cannot strip a hidden resource");
   CCL.Objects.Bind (Types, Collection, [1, 2, 3, 4], Contract, Accepted);
   Check (not Accepted, "resource type cannot acquire a persistence binding");
   Check (not CCL.Host_Values.Portable_Contract
     ((Result => Resource_Value, others => <>), CCL.Objects.No_Schema, CCL.Objects.No_Schema),
     "malformed resource descriptor cannot become a scalar host contract");

   Candidate := Template; Candidate.Types (1).Mode := CCL.Ownership.Unrestricted;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Ownership, "resource cannot be unrestricted");
   Candidate := Template; Candidate.Imports (0).Result_Type_Tag := 0;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Ownership, "factory must declare owned result");
   Candidate := Template; Candidate.Imports (1).Transfer := CCL.Imports.Copy_Argument;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Import, "resource argument cannot copy");
   Candidate := Template; Candidate.Imports (1).Ownership_Argument := False;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Import, "resource cannot be scalar stack argument");
   Candidate := Template; Candidate.Imports (1).Argument_Data_Type := Other;
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Import, "owned import must match full local data type");
   Candidate := Template; Candidate.Code (3) := (Op => Copy_Local, Local => 0, others => <>);
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Ownership, "script cannot duplicate reference");
   Candidate := Template; Candidate.Code (5) := (Op => Push_Integer, others => <>);
   Verify (Candidate, Checked, Error); Check (Error = Invalid_Ownership, "must-handle resource needs close or transfer");

   declare
      Shifted, Conflicting : T.Registry;
      Dummy, Shifted_Collection, Bad_Collection : T.Type_Reference;
   begin
      T.Define (Shifted, (Identifier => T.Named ("Prefix"), Form => T.Product, others => <>), Dummy, Defined);
      Check (Defined = T.Defined, "unrelated prefix");
      T.Define (Shifted, T.Describe (Types, Collection), Shifted_Collection, Defined);
      Check (Defined = T.Defined and then Shifted_Collection /= Collection, "different local type numbering");
      Candidate := Template; Candidate.Data_Types := Shifted;
      Candidate.Local_Data_Types (0) := Shifted_Collection;
      Candidate.Imports (0).Result_Data_Type := Shifted_Collection;
      Candidate.Imports (1).Argument_Data_Type := Shifted_Collection;
      Candidate.Imports (2).Argument_Data_Type := Shifted_Collection;
      Verify (Candidate, Checked, Error); Check (Error = Valid, "verify shifted resource definition");
      Wait_For_Factory;
      V.Resource_Values.Complete (Checked, State, Owner, Ref, Accepted);
      Check (Accepted, "full correspondence accepts different local numbering");
      Continue_Execution (Checked, State, Step);
      Check (Step.Status = Waiting_For_Host and then Step.Request_Argument.Data_Type = Shifted_Collection and then
        Step.Request_Argument.Resource = Ref, "reference retains identity under local type translation");
      Stop (State);
      T.Define (Conflicting, (Identifier => T.Named ("Collection"), Form => T.Resource,
        Count => 1, Parts => [1 => (T.Named ("Value"), T.Boolean_Type), others => <>]), Bad_Collection, Defined);
      Check (Defined = T.Defined and then Bad_Collection = Collection, "same number is not same type");
      Candidate := Template; Candidate.Data_Types := Conflicting;
      Verify (Candidate, Checked, Error); Check (Error = Valid, "locally consistent conflicting program");
      Wait_For_Factory;
      V.Resource_Values.Complete (Checked, State, Owner, Ref, Accepted);
      Check (not Accepted, "parameter mismatch rejected despite identical local type number and name");
   end;
   R.Stop (Owner, Session, Result); Check (Result = R.Succeeded, "stop owner");
   Candidate := Template; Verify (Candidate, Checked, Error); Check (Error = Valid, "restore valid program");
   Wait_For_Factory;
   V.Resource_Values.Complete (Checked, State, Owner, Ref, Accepted);
   Check (not Accepted, "stopped registry cannot supply a resource");
   Ada.Text_IO.Put_Line ("Opaque VM resource values: PASS" & Checks'Image & " checks");
end Resource_Value_Tests;
