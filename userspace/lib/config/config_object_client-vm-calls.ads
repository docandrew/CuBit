with CCL.VM.Native_Objects;
with Config_Read_Outcomes;

-- Nonblocking bridge between a host's authorized operation dispatch and Config.
-- The host retains the client, program and suspended machine as one single-owner
-- lifetime. It selects Action from its granted binding table, never from script
-- integers. This adapter grants nothing and does not own an event loop.
package Config_Object_Client.VM.Calls is
   type Operation is (Read_Value, Write_Value);

   procedure Submit
     (Object : in out Client; Action : Operation;
      Item : CCL.VM.Validated_Program; Machine : CCL.VM.Native_Objects.Machine;
      Read_Description : Config_Read_Outcomes.Description;
      Binding : Interfaces.Unsigned_32; Expected_Revision, Token : Number;
      Result : out Submission)
     with Pre => CCL.VM.Is_Valid (Item);
   -- Reads the actual suspended call, not a supplied object index. Checks the
   -- host-selected binding and complete result type BEFORE the service effect.
   -- Write arguments are exported under the collection's retained contract.
   -- Read_Description is used only for reads; writes use ConfigWrite's schema.
   -- These fixed-client adapters reject owned receiver calls. A resource-aware
   -- host must first establish the live reference -> client association and
   -- manage its borrow/submission lifecycle; a binding number is insufficient.

   type Resume_State is (No_Completion, Other_Call, Type_Mismatch, Resumed);
   procedure Resume
     (Object : in out Client; Item : CCL.VM.Validated_Program;
      Machine : in out CCL.VM.Native_Objects.Machine;
      Read_Description : Config_Read_Outcomes.Description;
      Binding : Interfaces.Unsigned_32; Result : out Resume_State)
     with Pre => CCL.VM.Is_Valid (Item);
   -- After Complete authenticates/correlates a kernel completion, resumes once
   -- with a typed read/write outcome. A stopped machine or mismatched call/schema leaves
   -- the result unconsumed so its owner can drain it. The host MUST retain the
   -- same client/program/machine/run association until completion or retirement;
   -- a binding number is not a run identity. Stop does not cancel Config writes.

   procedure Submit
     (Object : in out Client; Action : Operation;
      Local_Types : CCL.Types.Registry; Request : CCL.VM.Execution_Result;
      Expected_Revision, Token : Number; Result : out Submission);
   -- Only a suspended, bound import is admissible. Read has the canonical
   -- integer-zero argument. Repeated submission while waiting returns Busy;
   -- no request replay, wait, polling or speculative successful VM completion.

   type Outcome_State is
     (No_Outcome, Other_Operation, Type_Mismatch, Service_Outcome, Uncertain);
   type Outcome is record
      State : Outcome_State := No_Outcome;
      Action : Operation := Read_Value;
      Code : Config_Object_Messages.Status := Config_Object_Messages.Unavailable;
      Revision : Number := 0;
      Has_Value : Boolean := False;
      Value : CCL.VM.Value := CCL.VM.Integer_Constant (0);
   end record;
   procedure Take_Outcome
     (Object : in out Client; Local_Types : CCL.Types.Registry;
      Result : out Outcome);
   -- Call after dispatching authenticated kernel completions through Complete.
   -- Every service status survives unchanged. Writes return the nominal
   -- ConfigWrite variant, including explicit failures and Uncertain. Stale
   -- reads retain their value and revision, but are not implicitly fresh.
   -- Malformed/failed transport is Uncertain, never a definite rejection.
   -- Other_Operation and Type_Mismatch leave the parent's result unconsumed.

   type Freshness is (Require_Current, Accept_Stale);
   function Can_Resume (Result : Outcome; Policy : Freshness := Require_Current)
     return Boolean;
   -- The host can use this when calling VM.Complete_Host_Call. Write failures
   -- resume normally with their typed alternative, NOT as successful writes.
   -- Uncertain still poisons the client; handling it cannot authorize a retry.
   -- Read failures need general Result<T> support before they carry VM values.
end Config_Object_Client.VM.Calls;
