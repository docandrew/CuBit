with Interfaces; use Interfaces;

with CCL.Language;
with CCL.Scheduler;
with CCL.Format;
with CuBit.Protocols;
with CCL.Ownership;
with CCL.VM; use CCL.VM;
with CuBit.Messages; use CuBit.Messages;

procedure Main is
   use ASCII;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Scheduler.Event_Kind;
   use type CCL.Format.Format_Error;

   Candidate : Program;
   Checked   : Validated_Program;
   Error     : Validation_Error;
   VM_Result : Execution_Result;
   Source_Result : CCL.Language.Interpretation_Result;
   All_Passed : Boolean := True;
begin
   debugPrint ("ccl-vm: starting" & LF);

   Candidate.Length := 4;
   Candidate.Code (0) :=
     (Op => Push_Integer, Immediate => 20, others => <>);
   Candidate.Code (1) :=
     (Op => Push_Integer, Immediate => 22, others => <>);
   Candidate.Code (2) :=
     (Op => Add_Integer, others => <>);
   Candidate.Code (3) :=
     (Op => Halt, others => <>);

   Verify (Candidate, Checked, Error);
   if Error /= Valid then
      debugPrint ("ccl-vm: bytecode verification failed" & LF);
      return;
   end if;

   Execute (Checked, 16, VM_Result);
   if VM_Result.Status = Completed and then
     VM_Result.Has_Value and then
     VM_Result.Result_Value.Kind = Integer_Value and then
     VM_Result.Result_Value.Integer = 42
   then
      debugPrint ("ccl-vm: bytecode PASS" & LF);
   else
      debugPrint ("ccl-vm: bytecode FAIL" & LF);
      All_Passed := False;
   end if;

   declare
      Data : CCL.Format.Byte_Array;
      Length : CCL.Format.Module_Length;
      Limits : constant CCL.Format.Resource_Limits :=
        (Fuel => 16, Memory => 4_096, In_Flight => 1);
      Decoded_Limits : CCL.Format.Resource_Limits;
      Format_Error : CCL.Format.Format_Error;
      Decoded : Validated_Program;
   begin
      CCL.Format.Encode
        (Candidate, Limits, Data, Length, Format_Error, Error);
      if Format_Error = CCL.Format.Format_Valid then
         CCL.Format.Decode
           (Data, Length, Decoded, Decoded_Limits, Format_Error, Error);
      end if;
      if Format_Error = CCL.Format.Format_Valid then
         Execute (Decoded, Decoded_Limits.Fuel, VM_Result);
      end if;
      if Format_Error = CCL.Format.Format_Valid and then
        VM_Result.Status = Completed and then VM_Result.Has_Value and then
        VM_Result.Result_Value.Integer = 42
      then
         debugPrint ("ccl-vm: module PASS" & LF);
      else
         debugPrint ("ccl-vm: module FAIL" & LF);
         All_Passed := False;
      end if;
   end;

   CCL.Language.Interpret
     ("(let ((answer (+ 20 22))) (= answer 42))", 64, Source_Result);
   if Source_Result.Status = CCL.Language.Succeeded and then
     Source_Result.Has_Value and then
     Source_Result.Result_Value.Kind = Boolean_Value and then
     Source_Result.Result_Value.Boolean
   then
      debugPrint ("ccl-vm: source PASS" & LF);
   else
      debugPrint ("ccl-vm: source FAIL" & LF);
      All_Passed := False;
   end if;

   declare
      IMPORT_SLOT : constant CapabilitySlot := 24;
      OP_INCREMENT : constant Unsigned_32 :=
        CuBit.Protocols.CCL_TEST_OP_INCREMENT;
      REPLY_OK : constant Unsigned_32 := 16#F000#;
      State : Machine_State;
      Request : Message := NULL_MESSAGE;
      Completions : CompletionRing := [others => NULL_COMPLETION];
      Completion_Count : Unsigned_64;
      Submit_OK : Boolean;
      IMPORT_TOKEN : constant Unsigned_64 := 16#CC10_0001#;
   begin
      Candidate := (others => <>);
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) :=
        (Argument => Integer_Value,
         Result => Integer_Value,
         Authority => Observe_Authority,
         Binding => Unsigned_32 (DRIVER_CCL_TEST), others => <>);
      Candidate.Length := 3;
      Candidate.Code (0) :=
        (Op => Push_Integer, Immediate => 41, others => <>);
      Candidate.Code (1) := (Op => Invoke_Import, Import => 0, others => <>);
      Candidate.Code (2) := (Op => Halt, others => <>);

      Verify (Candidate, Checked, Error);
      if Error = Valid then
         Initialize (Checked, 16, State);
         Continue_Execution (Checked, State, VM_Result);
      end if;

      if Error /= Valid or else VM_Result.Status /= Waiting_For_Host or else
        VM_Result.Requested_Authority /= Observe_Authority or else
        VM_Result.Requested_Binding /= Unsigned_32 (DRIVER_CCL_TEST) or else
        VM_Result.Request_Argument.Kind /= Integer_Value
      then
         debugPrint ("ccl-vm: import request FAIL" & LF);
         All_Passed := False;
      else
         Request.tag :=
           (label => OP_INCREMENT, length => 1, flags => 0, badge => 0);
         Request.words (0) := Unsigned_64 (VM_Result.Request_Argument.Integer);
         Submit_OK := capSubmit (IMPORT_SLOT, Request, IMPORT_TOKEN);
         if Submit_OK then
            Completion_Count :=
              waitCompletion (Completions'Address, max => 1, min => 1);
         else
            Completion_Count := 0;
         end if;
         Complete_Host_Call
           (Checked, State,
            Integer_Constant (Integer_64 (Completions (0).msg.words (0))),
            Completion_Count = 1 and then Completions (0).valid and then
            Completions (0).status = COMPLETION_OK and then
            Completions (0).token = IMPORT_TOKEN and then
            Completions (0).msg.tag.label = REPLY_OK);
         Continue_Execution (Checked, State, VM_Result);

         if VM_Result.Status = Completed and then VM_Result.Has_Value and then
           VM_Result.Result_Value.Kind = Integer_Value and then
           VM_Result.Result_Value.Integer = 42
         then
            debugPrint ("ccl-vm: import IPC PASS" & LF);
         else
            debugPrint ("ccl-vm: import IPC FAIL" & LF);
            All_Passed := False;
         end if;
      end if;
   end;

   declare
      IMPORT_SLOT : constant CapabilitySlot := 24;
      OP_INCREMENT : constant Unsigned_32 :=
        CuBit.Protocols.CCL_TEST_OP_INCREMENT;
      REPLY_OK : constant Unsigned_32 := 16#F000#;
      Import_Program : Program;
      Plain_Program : Program;
      Import_Checked : Validated_Program;
      Plain_Checked : Validated_Program;
      Scheduler : CCL.Scheduler.Scheduler_State;
      Event : CCL.Scheduler.Scheduler_Event;
      Started : Boolean;
      Import_Isolate : CCL.Scheduler.Isolate_Index;
      Plain_Isolate : CCL.Scheduler.Isolate_Index;
      Request : Message := NULL_MESSAGE;
      Completions : CompletionRing := [others => NULL_COMPLETION];
      Completion_Count : Unsigned_64;
      Submitted : Boolean;
      Matched : Boolean;
   begin
      Import_Program.Imports_Length := 1;
      Import_Program.Imports (0) :=
        (Argument => Integer_Value, Result => Integer_Value,
         Authority => Observe_Authority,
         Binding => Unsigned_32 (DRIVER_CCL_TEST), others => <>);
      Import_Program.Length := 3;
      Import_Program.Code (0) :=
        (Op => Push_Integer, Immediate => 41, others => <>);
      Import_Program.Code (1) :=
        (Op => Invoke_Import, Import => 0, others => <>);
      Import_Program.Code (2) := (Op => Halt, others => <>);
      Verify (Import_Program, Import_Checked, Error);

      Plain_Program.Length := 2;
      Plain_Program.Code (0) :=
        (Op => Push_Integer, Immediate => 7, others => <>);
      Plain_Program.Code (1) := (Op => Halt, others => <>);
      Verify (Plain_Program, Plain_Checked, Error);

      CCL.Scheduler.Initialize (Scheduler);
      CCL.Scheduler.Start
        (Scheduler, Import_Checked, 8, Started, Import_Isolate);
      if Started then
         CCL.Scheduler.Start
           (Scheduler, Plain_Checked, 8, Started, Plain_Isolate);
      end if;
      if Started then
         CCL.Scheduler.Dispatch_One (Scheduler, Event);
      end if;

      if not Started or else Event.Kind /= CCL.Scheduler.Host_Request then
         debugPrint ("ccl-vm: scheduler FAIL" & LF);
         All_Passed := False;
      else
         Request.tag :=
           (label => OP_INCREMENT, length => 1, flags => 0, badge => 0);
         Request.words (0) := Unsigned_64 (Event.Argument.Integer);
         Submitted := capSubmit (IMPORT_SLOT, Request, Event.Token);

         --  Prove useful work can run before harvesting this completion.
         CCL.Scheduler.Dispatch_One (Scheduler, Event);
         if not Submitted or else
           Event.Kind /= CCL.Scheduler.Isolate_Completed or else
           Event.Isolate /= Plain_Isolate or else Event.Value.Integer /= 7
         then
            debugPrint ("ccl-vm: scheduler FAIL" & LF);
            All_Passed := False;
         else
            Completion_Count :=
              waitCompletion (Completions'Address, max => 1, min => 1);
            CCL.Scheduler.Complete
              (Scheduler, Completions (0).token,
               Integer_Constant (Integer_64 (Completions (0).msg.words (0))),
               Completion_Count = 1 and then Completions (0).valid and then
               Completions (0).status = COMPLETION_OK and then
               Completions (0).msg.tag.label = REPLY_OK,
               Matched);
            CCL.Scheduler.Dispatch_One (Scheduler, Event);
            if Matched and then
              Event.Kind = CCL.Scheduler.Isolate_Completed and then
              Event.Isolate = Import_Isolate and then Event.Has_Value and then
              Event.Value.Integer = 42
            then
               debugPrint ("ccl-vm: scheduler PASS" & LF);
            else
               debugPrint ("ccl-vm: scheduler FAIL" & LF);
               All_Passed := False;
            end if;
         end if;
      end if;
   end;

   declare
      SEND : constant CCL.Ownership.Disposition_Id := 1;
      Owned : Program;
      Owned_Checked : Validated_Program;
      Owned_Error : Validation_Error;
      Owned_State : Machine_State;
      Owned_Values : Local_Value_Array := [others => (others => <>)];
      Accepted : Boolean;
   begin
      Owned.Types (2).Mode := CCL.Ownership.Must_Handle;
      Owned.Types (2).Dispositions_Length := 1;
      Owned.Types (2).Dispositions (0) :=
        (Verb => SEND, Effect => CCL.Ownership.Consume, Next_Type => 0);
      Owned.Locals_Length := 1;
      Owned.Local_Types (0) := 2;
      Owned.Length := 2;
      Owned.Code (0) :=
        (Op => Apply_Local_Disposition, Local => 0, Verb => SEND,
         others => <>);
      Owned.Code (1) := (Op => Halt, others => <>);
      Verify (Owned, Owned_Checked, Owned_Error);
      if Owned_Error = Valid then
         Owned_Values (0) := With_Type (Integer_Constant (99), 2);
         Initialize_With_Locals
           (Owned_Checked, 4, Owned_Values, 1, Owned_State, Accepted);
         if Accepted then
            Continue_Execution (Owned_Checked, Owned_State, VM_Result);
         end if;
      end if;
      Owned.Code (0) := (Op => Drop_Local, Local => 0, others => <>);
      Verify (Owned, Owned_Checked, Owned_Error);
      if Accepted and then VM_Result.Status = Completed and then
        Owned_Error = Invalid_Ownership
      then
         debugPrint ("ccl-vm: ownership PASS" & LF);
      else
         debugPrint ("ccl-vm: ownership FAIL" & LF);
         All_Passed := False;
      end if;
   end;

   if All_Passed then
      debugPrint ("ccl-vm: all tests passed" & LF);
   end if;
end Main;
