pragma Ada_2022;
-- ACPI 6.6 section 5.6.4: disable, clear edge status, handle, clear level
-- status, enable. This controller runs in the serialized userspace event loop,
-- not interrupt context. It contains no address-based I/O or interrupt routing.
package ACPI_GPE_Lifecycle with SPARK_Mode is
   type Trigger_Kind is (Edge_Triggered, Level_Triggered);
   type IO_Outcome is (Completed, Refused, Completion_Unknown);
   type Handler_Outcome is (Not_Invoked, Handler_Succeeded, Handler_Failed,
      Handler_Budget_Exhausted, Handler_Completion_Unknown);
   type Phase is (Idle, Masking, Clearing_Edge, Ready, Executing, Handler_Done,
      Clearing_Level, Enabling, Finished, Failed);
   type Operation is (No_Operation, Mask_Source, Clear_Status, Invoke_Handler, Enable_Source);
   type Source_State is (Unknown_State, Known_Masked, Known_Enabled);
   type Failure_Reason is (No_Failure, Operation_Refused, Uncertain_Completion,
      Handler_Error, Handler_Budget_Error, Provider_Contract_Error, Callback_Exception);
   type Action_Result is (Accepted, Wrong_Phase, No_Budget, Invalid_Source);
   type Report is record
      Stage : Phase := Idle;
      Trigger : Trigger_Kind := Edge_Triggered;
      Last_Operation : Operation := No_Operation;
      Source : Source_State := Unknown_State;
      IO_Status : IO_Outcome := Completion_Unknown;
      Handler_Status : Handler_Outcome := Not_Invoked;
      Charged, Reported_Charge : Natural := 0;
      Handler_Budget : Natural := 0;
      Deferred : Boolean := False;
      Reason : Failure_Reason := No_Failure;
   end record;
   generic
      type Source_ID is private;
      No_Source : Source_ID;
      -- IDs must come from the independently admitted provider inventory.
      -- The backend checks authority/epoch on every operation. No raw address,
      -- width, mask or offset is chosen by AML or an untrusted message here.
      -- Completed is attributable completion; Refused guarantees no effect.
      -- Clear may only acknowledge status; it must not change enable state.
      with procedure Mask (Source : Source_ID; Result : out IO_Outcome);
      with procedure Clear (Source : Source_ID; Result : out IO_Outcome);
      with procedure Enable (Source : Source_ID; Result : out IO_Outcome);
      -- Trusted handler owns method/driver selection and retained-root cleanup.
      -- It preserves source ownership/control state, reports charge <= Budget,
      -- and must not reset this session. No synchronous upward/peer callbacks.
      with procedure Invoke
        (Source : Source_ID; Budget : Natural; Result : out Handler_Outcome;
         Charged : out Natural);
   package Driver with SPARK_Mode is
      pragma Unevaluated_Use_Of_Old (Allow);
      type Session is limited private
        with Default_Initial_Condition => Valid (Session);
      function Observe (Run : Session) return Report;
      function Valid (Run : Session) return Boolean;
      -- One Session per attributable delivered event. Consumed/faulted sessions
      -- cannot restart. Serialized caller retains it across queue deferral.
      procedure Start
        (Run : in out Session; Source : Source_ID; Trigger : Trigger_Kind;
         Result : out Action_Result)
        with Always_Terminates, Pre => Valid (Run), Post => Valid (Run)
          and then Result =
            (if Observe (Run).Stage'Old /= Idle then Wrong_Phase
             elsif Source = No_Source then Invalid_Source else Accepted)
          and then (if Result = Accepted then Observe (Run).Stage in Ready | Failed)
          and then (if Result /= Accepted then Observe (Run) = Observe (Run)'Old);
      -- Zero budget defers at Ready without executing or consuming the handler.
      -- Positive-budget invocation is consumed before the callback, preventing
      -- reentrant/duplicate dispatch, including after errors/uncertainty.
      procedure Service
        (Run : in out Session; Budget : Natural; Result : out Action_Result)
        with Always_Terminates, Pre => Valid (Run), Post => Valid (Run)
          and then Result =
            (if Observe (Run).Stage'Old /= Ready then Wrong_Phase
             elsif Budget = 0 then No_Budget else Accepted)
          and then (if Result = Accepted then
            Observe (Run).Stage in Handler_Done | Failed
            and then Observe (Run).Source = Known_Masked)
          and then (if Result = Accepted then Observe (Run).Charged <= Budget
                    else Observe (Run) = Observe (Run)'Old);
      -- Serialized trusted owner only: claim a handler without invoking any
      -- callback. Source ownership/control and backing must remain held across
      -- all deferred work. No raw wire completion or caller-selected source.
      procedure Begin_Deferred
        (Run : in out Session; Budget : Natural; Result : out Action_Result)
        with Always_Terminates, Pre => Valid (Run), Post => Valid (Run)
          and then Result =
            (if Observe (Run).Stage'Old /= Ready then Wrong_Phase
             elsif Budget = 0 then No_Budget else Accepted)
          and then (if Result = Accepted then
            Observe (Run).Stage = Executing and then Observe (Run).Deferred
            and then Observe (Run).Handler_Budget = Budget
            and then Observe (Run).Source = Known_Masked
          else Observe (Run) = Observe (Run)'Old);
      -- Record exactly one attributable completion from that deferred owner.
      -- No callback, clearing, enabling or cancellation occurs. Overcharge is
      -- a contract error, not clamped success. Unknown/failed completion keeps
      -- the source masked and never permits Finish. Deferred=False is only
      -- logical completion storage, not hardware quiescence or exit permission.
      procedure Complete_Deferred
        (Run : in out Session; Outcome : Handler_Outcome; Charged : Natural;
         Result : out Action_Result)
        with Always_Terminates, Pre => Valid (Run), Post => Valid (Run)
          and then Result =
            (if Observe (Run).Stage'Old = Executing and then Observe (Run).Deferred'Old
             then Accepted else Wrong_Phase)
          and then (if Result = Accepted then
            not Observe (Run).Deferred and then Observe (Run).Source = Known_Masked
            and then Observe (Run).Stage in Handler_Done | Failed
            and then Observe (Run).Charged <= Observe (Run).Handler_Budget
          else Observe (Run) = Observe (Run)'Old);
      -- Only a successful handler can acknowledge a level source and re-enable.
      -- No automatic retry/replay after refused, uncertain or raised callbacks.
      -- A failed enable may leave source state unknown; never report it masked.
      procedure Finish (Run : in out Session; Result : out Action_Result)
        with Always_Terminates, Pre => Valid (Run), Post => Valid (Run)
          and then Result =
            (if Observe (Run).Stage'Old = Handler_Done then Accepted else Wrong_Phase)
          and then (if Result = Accepted then Observe (Run).Stage in Finished | Failed)
          and then (if Result /= Accepted then Observe (Run) = Observe (Run)'Old);
   private
      type Session is limited record
         ID : Source_ID := No_Source;
         Info : Report;
      end record;
   end Driver;
end ACPI_GPE_Lifecycle;
