with Interfaces; use Interfaces;
with CuBit.GPU_Queues;
with Intel_GPU_Timeline;
with Intel_GPU_GuC_Submission_Policy;
-- Decode and admission of one session-queue descriptor (GPU-001 step 2).
-- Pure logic, no I/O; proved at GNATprove level 2.
--
-- The driver copies a descriptor out of the client's ring (Copy, then
-- validate) and decides on the copy, against what it knows of the session's
-- contexts. Rules, in order:
--   1. A descriptor without a valid opcode, flags and context is rejected.
--   2. A context that no longer takes jobs refuses it (Context_Faulted),
--      whatever else it says: every later value of a faulted context
--      mismatches.
--   3. Otherwise a malformed descriptor is rejected and faults its
--      context: a signal value other than Last_Accepted + 1, a wait on a
--      context that is not the session's or on a value not yet accepted
--      (in-order processing could never reach it), a batch outside the
--      admission policy, or batch fields on a Signal.
--   4. A deadline already passed expires it, and faults its context.
--   5. A wait on a context that hung or was lost, not yet reached, loses it.
--   6. Otherwise it waits as the session's head, in order, for its waits to
--      be reached, for its context to have room (Max_In_Flight_Per_Context)
--      and for the session not to be quiescing (VM updates and other
--      parking work). Waiting is backpressure, never a refusal.
--   7. Otherwise it is admitted.
package Intel_GPU_Queue_Admission with SPARK_Mode is
   package Q renames CuBit.GPU_Queues;
   package Policy renames Intel_GPU_GuC_Submission_Policy;
   subtype Value is Intel_GPU_Timeline.Value;
   subtype Microseconds is Intel_GPU_Timeline.Microseconds;
   use type Value, Microseconds, Q.Opcode, Q.Fault_Reason, Q.Nibble;

   -- What the driver knows of one context of the session.
   type Context_View is record
      Open : Boolean := False;      -- one of the session's registered contexts
      Taking : Boolean := False;    -- Active: takes jobs
      Failed : Boolean := False;    -- Hung or Lost
      Accepted : Value := 0;        -- Last_Accepted
      Completed : Value := 0;
      Owed : Natural := 0;          -- jobs in flight
   end record;
   type Session_View is array (Q.Context_Index) of Context_View;

   type Verdict is
     (Admit,            -- publish it now
      Await_Waits,      -- keep it as the head: a wait is not reached yet
      Await_Capacity,   -- keep it: its context has Max_In_Flight jobs
      Await_Quiesce,    -- keep it: the session is quiescing
      Refuse_Faulted,   -- complete Context_Faulted
      Refuse_Lost,      -- complete Device_Lost: a wait's context failed
      Expire,           -- complete Deadline_Expired; fault its context
      Reject);          -- complete Rejected; fault its context if it has one
   subtype Waiting is Verdict range Await_Waits .. Await_Quiesce;

   type Decision is record
      Kind : Verdict := Reject;
      Cause : Q.Fault_Reason := Q.Bad_Opcode;
      Operation : Q.Opcode := Q.Invalid;
      Has_Context : Boolean := False;
      Context : Q.Context_Index := 0;
   end record;

   function Decode_Opcode (Raw : Unsigned_8) return Q.Opcode is
     (case Raw is
        when 1 => Q.Execute, when 2 => Q.Signal, when 3 => Q.VM_Bind,
        when 4 => Q.VM_Unbind, when 5 => Q.Wait, when others => Q.Invalid);

   -- The descriptor's context index names one of the session's contexts.
   function Context_Named (D : Q.Descriptor; View : Session_View) return Boolean is
     (D.Context <= Q.Context_Index'Last and then View (D.Context).Open);

   -- A wait: none (value 0, context 0), or on an open context and a value
   -- that context has already accepted.
   function Wait_Valid (Context : Q.Nibble; Target : Unsigned_64; View : Session_View)
     return Boolean is
     (if Target = 0 then Context = 0
      else Context <= Q.Nibble (Q.Context_Index'Last) and then
           View (Q.Context_Index (Context)).Open and then
           Value (Target) <= View (Q.Context_Index (Context)).Accepted);

   function Wait_Reached (Context : Q.Nibble; Target : Unsigned_64; View : Session_View)
     return Boolean is
     (Target = 0 or else Value (Target) <= View (Q.Context_Index (Context)).Completed)
     with Pre => Wait_Valid (Context, Target, View);

   function Wait_Lost (Context : Q.Nibble; Target : Unsigned_64; View : Session_View)
     return Boolean is
     (not Wait_Reached (Context, Target, View) and then
      View (Q.Context_Index (Context)).Failed)
     with Pre => Wait_Valid (Context, Target, View);

   function Batch_Valid (D : Q.Descriptor) return Boolean is
     (case Decode_Opcode (D.Operation) is
        when Q.Execute => Policy.Batch_Admissible
          (Unsigned_64 (D.Batch_Handle), D.Batch_GPU,
           Unsigned_64 (D.Batch_Offset), Unsigned_64 (D.Batch_Bytes)),
        when Q.Signal => D.Batch_Handle = 0 and D.Batch_GPU = 0 and
                         D.Batch_Offset = 0 and D.Batch_Bytes = 0,
        when others => False);

   -- Rule 1: a valid opcode, flags and context.
   function Addressed (D : Q.Descriptor; View : Session_View) return Boolean is
     (Decode_Opcode (D.Operation) in Q.Execute | Q.Signal and then D.Flags = 0 and then
      Context_Named (D, View));

   -- Well formed: rules 1 and 3 hold.
   function Well_Formed (D : Q.Descriptor; View : Session_View) return Boolean is
     (Addressed (D, View) and then
      View (D.Context).Accepted < Value'Last and then
      Value (D.Signal_Value) = View (D.Context).Accepted + 1 and then
      Wait_Valid (D.Wait_1_Context, D.Wait_1_Value, View) and then
      Wait_Valid (D.Wait_2_Context, D.Wait_2_Value, View) and then
      Batch_Valid (D));

   function Decide
     (D : Q.Descriptor; View : Session_View; Quiescing : Boolean; Now : Microseconds;
      Capacity : Positive) return Decision
     with Post =>
       (if Decide'Result.Kind = Reject then not Well_Formed (D, View)
        else Addressed (D, View) and then Decide'Result.Has_Context and then
             Decide'Result.Context = D.Context and then
             Decide'Result.Operation = Decode_Opcode (D.Operation)) and then
       (if Decide'Result.Kind not in Reject | Refuse_Faulted then Well_Formed (D, View)) and then
       (if Decide'Result.Kind = Reject and then Addressed (D, View) then
          View (D.Context).Taking) and then
       (if Decide'Result.Kind = Reject and then Context_Named (D, View) then
          Decide'Result.Has_Context and Decide'Result.Context = D.Context) and then
       (if Decide'Result.Kind = Reject then Decide'Result.Cause /= Q.None) and then
       (if Decide'Result.Kind = Refuse_Faulted then not View (D.Context).Taking) and then
       (if Decide'Result.Kind in Refuse_Lost | Expire | Waiting | Admit then
          View (D.Context).Taking) and then
       (if Decide'Result.Kind = Expire then Now >= Microseconds (D.Deadline)) and then
       (if Decide'Result.Kind in Refuse_Lost | Waiting | Admit then
          Now < Microseconds (D.Deadline)) and then
       (if Decide'Result.Kind = Admit then
          Wait_Reached (D.Wait_1_Context, D.Wait_1_Value, View) and then
          Wait_Reached (D.Wait_2_Context, D.Wait_2_Value, View) and then
          View (D.Context).Owed < Capacity and then not Quiescing) and then
       (if Decide'Result.Kind = Await_Capacity then View (D.Context).Owed >= Capacity) and then
       (if Decide'Result.Kind = Await_Quiesce then Quiescing);
end Intel_GPU_Queue_Admission;
