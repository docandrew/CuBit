with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Lifecycle;
-- Pure policy for GuC-resident application submission (no I/O).
--
-- Model (Linux v6.16 i915 intel_guc_submission.c __guc_add_request and
-- xe_guc_submit.c submit_exec_queue, single-LRC contexts): register and set
-- policy once. Every job first writes its segment and the LRC ring tail,
-- then kicks the GuC once: a parked context gets SCHED_CONTEXT_MODE_SET
-- (enable), which also submits the new tail, and an enabled one gets a FAST
-- SCHED_CONTEXT (no response, no G2H credit). Nothing waits for MODE_DONE;
-- it arrives later as a G2H event (H6). Scheduling disable and
-- deregistration are lifecycle events (VM exclusivity, retirement), never a
-- per-job step. Work queues/doorbells apply only to parallel (multi-LRC)
-- contexts in GuC v70 and are not used by this single-LRC RCS path.
package Intel_GPU_GuC_Submission_Policy with SPARK_Mode is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Life.Phase;

   type Scheduling_Step is (Enable_Submits, Schedule_Only, Refuse);
   -- The one GuC kick that follows publishing the next ring tail.
   function Plan (Current : Life.Phase) return Scheduling_Step is
     (case Current is
        when Life.Disabled => Enable_Submits,
        when Life.Enabled => Schedule_Only,
        when others => Refuse);

   -- Outcome of one non-blocking kick (MODE_SET enable or SCHED_CONTEXT).
   -- Backpressure published nothing and is retried on a later loop turn
   -- within the submission's deadline; Failed is terminal.
   type Kick_Result is (Kick_Queued, Kick_Backpressure, Kick_Failed);
   -- One non-blocking completion observation.
   type Observation is (Observe_Pending, Observe_Reached, Observe_Failed);

   -- G2H reply credits (H5). A context holds at most one MODE_SET or
   -- DEREGISTER reply credit at a time, of at most this many words
   -- (Intel_GPU_GuC_Context_Lifecycle.Credits_Held). The service checks at
   -- compile time that every context's credit plus i915's unsolicited
   -- reserve fits in the G2H ring.
   Reply_Credit_Words : constant := 4;

   -- Batch extent admission for a wire request. Raw 48-bit PPGTT address,
   -- QWORD-aligned start/offset, DWORD-multiple length, bounded slice and no
   -- wrap past the canonical lower half. Not a command-parser proof.
   Max_Handle : constant Unsigned_64 := Unsigned_64 (Unsigned_32'Last);
   GPU_Address_Limit : constant Unsigned_64 := 2 ** 48;
   Batch_Alignment : constant Unsigned_64 := 8;  -- MI_BATCH_BUFFER_START QWORD
   Command_Unit : constant Unsigned_64 := 4;     -- DWORD commands
   Max_Batch_Bytes : constant Unsigned_64 := 16 * 1024 * 1024;
   function Batch_Admissible (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean is
     (Handle /= 0 and then Handle <= Max_Handle and then
      GPU /= 0 and then GPU < GPU_Address_Limit and then GPU mod Batch_Alignment = 0 and then
      Offset mod Batch_Alignment = 0 and then
      Bytes /= 0 and then Bytes mod Command_Unit = 0 and then
      Bytes <= Max_Batch_Bytes and then Offset <= Max_Batch_Bytes - Bytes and then
      Bytes <= GPU_Address_Limit - GPU)
     with Post => (if Batch_Admissible'Result then
       GPU + Bytes <= GPU_Address_Limit and Offset + Bytes <= Max_Batch_Bytes);

   -- Diagnostic counters. Modular: totals wrap after 2**64, never saturate
   -- into a limit and never gate admission.
   type Event_Count is mod 2 ** 64;
   -- Log cadence: the first submissions, each power of two up to the
   -- interval, then every interval. Bounded log volume, visible progress.
   Report_Interval : constant Event_Count := 1024;
   function Report_Due (Count : Event_Count) return Boolean is
     (Count /= 0 and then
      (Count mod Report_Interval = 0 or else
       (Count < Report_Interval and then (Count and (Count - 1)) = 0)));
end Intel_GPU_GuC_Submission_Policy;
