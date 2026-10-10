------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Mesa's session queue (GPU-001 step 3, docs/gpu-async-submission.md):
--  the C glue (anv_cubit_memory.c, anv_cubit_sync.c) calls these exports
--  to open the GPU session's queue (CuBit.GPU_Sessions) beside its render
--  endpoint, write descriptors without waiting, read completion from the
--  status lines, and wait for a context's value. The C ABI is
--  native_gpu_queue.h.
--
--  @description
--  One queue per render endpoint capability slot. Threads: Open, Close and
--  Submit are serialized per slot by the caller (its per-queue lock);
--  Observe and Wait may run on any thread meanwhile. They read only the
--  driver's region and values Submit publishes atomically.
--
--  Submit never waits for the GPU and makes no blocking call: it reaps the
--  completion records waiting (a record that is not Completed fails the
--  session, sticky), writes one descriptor and kicks only when the driver's
--  wake word asks (an asynchronous OP_KICK). The one exception is a full
--  queue, which is backpressure: Submit then looks again every
--  Room_Pause_Ms, still without a call, for at most Room_Budget_Ms, past
--  which the driver's own hang watchdog (1 s per job) would have failed
--  the context. It answers Failed then.
--
--  Wait: one thread per session sleeps in OP_GPU_WAKE (the driver holds
--  one wake per session; two threads asking for it would supersede each
--  other in turn); any other thread waits on the status line alone
--  (CuBit.GPU_Sessions.Poll_Reached), as does a wait the driver answers
--  Not_Held.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.GPU_Queues;
with Native_GPU_Timeline;

package Native_GPU_Queue is

   package GQ renames CuBit.GPU_Queues;

   type Queue_Status is (OK, Full, Failed, Closed, Invalid, Timed_Out) with Convention => C;
   for Queue_Status use
     (OK => 0, Full => 1, Failed => 2, Closed => 3, Invalid => 4, Timed_Out => 5);

   --  A descriptor as C writes it (struct cubit_gpu_job). Operation is
   --  GQ.Opcode's representation (Execute or Signal); a wait's Target 0 is
   --  no wait.
   type Wait_Words is record
      Context : Unsigned_32 := 0;
      Reserved : Unsigned_32 := 0;
      Target  : Unsigned_64 := 0;
   end record with Convention => C;
   type Job is record
      Operation : Unsigned_32 := 0;
      Context   : Unsigned_32 := 0;
      Handle    : Unsigned_32 := 0;
      Offset    : Unsigned_32 := 0;
      Bytes     : Unsigned_32 := 0;
      Reserved  : Unsigned_32 := 0;
      GPU       : Unsigned_64 := 0;
      First, Second : Wait_Words;
      Deadline_Us : Unsigned_64 := 0;
   end record with Convention => C;
   Job_Bytes : constant := 72;
   pragma Compile_Time_Error (Job'Size /= Job_Bytes * 8, "native_gpu_queue.h struct cubit_gpu_job");

   --  How long a full queue may stay full before the session counts as
   --  failed, and how often Submit looks for room meanwhile.
   Room_Budget_Ms : constant := 2_000;
   Room_Pause_Ms  : constant := 1;

   --  Open Slot's queue (the session's contexts are registered). OK, or
   --  Closed: the driver refused it; Invalid: a bad slot or already open.
   function Open (Slot : Unsigned_64) return Queue_Status
     with Export, Convention => C, External_Name => "cubit_gpu_queue_open";
   --  Close the channel (a held wake is answered by the driver).
   procedure Close (Slot : Unsigned_64)
     with Export, Convention => C, External_Name => "cubit_gpu_queue_close";

   --  Write one descriptor: its signal value (the context's next) comes
   --  back in Signal. OK; Failed: the session failed, or the queue stayed
   --  full past Room_Budget_Ms; Invalid: the job is malformed (nothing was
   --  written); Closed.
   function Submit
     (Slot : Unsigned_64; Item : access constant Job; Signal : access Unsigned_64)
      return Queue_Status
     with Export, Convention => C, External_Name => "cubit_gpu_queue_submit";

   --  Each context's completed value (the status line, or a Completed
   --  record reaped since if later) and the last value submitted on it.
   --  OK; Failed: a context faulted, hung or was lost, or a record failed.
   function Observe
     (Slot : Unsigned_64; Completed, Submitted : access Native_GPU_Timeline.Completed_Values)
      return Queue_Status
     with Export, Convention => C, External_Name => "cubit_gpu_queue_observe";

   --  Wait until Context reaches Target, with Remaining_Ns left of the
   --  caller's deadline (Native_GPU_Job_Rules.No_Timeout_Ns: none). OK:
   --  reached; Failed: the context failed; Timed_Out; Closed: the queue
   --  ended.
   function Wait
     (Slot : Unsigned_64; Context : Unsigned_32; Target, Remaining_Ns : Unsigned_64)
      return Queue_Status
     with Export, Convention => C, External_Name => "cubit_gpu_queue_wait";

end Native_GPU_Queue;
