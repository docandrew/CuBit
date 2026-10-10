with Interfaces; use Interfaces;
with System;
with CuBit.GPU_Queues;
with Intel_GPU_Context_Ledger;
with Intel_GPU_GuC_Submission_Policy;
with Intel_GPU_Ring_Reservation;
with Intel_GPU_Session_Queue;
-- The driver loop's GPU work (GPU-001 step 2): every session's queue and
-- contexts, many jobs in flight per context, one loop turn at a time. No
-- handler waits for the GPU; nothing here waits at all.
--
-- Each Turn:
--   1. retries kicks the GuC refused for backpressure;
--   2. for each context with work: one ownership check, one timeline read,
--      one watchdog step; pops every job the timeline completed (a
--      completion record for queue jobs, Call_Finished for the synchronous
--      wrapper's) or that a hang or loss failed (Device_Lost, never OK);
--   3. answers a held wake whose condition now holds;
--   4. unless the driver quiesces, takes descriptors in order while their
--      waits hold and their context and ring have room, appends their
--      segments and moves each ring tail;
--   5. kicks each context appended to once: SCHED_CONTEXT if resident,
--      else the MODE_SET enable, which also submits (H6);
--   6. publishes the queue indices and status lines.
-- A job's completion is accepted only once the kick that submitted it was
-- queued and the context is acknowledged Enabled (the gate of step 1).
--
-- All callbacks act on the selection Select_Context made and must be
-- bounded and nonraising. None may call back into this package: the caller
-- acts on a Quarantine after Turn (or Submit_Call) returns. Any failed ownership check, ring write or kick
-- loses the session's contexts (owed jobs fail) and quarantines it.
generic
   type Session_Id is range <>;
   with function Select_Context
     (S : Session_Id; C : CuBit.GPU_Queues.Context_Index) return Boolean;
   with function Owner_Ready return Boolean;
   with function Batch_Ready (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean;
   with function Scheduling_Resident return Boolean;
   -- The selected context may take a new ring tail now (enabled, or parked
   -- and acknowledged). False while an enable awaits its MODE_DONE: the
   -- head then waits, it is not refused.
   with function Publish_Ready return Boolean;
   -- Write the selected context's next segment (Execute: a batch branch to
   -- Batch; Signal: a barrier) whose breadcrumb writes V, where Plan says:
   -- MI_NOOP padding to the ring's end first when it wraps. Then make it
   -- visible and move the saved ring tail from Expected_Tail to Plan.Tail.
   with procedure Write_Segment
     (Operation : CuBit.GPU_Queues.Opcode; V, Batch : Unsigned_64;
      Plan : Intel_GPU_Ring_Reservation.Plan; Expected_Tail : Unsigned_32; OK : out Boolean);
   with function Segment_Bytes (Operation : CuBit.GPU_Queues.Opcode) return Unsigned_32;
   with procedure Kick (Enable : Boolean; Result : out Intel_GPU_GuC_Submission_Policy.Kick_Result);
   -- The selected context's 64-bit timeline (high, low, high).
   with procedure Read_Timeline (V : out Unsigned_64; OK : out Boolean);
   -- Monotonic microseconds; Unsigned_64'Last when unavailable.
   with function Now_Us return Unsigned_64;
   -- Session S is lost: irreversible, backing retained (logged by the caller).
   with procedure Quarantine (S : Session_Id; Why : CuBit.GPU_Queues.Fault_Reason);
   -- The synchronous wrapper's job on S ended: OK when the timeline reached V.
   with procedure Call_Finished (S : Session_Id; V : Unsigned_64; OK : Boolean);
   -- Answer S's held wake request.
   with procedure Answer_Wake (S : Session_Id; Result : CuBit.GPU_Queues.Wake_Result);
   -- A live queue's regions: the client's (read-only here) and the driver's.
   with function Client_Region (S : Session_Id) return System.Address;
   with function Server_Region (S : Session_Id) return System.Address;
   -- The longest the GPU may go without timeline progress. Explicit.
   Hang_Budget_Us : Unsigned_64;
package Intel_GPU_Queue_Service is
   package Q renames CuBit.GPU_Queues;
   package Sessions renames Intel_GPU_Session_Queue;
   package Ledgers renames Intel_GPU_Context_Ledger;
   package Policy renames Intel_GPU_GuC_Submission_Policy;
   subtype Value is Sessions.Value;

   type Table is limited private;

   type Statistics is record
      Completed, Lost, Refused, Taken : Policy.Event_Count := 0;
      Enables, Notifies, Kick_Retries : Policy.Event_Count := 0;
      -- Jobs in flight now, and the most since Reset_Peak.
      In_Flight, In_Flight_Peak : Natural := 0;
      -- Submit-to-completion latency of completed jobs since Reset_Latency.
      Latency_Count, Latency_Min_Us, Latency_Max_Us, Latency_Total_Us : Unsigned_64 := 0;
   end record;
   function Stats (T : Table) return Statistics;
   procedure Reset_Peak (T : in out Table);
   procedure Reset_Latency (T : in out Table);

   function Session (T : Table; S : Session_Id) return Sessions.Session;
   function Live (T : Table; S : Session_Id) return Boolean;
   -- Nothing in flight anywhere.
   function Idle (T : Table) return Boolean;
   function Session_Idle (T : Table; S : Session_Id) return Boolean;
   -- Parking work may run: every session quiesces with nothing in flight.
   function Park_Allowed (T : Table) return Boolean;
   -- Descriptors wait to be taken in some live queue. (A head already
   -- taken waits only on GPU progress or a quiesce, which pace the loop.)
   function Work_Waiting (T : Table) return Boolean;

   -- A registered context whose setup breadcrumb wrote Done, its ring
   -- holding that one First_Bytes segment.
   procedure Open_Context
     (T : in out Table; S : Session_Id; C : Q.Context_Index; Done : Value;
      First_Bytes : Unsigned_32; Opened : out Boolean);
   -- The client opened S's queue (its regions are mapped): a fresh
   -- server side. Refused while jobs of an earlier queue are in flight.
   procedure Open_Queue (T : in out Table; S : Session_Id; Opened : out Boolean);
   -- S's queue ends: no more records are written there; a held wake is
   -- answered No_Queue. Jobs on the GPU stay in their ledgers.
   procedure End_Queue (T : in out Table; S : Session_Id);
   -- S is lost (device loss, ownership loss): every owed job fails now.
   procedure Lose_Session (T : in out Table; S : Session_Id; Why : Q.Fault_Reason);
   -- S retired: its state is forgotten. Only when nothing is in flight.
   procedure Forget_Session (T : in out Table; S : Session_Id; Forgotten : out Boolean);

   -- Driver-wide quiesce (parking work pending): no new job is published.
   procedure Set_Quiesce (T : in out Table; On : Boolean);

   procedure Turn (T : in out Table);

   -- The synchronous wrapper (0x0A27): publish one batch on S's context C
   -- with Budget_Us to complete in; Call_Finished answers it later.
   type Call_Result is
     (Submitted,     -- published and kicked (or kick retrying)
      Malformed,     -- the batch fields fail the admission policy
      Denied,        -- the batch is not mapped in the session's VM
      Busy,          -- no room now (context full, ring full, a call pending,
                     -- quiescing): defer the request, do not refuse it
      Faulted);      -- the context failed; the session is quarantined
   -- Submit_Call would have room on S's context C now (no selection
   -- needed): a request that finds none while work is in flight is
   -- deferred by the caller, not refused.
   function Call_Room (T : Table; S : Session_Id; C : Q.Context_Index) return Boolean;
   procedure Submit_Call
     (T : in out Table; S : Session_Id; C : Q.Context_Index;
      Handle, GPU, Offset, Bytes, Budget_Us : Unsigned_64; Result : out Call_Result);

   -- OP_GPU_WAKE from S for context C and target. Answer_Held: answer the
   -- held one first (Woken). Answer_Now: answer this one now with Result;
   -- otherwise the caller saves its reply capability (Hold_Failed if it
   -- cannot). One saved-reply slot serves wakes driver-wide.
   procedure Wake_Request
     (T : in out Table; S : Session_Id; C : Q.Context_Index; Target : Value;
      Answer_Held, Answer_Now : out Boolean; Result : out Q.Wake_Result);
   procedure Wake_Hold_Failed (T : in out Table; S : Session_Id);
   -- The session holding the driver's wake slot, if any.
   function Wake_Slot_Held (T : Table; S : Session_Id) return Boolean;

   -- Before the loop sleeps: arm every live queue's wake word so its client
   -- kicks, then look once more. True: descriptors arrived, do not sleep.
   function Arm_Wake_Words (T : in out Table) return Boolean;
private
   type Kick_Need is (No_Kick, Notify_Kick, Enable_Kick);
   type Kick_Row is array (Q.Context_Index) of Kick_Need;
   type Kick_Array is array (Session_Id) of Kick_Row;
   type Session_Array is array (Session_Id) of Sessions.Session;
   type Table is limited record
      Items : Session_Array := [others => Sessions.Empty];
      Kicks : Kick_Array := [others => [others => No_Kick]];
      Counters : Statistics;
      Quiesce : Boolean := False;
      Wake_Owned : Boolean := False;
      Wake_Owner : Session_Id := Session_Id'First;
      Wake_Epoch : Unsigned_32 := 0;
   end record;
end Intel_GPU_Queue_Service;
