------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The client's half of a GPU session queue (CuBit.GPU_Queues,
--  docs/gpu-async-submission.md), over the two shared regions and nothing
--  else: no system call, so it runs the same in a process and in a
--  Linux-hosted test. CuBit.GPU_Sessions opens the channel, kicks and wakes.
--
--  @description
--  Submit never waits: it writes one descriptor, publishes it and says
--  whether the driver asked to be kicked. The client chooses each job's
--  signal value itself (its context's Last_Accepted + 1, which it tracks),
--  so it knows the job's out-fence value at once. A full queue is
--  backpressure (Can_Submit is False); nothing is refused for being busy.
--  Completion: a record per job in the completion ring (Reap), and each
--  context's completed value in its status line (Reached), read without
--  IPC. Copy, then validate: everything read from the driver's region is
--  copied before it is used.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.GPU_Queues;

package CuBit.GPU_Queue_Clients is

   package GQ renames CuBit.GPU_Queues;
   package Q renames GQ.Queues;
   subtype Token is Q.Token;

   type Client is limited private;

   --  Own_Region: the client's region (it writes it); Driver_Region: the
   --  driver's, read-only here. Each context's next signal value comes
   --  from its status line.
   procedure Attach (C : in out Client; Own_Region, Driver_Region : System.Address);
   procedure Detach (C : in out Client);
   function Attached (C : Client) return Boolean;

   --  A consistent copy of a context's status line (sequence lock, a
   --  bounded number of tries). OK False: the driver kept rewriting it.
   procedure Read_Status
     (C : Client; Context : GQ.Context_Index; Line : out GQ.Status_Line; OK : out Boolean);
   function State (C : Client; Context : GQ.Context_Index) return GQ.Context_State;
   --  The context's completed value has reached Target (a wait on No_Wait
   --  is always reached).
   function Reached (C : Client; Context : GQ.Context_Index; Target : GQ.Timeline_Value)
     return Boolean;

   --  A job, before the queue fills in its signal value.
   type Wait is record
      Context : GQ.Context_Index := 0;
      Target  : GQ.Timeline_Value := GQ.No_Wait;
   end record;
   type Job is record
      Operation : GQ.Opcode := GQ.Execute;
      Context   : GQ.Context_Index := 0;
      Handle    : Unsigned_32 := 0;
      GPU       : Unsigned_64 := 0;     --  where the batch starts
      Offset    : Unsigned_32 := 0;     --  its offset within the BO
      Bytes     : Unsigned_32 := 0;
      First, Second : Wait;
      Deadline  : GQ.Deadline_Us := GQ.No_Deadline;
   end record;

   --  Room for one more descriptor (taking the driver's consumed index).
   function Can_Submit (C : in out Client) return Boolean;
   --  Descriptors whose records are not reaped yet.
   function Outstanding (C : Client) return Natural;
   --  The value the next job on Context will signal.
   function Next_Signal (C : Client; Context : GQ.Context_Index) return GQ.Timeline_Value;

   --  Write and publish one descriptor. Submitted False: no room (nothing
   --  was written). Kick: the driver sleeps and asked to be kicked (once
   --  per arming).
   procedure Submit
     (C : in out Client; Item : Job; Tag : out Token; Signal : out GQ.Timeline_Value;
      Submitted, Kick : out Boolean);

   --  Take one completion record if one waits; never blocks.
   procedure Reap (C : in out Client; Item : out Q.Completion; Got : out Boolean);
   function Records_Waiting (C : in out Client) return Boolean;

private
   type Signal_Array is array (GQ.Context_Index) of GQ.Timeline_Value;
   type Client is limited record
      Own, Driver : System.Address := System.Null_Address;
      Ring : Q.Client;
      Next_Tag : Token := 0;
      Kicked : Unsigned_32 := 0;
      Next : Signal_Array := [others => 0];
   end record;
end CuBit.GPU_Queue_Clients;
