------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Intel GPU driver's session queue (docs/gpu-async-submission.md,
--  GPU-001 step 2): a submission ring the client produces and a completion
--  ring the driver produces, in memory the two share over grants, so a
--  client submits many jobs and learns of their completion without a call
--  per job. The byte layout is the one C clients use.
--
--  The client opens one queue per GPU session with a Channel_Protocol open
--  (CuBit.Channels, docs/data-plane.md) on the driver's endpoint:
--    Queue_Connector: the queue pair (QUEUE_CONTRACT, duplex). The
--      client's region holds the descriptors and the indices it writes; the
--      driver's region (granted back, read-only) holds the completion
--      records, the indices it writes, its wake word and one status line
--      per context. Each side writes only its own region.
--  Kicks are the channel protocol's OP_KICK on the queue's channel number.
--
--  Rules (CuBit.Submission_Queues): the driver takes a descriptor only
--  while it holds a completion slot for it, so every taken descriptor owes
--  exactly one completion record and a record is never dropped. A full
--  queue is backpressure: the client's Can_Submit says no, and nothing is
--  ever answered "unavailable, retry".
--
--  A descriptor names its signal value, which must be its context's
--  Last_Accepted + 1 (the status line says what that is): the client knows
--  its out-fence value when it submits. Any mismatch faults the context.
--  Every descriptor carries an absolute deadline, in monotonic
--  microseconds (No_Deadline spelled out for none); the driver's own hang
--  watchdog bounds GPU work either way.
--
--  OP_GPU_WAKE [context, target] is a wake request, as OP_FS_WAKE is for the
--  filesystem: answered Woken once the context's completed value reaches
--  target, the context faults or the queue ends, at once if one of those
--  holds already. Submitted asynchronously, its completion wakes the
--  client's own event loop; called, it is the blocking wait. The driver
--  holds at most one per session; a newer one supersedes it (the held one
--  is answered first). Not_Held: the driver's saved-reply capacity is in use
--  by other sessions (the 64-slot capability table); the client then polls
--  its status line on its own timer. It has no timeout of its own: the
--  deadline is the waiting client's.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Protocols;
with CuBit.Submission_Queues;

package CuBit.GPU_Queues with Pure, SPARK_Mode is

   OP_GPU_WAKE : constant := 16#0A31#;

   Queue_Connector : constant := 1;

   Slot_Bits        : constant := 6;
   Slots            : constant := 64;
   Descriptor_Bytes : constant := 64;
   Completion_Bytes : constant := 32;
   Page_Bytes       : constant := 4_096;

   --  Limits (docs/gpu-async-submission.md, "Limits").
   --  Jobs in flight per context: below the ~42 segments its 16 KiB ring
   --  holds, so a context that may take a job always has ring space.
   Max_In_Flight_Per_Context : constant := 32;
   Contexts_Per_Session      : constant := 4;
   Waits_Per_Descriptor      : constant := 2;

   subtype Context_Index is Unsigned_8 range 0 .. Contexts_Per_Session - 1;
   subtype In_Flight_Count is Natural range 0 .. Max_In_Flight_Per_Context;

   --  A timeline value. Zero is reached before any work: a wait on zero
   --  is no wait.
   type Timeline_Value is new Unsigned_64;
   No_Wait : constant Timeline_Value := 0;

   --  Absolute monotonic microseconds.
   type Deadline_Us is new Unsigned_64;
   No_Deadline : constant Deadline_Us := Deadline_Us'Last;

   --  The client's region: what the client writes (indices as
   --  CuBit.Slot_Rings, free-running entry counts, 32 bits).
   Client_Pages          : constant := 2;
   Client_Submitted_At   : constant := 0;      --  descriptors produced
   Client_Reaped_At      : constant := 64;     --  completions consumed
   Client_Descriptors_At : constant := 4_096;

   --  The driver's region: what the driver writes.
   Server_Pages          : constant := 2;
   Server_Completed_At   : constant := 0;      --  completion records produced
   Server_Taken_At       : constant := 64;     --  descriptors consumed
   --  Nonzero while the driver sleeps and wants a kick for new descriptors;
   --  it changes each time it arms, so one kick answers one arming.
   Server_Wake_At        : constant := 128;
   --  One Status_Line_Bytes line per context.
   Server_Status_At      : constant := 1_024;
   Server_Completions_At : constant := 4_096;

   ------------------------------------------------------------------------
   --  Descriptors
   ------------------------------------------------------------------------
   type Opcode is (Invalid, Execute, Signal, VM_Bind, VM_Unbind, Wait);
   for Opcode use
     (Invalid => 0, Execute => 1, Signal => 2, VM_Bind => 3, VM_Unbind => 4, Wait => 5);
   --  Step 2 executes Execute (a batch) and Signal (a timeline-only barrier);
   --  VM_Bind, VM_Unbind and Wait are step 5 and fault the context today.

   --  Byte offsets within a descriptor (the token first).
   Token_At          : constant := 0;
   Opcode_At         : constant := 8;    --  8 bits
   Flags_At          : constant := 9;    --  8 bits, reserved: zero
   Context_At        : constant := 10;   --  8 bits
   Wait_Contexts_At  : constant := 11;   --  wait 1 in bits 0 .. 3, wait 2 in 4 .. 7
   Batch_Handle_At   : constant := 12;   --  32 bits
   Signal_Value_At   : constant := 16;
   Batch_GPU_At      : constant := 24;   --  raw 48-bit PPGTT address the batch starts at
   Batch_Offset_At   : constant := 32;   --  32 bits: the batch's offset within its BO
   Batch_Bytes_At    : constant := 36;   --  32 bits
   Wait_1_Value_At   : constant := 40;
   Wait_2_Value_At   : constant := 48;
   Deadline_At       : constant := 56;

   Byte_Bits : constant := 8;
   Nibble_Bits : constant := 4;
   Word_Bits : constant := 32;
   Long_Bits : constant := 64;

   type Nibble is mod 2 ** Nibble_Bits;

   --  A descriptor after its token, as the wire carries it: every field is
   --  raw until decoded (Intel_GPU_Queue_Admission in the driver).
   type Descriptor is record
      Operation      : Unsigned_8 := 0;
      Flags          : Unsigned_8 := 0;
      Context        : Unsigned_8 := 0;
      Wait_1_Context : Nibble := 0;
      Wait_2_Context : Nibble := 0;
      Batch_Handle   : Unsigned_32 := 0;
      Signal_Value   : Unsigned_64 := 0;
      Batch_GPU      : Unsigned_64 := 0;
      Batch_Offset   : Unsigned_32 := 0;
      Batch_Bytes    : Unsigned_32 := 0;
      Wait_1_Value   : Unsigned_64 := 0;
      Wait_2_Value   : Unsigned_64 := 0;
      Deadline       : Unsigned_64 := Unsigned_64'Last;
   end record;
   for Descriptor use record
      Operation      at Opcode_At - Opcode_At range 0 .. Byte_Bits - 1;
      Flags          at Flags_At - Opcode_At range 0 .. Byte_Bits - 1;
      Context        at Context_At - Opcode_At range 0 .. Byte_Bits - 1;
      Wait_1_Context at Wait_Contexts_At - Opcode_At range 0 .. Nibble_Bits - 1;
      Wait_2_Context at Wait_Contexts_At - Opcode_At range Nibble_Bits .. Byte_Bits - 1;
      Batch_Handle   at Batch_Handle_At - Opcode_At range 0 .. Word_Bits - 1;
      Signal_Value   at Signal_Value_At - Opcode_At range 0 .. Long_Bits - 1;
      Batch_GPU      at Batch_GPU_At - Opcode_At range 0 .. Long_Bits - 1;
      Batch_Offset   at Batch_Offset_At - Opcode_At range 0 .. Word_Bits - 1;
      Batch_Bytes    at Batch_Bytes_At - Opcode_At range 0 .. Word_Bits - 1;
      Wait_1_Value   at Wait_1_Value_At - Opcode_At range 0 .. Long_Bits - 1;
      Wait_2_Value   at Wait_2_Value_At - Opcode_At range 0 .. Long_Bits - 1;
      Deadline       at Deadline_At - Opcode_At range 0 .. Long_Bits - 1;
   end record;
   for Descriptor'Size use (Descriptor_Bytes - 8) * 8;
   for Descriptor'Alignment use 8;

   ------------------------------------------------------------------------
   --  Completion records
   ------------------------------------------------------------------------
   --  Completed: the GPU reached the descriptor's signal value.
   --  Rejected: the descriptor was malformed or not admissible; its context
   --    is faulted (its value is never signalled).
   --  Deadline_Expired: its waits did not hold before its deadline; its
   --    context is faulted.
   --  Context_Faulted: its context had faulted before it ran.
   --  Device_Lost: the GPU hung, reset or failed with it outstanding.
   --  Never Completed for work that did not complete.
   type Completion_Status is
     (Completed, Rejected, Deadline_Expired, Context_Faulted, Device_Lost);
   for Completion_Status use
     (Completed => 1, Rejected => 2, Deadline_Expired => 3, Context_Faulted => 4,
      Device_Lost => 5);

   --  Why a context faulted (the status line's Error, a record's Detail).
   type Fault_Reason is
     (None, Bad_Opcode, Unsupported_Opcode, Bad_Flags, Bad_Context, Signal_Mismatch,
      Bad_Wait, Bad_Batch, Batch_Not_Mapped, Deadline, Hang, Timeline_Fault,
      Ring_Fault, Kick_Failed, Device_Fault, Session_Ended);
   for Fault_Reason use
     (None => 0, Bad_Opcode => 1, Unsupported_Opcode => 2, Bad_Flags => 3,
      Bad_Context => 4, Signal_Mismatch => 5, Bad_Wait => 6, Bad_Batch => 7,
      Batch_Not_Mapped => 8, Deadline => 9, Hang => 10, Timeline_Fault => 11,
      Ring_Fault => 12, Kick_Failed => 13, Device_Fault => 14, Session_Ended => 15);

   Status_At  : constant := 8;    --  Completion_Status, 32 bits
   Record_Context_At : constant := 12;   --  32 bits
   Value_At   : constant := 16;   --  the descriptor's signal value
   Detail_At  : constant := 24;   --  Fault_Reason, 32 bits; then 32 reserved

   --  A completion record after its token.
   type Answer is record
      Status   : Unsigned_32 := 0;
      Context  : Unsigned_32 := 0;
      Value    : Unsigned_64 := 0;
      Detail   : Unsigned_32 := 0;
      Reserved : Unsigned_32 := 0;
   end record;
   for Answer use record
      Status   at Status_At - Status_At range 0 .. Word_Bits - 1;
      Context  at Record_Context_At - Status_At range 0 .. Word_Bits - 1;
      Value    at Value_At - Status_At range 0 .. Long_Bits - 1;
      Detail   at Detail_At - Status_At range 0 .. Word_Bits - 1;
      Reserved at Detail_At - Status_At + 4 range 0 .. Word_Bits - 1;
   end record;
   for Answer'Size use (Completion_Bytes - 8) * 8;
   for Answer'Alignment use 8;

   package Queues is new CuBit.Submission_Queues
     (Descriptor, Answer, Slot_Bits, Slot_Bits);

   ------------------------------------------------------------------------
   --  Status lines (driver CPU writes; one 64-byte line per context)
   ------------------------------------------------------------------------
   type Context_State is (Unused, Active, Faulted, Hung, Lost, Retired);
   for Context_State use
     (Unused => 0, Active => 1, Faulted => 2, Hung => 3, Lost => 4, Retired => 5);

   Status_Line_Bytes  : constant := 64;
   --  Odd while the driver rewrites the line; a reader keeps a copy only if
   --  the count is even and unchanged across it (a sequence lock).
   Line_Version_At    : constant := 0;    --  64 bits
   Line_State_At      : constant := 8;    --  Context_State, 32 bits
   Line_Error_At      : constant := 12;   --  Fault_Reason, 32 bits
   Line_Accepted_At   : constant := 16;   --  Last_Accepted
   --  The context's completed value as the driver last read the GPU's
   --  timeline (a CPU copy, at most one driver turn behind). The GPU-written
   --  timeline page itself is granted in step 3.
   Line_Completed_At  : constant := 24;
   Line_Resets_At     : constant := 32;   --  Reset_Count
   Line_VM_Timeline_At : constant := 40;

   type Status_Line is record
      Version     : Unsigned_64 := 0;
      State       : Unsigned_32 := 0;
      Error       : Unsigned_32 := 0;
      Accepted    : Unsigned_64 := 0;
      Completed   : Unsigned_64 := 0;
      Resets      : Unsigned_64 := 0;
      VM_Timeline : Unsigned_64 := 0;
      Reserved_1  : Unsigned_64 := 0;
      Reserved_2  : Unsigned_64 := 0;
   end record;
   for Status_Line use record
      Version     at Line_Version_At range 0 .. Long_Bits - 1;
      State       at Line_State_At range 0 .. Word_Bits - 1;
      Error       at Line_Error_At range 0 .. Word_Bits - 1;
      Accepted    at Line_Accepted_At range 0 .. Long_Bits - 1;
      Completed   at Line_Completed_At range 0 .. Long_Bits - 1;
      Resets      at Line_Resets_At range 0 .. Long_Bits - 1;
      VM_Timeline at Line_VM_Timeline_At range 0 .. Long_Bits - 1;
      Reserved_1  at Line_VM_Timeline_At + 8 range 0 .. Long_Bits - 1;
      Reserved_2  at Line_VM_Timeline_At + 16 range 0 .. Long_Bits - 1;
   end record;
   for Status_Line'Size use Status_Line_Bytes * 8;
   for Status_Line'Alignment use 8;

   type Status_Lines is array (Context_Index) of Status_Line;

   ------------------------------------------------------------------------
   --  OP_GPU_WAKE: request words 0 = context, 1 = target value. The reply
   --  carries the label and word 0 = Wake_Result.
   ------------------------------------------------------------------------
   type Wake_Result is (Woken, Not_Held, No_Queue);
   for Wake_Result use (Woken => 1, Not_Held => 2, No_Queue => 3);
   Wake_Context_Word : constant := 0;
   Wake_Target_Word  : constant := 1;
   Wake_Result_Word  : constant := 0;

   pragma Compile_Time_Error
     (Queues.Submission'Size /= Descriptor_Bytes * 8 or else
      Queues.Completion'Size /= Completion_Bytes * 8,
      "GPU queue entries must be Descriptor_Bytes and Completion_Bytes");
   pragma Compile_Time_Error
     (Queues.Submissions.Slots /= Slots or else
      Client_Descriptors_At + Slots * Descriptor_Bytes > Client_Pages * Page_Bytes or else
      Server_Wake_At + 4 > Server_Status_At or else
      Server_Status_At + Contexts_Per_Session * Status_Line_Bytes > Server_Completions_At or else
      Server_Completions_At + Slots * Completion_Bytes > Server_Pages * Page_Bytes or else
      Server_Status_At mod Status_Line_Bytes /= 0,
      "GPU queue layout");
   --  A context may take a job only while the completion ring could hold
   --  all of its context's jobs at once.
   pragma Compile_Time_Error
     (Max_In_Flight_Per_Context > Slots, "in-flight jobs exceed completion slots");

   QUEUE_SCHEMA : constant CuBit.Protocols.Schema_Id := 16#4750_5155_4555_0001#;

   QUEUE_CONTRACT : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => QUEUE_SCHEMA, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Descriptor_Bytes),
      Kind    => CuBit.Channel_Contracts.Duplex,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => Client_Pages,
      Buffers => Server_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

end CuBit.GPU_Queues;
