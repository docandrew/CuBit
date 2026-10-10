------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The pure rules Mesa's session-queue glue (Native_GPU_Queue, GPU-001
--  step 3) applies before it writes a descriptor or waits: which batches it
--  may describe, how a Vulkan wait's remaining nanoseconds become a call
--  deadline, and which status-line states mean the device is lost. Proved
--  (SPARK level 2); no IPC, no clock.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.GPU_Queues;

package Native_GPU_Job_Rules with Pure, SPARK_Mode is

   package GQ renames CuBit.GPU_Queues;

   --  A batch the driver's admission accepts (Intel_GPU_Queue_Admission
   --  checks the same and more): a named BO, a raw 48-bit PPGTT address,
   --  qword-aligned start, whole dwords, at most Max_Batch_Bytes, inside
   --  the first Max_Batch_Bytes of its BO, not wrapping the address space.
   GPU_Address_Limit : constant := 2 ** 48;
   Max_Batch_Bytes   : constant := 16 * 1024 * 1024;
   Batch_Alignment   : constant := 8;
   Dword_Bytes       : constant := 4;

   function Valid_Batch
     (Handle : Unsigned_32; GPU : Unsigned_64; Offset, Bytes : Unsigned_32) return Boolean is
     (Handle /= 0 and then GPU /= 0 and then GPU < GPU_Address_Limit and then
      GPU mod Batch_Alignment = 0 and then Bytes /= 0 and then Bytes mod Dword_Bytes = 0 and then
      Bytes <= Max_Batch_Bytes and then Offset mod Batch_Alignment = 0 and then
      Offset <= Max_Batch_Bytes - Bytes and then Unsigned_64 (Bytes) <= GPU_Address_Limit - GPU);

   --  Vulkan's "no timeout" (OS_TIMEOUT_INFINITE): Remaining_Ns of
   --  No_Timeout_Ns waits for ever, spelled out as Forever, which is
   --  CuBit.Messages.Wait_Forever (Native_GPU_Queue checks they agree).
   No_Timeout_Ns : constant Unsigned_64 := Unsigned_64'Last;
   Forever       : constant Unsigned_64 := Unsigned_64'Last;
   Nanoseconds_Per_Millisecond : constant := 1_000_000;

   --  The absolute millisecond deadline for a wait with Remaining_Ns left
   --  when the millisecond clock reads Now_Ms: rounded up, so it never
   --  expires early, and saturated below Forever, so a finite wait stays
   --  finite.
   function Call_Deadline (Now_Ms, Remaining_Ns : Unsigned_64) return Unsigned_64 is
     (if Remaining_Ns = No_Timeout_Ns then Forever
      else (declare
              Whole : constant Unsigned_64 :=
                Remaining_Ns / Nanoseconds_Per_Millisecond +
                (if Remaining_Ns mod Nanoseconds_Per_Millisecond = 0 then 0 else 1);
            begin
              (if Whole >= Forever - 1 - Now_Ms then Forever - 1 else Now_Ms + Whole)))
     with Pre  => Now_Ms < Forever,
          Post => (if Remaining_Ns = No_Timeout_Ns then Call_Deadline'Result = Forever
                   else Call_Deadline'Result < Forever and then Call_Deadline'Result >= Now_Ms and then
                        (Call_Deadline'Result = Forever - 1 or else
                         (Call_Deadline'Result - Now_Ms >= Remaining_Ns / Nanoseconds_Per_Millisecond
                          and then
                            (if Remaining_Ns mod Nanoseconds_Per_Millisecond /= 0 then
                               Call_Deadline'Result - Now_Ms >
                                 Remaining_Ns / Nanoseconds_Per_Millisecond))));

   --  A status line's state: the context still runs work (Active), or the
   --  session has no such context (Unused). Anything else, or a value the
   --  driver does not write, means the device is lost to this session.
   function Healthy (State : Unsigned_32) return Boolean is
     (State = GQ.Context_State'Enum_Rep (GQ.Active) or else
      State = GQ.Context_State'Enum_Rep (GQ.Unused));

end Native_GPU_Job_Rules;
