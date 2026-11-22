------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel trace object: reset the scheduling and system-call trace,
--  print its summary, and (LATENCY_TRACE kernels) record, freeze and dump
--  one CPU's event timeline, as a Call on a CAP_TRACE handle. Shared by the
--  kernel (Trace_Object) and the runtime (CuBit.Handles.Tracing). Replaces
--  SYSCALL_TRACE_RESET (82) and SYSCALL_TRACE_SUMMARY (83) (ABI-002).
--
--  @description
--  A kernel object reached through the ordinary Call: the kernel resolves
--  the handle (type, generation, rights: Write) and runs the operation in
--  ring 0 with no process switch, then writes the reply into the caller's
--  message. Only benchmark programs hold one: procmgr installs a Write-only
--  copy where a manifest declares (Request.Trace "<binding>") and the
--  startup plan approves it, (trace approve-declared).
--
--  Request: MessageTag.label = Operation'Enum_Rep, length = Request_Words
--  (no words). Reply: label = the request's, length = Reply_Words,
--  words (Outcome_Word) = Outcome'Enum_Rep.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Trace_ABI with Pure, SPARK_Mode is

   --  Reset: clear every CPU's trace and start recording.
   --  Summary: print the event counts and duration histograms and the
   --    caller's CPU accounting on the console.
   --  Start_Local, Freeze_Local, Dump_Local: the calling CPU's event
   --    timeline (LATENCY_TRACE kernels only, else Not_In_This_Kernel).
   type Operation is (Reset, Summary, Start_Local, Freeze_Local, Dump_Local);
   for Operation use
     (Reset => 1, Summary => 2, Start_Local => 3, Freeze_Local => 4, Dump_Local => 5);

   Request_Words : constant := 0;
   Outcome_Word  : constant := 0;
   Reply_Words   : constant := 1;

   --  Codes start at 1 and are fixed.
   type Outcome is
     (Done,                --  the operation ran
      Malformed,           --  a request with words
      Unknown_Operation,   --  the label is no Operation
      Not_Permitted,       --  the handle lacks Write
      Not_In_This_Kernel); --  a timeline operation without LATENCY_TRACE
   for Outcome use
     (Done               => 1,
      Malformed          => 2,
      Unknown_Operation  => 3,
      Not_Permitted      => 4,
      Not_In_This_Kernel => 5);

   function Outcome_Word_Of (What : Outcome) return Unsigned_64 is
     (Unsigned_64 (Outcome'Enum_Rep (What)));

   function Is_Outcome_Word (Word : Unsigned_64) return Boolean is
     (Word in Outcome_Word_Of (Outcome'First) .. Outcome_Word_Of (Outcome'Last));

   function Is_Operation_Label (Label : Unsigned_64) return Boolean is
     (Label in Unsigned_64 (Operation'Enum_Rep (Operation'First))
             .. Unsigned_64 (Operation'Enum_Rep (Operation'Last)));

   --  What a request asks: Valid with its operation, or the refusal.
   type Decision (Valid : Boolean := False) is record
      case Valid is
         when True  => Op  : Operation;
         when False => Why : Outcome;
      end case;
   end record;

   --  Writable: the handle carries Write. Timeline: the kernel was built
   --  with LATENCY_TRACE. Checked in this order: rights, operation, words,
   --  then whether the kernel has the operation.
   function Decide
     (Writable : Boolean; Label : Unsigned_64; Length : Unsigned_64; Timeline : Boolean)
      return Decision
   is (if not Writable then (Valid => False, Why => Not_Permitted)
       elsif not Is_Operation_Label (Label) then (Valid => False, Why => Unknown_Operation)
       elsif Length /= Request_Words then (Valid => False, Why => Malformed)
       elsif not Timeline
         and then Label >= Unsigned_64 (Operation'Enum_Rep (Start_Local))
       then (Valid => False, Why => Not_In_This_Kernel)
       else (Valid => True, Op => Operation'Enum_Val (Label)))
   with Post => (if Decide'Result.Valid
                 then Writable and then Length = Request_Words
                      and then Operation'Enum_Rep (Decide'Result.Op) = Label
                      and then (Timeline or else Decide'Result.Op in Reset | Summary));

end Trace_ABI;
