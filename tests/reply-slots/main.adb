--  Hosted tests for the per-thread reply slot (BUG-004, IPC-000 rule 3):
--  a server receives a call and, without answering it, receives again. The
--  first call's authority must come back from the second receive (to be
--  ended by the kernel), never be dropped; only the second call can still
--  be answered. Whether the kernel then wakes the first caller is
--  Call_Sequences.Accepts: only while it still waits on that call.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Capabilities.Operations; use Capabilities.Operations;
with Call_Sequences;
with IPC_Labels;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   No_Request : constant Unsigned_64 := 0;

   --  A synchronous call's authority, as installReceivedWork mints it: the
   --  caller thread, no request ID, the caller's call sequence.
   function Call_Reply (Thread : Unsigned_64; Sequence : Call_Sequences.Sequence)
     return Capability is
     ((capType => CAP_REPLY, rights => [RIGHT_READ | RIGHT_WRITE => True, others => False],
       authorityTag => Sequence, object => (ref => Thread, param => No_Request), gen => 1));

   --  An asynchronous request's authority: the submitting process and the
   --  request ID.
   function Async_Reply (Process, Request : Unsigned_64) return Capability is
     ((capType => CAP_REPLY, rights => [RIGHT_READ | RIGHT_WRITE => True, others => False],
       authorityTag => NO_AUTHORITY_TAG, object => (ref => Process, param => Request), gen => 1));

   Slot      : Capability := NULL_CAPABILITY;
   Displaced : Capability;
   Answered  : Capability;
   Taken     : Boolean;
   First     : constant Capability := Call_Reply (7, 1);
   Second    : constant Capability := Call_Reply (8, 1);
   Caller_Sequence : Call_Sequences.Sequence := 1;
begin
   --  The regression: receive, receive again, then answer.
   Replace_Reply : begin
      replaceReplyCap (Slot, First, Displaced);
      Check (Displaced = NULL_CAPABILITY, "the first receive displaces nothing");
      replaceReplyCap (Slot, Second, Displaced);
      Check (Displaced = First, "the second receive hands back the first call's authority");
      Check (Slot = Second, "the slot holds the second call");
      Check (Call_Sequences.Accepts (True, Caller_Sequence, Displaced.authorityTag),
             "the first caller, still waiting, is ended");
      takeReplyCapFrom (Slot, Answered, Taken);
      Check (Taken and then Answered = Second, "only the second call is answered");
      takeReplyCapFrom (Slot, Answered, Taken);
      Check (not Taken, "and only once");
   end Replace_Reply;

   --  A caller that timed out and began another call is not ended by the
   --  stale authority (it waits on a different call now).
   Stale : begin
      replaceReplyCap (Slot, First, Displaced);
      Caller_Sequence := Call_Sequences.Next (Caller_Sequence);
      replaceReplyCap (Slot, NULL_CAPABILITY, Displaced);
      Check (Displaced = First, "an empty poll hands the authority back too");
      Check (not Call_Sequences.Accepts (True, Caller_Sequence, Displaced.authorityTag),
             "a caller now in its next call is left alone");
      Check (Slot = NULL_CAPABILITY, "after an empty poll the slot is empty");
   end Stale;

   --  An asynchronous request displaced by a call.
   Async : begin
      replaceReplyCap (Slot, Async_Reply (3, 42), Displaced);
      replaceReplyCap (Slot, Second, Displaced);
      Check (Displaced = Async_Reply (3, 42), "a displaced async request comes back");
      Check (Displaced.object.param = 42, "with its request ID, to complete it");
   end Async;

   --  A thread that exits owing a reply: retiring the slot hands it back.
   Retire : begin
      replaceReplyCap (Slot, NULL_CAPABILITY, Displaced);
      Check (Displaced = Second, "an exiting thread's owed reply comes back");
      replaceReplyCap (Slot, NULL_CAPABILITY, Displaced);
      Check (Displaced = NULL_CAPABILITY, "an empty slot owes nothing");
   end Retire;

   --  What an ended call wakes with: a label only the kernel gives, so a
   --  server cannot answer with it, and distinct from the timeout and from
   --  a server's own label 0.
   Labels : begin
      Check (IPC_Labels.Is_Kernel_Reply (IPC_Labels.REPLY_ENDED), "REPLY_ENDED is a kernel-only label");
      Check (IPC_Labels.REPLY_ENDED /= IPC_Labels.REPLY_TIMEOUT, "an end is not a timeout");
      Check (not IPC_Labels.Is_Kernel_Reply (0), "a server may answer label 0, never read as an end");
   end Labels;

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("reply-slots: PASS");
   else
      Ada.Text_IO.Put_Line ("reply-slots: FAIL" & Failures'Image);
      raise Program_Error with "reply-slots failed";
   end if;
end Main;
