------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Channel wakes (CAP-002 step 3; docs/capability-handles.md section 4),
--  shared by the kernel and the runtime. No system call of their own: a
--  wake is SEND_EVENT (19) with Wake_Label, coalesced in the kernel.
--
--  @description
--  A channel side wakes its peer when the peer declared itself asleep (its
--  needs-wake word in the shared ring, CuBit.Request_Channels): SEND_EVENT
--  with
--    label   Wake_Label, length 1 (or 2 with a Sync_Hint in word 1);
--    word 0  the reference word of the grant of the region the sender owns
--            (CuBit.Grant_References' wire form: generation << 32 | slot);
--    dest    ignored: the wake goes to that grant's grantee;
--    arg 6   ignored: owning the grant is the authority.
--  Result 1: accepted (pending, or already pending: coalesced); 0: refused
--  (not the grant's owner, or the grant has ended).
--
--  The kernel keeps one pending wake per grant (per sender and channel), so
--  wakes never consume the receiver's request credits, never overflow and
--  are never lost. The grantee's receive (any mixed or event receive) takes
--  them as one event: label Wake_Label, sender NO_PROCESS, words 0 .. n-1 up
--  to Wakes_Per_Event grant reference words. A process cannot forge one:
--  every SEND_EVENT with Wake_Label goes through the coalescing path.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Channel_Wake_ABI with Pure, SPARK_Mode is

   Wake_Label      : constant := 16#010F#;
   Wake_Length     : constant := 1;
   --  Length 2 with word 1 = Sync_Hint: the sender is about to block (its
   --  end of turn); the kernel places the woken thread on the sender's CPU,
   --  so it runs there when the sender sleeps, with no inter-processor wake
   --  (Linux's WF_SYNC). Only placement changes; the scheduler still orders.
   Sync_Length     : constant := 2;
   Sync_Hint       : constant Unsigned_64 := 1;
   Wakes_Per_Event : constant := 4;

   Accepted_Word : constant Unsigned_64 := 1;
   Refused_Word  : constant Unsigned_64 := 0;

end Channel_Wake_ABI;
