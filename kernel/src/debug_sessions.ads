-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Debug sessions (DBG-001, docs/debugging.md; Debug_ABI): four tightly
-- scoped debug capabilities, minted only from a target's Control, and the
-- kernel object each one names. Reached by Call (Kernel_Object_Calls); no
-- system call of its own.
--
-- @description
-- A session belongs to one target process instance and is one of four
-- kinds (CAP_DEBUG_OBSERVE, _PAUSE, _MODIFY, _INSTRUMENT). Each Mint_Debug
-- makes a new session with its own generation, so each is revoked on its
-- own (Revoke_Debug through the Control, Detach through the session).
-- Only Debug_Observe's operations run in this slice (Debug_Admission):
--  * Threads: each live thread's slot, generation, state, what it waits on;
--  * Registers: a thread's saved user registers, while it is off every CPU
--    (its InterruptStackFrame at the top of its kernel stack, which SYSCALL
--    entry and every interrupt from ring 3 build, and its XSAVE image);
--  * Map_View / Unmap_View: the target's own frames, read-only and never
--    executable, in the viewer's slice of the view aperture
--    (Debug_ABI.Aperture_First). Each mapped frame holds a pin, so a frame
--    the target frees meanwhile is reclaimed only when the view goes
--    (BuddyAllocator's deferred free): it never reaches another owner while
--    viewed. The kernel never maps those frames itself (KERN-014).
-- Shielded memory: SEC-022's shielded regions do not exist yet; when they
-- do, Map_View refuses them, whatever the kind.
--
-- Locking: Table_Lock serializes every session operation, and is taken
-- before Process.lock (Threads, Registers) or an address-space lock
-- (views), never after one. TLB rounds run under it (spinning waiters
-- service rounds, Spinlocks).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Capabilities;
with Process;

package Debug_Sessions is

   -- The four debug capability kinds.
   function Is_Debug_Kind (K : Capabilities.CapabilityType) return Boolean is
     (K in Capabilities.CAP_DEBUG_OBSERVE | Capabilities.CAP_DEBUG_PAUSE |
           Capabilities.CAP_DEBUG_MODIFY | Capabilities.CAP_DEBUG_INSTRUMENT);

   -- A Call on a CAP_PROCESS handle: Mint_Debug or Revoke_Debug. Answered
   -- False (Reply untouched) for any other label: not this object's.
   procedure Control_Call
     (Caller   : Process.ProcessID;
      Control  : Capabilities.Capability;
      Request  : Process.Message;
      Reply    : out Process.Message;
      Answered : out Boolean);

   -- A Call on a debug handle (Is_Debug_Kind (Cap.capType)).
   procedure Call
     (Caller  : Process.ProcessID;
      Cap     : Capabilities.Capability;
      Request : Process.Message;
      Reply   : out Process.Message);

   -- Pid is being reclaimed (Process.reclaimProcess): its page tables still
   -- exist and no global lock is held. Ends every session that targets it
   -- or that it views through, removing their views first.
   procedure Process_Ended (Pid : Process.ProcessID);

end Debug_Sessions;
