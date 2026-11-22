-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Debug exceptions (#DB) that arrive in kernel mode (DBG-001).
--
-- @description
-- A #DB in kernel mode comes from a user data breakpoint whose condition
-- was deferred past MOV SS or POP SS onto the kernel's first instruction
-- (CVE-2018-8897), or from a user-address watchpoint hit by a kernel copy.
-- It arrives on IST 4 through the GS-exact entry (interrupt_handlers.asm,
-- isr1 and istCommon). The kernel records it and returns; it never halts
-- for one. Nothing here blocks or takes a lock: an IST handler must not.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Debug_Exceptions is
   -- Kernel-mode #DB exceptions since boot. Diagnostic: two CPUs counting
   -- at once may lose one increment.
   Kernel_Count : Unsigned_64 := 0 with Atomic;

   -- DR6 at the last kernel-mode #DB (diagnostics).
   Last_Kernel_DR6 : Unsigned_64 := 0 with Atomic;

   -- Clear DR6 so the next #DB reports only its own condition, and count.
   -- A probe kernel also disarms its breakpoint (Debug_Register_Probe).
   procedure Handle_Kernel;
end Debug_Exceptions;
