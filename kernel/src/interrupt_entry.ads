-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary The flags state the interrupt entry stubs give the kernel
--
-- SYSCALL enters with a known RFLAGS (x86.SYSCALL_ENTRY_FLAGS_MASK). An
-- interrupt or exception keeps the interrupted code's flags, so the stubs in
-- interrupt_handlers.asm (isrCommon, istCommon) clear the ones compiled
-- kernel code depends on: DF always (CLD), and AC where the CPU has SMAP.
-- User code may hold AC set (POPFQ at CPL 3 changes it), and under SMAP a
-- set AC lets supervisor code read and write user pages: an interrupt taken
-- then would run its handler with SMAP off. CLAC raises #UD on a CPU
-- without SMAP (Haswell, the x86-64-v3 floor, has none), so the stubs
-- execute it only when this is set; kmain sets it, on such a CPU, before it
-- turns CR4.SMAP on. Without SMAP a set AC changes nothing at CPL 0.
-------------------------------------------------------------------------------
package Interrupt_Entry is
    Clears_Alignment_Check : Boolean := False
        with Export, Convention => C, External_Name => "cubitEntryClearsAC",
             Volatile, Size => 8;
end Interrupt_Entry;
