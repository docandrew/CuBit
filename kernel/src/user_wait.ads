-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary The futex pause (IPC-010): UMWAIT/TPAUSE in user mode, bounded
-- by the kernel.
--
-- With WAITPKG (CPUID.07H:ECX[5]) a thread may wait a few microseconds in
-- user mode on its futex word (UMONITOR, UMWAIT) before it asks the kernel
-- to sleep: a peer that answers within the pause costs no system call. The
-- kernel bounds every such wait: IA32_UMWAIT_CONTROL's maximum on each CPU
-- (Linux's value, C0.2 allowed). The pause is published to userspace
-- (System_Publications.Facts_Page.User_Wait_Cycles) only when the build
-- enables it (Build.User_Wait_Pause, off by default until proved on the
-- laptop), the CPU says WAITPKG, and every CPU accepted the MSR write: the
-- write goes through Msr_Safe, so a CPU that refuses it is reported, not a
-- panic, and the pause stays off.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package User_Wait is

    IA32_UMWAIT_CONTROL : constant Unsigned_32 := 16#E1#;
    -- Linux's maximum: 100,000 TSC cycles (bits 31:2), C0.2 allowed (bit 0
    -- clear).
    Maximum_Cycles : constant Unsigned_64 := 100_000;
    Control_Value  : constant Unsigned_64 := Maximum_Cycles;
    pragma Compile_Time_Error (Control_Value mod 4 /= 0,
                               "IA32_UMWAIT_CONTROL's low two bits are not time");

    -- Each CPU, once, as it starts. The boot CPU first.
    procedure Setup_This_CPU (Boot_CPU : Boolean);

    -- What to publish: the maximum, or 0 (no pause) when the build, the
    -- CPU or any CPU's MSR write said no.
    function Published_Cycles return Unsigned_64;

end User_Wait;
