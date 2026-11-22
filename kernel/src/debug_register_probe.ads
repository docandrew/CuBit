-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The debug-register probe (DBG-001, docs/debugging.md): test kernels only
-- (Build.Debug_Register_Probe, DEBUG_REGISTER_PROBE=1), for headless
-- --test mov-ss-debug.
--
-- @description
-- No user debug register can be set in a normal kernel. To check that the
-- kernel survives a data breakpoint that fires on its first instruction
-- (MOV SS or POP SS followed by SYSCALL or INT3, CVE-2018-8897), this probe
-- arms DR0 for the requesting process on this CPU:
--  * "DBGK-PROBE-ARM " and 16 hex digits: a read/write data breakpoint on
--    the byte at that user address (DR0, DR7 = L0 | RW0 read/write);
--  * "DBGK-PROBE-COUNT " and 16 hex digits: store how many #DB exceptions
--    arrived in kernel mode since boot into the caller's word there.
-- Every context switch disarms it (Disarm_Local), so it never reaches
-- another process. Nothing here is reachable in a normal kernel.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Process;

package Debug_Register_Probe is
   Arm_Prefix   : constant String := "DBGK-PROBE-ARM ";
   Count_Prefix : constant String := "DBGK-PROBE-COUNT ";
   Hex_Digits   : constant := 16;

   -- DR7: L0 (bit 0), RW0 = read/write (bits 16-17 = 2#11#), LEN0 = 1 byte.
   Data_Watch_DR7 : constant Unsigned_64 := 16#0003_0001#;

   -- Text is the caller's console write (a chunk): a request when it is a
   -- prefix and Hex_Digits lowercase hex digits. Handled reports True.
   procedure Handle_Request
     (Caller : Process.ProcessID; Text : String; Handled : out Boolean);

   -- DR7 := 0 on this CPU (each context switch, and each kernel #DB).
   procedure Disarm_Local;
end Debug_Register_Probe;
