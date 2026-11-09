-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Why the kernel stopped a process, as a typed report in the kernel log ring
-- (Fault_Reports, Kernel_Log; LOG-004), filed under the process's own
-- identity so (logs.recent "<program>") shows it after it is gone.
--
-- @description
-- Called on the paths that stop a process: a CPU exception in user mode, a
-- page fault the kernel refuses or cannot back, and SYSCALL_KILL from
-- another process. The address-space facts come from what Process already
-- keeps (the stack and heap reservations, the loaded image); nothing is
-- tracked for this. Never blocks: interrupt and fault paths call it.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Process;

package Process_Stops with
    SPARK_Mode,
    Abstract_State => (Reports with External),
    Initializes => Reports
is
    -- A CPU exception (Vector, Error_Code) in user mode at RIP with RSP;
    -- Address is CR2 for a page fault, else 0.
    procedure Report_Exception
      (Victim : Process.ProcessID;
       Vector, Error_Code, Address, RIP, RSP : Unsigned_64)
      with Global => (In_Out => Reports);

    -- A demand page at Address the kernel could not back.
    procedure Report_Out_Of_Memory
      (Victim : Process.ProcessID; Address, RIP, RSP : Unsigned_64)
      with Global => (In_Out => Reports);

    -- Stopper stopped Victim (SYSCALL_KILL).
    procedure Report_Stopped (Victim, Stopper : Process.ProcessID)
      with Global => (In_Out => Reports);
end Process_Stops;
