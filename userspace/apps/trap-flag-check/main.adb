with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

--  Syscall-entry trap flag check (SEC-023 #1): set RFLAGS.TF and execute
--  SYSCALL. The kernel's FMASK must clear TF on entry; otherwise a
--  single-step #DB fires on the first kernel instruction while RSP is still
--  this process's stack pointer, and the CPU writes the exception frame
--  there (a double fault at best, a kernel write where the process chose at
--  worst). With the fix the syscall completes, SYSRET restores TF, and the
--  single-step trap arrives in user mode on the next instruction, so the
--  kernel kills this process for exception 1 and keeps running. The headless
--  test trap-flag looks for that kill and for no double fault.
procedure Main is
   use ASCII;
   --  syscall.ads: SYSCALL_GETPID; harmless, takes no arguments.
   Syscall_Get_PID : constant Unsigned_64 := 6;
   --  RFLAGS.TF (x86.FLAGS_TRAP in the kernel).
   Trap_Flag       : constant Unsigned_64 := 16#100#;
begin
   debugPrint ("trap-flag-check: setting TF, then SYSCALL" & LF);
   --  POPFQ sets TF; the instruction after POPFQ must be SYSCALL itself.
   Asm ("movq %0, %%rax"             & LF & HT &
        "pushfq"                     & LF & HT &
        "orq %1, (%%rsp)"            & LF & HT &
        "popfq"                      & LF & HT &
        "syscall",
        Inputs   => (Unsigned_64'Asm_Input ("r", Syscall_Get_PID),
                     Unsigned_64'Asm_Input ("r", Trap_Flag)),
        Clobber  => "rax,rcx,r11,memory,cc",
        Volatile => True);
   debugPrint ("trap-flag-check: FAIL single-step did not trap" & LF);
end Main;
