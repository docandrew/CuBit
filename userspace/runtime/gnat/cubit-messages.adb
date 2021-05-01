 ------------------------------------------------------------------------------
 -- CuBit
 -- Copyright (C) 2021 Jon Andrew
 --
 -- @summary
 -- IPC Messages / Syscalls
 ------------------------------------------------------------------------------
with System.Machine_Code; use System.Machine_Code;

package body CuBit.Messages is

    ---------------------------------------------------------------------------
    -- syscall
    -- Ada interface to syscall instruction (x86_64)
    ---------------------------------------------------------------------------
   procedure syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0)
   is
      use ASCII;
   begin
      Asm
        ("mov %0, %%rax" & LF & "mov %1, %%rdi" & LF & "mov %2, %%rsi" & LF &
         "mov %3, %%rdx" & LF & "mov %4, %%rcx" & LF & "mov %5, %%r8" & LF &
         "mov %6, %%r9" & LF & "syscall",
         Inputs =>
           (Unsigned_64'Asm_Input ("g", call),
            Unsigned_64'Asm_Input ("g", arg0),
            Unsigned_64'Asm_Input ("g", arg1),
            Unsigned_64'Asm_Input ("g", arg2),
            Unsigned_64'Asm_Input ("g", arg3),
            Unsigned_64'Asm_Input ("g", arg4),
            Unsigned_64'Asm_Input ("g", arg5)),
         Clobber => "rax, rdi, rsi, rdx, rcx, r8, r9", Volatile => True);
   end syscall;

    ---------------------------------------------------------------------------
    -- sendMsg
    ---------------------------------------------------------------------------
   procedure sendMsg (to : Unsigned_64; msg : Unsigned_64) is
   begin
      syscall (42, to, msg);
   end sendMsg;

end CuBit.Messages;
