 ------------------------------------------------------------------------------
 -- CuBit
 -- Copyright (C) 2021 Jon Andrew
 --
 -- @summary
 -- IPC Messages / Syscalls
 ------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System;
with System.Machine_Code; use System.Machine_Code;

package body CuBit.Messages is

   ------------------
   -- System Calls --
   ------------------
   subtype SyscallNumber is Unsigned_64;
 -- SYSCALL_EXIT          : constant SyscallNumber := 0;
 -- SYSCALL_READ          : constant SyscallNumber := 1;
 -- SYSCALL_CLOSE         : constant SyscallNumber := 2;
 -- SYSCALL_EXECVE        : constant SyscallNumber := 3;
 -- SYSCALL_FORK          : constant SyscallNumber := 4;
 -- SYSCALL_FSTAT         : constant SyscallNumber := 5;
 -- SYSCALL_GETPID        : constant SyscallNumber := 6;
 -- SYSCALL_KILL          : constant SyscallNumber := 7;
 -- SYSCALL_SBRK          : constant SyscallNumber := 8;
 -- SYSCALL_TIMES         : constant SyscallNumber := 9;
 -- SYSCALL_UNLINK        : constant SyscallNumber := 10;
 -- SYSCALL_WAIT          : constant SyscallNumber := 11;
   SYSCALL_WRITE         : constant SyscallNumber := 12;

   SYSCALL_INFO          : constant SyscallNumber := 15;
   SYSCALL_SEND          : constant SyscallNumber := 16;
   SYSCALL_RECEIVE       : constant SyscallNumber := 17;
 -- SYSCALL_REPLY         : constant SyscallNumber := 18;
 -- SYSCALL_SEND_EVENT    : constant SyscallNumber := 19;
 -- SYSCALL_RECEIVE_EVENT : constant SyscallNumber := 20;

   -- Access Controller Syscalls --
 -- SYSCALL_CONTROLACCESS : constant SyscallNumber := 100;
 -- SYSCALL_GETTICKET     : constant SyscallNumber := 101;
 -- SYSCALL_GRANT         : constant SyscallNumber := 102;
 -- SYSCALL_REVOKE        : constant SyscallNumber := 103;

   SYSCALL_REGISTER_DRIVER : constant SyscallNumber := 2000;
   
   STDOUT        : constant := 1;

    ---------------------------------------------------------------------------
    -- syscall
    -- Ada interface to syscall instruction (x86_64)
    ---------------------------------------------------------------------------
   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64
   is
      use ASCII;

      ret : Unsigned_64;
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

      Asm
        ("mov %%rax, %0" & LF,
         Outputs => (Unsigned_64'Asm_Output ("=g", ret)),
         Clobber => "memory",
         Volatile => True);

      return ret;
   end syscall;

    ---------------------------------------------------------------------------
    -- sendMsg
    ---------------------------------------------------------------------------
   function sendMsg (to : Unsigned_64; msg : Unsigned_64)
      return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SEND, to, msg);
   end sendMsg;

   function recvMsg (from : out Unsigned_64) return Unsigned_64 is
      function toNum is new
         Ada.Unchecked_Conversion (System.Address, Unsigned_64);
      retfrom : Unsigned_64;
   begin
      return syscall (SYSCALL_RECEIVE, toNum(retfrom'Address));
   end recvMsg;

   function replyMsg return Unsigned_64 is
   begin
      return syscall (SYSCALL_REPLY);
   end replyMsg;

   function getInfo (query : Unsigned_64; detail : Unsigned_64 := 0)
      return Unsigned_64 is
   begin
      return syscall (SYSCALL_INFO, query, detail);
   end getInfo;

   function registerDriver (driver : Unsigned_64) return Unsigned_64 is
   begin
      return syscall (SYSCALL_REGISTER_DRIVER, driver);
   end registerDriver;

   procedure debugPrint (str : String) is
      function toNum is new
         Ada.Unchecked_Conversion (System.Address, Unsigned_64);
      ignore : Unsigned_64;
   begin
      ignore :=
         syscall (SYSCALL_WRITE, STDOUT, toNum (str'Address), str'Length);
   end debugPrint;

   function getSecondaryStack return System.Secondary_Stack.SS_Stack_Ptr
   is
      SYSINFO_SECONDARY_STACK_START : constant Unsigned_64 := 1001;

      function toPtr is
         new Ada.Unchecked_Conversion
            (Source => Unsigned_64,
             Target => System.Secondary_Stack.SS_Stack_Ptr);
   begin
      return toPtr (getInfo (SYSINFO_SECONDARY_STACK_START));
   end getSecondaryStack;

end CuBit.Messages;
