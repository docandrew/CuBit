with Interfaces; use Interfaces;
with System; use System;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;

--  MOV SS / POP SS debug-exception check (DBG-001, docs/debugging.md 3.3;
--  CVE-2018-8897). A data breakpoint whose condition is met by MOV SS is
--  held back one instruction. If that instruction is SYSCALL or INT3, the
--  #DB arrives on the kernel's first instruction: CPL 0, the user's GS base,
--  and (after SYSCALL) the user's stack pointer. A kernel whose #DB entry
--  trusts the pushed CS, or runs on the current stack, double-faults or
--  runs on the user's GS.
--
--  No user debug register exists in a normal kernel, so a probe kernel
--  (DEBUG_REGISTER_PROBE=1) arms DR0 on this process's own word when asked.
--  1. MOV SS from the watched word, then SYSCALL (YIELD): the kernel must
--     take the #DB on its own stack, count it and finish the call.
--  2. MOV SS from the watched word, then INT3: the kernel must take the #DB
--     the same way, then the #BP from user mode, and kill this process for
--     vector 3 (a user INT3 is a #BP, not a #GP).
procedure Main is
   use ASCII;
   Syscall_Yield : constant Unsigned_64 := 118;
   Attempts      : constant := 32;

   Selector : aliased Unsigned_16 := 0 with Volatile;
   Count    : aliased Unsigned_64 := 0 with Volatile;

   function Hex (Value : Unsigned_64) return String is
      Digit_Text : constant String := "0123456789abcdef";
      Result : String (1 .. 16);
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digit_Text (Natural (V and 15) + 1);
         V := Shift_Right (V, 4);
      end loop;
      return Result;
   end Hex;

   --  Straight to the kernel's console write: the probe kernel reads it.
   procedure Request (Text : String) is
      Ignore : constant Unsigned_64 := syscall
        (SYSCALL_WRITE, STDOUT, Unsigned_64 (To_Integer (Text'Address)), Text'Length);
   begin
      null;
   end Request;

   function Address_Hex (A : System.Address) return String is
     (Hex (Unsigned_64 (To_Integer (A))));

   function Kernel_Debug_Count return Unsigned_64 is
   begin
      Request ("DBGK-PROBE-COUNT " & Address_Hex (Count'Address));
      return Count;
   end Kernel_Debug_Count;

   Before, After : Unsigned_64;
   Current_SS : Unsigned_16;
begin
   Asm ("movw %%ss, %0",
        Outputs  => Unsigned_16'Asm_Output ("=r", Current_SS),
        Volatile => True);
   Selector := Current_SS;
   Before := Kernel_Debug_Count;
   debugPrint ("mov-ss-check: kernel-mode #DB before arming: 0x" & Hex (Before) & LF);
   debugPrint ("mov-ss-check: MOV SS from a watched word, then SYSCALL" & LF);
   for Attempt in 1 .. Attempts loop
      Request ("DBGK-PROBE-ARM " & Address_Hex (Selector'Address));
      Asm ("movq %0, %%rax"     & LF & HT &
           "movw (%1), %%ss"    & LF & HT &
           "syscall",
           Inputs   => (Unsigned_64'Asm_Input ("r", Syscall_Yield),
                        System.Address'Asm_Input ("r", Selector'Address)),
           Clobber  => "rax,rcx,r11,memory,cc",
           Volatile => True);
      After := Kernel_Debug_Count;
      exit when After > Before;
   end loop;
   if After > Before then
      debugPrint ("mov-ss-check: SYSCALL variant survived, kernel #DB taken" & LF);
   else
      debugPrint ("mov-ss-check: FAIL the breakpoint never fired" & LF);
   end if;

   debugPrint ("mov-ss-check: MOV SS from a watched word, then INT3" & LF);
   Request ("DBGK-PROBE-ARM " & Address_Hex (Selector'Address));
   Asm ("movw (%0), %%ss" & LF & HT &
        "int3",
        Inputs   => System.Address'Asm_Input ("r", Selector'Address),
        Clobber  => "memory,cc",
        Volatile => True);
   debugPrint ("mov-ss-check: FAIL INT3 returned" & LF);
end Main;
