------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Kernel thread regression checks (docs/threads.md). Run as two instances
--  on one CPU so that every context switch hands the CPU to the other.
--
--  Per-thread CPU state: each instance writes its own FS base (thread-local
--  storage) and GS base with WRFSBASE/WRGSBASE, then forces hundreds of
--  context switches. Its FS base must survive every switch unchanged, and
--  its user GS base must read as zero afterwards (user GS is unsupported and
--  cleared on dispatch), so neither can carry the other instance's value.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

procedure Main is
   use ASCII;

   Rounds : constant := 400;

   Failures : Natural := 0;
   Ignore : Unsigned_64;

   function Read_FS return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("rdfsbase %0", Outputs => Unsigned_64'Asm_Output ("=r", Value),
           Volatile => True);
      return Value;
   end Read_FS;

   function Read_GS return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("rdgsbase %0", Outputs => Unsigned_64'Asm_Output ("=r", Value),
           Volatile => True);
      return Value;
   end Read_GS;

   procedure Write_FS (Value : Unsigned_64) is
   begin
      Asm ("wrfsbase %0", Inputs => Unsigned_64'Asm_Input ("r", Value),
           Volatile => True);
   end Write_FS;

   procedure Write_GS (Value : Unsigned_64) is
   begin
      Asm ("wrgsbase %0", Inputs => Unsigned_64'Asm_Input ("r", Value),
           Volatile => True);
   end Write_GS;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("thread-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("thread-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   --  Canonical, distinct per instance; FS is never dereferenced here.
   FS_Value : constant Unsigned_64 := 16#0000_5A5A_0000_0000# + PID * 16#1000#;
   GS_Value : constant Unsigned_64 := 16#0000_3C3C_0000_0000# + PID * 16#1000#;
   FS_Kept, GS_Cleared : Boolean := True;
begin
   debugPrint ("thread-check: starting" & LF);

   Write_FS (FS_Value);
   for Round in 1 .. Rounds loop
      Write_GS (GS_Value);
      --  Sleeping blocks this process; the other instance runs meanwhile.
      Ignore := syscall (SYSCALL_SLEEP, 1);
      if Read_FS /= FS_Value then
         FS_Kept := False;
      end if;
      if Read_GS /= 0 then
         GS_Cleared := False;
      end if;
   end loop;
   Check (FS_Kept, "fs base survives context switches");
   Check (GS_Cleared, "user gs base cleared across context switches");

   if Failures = 0 then
      debugPrint ("TEST: PASS thread-cpu-state" & LF);
   else
      debugPrint ("TEST: FAIL thread-cpu-state" & LF);
   end if;
   Ignore := syscall (SYSCALL_EXIT);
end Main;
