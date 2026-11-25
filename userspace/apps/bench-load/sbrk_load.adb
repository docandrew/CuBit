--  SPDX-License-Identifier: GPL-3.0-or-later
--  bench-sbrk-load: heap growth beside other work (tests/headless
--  init-window-stress.ccl). Grows its heap by Step_Bytes with SBRK Rounds
--  times; the kernel prepares each step's frames with interrupt windows
--  open, where the thread may be preempted and resume on another CPU.
--  Each step's pages get this process's mark, and every page grown so far
--  is checked after each step: a growth the kernel applied to another
--  process (a system call that read its caller through a stale per-CPU
--  overlay after a move) leaves pages missing or marked by someone else.
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Waits;
procedure Sbrk_Load is
   Settle_Ms  : constant := 500;
   Step_Bytes : constant := 4 * 1024 * 1024;
   Rounds     : constant := 16;
   Page_Bytes : constant := 4_096;
   Failed : constant Unsigned_64 := Unsigned_64'Last;
   Ignored : Unsigned_64;
   First_Base, Base : Unsigned_64 := 0;
   Mark : Unsigned_64;
   Bad_Pages, Misplaced : Unsigned_64 := 0;
   Grown : Natural := 0;

   function Word_At (Address : Unsigned_64) return Unsigned_64 is
      W : Unsigned_64 with Import, Volatile,
        Address => To_Address (Integer_Address (Address));
   begin
      return W;
   end Word_At;

   procedure Set_Word (Address, Value : Unsigned_64) is
      W : Unsigned_64 with Import, Volatile,
        Address => To_Address (Integer_Address (Address));
   begin
      W := Value;
   end Set_Word;
begin
   Ignored := CuBit.Waits.Sleep_For (Settle_Ms);
   Mark := Own_Process_Word;
   debugPrint ("SBRK-LOAD: START step_bytes=" & Unsigned_64'Image (Step_Bytes) &
               " rounds=" & Natural'Image (Rounds) & " pid=" & Mark'Image & ASCII.LF);
   for Round in 1 .. Rounds loop
      --  SBRK answers the old break: the step's first byte.
      Base := syscall (SYSCALL_SBRK, Step_Bytes);
      exit when Base = Failed or else Base = 0;
      if First_Base = 0 then
         First_Base := Base;
      elsif Base /= First_Base + Unsigned_64 (Round - 1) * Step_Bytes then
         --  The break moved by other than this process's own steps.
         Misplaced := Misplaced + 1;
         exit;
      end if;
      Grown := Round;
      for Page in 0 .. Unsigned_64 (Step_Bytes / Page_Bytes) - 1 loop
         Set_Word (Base + Page * Page_Bytes, Mark);
      end loop;
      for Page in 0 .. Unsigned_64 (Round) * Unsigned_64 (Step_Bytes / Page_Bytes) - 1 loop
         if Word_At (First_Base + Page * Page_Bytes) /= Mark then
            Bad_Pages := Bad_Pages + 1;
         end if;
      end loop;
   end loop;
   debugPrint ("SBRK-LOAD: COMPLETE rounds=" & Natural'Image (Grown) &
               " bad_pages=" & Bad_Pages'Image &
               " misplaced=" & Misplaced'Image & " pid=" & Mark'Image & ASCII.LF);
   if Grown = Rounds and then Bad_Pages = 0 and then Misplaced = 0 then
      debugPrint ("SBRK-LOAD: PASS pid=" & Mark'Image & ASCII.LF);
   else
      debugPrint ("SBRK-LOAD: FAIL pid=" & Mark'Image & ASCII.LF);
   end if;
end Sbrk_Load;
