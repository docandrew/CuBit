with CuBit.Console_Route;
with Interfaces; use Interfaces;
with System; use System;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

--  Interrupt-entry direction flag check (WEB-001 guest resets): user code
--  may run with RFLAGS.DF set (a backward memmove is STD; REP MOVS; CLD),
--  and an interrupt or exception does not clear it. The kernel's compiled
--  code assumes DF clear, so without a CLD on entry each REP MOVS/STOS it
--  runs copies backwards and writes below its buffers: under Penny the
--  kernel overwrote a return address, recursed on its own page faults down
--  through its .bss (its page tables among it) and the machine reset.
--  Here every round sets DF, touches fresh pages of a buffer (page faults)
--  and spins between them (timer interrupts), all in one assembly block,
--  then clears DF and checks the buffer. Last, with DF set, it executes UD2:
--  the kernel kills it for exception 6, and its stop record is built with
--  REP STOS (Kernel_Log_Publication.Make_Stop), which with DF set zeroed
--  Make_Stop's own return address. The headless test direction-flag looks
--  for the rounds, for the kernel's stop record of this process, and for no
--  kernel fault.
procedure Main is
   use ASCII;
   Page_Bytes      : constant := 4096;
   Rounds          : constant := 8;
   Pages_Per_Round : constant := 256;
   --  Timestamp-counter ticks spun after each page: interrupts land with
   --  DF set (about 2 s in all at a few GHz).
   Ticks_Per_Page  : constant Unsigned_64 := 4_000_000;
   Marker          : constant Unsigned_8 := 16#A5#;

   type Byte_Array is array (Natural range <>) of Unsigned_8;
   type Byte_Access is access Byte_Array;
   Buffer : constant Byte_Access :=
     new Byte_Array (0 .. Rounds * Pages_Per_Round * Page_Bytes - 1);
   Wrong : Natural := 0;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("direction-flag-check: running with DF set" & LF);
   for Round in 0 .. Rounds - 1 loop
      Asm ("movq %0, %%r8"                & LF & HT &
           "movq %1, %%r9"                & LF & HT &
           "std"                          & LF & HT &
           "1: movb %3, (%%r8)"           & LF & HT &
           "addq %4, %%r8"                & LF & HT &
           "rdtsc"                        & LF & HT &
           "shlq $32, %%rdx"              & LF & HT &
           "orq %%rdx, %%rax"             & LF & HT &
           "movq %%rax, %%r10"            & LF & HT &
           "2: pause"                     & LF & HT &
           "rdtsc"                        & LF & HT &
           "shlq $32, %%rdx"              & LF & HT &
           "orq %%rdx, %%rax"             & LF & HT &
           "subq %%r10, %%rax"            & LF & HT &
           "cmpq %2, %%rax"               & LF & HT &
           "jb 2b"                        & LF & HT &
           "decq %%r9"                    & LF & HT &
           "jnz 1b"                       & LF & HT &
           "cld",
           Inputs   => (Address'Asm_Input
                          ("r", Buffer (Round * Pages_Per_Round * Page_Bytes)'Address),
                        Unsigned_64'Asm_Input ("r", Pages_Per_Round),
                        Unsigned_64'Asm_Input ("r", Ticks_Per_Page),
                        Unsigned_8'Asm_Input ("q", Marker),
                        Unsigned_64'Asm_Input ("r", Page_Bytes)),
           Clobber  => "rax,rdx,r8,r9,r10,memory,cc",
           Volatile => True);
      for Page in Round * Pages_Per_Round .. (Round + 1) * Pages_Per_Round - 1 loop
         if Buffer (Page * Page_Bytes) /= Marker then
            Wrong := Wrong + 1;
         end if;
      end loop;
      debugPrint ("direction-flag-check: round" & Integer'Image (Round + 1) & LF);
   end loop;
   if Wrong /= 0 then
      debugPrint ("direction-flag-check: FAIL" & Natural'Image (Wrong) & " pages lost their write" & LF);
      return;
   end if;
   debugPrint ("direction-flag-check: rounds done; faulting with DF set" & LF);
   Asm ("std" & LF & HT & "ud2", Volatile => True);
   debugPrint ("direction-flag-check: FAIL UD2 did not fault" & LF);
end Main;
