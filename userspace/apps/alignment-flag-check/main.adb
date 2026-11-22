with CuBit.Console_Route;
with Interfaces; use Interfaces;
with System; use System;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

--  Interrupt-entry alignment-check flag check (SEC-026): user code may set
--  RFLAGS.AC (POPFQ changes it at CPL 3), and an interrupt or exception
--  keeps it. Under SMAP a set AC lets supervisor code touch user pages, so
--  unless the entry stubs clear it every interrupt handler runs with SMAP
--  off. SYSCALL already clears AC (its flags mask).
--  Each round sets AC, touches fresh pages of a buffer (page faults) and
--  spins between them (timer interrupts), all in one assembly block, then
--  clears AC. A test kernel (SMAP_ENTRY_PROBE=1) reads this code's own page
--  from each such interrupt and reports whether SMAP stopped the read; the
--  headless test alignment-flag wants the fault and never the read.
procedure Main is
   use ASCII;
   Page_Bytes      : constant := 4096;
   Rounds          : constant := 8;
   Pages_Per_Round : constant := 64;
   --  Timestamp-counter ticks spun after each page: interrupts land with
   --  AC set (about 1 s in all at a few GHz).
   Ticks_Per_Page  : constant Unsigned_64 := 4_000_000;
   Marker          : constant Unsigned_8 := 16#5A#;
   --  RFLAGS.AC (the assembly's $0x40000 and its complement $-0x40001).
   Alignment_Check : constant Unsigned_64 := 16#0004_0000#;

   type Byte_Array is array (Natural range <>) of Unsigned_8;
   type Byte_Access is access Byte_Array;
   Buffer : constant Byte_Access :=
     new Byte_Array (0 .. Rounds * Pages_Per_Round * Page_Bytes - 1);
   Held  : Unsigned_64;
   Wrong : Natural := 0;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("alignment-flag-check: running with AC set" & LF);
   for Round in 0 .. Rounds - 1 loop
      Asm ("movq %1, %%r8"                & LF & HT &
           "movq %2, %%r9"                & LF & HT &
           "pushfq"                       & LF & HT &
           "orq $0x40000, (%%rsp)"        & LF & HT &
           "popfq"                        & LF & HT &
           "1: movb %4, (%%r8)"           & LF & HT &
           "addq %5, %%r8"                & LF & HT &
           "rdtsc"                        & LF & HT &
           "shlq $32, %%rdx"              & LF & HT &
           "orq %%rdx, %%rax"             & LF & HT &
           "movq %%rax, %%r10"            & LF & HT &
           "2: pause"                     & LF & HT &
           "rdtsc"                        & LF & HT &
           "shlq $32, %%rdx"              & LF & HT &
           "orq %%rdx, %%rax"             & LF & HT &
           "subq %%r10, %%rax"            & LF & HT &
           "cmpq %3, %%rax"               & LF & HT &
           "jb 2b"                        & LF & HT &
           "decq %%r9"                    & LF & HT &
           "jnz 1b"                       & LF & HT &
           "pushfq"                       & LF & HT &
           "movq (%%rsp), %0"             & LF & HT &
           "andq $-0x40001, (%%rsp)"      & LF & HT &
           "popfq",
           Outputs  => Unsigned_64'Asm_Output ("=&r", Held),
           Inputs   => (Address'Asm_Input
                          ("r", Buffer (Round * Pages_Per_Round * Page_Bytes)'Address),
                        Unsigned_64'Asm_Input ("r", Pages_Per_Round),
                        Unsigned_64'Asm_Input ("r", Ticks_Per_Page),
                        Unsigned_8'Asm_Input ("q", Marker),
                        Unsigned_64'Asm_Input ("r", Page_Bytes)),
           Clobber  => "rax,rdx,r8,r9,r10,memory,cc",
           Volatile => True);
      if (Held and Alignment_Check) = 0 then
         debugPrint ("alignment-flag-check: FAIL AC did not stay set" & LF);
         return;
      end if;
      for Page in Round * Pages_Per_Round .. (Round + 1) * Pages_Per_Round - 1 loop
         if Buffer (Page * Page_Bytes) /= Marker then
            Wrong := Wrong + 1;
         end if;
      end loop;
      debugPrint ("alignment-flag-check: round" & Integer'Image (Round + 1) & LF);
   end loop;
   if Wrong /= 0 then
      debugPrint ("alignment-flag-check: FAIL" & Natural'Image (Wrong) & " pages lost their write" & LF);
      return;
   end if;
   debugPrint ("alignment-flag-check: done" & LF);
end Main;
