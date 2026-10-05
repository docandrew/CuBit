with Interfaces; use Interfaces;
with System;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;

--  AVX state isolation (the kernel's eager XSAVE): every 256-bit YMM register
--  keeps this process's own pattern across thousands of yields, while
--  another instance on the same CPU fills them with a different one. The
--  loads, the yield system call and the stores are one assembly block, so
--  no compiler-generated code can touch the registers in between.
procedure Main is
   use ASCII;
   type Register_Image is array (0 .. 31) of Unsigned_8;
   type Register_File is array (0 .. 15) of Register_Image with Alignment => 32;
   Want, Got : Register_File;
   Seed : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   ROUNDS : constant := 20_000;
   AVX_ECX_OSXSAVE : constant Unsigned_32 := 2 ** 27;
   AVX_ECX_AVX : constant Unsigned_32 := 2 ** 28;
   XCR0_SSE_AVX : constant Unsigned_32 := 2#110#;

   function AVX_Available return Boolean is
      A, B, C, D, Low, High : Unsigned_32;
   begin
      Asm ("cpuid", Inputs => (Unsigned_32'Asm_Input ("a", 1), Unsigned_32'Asm_Input ("c", 0)),
           Outputs => (Unsigned_32'Asm_Output ("=a", A), Unsigned_32'Asm_Output ("=b", B),
                       Unsigned_32'Asm_Output ("=c", C), Unsigned_32'Asm_Output ("=d", D)),
           Volatile => True);
      if (C and AVX_ECX_OSXSAVE) = 0 or else (C and AVX_ECX_AVX) = 0 then
         return False;
      end if;
      Asm ("xgetbv", Inputs => Unsigned_32'Asm_Input ("c", 0),
           Outputs => (Unsigned_32'Asm_Output ("=a", Low), Unsigned_32'Asm_Output ("=d", High)),
           Volatile => True);
      return (Low and XCR0_SSE_AVX) = XCR0_SSE_AVX;
   end AVX_Available;

   --  Load all sixteen YMM registers from From, yield the CPU, store them to Into.
   procedure Round_Trip (From, Into : System.Address) is
      Ignore : Unsigned_64;
   begin
      Asm ("vmovdqu   0(%1), %%ymm0"  & LF & "vmovdqu  32(%1), %%ymm1"  & LF &
           "vmovdqu  64(%1), %%ymm2"  & LF & "vmovdqu  96(%1), %%ymm3"  & LF &
           "vmovdqu 128(%1), %%ymm4"  & LF & "vmovdqu 160(%1), %%ymm5"  & LF &
           "vmovdqu 192(%1), %%ymm6"  & LF & "vmovdqu 224(%1), %%ymm7"  & LF &
           "vmovdqu 256(%1), %%ymm8"  & LF & "vmovdqu 288(%1), %%ymm9"  & LF &
           "vmovdqu 320(%1), %%ymm10" & LF & "vmovdqu 352(%1), %%ymm11" & LF &
           "vmovdqu 384(%1), %%ymm12" & LF & "vmovdqu 416(%1), %%ymm13" & LF &
           "vmovdqu 448(%1), %%ymm14" & LF & "vmovdqu 480(%1), %%ymm15" & LF &
           "mov $118, %%rax" & LF & "syscall" & LF &
           "vmovdqu %%ymm0,    0(%2)" & LF & "vmovdqu %%ymm1,   32(%2)" & LF &
           "vmovdqu %%ymm2,   64(%2)" & LF & "vmovdqu %%ymm3,   96(%2)" & LF &
           "vmovdqu %%ymm4,  128(%2)" & LF & "vmovdqu %%ymm5,  160(%2)" & LF &
           "vmovdqu %%ymm6,  192(%2)" & LF & "vmovdqu %%ymm7,  224(%2)" & LF &
           "vmovdqu %%ymm8,  256(%2)" & LF & "vmovdqu %%ymm9,  288(%2)" & LF &
           "vmovdqu %%ymm10, 320(%2)" & LF & "vmovdqu %%ymm11, 352(%2)" & LF &
           "vmovdqu %%ymm12, 384(%2)" & LF & "vmovdqu %%ymm13, 416(%2)" & LF &
           "vmovdqu %%ymm14, 448(%2)" & LF & "vmovdqu %%ymm15, 480(%2)" & LF &
           "vzeroupper",
           Outputs => Unsigned_64'Asm_Output ("=a", Ignore),
           Inputs => (System.Address'Asm_Input ("r", From), System.Address'Asm_Input ("r", Into)),
           Clobber => "rcx,r11,memory,xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7," &
                      "xmm8,xmm9,xmm10,xmm11,xmm12,xmm13,xmm14,xmm15",
           Volatile => True);
   end Round_Trip;
begin
   if not AVX_Available then
      debugPrint ("avx-check: AVX unavailable on this CPU" & LF);
      return;
   end if;
   for R in Want'Range loop
      for B in Register_Image'Range loop
         Want (R) (B) := Unsigned_8 ((Seed * 37 + Unsigned_64 (R) * 11 + Unsigned_64 (B) * 3) mod 251);
      end loop;
   end loop;
   for Round in 1 .. ROUNDS loop
      Got := (others => (others => 0));
      Round_Trip (Want'Address, Got'Address);
      if Got /= Want then
         debugPrint ("avx-check: YMM state changed across a context switch FAIL" & LF);
         return;
      end if;
   end loop;
   debugPrint ("avx-check: all 16 YMM registers kept across 20000 yields PASS" & LF);
end Main;
