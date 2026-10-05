------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;

package body CuBit.Kernel_Calls is

   function Call
     (Number : CuBit.Kernel_ABI.System_Call;
      A0, A1, A2, A3, A4, A5 : Unsigned_64 := 0) return Unsigned_64
   is
      LF : constant Character := ASCII.LF;
      Result : Unsigned_64;
   begin
      --  RDI, RSI, RDX have constraint letters; R10, R8, R9 are loaded in
      --  the same block, so nothing can clobber them before the syscall.
      Asm ("mov %5, %%r10" & LF &
           "mov %6, %%r8" & LF &
           "mov %7, %%r9" & LF &
           "syscall",
           Outputs => Unsigned_64'Asm_Output ("=a", Result),
           Inputs => [Unsigned_64'Asm_Input ("a", Number),
                      Unsigned_64'Asm_Input ("D", A0),
                      Unsigned_64'Asm_Input ("S", A1),
                      Unsigned_64'Asm_Input ("d", A2),
                      Unsigned_64'Asm_Input ("rm", A3),
                      Unsigned_64'Asm_Input ("rm", A4),
                      Unsigned_64'Asm_Input ("rm", A5)],
           Clobber => "r10, rcx, r8, r9, r11, memory",
           Volatile => True);
      return Result;
   end Call;

   function Submit
     (Slot : Unsigned_64; Label : Unsigned_32; Length : Unsigned_8;
      W0, W1, W2, W3 : Unsigned_64; Token : Unsigned_64) return Unsigned_64
   is
      LF : constant Character := ASCII.LF;
      Tag : constant Unsigned_64 :=
        Unsigned_64 (Label) or Shift_Left (Unsigned_64 (Length), 32);
      Result : Unsigned_64;
   begin
      --  RDI slot, RSI tag, RDX word 0, R10/R8/R9 words 1 .. 3, R12 token.
      Asm ("mov %5, %%r10" & LF &
           "mov %6, %%r8" & LF &
           "mov %7, %%r9" & LF &
           "mov %8, %%r12" & LF &
           "syscall",
           Outputs => Unsigned_64'Asm_Output ("=a", Result),
           Inputs => [Unsigned_64'Asm_Input
                        ("a", CuBit.Kernel_ABI.Submit_Via_Endpoint_Capability),
                      Unsigned_64'Asm_Input ("D", Slot),
                      Unsigned_64'Asm_Input ("S", Tag),
                      Unsigned_64'Asm_Input ("d", W0),
                      Unsigned_64'Asm_Input ("m", W1),
                      Unsigned_64'Asm_Input ("m", W2),
                      Unsigned_64'Asm_Input ("m", W3),
                      Unsigned_64'Asm_Input ("m", Token)],
           Clobber => "r10, rcx, r8, r9, r11, r12, memory",
           Volatile => True);
      return Result;
   end Submit;

end CuBit.Kernel_Calls;
