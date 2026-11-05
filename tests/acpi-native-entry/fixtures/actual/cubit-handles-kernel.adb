------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  CuBit.Handles.Kernel - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;

package body CuBit.Handles.Kernel is

   --  The syscall instruction clobbers RCX and R11; the kernel entry moves
   --  R10 to RCX for the fourth argument and reads the seventh from R12.
   function Call
     (Number : Unsigned_64;
      A0, A1, A2, A3, A4, A5, A6 : Unsigned_64 := 0) return Unsigned_64
   is
      Result : Unsigned_64;
   begin
      Asm
        ("mov %5, %%r10" & ASCII.LF &
         "mov %6, %%r8" & ASCII.LF &
         "mov %7, %%r9" & ASCII.LF &
         "mov %8, %%r12" & ASCII.LF & "syscall",
         Outputs => Unsigned_64'Asm_Output ("=a", Result),
         Inputs =>
           (Unsigned_64'Asm_Input ("a", Number),
            Unsigned_64'Asm_Input ("D", A0),
            Unsigned_64'Asm_Input ("S", A1),
            Unsigned_64'Asm_Input ("d", A2),
            Unsigned_64'Asm_Input ("rm", A3),
            Unsigned_64'Asm_Input ("rm", A4),
            Unsigned_64'Asm_Input ("rm", A5),
            Unsigned_64'Asm_Input ("rm", A6)),
         Clobber => "r10, r8, r9, r12, rcx, r11, memory",
         Volatile => True);
      return Result;
   end Call;

end CuBit.Handles.Kernel;
