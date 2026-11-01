------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Fixed addresses in every user address space that the kernel and the
--  user runtimes agree on.
--
--  @description
--  The kernel (Process) checks its own constants against these when it is
--  compiled, so the runtime needs no system call to learn them.
------------------------------------------------------------------------------
pragma Ada_2022;

package Process_Layout with Pure, SPARK_Mode is

   --  Top of the main thread's stack, exclusive.
   Stack_Top : constant := 16#0000_8000_0000_0000#;
   --  The main thread's initial stack (the bootstrap process's; ELF
   --  programs declare theirs in PT_GNU_STACK). No secondary stack lives
   --  in any primary stack: each thread's is its own owned-memory
   --  reservation (CuBit.Thread_Areas).
   Initial_Stack_Bytes : constant := 16 * 1024 * 1024;

end Process_Layout;
