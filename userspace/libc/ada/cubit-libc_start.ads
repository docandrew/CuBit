------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc program start (docs/process-arguments.md, docs/c-removal.md).
--  CuBit's loader enters _start (crt/crt1.S) with the stack pointer at the
--  top of an empty stack and RDI = the length of the process's launch
--  block (0: none), mapped read-only at Block_Address. Start builds the
--  block musl's __libc_start_main reads: argv and the environment from the
--  launch block once the proved validator accepts it (otherwise argv =
--  { "cubit-program" } and an empty environment), the working directory,
--  then the auxiliary entries musl needs (program headers for the TLS
--  template, page size, 16 random bytes for the stack protector).
--
--  The strings are copied to the stack, so programs may write to them as
--  POSIX allows; the copies and vectors live in this never-returning frame,
--  bounded by the launch limits (64 KiB, 4096 strings).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Libc_Start is

   --  The main thread's stack: its top (the loader's initial stack pointer)
   --  and its size (the PT_GNU_STACK contract), for pthread_getattr_np.
   Stack_Top : Unsigned_64 := 0
   with Export, Convention => C, External_Name => "__cubit_stack_top";
   Stack_Size : Unsigned_64 := 0
   with Export, Convention => C, External_Name => "__cubit_stack_size";

   procedure Start (Initial_Stack, Launch_Length : Unsigned_64)
   with Export, Convention => C, External_Name => "__cubit_start", No_Return;

end CuBit.Libc_Start;
