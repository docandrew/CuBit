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
   --  The main thread's initial stack.
   Initial_Stack_Bytes : constant := 16 * 1024 * 1024;
   --  The secondary stack of the user runtime starts at the bottom of the
   --  main stack and grows into it (__gnat_get_secondary_stack).
   Secondary_Stack_Address : constant := Stack_Top - Initial_Stack_Bytes;

end Process_Layout;
