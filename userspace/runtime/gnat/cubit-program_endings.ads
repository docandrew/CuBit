------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  How a program ends when an exception no handler took reaches the
--  runtime (EXC-001): the last-chance handler (Ada.Exceptions
--  .Last_Chance_Handler) reports it, then ends the process with
--  Unhandled_Exception_Status, which a launcher (the CCL console,
--  CCL_Program_Bindings) reads as the typed outcome
--  Failed (Run_Failure.Unhandled_Exception).
--
--  @description
--  The kernel's exit report (CuBit.Child_Exits) carries only an exit code,
--  so the outcome is that code: 134, as a Unix shell reports SIGABRT
--  (128 + 6), which upstream GNAT's unhandled-exception path raises on
--  Linux. A program that chooses this code itself reads the same until the
--  exit report is typed (ABI-002); the exception's name, message and
--  traceback are in the program's log (CuBit.Last_Words). Shared with
--  hosted builds (userspace/ccl/runtime-shared).
------------------------------------------------------------------------------
package CuBit.Program_Endings with Pure is

   Unhandled_Exception_Status : constant := 134;

end CuBit.Program_Endings;
