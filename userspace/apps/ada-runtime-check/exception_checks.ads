------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Zero-cost Ada exceptions in the user runtime (EXC-001): raised and
--  handled across subprograms, run-time checks, finalization while an
--  exception propagates, re-raise, occurrences, exceptions from Finalize.
------------------------------------------------------------------------------
pragma Ada_2022;
package Exception_Checks is

   procedure Run;

end Exception_Checks;
