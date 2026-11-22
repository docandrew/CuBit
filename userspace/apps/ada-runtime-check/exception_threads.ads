------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Exceptions on several threads at once (EXC-001, native only): each
--  thread raises and handles its own, interleaved, and every handler sees
--  its own thread's occurrence (the current occurrence is thread-local).
------------------------------------------------------------------------------
pragma Ada_2022;
package Exception_Threads is

   procedure Run;

end Exception_Threads;
