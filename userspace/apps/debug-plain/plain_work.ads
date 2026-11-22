------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The plain debugger target's work (DBG-001): rounds that sleep, so a
--  debugger attached through the kernel finds the thread waiting
--  (Sleeping) in Plain_Work.Pause_Between, called from Plain_Work.Run, with
--  live locals and a global it can read while it runs.
------------------------------------------------------------------------------
pragma Ada_2022;

package Plain_Work is

   Label_Bytes : constant := 8;
   type Tally is record
      Rounds : Natural := 0;
      Last   : Integer := 0;
      Label  : String (1 .. Label_Bytes) := "plain-01";
   end record;

   --  The global a debugger reads live.
   Totals : Tally;

   --  Count rounds forever, sleeping between them.
   procedure Run;

end Plain_Work;
