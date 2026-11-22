------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A program's last words (LOG-004): what the runtime's last-chance handler
--  says about an unhandled exception, published as an Error record through
--  the program's own log (its publisher ring to logstore, LOG-001), not
--  through the kernel.
--
--  @description
--  The handler (Ada.Exceptions.Last_Chance_Handler) cannot name the log
--  itself: it is in every program, most of which never log. CuBit.Logging
--  registers a reporter here when a program first opens a log channel to
--  logstore; the handler calls Report, which does nothing in a program that
--  never did. Only this tiny unit is in every program.
------------------------------------------------------------------------------
package CuBit.Last_Words with Preelaborate is

   --  Text: what failed ("unhandled exception PROGRAM_ERROR: main.adb:14
   --  explicit raise; traceback 0x..."); Line: the source line, 0 when
   --  unknown (the exception's report carries it in its message). Returns when the record is in logstore
   --  or its bounded wait is over.
   type Reporter is access procedure (Text : String; Line : Natural);

   procedure Register (Handler : Reporter);

   procedure Report (Text : String; Line : Natural);

end CuBit.Last_Words;
