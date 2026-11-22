------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  xhci.drv's log lines, in its logstore ring.
--
--  @description
--  Bring-up convention (coordination/release-stack.md): one
--  "xhci: <location> stage <name> ok|fail ..." line per stage, so one photo
--  of the Boot diagnostics app (filter xhci, or B for bring-up) shows a
--  controller's bring-up. xhci.drv starts before logstore: devmgr
--  introduces logstore once procmgr offers it, and lines written before
--  then wait in the program's queue (CuBit.Console_Route, ABI-002).
------------------------------------------------------------------------------
package Boot_Log is
   --  A stage line (Information; Warning marks a failure).
   procedure Line (Text : String; Warning : Boolean := False);
   --  Debug text (QEMU regression evidence, per-second statistics): kept
   --  only when logstore's minimum is lowered; test builds mirror it.
   procedure Debug (Text : String);
   --  Wait briefly until logstore has taken the lines (before exiting).
   procedure Flush;
end Boot_Log;
