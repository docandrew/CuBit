------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  xhci.drv's log lines: serial always, logstore once devmgr grants it.
--
--  @description
--  Bring-up convention (coordination/release-stack.md): one
--  "xhci: <location> stage <name> ok|fail ..." line per stage, so one photo
--  of the Boot diagnostics app (filter xhci, or B for bring-up) shows a
--  controller's bring-up. Lines written before the grant are captured
--  (Capture_Capacity lines, the overflow counted and reported); after it
--  every line is emitted at once (a copy into the publisher's ring, never a
--  wait). The grant request retries (Driver_Log_Grant); Poll is the
--  process's only Poll_Completion site, and a completion that is not the
--  grant reply is counted and reported, never silently dropped.
------------------------------------------------------------------------------
with Interfaces;
package Boot_Log is
   Maximum_Line : constant := 160;
   --  A stage line, through logstore and serial. Warning marks a failure.
   procedure Line (Text : String; Warning : Boolean := False);
   --  Serial only (QEMU regression evidence, per-second statistics).
   procedure Serial (Text : String);
   procedure Poll;
   --  When Poll next has work; Interfaces.Unsigned_64'Last for none.
   function Deadline return Interfaces.Unsigned_64;
end Boot_Log;
