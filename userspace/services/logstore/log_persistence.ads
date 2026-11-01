------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The persistent log (docs/log-persistence-design.md, first slice): on an
--  installed system (storage.system-volume names its root, "ufs:0"), every
--  record logstore keeps is also appended, as one text line, to
--  @<volume>/logstore.log, so a later boot can read it. It reads the
--  broker's per-source histories in publish order, like a subscriber.
--
--  The volume attaches after logstore starts (its driver is started once
--  logstore runs), so the file is opened lazily and retried every
--  Retry_Milliseconds; records published before then wait in the broker's
--  histories (bounded per source, each source's boot window kept: the
--  number given up before the file took them is written when it opens). Writes are flushed every Flush_Milliseconds and whenever the
--  queue empties after a write.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Log_Fanout;

package Log_Persistence is
   Retry_Milliseconds : constant := 1_000;
   Flush_Milliseconds : constant := 1_000;

   --  Volume: the system volume's name ("ufs:0"); "" turns persistence off.
   procedure Configure (Volume : String);

   procedure Pump (Store : in out Log_Fanout.Broker; Now_Ms : Unsigned_64);

   --  The file is open and records are being written.
   function Active return Boolean;
end Log_Persistence;
