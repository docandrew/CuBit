------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The persistent log (docs/log-persistence-design.md, first slice): on an
--  installed system (storage.system-volume names its root, "ufs:0"), every
--  record logstore keeps is also appended, as one text line, to
--  @<volume>/logstore.log, so a later boot can read it. The RAM FIFO stays
--  as it is; this is a second consumer of the same records.
--
--  The volume attaches after logstore starts (its driver is started once
--  logstore runs), so the file is opened lazily and retried every
--  Retry_Milliseconds; records published before then wait in the broker's
--  persistent queue (bounded: the number dropped is written when the file
--  opens). Writes are flushed every Flush_Milliseconds and whenever the
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
