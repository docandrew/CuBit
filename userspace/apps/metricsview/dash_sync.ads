------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The one lock between the metrics viewer's two threads: the UI thread
--  (input, drawing) and the data thread (the store's ring and calls, the
--  columns' summaries). The data thread holds it while it changes the
--  columns, never across a call to the store (Dash_Tracks lets it go around
--  every call); the UI thread while it reads them to draw. Each side holds
--  it for microseconds, so neither waits on the other's I/O.
--
--  @description
--  On CuBit a futex lock (native/dash_sync.adb, CuBit.Futex_Locks); in the
--  hosted tests, which run both sides on one thread, nothing.
------------------------------------------------------------------------------
package Dash_Sync is
   procedure Lock;
   procedure Unlock;
end Dash_Sync;
