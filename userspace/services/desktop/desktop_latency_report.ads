with Interfaces;
with Compositor_Latency;
--  LAT-002: Desktop's input-to-photon summary. Every Report_Interval_Us
--  with key presses or frames, a few lines go to the log (logstore, so
--  (logs.recent "desktop") and cubit-remote logs desktop read them):
--
--    desktop: latency inputs=.. shown=.. unshown=.. evicted=.. untimed=..
--      grid=hardware|virtual period_us=.. frames_timed=.. vblanks=..
--      refreshes_late 0=.. 1=.. 2=.. 3=.. 4+=..
--    desktop: latency phase=K/8 n=.. late0=.. late1=.. late2+=..   (each eighth)
--    desktop: latency stage=NAME n=.. p50=.. p90=.. p99=.. max=.. budget_us=..
--    desktop: latency path=cpu|gpu frames=.. px_avg=.. rects_avg=..
--      render p50=.. p99=.. max=.. submit_to_ready p50=.. p99=.. max=..
--
--  Quantiles are histogram bucket upper bounds (four per octave), not
--  exact order statistics. Budgets are printed, not enforced.
package Desktop_Latency_Report with SPARK_Mode => Off is
   Report_Interval_Us : constant := 5_000_000;
   --  How often unshown inputs are expired.
   Expire_Interval_Us : constant := 100_000;
   --  One loop turn: expire, and report when the interval is over.
   procedure Tick (L : in out Compositor_Latency.Ledger; S : in out Compositor_Latency.Stats;
                   Now : Interfaces.Unsigned_64);
end Desktop_Latency_Report;
