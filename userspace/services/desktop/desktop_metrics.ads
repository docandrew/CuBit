with Interfaces;
with Compositor_Frame_Trace;
with Compositor_Latency;
with Compositor_Stage_Metrics;
with Compositor_Work_Metrics;

--  Desktop's metrics (MET-002): always on, published through the
--  com.cubit-os.stdmetric outlet Desktop's manifest declares (CuBit.Metrics),
--  a copy into a ring with no system call per sample.
--
--  * The compositor families (Compositor_Stage_Metrics, _Work_Metrics,
--    _Release_Metrics) as series under the names their declarations give.
--  * LAT-002's input-to-photon numbers, each sample as Compositor_Latency
--    observed it (Publish_Latency): desktop.latency.<stage> for the ten
--    stage times (microseconds; the store keeps them in mergeable sketches,
--    so any window's p50/p90/p99/max comes from the store, not from here),
--    desktop.latency.refreshes_late, the input counts (inputs, shown,
--    unshown, evicted, untimed), desktop.frames.dropped,
--    desktop.frames.<cpu|gpu>.{damage_px,damage_rects,render_us,submit_to_ready_us}
--    and desktop.frames.vblanks_per_frame (refreshes between two timed
--    frames of an output: frames against vblanks over any window).
--  The "desktop: latency" log lines stay the human summary
--  (Desktop_Latency_Report).
package Desktop_Metrics with SPARK_Mode => Off is
   --  Kept for the call sites that ask: metrics are never off now.
   Enabled : constant Boolean := True;
   function Disabled return Boolean is (False);
   procedure Record_Work
     (Kind : Compositor_Work_Metrics.Work_Kind; Pixels, Now : Interfaces.Unsigned_64);
   procedure Record_Stage
     (Stage : Compositor_Stage_Metrics.Stage; First, Last : Interfaces.Unsigned_64);
   procedure Record_Completion (Frame : Compositor_Frame_Trace.Record_Value);
   --  Every observation the ledger queued since the last call (and what it
   --  could not queue, as a loss of its series).
   procedure Publish_Latency (L : in out Compositor_Latency.Ledger);
end Desktop_Metrics;
