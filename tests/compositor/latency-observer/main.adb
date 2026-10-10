--  Desktop latency watcher: every Report_Interval_Ms, print metricsvc's
--  summaries of Desktop's loop-turn duration, input-to-present latency and
--  pointer source age
--  (cumulative histograms: count, p50/p99 bucket upper bounds, maximum).
--  A periodic stall shows as p99/maximum growth (docs/compositor-backends.md).
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metrics;
with CuBit.Metric_Protocol;
with Compositor_Stage_Metrics;
with CCL_Manifest_Bindings;
procedure Main is
   package P renames CuBit.Metric_Protocol;
   package SM renames Compositor_Stage_Metrics;
   use type P.Status;
   Report_Interval_Ms : constant := 5_000;
   Watcher : CuBit.Metrics.Observer (CCL_Manifest_Bindings.Slot_metrics_observer);
   Rows : P.Summary_Page;
   Written : P.Row_Count;
   Next, Ignore : Unsigned_64 := 0;
   Result : P.Status;
   function Watched (Key : Unsigned_64) return Boolean is
     (Key = Unsigned_64 (SM.Key (SM.Loop_Turn)) or else
      Key = Unsigned_64 (SM.Key (SM.Input_To_Present)) or else
      Key = Unsigned_64 (SM.Key (SM.Input_Source_Age)));
   function Name (Key : Unsigned_64) return String is
     (if Key = Unsigned_64 (SM.Key (SM.Loop_Turn)) then "loop_turn"
      elsif Key = Unsigned_64 (SM.Key (SM.Input_To_Present)) then "input_to_present"
      else "input_source_age");
begin
   loop
      Next := 0;
      loop
         CuBit.Metrics.Query (Watcher, Next, Rows, Written, Next, Result);
         exit when Result /= P.OK;
         for I in 0 .. Written - 1 loop
            if Rows (I) (P.Row_Source) = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP)
              and then Watched (Rows (I) (P.Row_Key))
            then
               debugPrint ("DESKTOP-LATENCY: " & Name (Rows (I) (P.Row_Key)) &
                 " count=" & Rows (I) (P.Row_Count_Word)'Image &
                 " p50_upper_us=" & Rows (I) (P.Row_P50)'Image &
                 " p99_upper_us=" & Rows (I) (P.Row_P99)'Image &
                 " max_us=" & Rows (I) (P.Row_Maximum)'Image &
                 " producer_dropped=" & Rows (I) (P.Row_Source_Producer_Dropped)'Image & ASCII.LF);
            end if;
         end loop;
         exit when Next = 0 or else Written = 0;
      end loop;
      Ignore := syscall (SYSCALL_SLEEP, Report_Interval_Ms);
   end loop;
end Main;
