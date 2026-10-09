with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metric_Raw_Observer;
with CuBit.Metric_Protocol;
with Compositor_Trace_Stream;
with Compositor_Render_Trace;
with CCL_Manifest_Bindings;
procedure Main is
   package P renames CuBit.Metric_Protocol;
   package O renames CuBit.Metric_Raw_Observer;
   package S renames Compositor_Trace_Stream;
   package W renames S.W;
   use type P.Status, W.Event_Kind, Compositor_Render_Trace.Phase;
   Raw : O.Observer (CCL_Manifest_Bindings.Slot_metrics_observer);
   Collector : S.State;
   Page : P.Raw_Page;
   N : P.Raw_Row_Count;
   Cursor : Unsigned_64 := 1;
   Resume, Gap, Lost, Ignore : Unsigned_64;
   Result : P.Status;
   Started, Done : Boolean := False;
   Counts : array (W.Event_Kind) of Natural := [others => 0];
   Draws, Submits : Natural := 0;
   Publisher, Last_ID, Drops, Batch_Gaps, History_Loss : Unsigned_64 := 0;
   Capture : S.Capture;
   procedure Fail (Why : String) is
   begin
      debugPrint ("TEST: FAIL desktop-trace " & Why & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT, 1);
      loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
   end Fail;
begin
   -- Bounded collector workload. It may lag and lose history; identity and
   -- completeness checks never turn an incomplete fragment group into an event.
   for Tick in 1 .. 1000 loop
      for Drain in 1 .. 8 loop
         O.Query (Raw, Cursor, Page, N, Resume, Gap, Lost, Result);
         if Result /= P.OK then
            S.Discard_Partial (Collector);
            Fail ("raw query");
         end if;
         if not Started then
            if O.Incarnation (Raw) = 0 then Fail ("missing endpoint"); end if;
            S.Start (Collector, O.Incarnation (Raw), Cursor);
            Started := True;
         end if;
         if Lost < History_Loss then Fail ("history loss decreased"); end if;
         History_Loss := Lost;
         for I in 0 .. N - 1 loop
            S.Feed (Collector, O.Incarnation (Raw), Page (I), Capture);
            if Capture.Success and then
              Capture.Pid = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP)
            then
               if Publisher = 0 then Publisher := Capture.Publisher; end if;
               if Capture.Publisher /= Publisher or else
                 Capture.Value.Event_ID <= Last_ID or else
                 Capture.Producer_Dropped < Drops or else Capture.Batch_Gaps < Batch_Gaps
               then Fail ("identity or monotonic counters"); end if;
               Last_ID := Capture.Value.Event_ID;
               Drops := Capture.Producer_Dropped;
               Batch_Gaps := Capture.Batch_Gaps;
               Counts (Capture.Value.Kind) := Counts (Capture.Value.Kind) + 1;
               if Capture.Value.Kind = W.Render_Event then
                  if Capture.Value.Render.Kind = Compositor_Render_Trace.Draw
                  then Draws := Draws + 1; else Submits := Submits + 1; end if;
               end if;
            end if;
         end loop;
         Cursor := Resume;
         exit when N = 0;
      end loop;
      Ignore := syscall (SYSCALL_SLEEP, 20);
   end loop;
   S.Discard_Partial (Collector);
   for Retry in 1 .. 100 loop
      O.Disconnect (Raw, Done);
      exit when Done;
      Ignore := syscall (SYSCALL_SLEEP, 10);
   end loop;
   if not Done then Fail ("grant retirement"); end if;
   if Counts (W.Input_Event) < 3 or else Counts (W.Source_Event) = 0 or else
     Counts (W.Frame_Event) < 3 or else Draws = 0 or else Submits < 3
   then
      debugPrint ("TRACE: input=" & Counts (W.Input_Event)'Image &
        " source=" & Counts (W.Source_Event)'Image & " frame=" & Counts (W.Frame_Event)'Image &
        " draws=" & Draws'Image & " submits=" & Submits'Image & ASCII.LF);
      Fail ("missing real Desktop event types");
   end if;
   debugPrint ("TEST: PASS desktop-trace input=" & Counts (W.Input_Event)'Image &
     " source=" & Counts (W.Source_Event)'Image & " frame=" & Counts (W.Frame_Event)'Image &
     " draws=" & Draws'Image & " submits=" & Submits'Image &
     " producer_dropped=" & Drops'Image & " batch_gaps=" & Batch_Gaps'Image &
     " history_loss=" & History_Loss'Image &
     " skipped_rows=" & S.Counts (Collector).Skipped_Rows'Image &
     " rejected_rows=" & S.Counts (Collector).Rejected_Rows'Image &
     " retirement=confirmed" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
