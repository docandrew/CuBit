with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Metrics;
with CuBit.Metric_Protocol;
with CuBit.Metric_Records;
with CCL_Manifest_Bindings;
procedure Main is
   package P renames CuBit.Metric_Protocol;
   package R renames CuBit.Metric_Records;
   use type P.Status;
   Watcher : CuBit.Metrics.Observer (CCL_Manifest_Bindings.Slot_metrics_observer);
   Rows : P.Summary_Page;
   Written : P.Row_Count;
   Next, Ignore, First_Count : Unsigned_64 := 0;
   Result : P.Status;
   Expected : constant String := "desktop.out0.submit_release";
   procedure Fail (Reason : String) is
   begin
      debugPrint ("TEST: FAIL desktop-metrics " & Reason & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT, 1);
      loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
   end Fail;
begin
   for Attempt in 1 .. 400 loop
      CuBit.Metrics.Query (Watcher, 0, Rows, Written, Next, Result);
      if Result /= P.OK then Fail ("observer query"); end if;
      for I in 0 .. Written - 1 loop
         if Rows (I) (P.Row_Source) = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP) and then
           Rows (I) (P.Row_Key) = 1
         then
            if not P.Is_Publisher (Rows (I) (P.Row_Publisher_Tag)) or else
              Rows (I) (P.Row_Kind) /= R.Record_Kind'Enum_Rep (R.Span) or else
              Rows (I) (P.Row_Unit) /= R.Unit'Enum_Rep (R.Microseconds) or else
              Rows (I) (P.Row_Source_Rejected) /= 0 or else
              Rows (I) (P.Row_Source_Producer_Dropped) /= 0 or else
              Rows (I) (P.Row_Source_Batch_Gaps) /= 0
            then Fail ("identity, schema or loss"); end if;
            for J in Expected'Range loop
               if (Shift_Right (Rows (I) (P.Row_First_Name + (J - 1) / 8),
                                ((J - 1) mod 8) * 8) and 255) /= Character'Pos (Expected (J))
               then Fail ("metric name"); end if;
            end loop;
            if First_Count = 0 then First_Count := Rows (I) (P.Row_Count_Word);
            elsif Rows (I) (P.Row_Count_Word) > First_Count and then
              Rows (I) (P.Row_Count_Word) >= 3
            then
               debugPrint ("TEST: PASS desktop-metrics frames=" &
                 Rows (I) (P.Row_Count_Word)'Image & " batches=" &
                 Rows (I) (P.Row_Source_Batches)'Image & ASCII.LF);
               Ignore := syscall (SYSCALL_EXIT, 0);
               return;
            end if;
         end if;
      end loop;
      Ignore := syscall (SYSCALL_SLEEP, 50);
   end loop;
   Fail ("no growing Desktop release series");
end Main;
