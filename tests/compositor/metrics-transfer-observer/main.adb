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
   Seen : array (1 .. 10) of Boolean := [others => False];
   Frames, Batches : Unsigned_64 := 0;
   function Expected (Key : Natural) return String is
     (case Key is
      when 1 => "desktop.out0.submit_release",
      when 3 => "desktop.input_dispatch",
      when 5 => "desktop.scene_draw",
      when 6 => "desktop.submit_call",
      when 9 => "desktop.gpu_readback_bytes",
      when 10 => "desktop.cpu_copy_bytes",
      when others => "");
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
           Rows (I) (P.Row_Key) in 1 | 3 | 5 | 6 | 9 | 10
         then
            if not P.Is_Publisher (Rows (I) (P.Row_Publisher_Tag)) or else
              Rows (I) (P.Row_Kind) /= R.Record_Kind'Enum_Rep
                ((if Rows (I) (P.Row_Key) >= 9 then R.Counter elsif Rows (I) (P.Row_Key) = 1 then R.Span else R.Latency)) or else
              Rows (I) (P.Row_Unit) /= R.Unit'Enum_Rep ((if Rows (I) (P.Row_Key) >= 9 then R.Bytes else R.Microseconds)) or else
              Rows (I) (P.Row_Series_Rejected) /= 0 or else
              Rows (I) (P.Row_Source_Rejected) /= 0 or else
              Rows (I) (P.Row_Source_Producer_Dropped) /= 0 or else
              Rows (I) (P.Row_Source_Batch_Gaps) /= 0
            then Fail ("identity, schema or loss"); end if;
            declare
               Key : constant Natural := Natural (Rows (I) (P.Row_Key));
               Name : constant String := Expected (Key);
            begin
               for J in 1 .. 64 loop
                  if (Shift_Right (Rows (I) (P.Row_First_Name + (J - 1) / 8),
                                   ((J - 1) mod 8) * 8) and 255) /= (if J <= Name'Length then Character'Pos (Name (J)) else 0)
                  then Fail ("metric name"); end if;
               end loop;
               Seen (Key) := (if Key >= 9 then Rows (I) (P.Row_Count_Word) = 0 and Rows (I) (P.Row_Total) = 0 else Rows (I) (P.Row_Count_Word) > 0);
               if Key = 1 then
                  Frames := Rows (I) (P.Row_Count_Word);
                  Batches := Rows (I) (P.Row_Source_Batches);
                  if First_Count = 0 then First_Count := Frames; end if;
               end if;
            end;
         end if;
      end loop;
      if Seen (1) and then Seen (3) and then Seen (5) and then Seen (6) and then
        Seen (9) and then Seen (10) and then Frames > First_Count and then Frames >= 3
      then
         debugPrint ("TEST: PASS desktop-metrics frames=" & Frames'Image &
           " batches=" & Batches'Image & " stages=input,draw,submit byte-schema=valid fallback-transfer=zero" & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 0);
         return;
      end if;
      Ignore := syscall (SYSCALL_SLEEP, 50);
   end loop;
   Fail ("missing release growth or input/draw/submit samples");
end Main;
