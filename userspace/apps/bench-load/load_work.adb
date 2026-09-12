with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Benchmark_Clock;
procedure Load_Work (Duration_Ms : Unsigned_64; CPU_Control : Boolean := False) is
   Started, Ignored, Now, Last : Unsigned_64;
   Batches, Maximum_Gap : Unsigned_64 := 0;
   State : Unsigned_64 := 1 with Volatile;
   PID, Rate : Unsigned_64 := 0;
begin
   Ignored := syscall (SYSCALL_SLEEP, 250);
   PID := syscall (SYSCALL_GETPID);
   if CPU_Control then
      CuBit.Benchmark_Clock.Calibrate (Rate);
   end if;
   Started := syscall (SYSCALL_GETTIME);
   Last := Started;
   debugPrint ("BENCH-LOAD: START busy_peer=1 duration_ms=" &
     Duration_Ms'Image & " pid=" & PID'Image & ASCII.LF);
   if CPU_Control then
      debugPrint ("CPU-CONTROL: START pid=" & PID'Image &
        " start_ms=" & Started'Image & " ticks_per_ms=" & Rate'Image & ASCII.LF);
   end if;
   loop
      for I in 1 .. 4096 loop
         State := State * 6364136223846793005 + 1;
      end loop;
      Batches := Batches + 1;
      Now := syscall (SYSCALL_GETTIME);
      Maximum_Gap := Unsigned_64'Max (Maximum_Gap, Now - Last);
      Last := Now;
      exit when Now - Started >= Duration_Ms;
   end loop;
   -- Aggregate progress only: serial output never runs in the measured loop.
   debugPrint ("BENCH-LOAD: COMPLETE batches=" & Batches'Image &
     " max_gap_ms=" & Maximum_Gap'Image & " pid=" & PID'Image & ASCII.LF);
   if CPU_Control then
      debugPrint ("CPU-CONTROL: COMPLETE pid=" & PID'Image &
        " finish_ms=" & Now'Image & " batches=" & Batches'Image & ASCII.LF);
      Ignored := syscall (SYSCALL_TRACE_SUMMARY);
   end if;
end Load_Work;
