with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
procedure Load_Work (Duration_Ms : Unsigned_64) is
   Started, Ignored, Now, Last : Unsigned_64;
   Batches, Maximum_Gap : Unsigned_64 := 0;
   State : Unsigned_64 := 1 with Volatile;
begin
   Ignored := syscall (SYSCALL_SLEEP, 250);
   Started := syscall (SYSCALL_GETTIME);
   Last := Started;
   debugPrint ("BENCH-LOAD: START busy_peer=1 duration_ms=" &
     Duration_Ms'Image & ASCII.LF);
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
     " max_gap_ms=" & Maximum_Gap'Image & ASCII.LF);
end Load_Work;
