with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
procedure Main is
   Started, Ignored : Unsigned_64;
   State : Unsigned_64 := 1 with Volatile;
begin
   Ignored := syscall (SYSCALL_SLEEP, 250);
   Started := syscall (SYSCALL_GETTIME);
   debugPrint ("BENCH-LOAD: START busy_peer=1 duration_ms=12000" & ASCII.LF);
   loop
      for I in 1 .. 4096 loop
         State := State * 6364136223846793005 + 1;
      end loop;
      exit when syscall (SYSCALL_GETTIME) - Started >= 12_000;
   end loop;
   debugPrint ("BENCH-LOAD: COMPLETE" & ASCII.LF);
end Main;
