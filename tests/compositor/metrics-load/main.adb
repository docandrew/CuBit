with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
procedure Main is
   Ignore, Start, Now, Chunks : Unsigned_64 := 0;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   State : Unsigned_64 := PID with Volatile;
   X : Unsigned_64;
begin
   Ignore := syscall (SYSCALL_SLEEP, 5_000);
   Start := syscall (SYSCALL_GETTIME);
   debugPrint ("TEST: metrics-load begin pid=" & PID'Image & ASCII.LF);
   loop
      X := State;
      for I in 1 .. 100_000 loop
         X := X * 6364136223846793005 + 1442695040888963407;
      end loop;
      State := X;
      Chunks := Chunks + 1;
      Now := syscall (SYSCALL_GETTIME);
      exit when Now >= Start and then Now - Start >= 20_000;
   end loop;
   debugPrint ("TEST: metrics-load done pid=" & PID'Image &
     " chunks=" & Chunks'Image & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
