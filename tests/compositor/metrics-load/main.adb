with CuBit.Console_Route;
with CuBit.Waits;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
procedure Main is
   Ignore, Start, Now, Chunks : Unsigned_64 := 0;
   PID : constant Unsigned_64 := Own_Process_Word;
   State : Unsigned_64 := PID with Volatile;
   X : Unsigned_64;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   Ignore := CuBit.Waits.Sleep_For (5_000);
   Start := CuBit.Monotonic.Milliseconds;
   debugPrint ("TEST: metrics-load begin pid=" & PID'Image & ASCII.LF);
   loop
      X := State;
      for I in 1 .. 100_000 loop
         X := X * 6364136223846793005 + 1442695040888963407;
      end loop;
      State := X;
      Chunks := Chunks + 1;
      Now := CuBit.Monotonic.Milliseconds;
      exit when Now >= Start and then Now - Start >= 20_000;
   end loop;
   debugPrint ("TEST: metrics-load done pid=" & PID'Image &
     " chunks=" & Chunks'Image & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
