------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  CPU-spread benchmark. Several instances are started together by procmgr,
--  so they all begin queued on the same CPU. Each does the same fixed amount
--  of CPU-bound work and reports its elapsed guest milliseconds. With work
--  stealing, idle CPUs take instances and elapsed time stays near one
--  instance's; without it, instances share one CPU.
------------------------------------------------------------------------------
with CuBit.Console_Route;
with CuBit.Waits;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;

procedure Main is
   Iterations : constant := 400_000_000;
   State : Unsigned_64 := Own_Process_Word with Volatile;
   Start, Finish : Unsigned_64;
   Ignore : Unsigned_64;
   X : Unsigned_64;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   --  Let every instance be spawned before any starts measuring.
   Ignore := CuBit.Waits.Sleep_For (500);
   Start := CuBit.Monotonic.Milliseconds;
   X := State;
   for I in 1 .. Iterations loop
      X := X * 6364136223846793005 + 1442695040888963407;
   end loop;
   State := X;
   Finish := CuBit.Monotonic.Milliseconds;
   debugPrint ("BENCH: spread elapsed_ms" & Unsigned_64'Image (Finish - Start) &
               ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
end Main;
