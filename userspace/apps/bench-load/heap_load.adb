--  SPDX-License-Identifier: GPL-3.0-or-later
--  bench-heap-load: a growing heap beside a benchmark (MEM-006). Commits,
--  touches and releases Heap_Bytes of owned memory in a loop for
--  Duration_Ms: each round is CHANGE_OWNED_RANGE commits in
--  Maximum_Owned_Commit_Bytes steps and one RELEASE_OWNED_REGION, the
--  kernel work that once held grantLock (and so every CPU's waits) for
--  tens of milliseconds. Prints its rate; the benchmark beside it is what
--  is measured (tests/headless/run.sh --load-kind heap).
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Owned_Reservations;
with CuBit.Waits;
procedure Heap_Load is
   Duration_Ms : constant := 12_000;
   Settle_Ms   : constant := 250;
   -- Three copies run beside bench-audio (init-bench-audio-heap.ccl): 96 MiB
   -- in all, within the 256 MiB guest.
   Heap_Bytes  : constant := 32 * 1024 * 1024;
   Mebibyte    : constant := 1024 * 1024;
   Started, Now : Unsigned_64;
   Rounds, Failures : Unsigned_64 := 0;
   Ignored : Unsigned_64;
   Base : Unsigned_64;
   PID : Unsigned_64;
begin
   Ignored := CuBit.Waits.Sleep_For (Settle_Ms);
   Started := CuBit.Monotonic.Milliseconds;
   PID := Own_Process_Word;
   debugPrint ("BENCH-LOAD: START heap_peer=1 duration_ms=" & Unsigned_64'Image (Duration_Ms)
               & " heap_bytes=" & Unsigned_64'Image (Heap_Bytes) & " pid=" & PID'Image & ASCII.LF);
   loop
      Base := CuBit.Owned_Reservations.Allocate (Heap_Bytes);
      if Base = 0 then
         Failures := Failures + 1;
      else
         declare
            First : Storage_Element with Import, Volatile,
              Address => To_Address (Integer_Address (Base));
         begin
            First := 1;
         end;
         if not CuBit.Owned_Reservations.Free (Base, Heap_Bytes) then
            Failures := Failures + 1;
         end if;
         Rounds := Rounds + 1;
      end if;
      Now := CuBit.Monotonic.Milliseconds;
      exit when Now - Started >= Duration_Ms;
   end loop;
   debugPrint ("BENCH-LOAD: COMPLETE heap_rounds=" & Rounds'Image & " failures=" & Failures'Image
               & " mib_per_s=" & Unsigned_64'Image
                   (Rounds * (Heap_Bytes / Mebibyte) * 1000 / Unsigned_64'Max (1, Now - Started))
               & " pid=" & PID'Image & ASCII.LF);
end Heap_Load;
