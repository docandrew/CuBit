with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Audio;
with CuBit.Benchmark_Clock;
with CuBit.Timing_Histograms;

procedure Main is
   package Audio renames CuBit.Audio;
   package Clock renames CuBit.Benchmark_Clock;
   package Timing renames CuBit.Timing_Histograms;
   Stream : Audio.StreamHandle;
   Rate, Started, Now, Previous, Ignored : Unsigned_64;
   Phase : Natural := 0;
   Generated : Unsigned_64 := 0;
   Queued_Min : Natural := Natural'Last;
   Queued_Max : Natural := 0;
   Refill_Timing : Timing.Histogram;
   Snapshot, Baseline, Final : Audio.Stream_Statistics;
   Baseline_Set : Boolean := False;
   Targets : constant array (Natural range 0 .. 3) of Natural :=
     (2032, 1024, 512, 256);
   Current_Phase : Natural range 0 .. 3 := 0;
   Target : Natural := Targets (0);
   type Phase_Result is record
      Minimum : Natural := Natural'Last;
      Maximum : Natural := 0;
      Underruns_Start, Underruns_End : Unsigned_32 := 0;
      Started : Boolean := False;
   end record;
   Phases : array (Natural range 0 .. 3) of Phase_Result;
   Elapsed : Unsigned_64;

   procedure Fill (Span : Audio.BufferSpan) is
      Value : Integer_16;
   begin
      if Span.frames = 0 then return; end if;
      for Frame in 0 .. Span.frames - 1 loop
         -- Exactly 500 Hz at 48 kHz, no floating point or decoder involved.
         -- Stereo triangle plus a bounded impulse once per generated second.
         Value := Integer_16
           (if Phase < 48 then -4096 + Phase * 8192 / 48
            else 4096 - (Phase - 48) * 8192 / 48);
         if Generated mod 48_000 = 0 then Value := Value + 4096; end if;
         declare
            Samples : array (Natural range 0 .. 1) of Integer_16
              with Address => Span.address + Storage_Offset (Frame * 4), Volatile;
         begin
            Samples := (others => Value);
         end;
         Phase := (Phase + 1) mod 96;
         Generated := Generated + 1;
      end loop;
   end Fill;

   procedure Refill is
      Status : constant Audio.Stream_Statistics := Audio.Statistics (Stream);
      Reservation : Audio.WriteReservation;
      Committed : Boolean;
   begin
      if not Status.Valid or else Status.Queued_Frames >= Target then return; end if;
      Reservation := Audio.reserveWrite (Stream, Target - Status.Queued_Frames);
      if Audio.isReservationValid (Reservation) then
         Fill (Audio.firstSpan (Reservation));
         Fill (Audio.secondSpan (Reservation));
         Audio.commitWrite (Stream, Reservation,
                            Audio.reservedFrames (Reservation), Committed);
         if not Committed then
            debugPrint ("BENCH: FAIL audio commit" & ASCII.LF);
         end if;
      end if;
   end Refill;
begin
   Clock.Calibrate (Rate);
   if Rate = 0 then
      debugPrint ("BENCH: FAIL audio calibration" & ASCII.LF);
      return;
   end if;
   Stream := Audio.open (48_000, 2);
   if not Audio.isValid (Stream) then
      debugPrint ("BENCH: FAIL audio open" & ASCII.LF);
      return;
   end if;
   Refill;
   Refill;
   debugPrint ("AUDIO-BENCH: START tone_hz=500 rate=48000 duration_ms=8000" & ASCII.LF);
   Audio.start (Stream);
   Started := syscall (SYSCALL_GETTIME);
   Previous := Clock.Read_Counter;
   loop
      Now := Clock.Read_Counter;
      if Now < Previous then
         debugPrint ("BENCH: FAIL audio counter-went-backwards" & ASCII.LF);
         exit;
      end if;
      Timing.Add (Refill_Timing, Now - Previous);
      Previous := Now;
      Snapshot := Audio.Statistics (Stream);
      if not Snapshot.Valid then
         debugPrint ("BENCH: FAIL audio statistics" & ASCII.LF);
         exit;
      end if;
      Elapsed := syscall (SYSCALL_GETTIME) - Started;
      Current_Phase := Natural'Min (3, Natural (Elapsed / 2000));
      Target := Targets (Current_Phase);
      -- Allow each reduced queue to settle before measuring its occupancy.
      if Elapsed mod 2000 >= 500 then
         if not Phases (Current_Phase).Started then
            Phases (Current_Phase).Started := True;
            Phases (Current_Phase).Underruns_Start := Snapshot.Underruns;
         end if;
         Phases (Current_Phase).Minimum := Natural'Min
           (Phases (Current_Phase).Minimum, Snapshot.Queued_Frames);
         Phases (Current_Phase).Maximum := Natural'Max
           (Phases (Current_Phase).Maximum, Snapshot.Queued_Frames);
         Phases (Current_Phase).Underruns_End := Snapshot.Underruns;
      end if;
      if Elapsed >= 500 then
         if not Baseline_Set then
            Baseline := Snapshot;
            Baseline_Set := True;
         end if;
         Queued_Min := Natural'Min (Queued_Min, Snapshot.Queued_Frames);
         Queued_Max := Natural'Max (Queued_Max, Snapshot.Queued_Frames);
      end if;
      Refill;
      exit when syscall (SYSCALL_GETTIME) - Started >= 8_000;
      -- Explicit producer model: 1 ms timer refill, NOT the mixer design.
      Ignored := syscall (SYSCALL_SLEEP, 1);
   end loop;
   Audio.pause (Stream);
   -- close is synchronous and ensures the service processes the pause first.
   Final := Audio.Statistics (Stream);
   Audio.close (Stream);
   Clock.Report ("audio-producer-refill-interval", Refill_Timing);
   for I in Phases'Range loop
      debugPrint
        ("AUDIO-BENCH: phase target_frames=" & Natural'Image (Targets (I)) &
         " queued_min=" & Natural'Image (Phases (I).Minimum) &
         " queued_max=" & Natural'Image (Phases (I).Maximum) &
         " underruns=" & Unsigned_32'Image
           (Phases (I).Underruns_End - Phases (I).Underruns_Start) & ASCII.LF);
   end loop;
   debugPrint
     ("AUDIO-BENCH: counters queued_min=" & Natural'Image (Queued_Min) &
      " queued_max=" & Natural'Image (Queued_Max) &
      " capacity_frames=" & Natural'Image (Final.Capacity_Frames) &
      " underruns=" & Unsigned_32'Image (Final.Underruns - Baseline.Underruns) &
      " overruns=" & Unsigned_32'Image (Final.Overruns - Baseline.Overruns) &
      " generated_frames=" & Unsigned_64'Image (Generated) & ASCII.LF);
   debugPrint ("AUDIO-BENCH: COMPLETE" & ASCII.LF);
end Main;
