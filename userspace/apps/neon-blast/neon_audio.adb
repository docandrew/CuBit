------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Futex_Locks;
with CuBit.Handles.Devices;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Monotonic;
with CuBit.Owned_Reservations;
with CuBit.Thread_Areas;
with CuBit.Waits;
with Neon_Replay;

package body Neon_Audio is
   package ABI renames CuBit.Kernel_ABI;

   --  The audio thread's stack (a guard page at its bottom).
   Stack_Bytes : constant := 256 * 1024;
   Stack_Alignment : constant := 16;
   Return_Address_Bytes : constant := 8;
   --  No flags in the latency contract.
   No_Flags : constant := 0;
   --  SET_LATENCY_CONTRACT's answer when admitted.
   Admitted : constant := 0;

   Game_Lock : CuBit.Futex_Locks.Mutex;
   Exit_Word : aliased CuBit.Futex_Locks.Word := 1;

   The_Authority : CuBit.Handles.Scheduling;
   Has_Authority : Boolean := False;
   Is_Running, Is_Realtime : Boolean := False with Atomic;

   procedure Lock is
   begin
      CuBit.Futex_Locks.Lock (Game_Lock);
   end Lock;

   procedure Unlock is
   begin
      CuBit.Futex_Locks.Unlock (Game_Lock);
   end Unlock;

   function Running return Boolean is (Is_Running);
   function Realtime return Boolean is (Is_Realtime);

   function Now_Us return Unsigned_64 is
      Reading : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Reading.Available then Reading.Microseconds
              else CuBit.Monotonic.Milliseconds * 1_000);
   end Now_Us;

   procedure Synthesize (Frames : Natural) is
      Began : constant Unsigned_64 := Now_Us;
      Spent : Unsigned_64;
   begin
      Neon_Game.Render_Audio (Game.all, Mix_Left, Mix_Right, Frames);
      Spent := Now_Us - Began;
      Synth_Us := Synth_Us + Spent;
      Synth_Frames := Synth_Frames + Unsigned_64 (Frames);
      if Frames > 0 then
         Synth_Worst_Us_Per_Block :=
           Float'Max (Synth_Worst_Us_Per_Block,
                      Float (Spent) * Float (Neon_Synth.Maximum_Block) / Float (Frames));
      end if;
   end Synthesize;

   procedure Fill (Span : CuBit.Audio.BufferSpan; From : Positive) is
      function PCM (Sample : Float) return Integer_16 renames Neon_Replay.PCM;
   begin
      for Frame in 0 .. Span.frames - 1 loop
         declare
            Pair : array (0 .. 1) of Integer_16
              with Import, Address => Span.address + Storage_Offset (Frame * 4);
         begin
            Pair (0) := PCM (Mix_Left (From + Frame));
            Pair (1) := PCM (Mix_Right (From + Frame));
         end;
      end loop;
   end Fill;

   procedure Refill (Now : Unsigned_64) is
      Status : CuBit.Audio.Stream_Statistics;
      Reservation : CuBit.Audio.WriteReservation;
      Wanted, Frames : Natural;
      Committed : Boolean;
   begin
      if not Audio_On then
         return;
      end if;
      for Round in 1 .. 4 loop
         Status := CuBit.Audio.Statistics (Stream);
         exit when not Status.Valid;
         Queued_Now := Status.Queued_Frames;
         Underruns_Now := Status.Underruns;
         exit when Status.Queued_Frames >= AUDIO_TARGET_FRAMES;
         Wanted := Natural'Min (Neon_Synth.Maximum_Block, AUDIO_TARGET_FRAMES - Status.Queued_Frames);
         Reservation := CuBit.Audio.reserveWrite (Stream, Wanted);
         exit when not CuBit.Audio.isReservationValid (Reservation);
         Frames := CuBit.Audio.reservedFrames (Reservation);
         Synthesize (Frames);
         Fill (CuBit.Audio.firstSpan (Reservation), 1);
         Fill (CuBit.Audio.secondSpan (Reservation), 1 + CuBit.Audio.firstSpan (Reservation).frames);
         CuBit.Audio.commitWrite (Stream, Reservation, Frames, Committed);
         exit when not Committed;
      end loop;
      if Now - Last_Playback_Us >= PLAYBACK_POLL_US then
         Last_Playback_Us := Now;
         declare
            P : constant CuBit.Audio.Playback_Status := CuBit.Audio.Playback (Stream);
         begin
            if P.Valid then
               Device_Frames := P.Device_Frames;
            end if;
         end;
      end if;
   end Refill;

   procedure Loop_Body (Argument : Unsigned_64) with Convention => C, No_Return;
   procedure Loop_Body (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
      Ignore : Unsigned_64;
   begin
      if Has_Authority then
         --  The calling thread's contract: this thread, not the window's.
         Is_Realtime := CuBit.Handles.Devices.Reserve_Realtime
           (Period_Us => Period_Us, Budget_Us => Budget_Us, Flags => No_Flags,
            Authority => The_Authority) = Admitted;
      end if;
      loop
         Lock;
         Refill (Now_Us);
         Unlock;
         Ignore := CuBit.Waits.Sleep_For (Interval_Ms);
      end loop;
   end Loop_Body;

   procedure Start
     (Authority : CuBit.Handles.Scheduling; Granted : Boolean; Started : out Boolean)
   is
      Stack, Area : Unsigned_64;
   begin
      Started := False;
      The_Authority := Authority;
      Has_Authority := Granted;
      Stack := CuBit.Owned_Reservations.Reserve (Stack_Bytes);
      if Stack = 0 then
         return;
      end if;
      if not CuBit.Owned_Reservations.Commit_Steps
           (Stack + ABI.Page_Bytes, Stack_Bytes - ABI.Page_Bytes)
      then
         return;
      end if;
      Area := CuBit.Thread_Areas.Create;
      if Area = CuBit.Thread_Areas.No_Area then
         return;
      end if;
      Is_Running := True;
      if CuBit.Kernel_Calls.Call
           (ABI.Thread_Create, Unsigned_64 (To_Integer (Loop_Body'Address)),
            ((Stack + Stack_Bytes) and not (Stack_Alignment - 1)) - Return_Address_Bytes,
            0, Area, Unsigned_64 (To_Integer (Exit_Word'Address))) = ABI.Failed
      then
         Is_Running := False;
         CuBit.Thread_Areas.Discard (Area);
         return;
      end if;
      Started := True;
   end Start;

end Neon_Audio;
