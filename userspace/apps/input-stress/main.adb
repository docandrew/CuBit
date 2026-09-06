------------------------------------------------------------------------------
--  Deterministic typed-input publication and recovery regression.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Input; use CuBit.Input;
with CuBit.Messages; use CuBit.Messages;

procedure Main is
   use ASCII;

   pointerConsumer  : ProcessID := NO_PROCESS;
   keyboardConsumer : ProcessID := NO_PROCESS;
   desktopConsumer  : ProcessID := NO_PROCESS;
   pointerSequence  : Source_Sequence := 0;
   keyboardSequence : Source_Sequence := 0;
   ignored          : Unsigned_64;
   passed           : Boolean := True;

   function Pack_Signed_12 (value : Integer) return Unsigned_64 is
     (Unsigned_64 (value mod 4096) and 16#FFF#);

   procedure Publish
     (destination : ProcessID;
      report      : Source_Report)
   is
      pending : Source_Report := report;
   begin
      for attempt in 1 .. 100 loop
         if trySendEvent (destination, Encode (pending)) then
            return;
         end if;
         pending.flags (RESYNCHRONIZE) := True;
         ignored := syscall (SYSCALL_SLEEP, 1);
      end loop;
      passed := False;
   end Publish;

   procedure Publish_Pointer
     (dx, dy  : Integer;
      buttons : Unsigned_64;
      resync  : Boolean := False)
   is
      packed : constant Unsigned_64 :=
        (buttons and 16#FF#) or
        Shift_Left (Pack_Signed_12 (dx), 8) or
        Shift_Left (Pack_Signed_12 (dy), 20);
   begin
      pointerSequence := Next_Sequence (pointerSequence);
      Publish
        (pointerConsumer,
         (sourceBadge => 0,
          sequence => pointerSequence,
          generation => 1,
          device => RELATIVE_POINTER,
          delivery => ACCUMULABLE_DISPLACEMENT,
          flags => (RESYNCHRONIZE => resync),
          payload => packed,
          snapshot => buttons));
   end Publish_Pointer;

   procedure Publish_Key (scancode : Unsigned_8) is
   begin
      keyboardSequence := Next_Sequence (keyboardSequence);
      Publish
        (keyboardConsumer,
         (sourceBadge => 0,
          sequence => keyboardSequence,
          generation => 1,
          device => KEYBOARD,
          delivery => ORDERED_TRANSITION,
          flags => NO_REPORT_FLAGS,
          payload => Unsigned_64 (scancode),
          snapshot => 0));
   end Publish_Key;

begin
   debugPrint ("input-stress: waiting for input consumers" & LF);
   for attempt in 1 .. 200 loop
      pointerConsumer :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_MOUSE);
      keyboardConsumer :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_KEYBOARD);
      desktopConsumer :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP);
      exit when desktopConsumer /= NO_PROCESS and then
        pointerConsumer = desktopConsumer and then
        keyboardConsumer = desktopConsumer;
      ignored := syscall (SYSCALL_SLEEP, 10);
   end loop;

   if desktopConsumer = NO_PROCESS or else
      pointerConsumer /= desktopConsumer or else
      keyboardConsumer /= desktopConsumer
   then
      debugPrint ("input-stress: consumers unavailable FAIL" & LF);
      passed := False;
   else
      --  Let the optional Workbench acceptance client finish opening. The
      --  stream assertions do not depend on it, but the headless profile also
      --  verifies that ordinary motion over a rich editor does not trigger a
      --  client repaint for every report.
      ignored := syscall (SYSCALL_SLEEP, 500);

      --  Move from desktop's deterministic boot position into the center of
      --  the Workbench source editor before exercising the paced stream.
      Publish_Pointer (320, -220, 0);
      ignored := syscall (SYSCALL_SLEEP, 10);

      --  A paced motion stream should remain continuous under admitted load.
      for sample in 1 .. 128 loop
         Publish_Pointer (1, (if sample mod 2 = 0 then 1 else -1), 0);
         ignored := syscall (SYSCALL_SLEEP, 1);
      end loop;

      Publish_Pointer (0, 0, 1);
      Publish_Pointer (0, 0, 0);

      --  Deliberately skip one source sequence. The following state-bearing
      --  report must be accepted as an explicit resynchronization boundary.
      pointerSequence := Next_Sequence (pointerSequence);
      Publish_Pointer (1, 0, 0, resync => True);

      Publish_Key (16#1E#);  -- A down
      Publish_Key (16#9E#);  -- A up

      --  Wake desktop after its one-second telemetry window so the headless
      --  gate can observe the accumulated continuity/rejection counters.
      ignored := syscall (SYSCALL_SLEEP, 1_200);
      Publish_Pointer (0, 0, 0);
   end if;

   if passed then
      debugPrint ("input-stress: publication and recovery PASS" & LF);
   else
      debugPrint ("input-stress: publication and recovery FAIL" & LF);
   end if;

   loop
      ignored := syscall (SYSCALL_SLEEP, 1_000);
   end loop;
end Main;
