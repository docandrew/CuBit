pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL_Manifest_Bindings;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Clocks;
with CuBit.Clock_Control;

--  Headless check for time synchronization. An ordinary clock client cannot
--  submit adjustments, and after timesync.svc runs the clock reports
--  network-synchronized time. The host fixture compares the reported UTC.
procedure Main is
   Clock_Slot : constant CapabilitySlot := CCL_Manifest_Bindings.Slot_Clock;
   OK_Label : constant Unsigned_32 := 16#F000#;
   Passed : Boolean := True;
   Request : Message;
   Reply : MessageTag;
   Stamp : CuBit.Clocks.Snapshot;
   Read_OK : Boolean;
   Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   Ignore : Unsigned_64;
   use type CuBit.Clocks.Time_Quality;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then Passed := False; end if;
      debugPrint ("timesync-check: " & Name &
                  (if Condition then " PASS" else " FAIL") & ASCII.LF);
   end Check;

   procedure Forge (Slot : CapabilitySlot) is
      Words : constant CuBit.Clock_Control.Words :=
        CuBit.Clock_Control.Encode
          ((UTC_MS => 1_900_000_000_000,
            Observed_Monotonic_MS => syscall (SYSCALL_GETTIME),
            Uncertainty_MS => 1, Sources => 9, Authenticated => True));
   begin
      Request := NULL_MESSAGE;
      Request.tag.label := CuBit.Clock_Control.Submit_Sample;
      Request.tag.length := 4;
      Request.words := [Words (0), Words (1), Words (2), Words (3)];
      Reply := capCall (Slot, Request);
   end Forge;
begin
   Forge (Clock_Slot);
   Check (Reply.label /= OK_Label, "ordinary clock endpoint cannot adjust time");
   Forge (CapabilitySlot (CuBit.Clock_Control.Endpoint_Slot));
   Check (Reply.label /= OK_Label, "undeclared clock-control slot is unusable");

   loop
      CuBit.Clocks.Read (Stamp, Read_OK);
      exit when Read_OK and then
        Stamp.Quality = CuBit.Clocks.Network_Unauthenticated;
      exit when syscall (SYSCALL_GETTIME) - Started > 60_000;
      Ignore := syscall (SYSCALL_SLEEP, 250);
   end loop;
   Check (Read_OK and then Stamp.Quality = CuBit.Clocks.Network_Unauthenticated,
          "clock reports network-synchronized time");
   debugPrint ("timesync-check: utc=" & Stamp.UTC_Seconds'Image & ASCII.LF);
   if Passed then
      debugPrint ("TEST: PASS timesync" & ASCII.LF);
   end if;
end Main;
