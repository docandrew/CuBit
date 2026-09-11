--  Native caller-death fixture. Both requests use the same FIFO async lane:
--  receipt of the second completion establishes that the first reply is saved.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;

procedure Departing is
   Service : constant CapabilitySlot := 18;
   Hold : constant Unsigned_32 := 16#090B#;
   Ready : constant Unsigned_32 := 16#090C#;
   Hold_Token : constant Unsigned_64 := 16#D1ED_0001#;
   Ready_Token : constant Unsigned_64 := 16#D1ED_0002#;
   Msg : Message := NULL_MESSAGE;
   Entries : CompletionRing := [others => NULL_COMPLETION];
   Count, Ignored : Unsigned_64;
begin
   Msg.tag := (Hold, 0, 0, 0);
   if not capSubmit (Service, Msg, Hold_Token) then
      debugPrint ("TEST: FAIL async-ipc departing hold submit" & ASCII.LF);
      return;
   end if;
   Msg.tag.label := Ready;
   if not capSubmit (Service, Msg, Ready_Token) then
      debugPrint ("TEST: FAIL async-ipc departing barrier submit" & ASCII.LF);
      return;
   end if;
   Count := waitCompletion (Entries'Address, 1, 1);
   if Count /= 1 or else Entries (0).status /= COMPLETION_OK or else
     Entries (0).token /= Ready_Token or else
     Entries (0).msg.tag.label /= 16#F000#
   then
      debugPrint ("TEST: FAIL async-ipc departing barrier" & ASCII.LF);
      debugPrint ("departing: count" & Count'Image &
                  " token" & Entries (0).token'Image &
                  " status" & Entries (0).status'Image &
                  " label" & Entries (0).msg.tag.label'Image & ASCII.LF);
      return;
   end if;
   debugPrint ("ipctest-departing: exiting with saved reply" & ASCII.LF);
   Ignored := syscall (SYSCALL_EXIT);
end Departing;
