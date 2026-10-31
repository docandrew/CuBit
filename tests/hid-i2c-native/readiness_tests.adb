pragma Ada_2022;
with Ada.Text_IO;
with CuBit.Handles;
with CuBit.Driver_Readiness;
with CuBit.Messages;
with HID_Native_Readiness;
with HID_Startup_Readiness;
procedure Readiness_Tests is
   package R renames HID_Startup_Readiness;
   package M renames CuBit.Messages;
   package Native is new HID_Native_Readiness (CuBit.Handles.No_Endpoint);
   use type R.Choice, R.W.Word, M.MessageTag;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   procedure Frame (Ready : Boolean) is
      Expected : M.MessageTag := M.NULL_TAG;
   begin
      Expected.label := (if Ready then CuBit.Driver_Readiness.Ready_Label else CuBit.Driver_Readiness.Not_Present_Label);
      Check (M.Sent_Message.tag = Expected);
      Check (for all Word of M.Sent_Message.words => Word = 0);
      Check (M.Sent_Message.authorityTag = 0);
   end Frame;
   Send : R.Choice;
   Ack, Queued : Boolean;
   S : R.State;
begin
   Check (not R.Must_Stop (S) and then not R.Confirmed_Ready (S) and then
      not R.Attempted (S) and then not R.Receipt_Acknowledged (S));
   Check (R.Wake_Deadline (S, 100, 500) = 100 and then R.Wake_Deadline (S, 100, 50) = 50);
   R.Decide (S, 1, 100, True, False, False, Send); Check (Send = R.No_Report);
   R.Decide (S, 99, 100, True, True, False, Send); Check (Send = R.Report_Ready);
   Check (not R.Confirmed_Ready (S) and then not R.Attempted (S));
   Native.Send (R.Outgoing (Send), 120, Ack); Check (Ack and then M.Sent_Deadline = 120 and then M.Send_Calls = 1); Frame (True);
   R.Delivery (S, Ack); Check (R.Confirmed_Ready (S) and then not R.Must_Stop (S) and then
      R.Attempted (S) and then R.Receipt_Acknowledged (S));
   R.Delivery (S, False); Check (R.Confirmed_Ready (S) and then R.Receipt_Acknowledged (S)); -- first receipt retained
   R.Decide (S, 101, 100, True, False, True, Send); Check (Send = R.No_Report);
   Check (R.Wake_Deadline (S, 100, 500) = 500);
   for Available in Boolean loop
      for Ready in Boolean loop
         for Stopping in Boolean loop
            for Now in R.W.Word range 99 .. 101 loop
               declare Pending : R.State; Want : R.Choice; begin
                  Want := (if not Available or else Stopping or else Now >= 100 then R.Report_Declined
                     elsif Ready then R.Report_Ready else R.No_Report);
                  R.Decide (Pending, Now, 100, Available, Ready, Stopping, Send);
                  Check (Send = Want);
                  if Send /= R.No_Report then
                     R.Delivery (Pending, False); Check (R.Must_Stop (Pending) and then not R.Confirmed_Ready (Pending)
                        and then R.Attempted (Pending) and then not R.Receipt_Acknowledged (Pending));
                     R.Decide (Pending, 99, 100, True, True, False, Send); Check (Send = R.No_Report);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   declare Pending : R.State; begin
      R.Decide (Pending, 100, 100, True, True, False, Send);
      Check (Send = R.Report_Declined and then R.Must_Stop (Pending));
      Native.Send (R.Report_Declined, 130, Ack); Check (Ack and then M.Sent_Deadline = 130); Frame (False);
      R.Delivery (Pending, Ack); Check (R.Must_Stop (Pending) and then not R.Confirmed_Ready (Pending));
   end;
   declare Pending : R.State; begin
      R.Decide (Pending, 1, 100, True, True, False, Send);
      M.Send_Result.label := 1; Native.Send (R.Report_Ready, 10, Ack); Check (not Ack); Frame (True);
      R.Delivery (Pending, Ack); Check (R.Must_Stop (Pending));
      R.Decide (Pending, 2, 100, True, True, False, Send); Check (Send = R.No_Report);
   end;
   Native.Decline_Without_Clock (Queued); Check (Queued); Frame (False);
   Check (M.Submitted_Token = M.NO_COMPLETION_TOKEN and then M.Submit_Calls = 1 and then M.Send_Calls = 3);
   M.Submit_Result := False; Native.Decline_Without_Clock (Queued); Check (not Queued); Frame (False);
   Check (M.Submit_Calls = 2 and then M.Send_Calls = 3);
   Ada.Text_IO.Put_Line ("HID readiness checks" & Checks'Image);
end Readiness_Tests;
