--  Linux-hosted check of TCP_Flow: a whole connection over a lossy link
--  with simulated time - handshake, transfer and close, each recovered by
--  replies and the retransmission timer alone. The event handling here is
--  what the netstack does with a flow's results.
with Ada.Text_IO;    use Ada.Text_IO;
with Interfaces;     use Interfaces;
with TCP_Sequence;   use TCP_Sequence;
with TCP_Connection; use TCP_Connection;
with Pool_Small;
with Send_Chunked_Small;
with Receive_Queue_64;
with Flow_Small;

procedure TCP_Flow_Sim is
   package FL renames Flow_Small;
   package SQ renames Send_Chunked_Small;
   package RQ renames Receive_Queue_64;
   use type Pool_Small.Byte_Array;

   Failures : Natural := 0;
   procedure Check (OK : Boolean; Name : String) is
   begin
      if not OK then
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   MSS          : constant := 8;
   Message      : constant := 2000;
   Loss_Percent : constant := 10;
   Loss_Percent_Now : Natural := Loss_Percent;
   Rand : Unsigned_32 := 777;
   --  Segments to drop before random loss resumes (forces a given loss).
   Drop_Next : Natural := 0;
   --  Drop the active side's first segment that carries its FIN.
   Drop_Fin  : Boolean := True;
   --  Drop segments that are only acknowledgements (no text, no SYN/FIN).
   Drop_Acks : Boolean := False;
   --  Hold segments in flight (so two can cross) until Release.
   Holding : Boolean := False;
   type Held_Segment is record
      From : Positive := 1;
      S    : Segment;
   end record;
   Held : array (1 .. 8) of Held_Segment;
   Held_Count : Natural := 0;
   function Lost return Boolean is
   begin
      if Drop_Next > 0 then
         Drop_Next := Drop_Next - 1;
         return True;
      end if;
      Rand := Rand * 1_103_515_245 + 12_345;
      return Shift_Right (Rand, 16) mod 100 < Unsigned_32 (Loss_Percent_Now);
   end Lost;

   subtype Side is Positive range 1 .. 2;   --  1 opens actively, 2 listens
   ISS : constant array (Side) of Seq := [100, 5000];

   P     : Pool_Small.Pool;
   Flows : array (Side) of FL.Flow;
   Now   : FL.Time := 0;
   None  : constant RQ.Byte_Array (1 .. 0) := [others => 0];
   Sent_Data : Pool_Small.Byte_Array (1 .. Message) :=
     [for I in 1 .. Message => Unsigned_8 (I mod 251)];
   Written, Got : Natural := 0;
   Received : Pool_Small.Byte_Array (1 .. Message) := [others => 0];
   Segments, Drops, Timeouts, Syn_Resent : Natural := 0;
   Gave_Up, Peer_Closed_Seen, Reset_Seen : Boolean := False;

   function Header (F : FL.Flow) return Segment is
     ((Seq_No => F.E.C.Snd_Nxt, Ack_No => F.E.C.Rcv_Nxt, ACK => True,
       Window => F.E.C.Rcv_Wnd, others => <>));

   procedure Transmit (From : Side; S : Segment; Text : RQ.Byte_Array);

   --  The reply a flow's state machine asked for.
   procedure Answer (Who : Side; O : Outcome) is
      F : FL.Flow renames Flows (Who);
   begin
      if O.Happened = Peer_Closed then
         Peer_Closed_Seen := True;
      elsif O.Happened in Reset | Refused then
         Reset_Seen := True;
      end if;
      case O.Answer is
         when No_Reply => null;
         when Send_Ack | Send_Challenge_Ack => Transmit (Who, Header (F), None);
         when Send_Syn_Ack =>
            Transmit (Who, (Seq_No => F.E.C.ISS, Ack_No => F.E.C.Rcv_Nxt, SYN => True,
                            ACK => True, Window => F.E.C.Rcv_Wnd, others => <>), None);
         when Send_Reset => Transmit (Who, (Seq_No => O.Reset_Seq, RST => True, others => <>), None);
         when Send_Reset_Ack =>
            Transmit (Who, (Ack_No => O.Reset_Ack, RST => True, ACK => True, others => <>), None);
      end case;
   end Answer;

   Depth : Natural := 0;
   procedure Transmit (From : Side; S : Segment; Text : RQ.Byte_Array) is
      To : constant Side := 3 - From;
      O  : Outcome;
   begin
      Segments := Segments + 1;
      if From = 1 and then S.FIN and then Drop_Fin then
         Drop_Fin := False;
         Drops := Drops + 1;
         return;
      end if;
      if Drop_Acks and then S.Length = 0 and then not S.FIN and then not S.SYN then
         Drops := Drops + 1;
         return;
      end if;
      if Holding and then Held_Count < Held'Last and then Text'Length = 0 then
         Held_Count := Held_Count + 1;
         Held (Held_Count) := (From => From, S => S);
         return;
      end if;
      if Lost then
         Drops := Drops + 1;
         return;
      end if;
      Check (Depth < 8, "replies do not ping-pong");
      if Depth >= 8 then
         return;
      end if;
      Depth := Depth + 1;
      FL.Arrive (Flows (To), P, Now, S, Text, ISS (To), O);
      if S.ACK then
         FL.Probe_Answered (Flows (To));
      end if;
      FL.Update_Persist (Flows (To), Now);
      Answer (To, O);
      Depth := Depth - 1;
   end Transmit;

   --  Send what the flow allows.
   procedure Pump (Who : Side) is
      F      : FL.Flow renames Flows (Who);
      Buffer : Pool_Small.Byte_Array (1 .. MSS);
      First  : Seq;
      Taken  : SQ.Byte_Count;
      FIN    : Boolean;
   begin
      while F.E.C.St in Established | Close_Wait | Fin_Wait_1 | Closing | Last_Ack loop
         FL.Send (F, P, Now, Buffer, First, Taken, FIN);
         exit when Taken = 0 and then not FIN;
         declare
            S    : Segment := Header (F);
            Text : constant RQ.Byte_Array (1 .. Taken) := [for K in 1 .. Taken => Buffer (K)];
         begin
            S.Seq_No := First;
            S.FIN := FIN;
            S.Length := Seq (Taken);
            Transmit (Who, S, Text);
         end;
      end loop;
      FL.Update_Persist (F, Now);
   end Pump;

   procedure Timer (Who : Side) is
      F : FL.Flow renames Flows (Who);
   begin
      if not F.Armed or else Now < F.Deadline or else F.E.C.St not in Syn_Sent | Synchronized_State then
         return;
      end if;
      if FL.Exhausted (F) then
         Gave_Up := True;
         return;
      end if;
      FL.Retransmit_Timeout (F, P, Now);
      Timeouts := Timeouts + 1;
      Check (F.Armed, "the timer restarts when it fires");
      case F.E.C.St is
         when Syn_Sent =>
            Syn_Resent := Syn_Resent + 1;
            Transmit (Who, (Seq_No => F.E.C.ISS, SYN => True, Window => F.E.C.Rcv_Wnd,
                            others => <>), None);
         when Syn_Received =>
            Transmit (Who, (Seq_No => F.E.C.ISS, Ack_No => F.E.C.Rcv_Nxt, SYN => True,
                            ACK => True, Window => F.E.C.Rcv_Wnd, others => <>), None);
         when others => Pump (Who);
      end case;
   end Timer;

   Out_Buf  : RQ.Byte_Array (1 .. 64);
   Read_Got : RQ.Byte_Count;
   Accepted : SQ.Byte_Count;
   A_Closed, B_Closed : Boolean := False;
begin
   Pool_Small.Initialize (P);
   FL.Open_Passive (Flows (2), P, 2, MSS);
   FL.Open_Active (Flows (1), P, 1, ISS (1), MSS, Now);
   Check (Flows (1).Armed, "the SYN starts the timer");
   Drop_Next := 1;   --  the first SYN is lost
   Transmit (1, (Seq_No => ISS (1), SYN => True, Window => Flows (1).E.C.Rcv_Wnd, others => <>), None);

   for Tick in 1 .. 50_000 loop
      exit when Gave_Up or else
        (Flows (1).E.C.St in Time_Wait | Closed and then Flows (2).E.C.St = Closed);
      Now := Now + 10;
      declare
         A : FL.Flow renames Flows (1);
         B : FL.Flow renames Flows (2);
      begin
         if A.E.C.St = Established and then Written < Message then
            FL.EP.Write (A.E, P, Sent_Data (Written + 1 .. Natural'Min (Written + 16, Message)),
                         Accepted);
            Written := Written + Accepted;
         end if;
         --  Close with data still in flight: an ACK for the data must not
         --  stop the timer that covers the (lost) FIN.
         if not A_Closed and then Written = Message then
            FL.EP.Close (A.E, P);
            A_Closed := True;
         end if;
         if B.E.C.St not in Closed | Listen | Syn_Sent then
            FL.EP.Read (B.E, P, Out_Buf, Read_Got);
            for K in 1 .. Read_Got loop
               if Got < Message then
                  Got := Got + 1;
                  Received (Got) := Out_Buf (K);
               end if;
            end loop;
            if Read_Got > 0 then
               Transmit (2, Header (B), None);   --  the window reopened
            end if;
            if not B_Closed and then B.E.C.St = Close_Wait and then Got = Message then
               FL.EP.Close (B.E, P);
               B_Closed := True;
            end if;
         end if;
      end;
      Pump (1);
      Pump (2);
      Timer (1);
      Timer (2);
   end loop;

   Check (not Gave_Up, "no side gave up");
   Check (Syn_Resent >= 1, "a lost SYN is resent");
   Check (not Drop_Fin, "a FIN was lost");
   Check (Got = Message and then Received = Sent_Data, "every byte arrives in order");
   Check (Peer_Closed_Seen and then not Reset_Seen, "the peer's FIN is seen, no reset");
   Check (Flows (1).E.C.St = Time_Wait, "the active closer waits in TIME-WAIT");
   Check (Flows (2).E.C.St = Closed, "the passive closer is closed");
   Check (not Flows (1).Armed and then not Flows (2).Armed, "no timer left running");
   Check (SQ.Chunks_Held (Flows (2).E.S) = 0, "the closed side holds no chunks");
   Check (Drops > Timeouts, "most losses are recovered without waiting for the timer");
   Put_Line ("segments:" & Segments'Image & ", lost:" & Drops'Image & ", timeouts:" & Timeouts'Image &
             ", time (ms):" & Now'Image & ", RTO:" & Flows (1).RTO.RTO'Image);

   --  Simultaneous close with lost acknowledgements (seen live under slirp:
   --  the peer's ACK of our FIN came in a segment we must drop as old): the
   --  FINs cross, both sides reach CLOSING, the ACKs that would end it are
   --  lost, and only retransmitting the FIN from CLOSING finishes the close.
   Pool_Small.Initialize (P);
   Loss_Percent_Now := 0;
   Drop_Fin := False;
   FL.Open_Passive (Flows (2), P, 2, MSS);
   FL.Open_Active (Flows (1), P, 1, ISS (1), MSS, Now);
   Transmit (1, (Seq_No => ISS (1), SYN => True, Window => Flows (1).E.C.Rcv_Wnd, others => <>), None);
   Check (Flows (1).E.C.St = Established and then Flows (2).E.C.St = Established,
          "crossing FINs: connected");
   FL.EP.Close (Flows (1).E, P);
   FL.EP.Close (Flows (2).E, P);
   Drop_Acks := True;
   Holding := True;
   Pump (1);
   Pump (2);   --  both FINs in flight at once
   Holding := False;
   for K in 1 .. Held_Count loop
      Transmit (Held (K).From, Held (K).S, None);
   end loop;
   Check (Flows (1).E.C.St = Closing and then Flows (2).E.C.St = Closing,
          "crossing FINs: both sides in CLOSING");
   Drop_Acks := False;
   for Tick in 1 .. 50_000 loop
      exit when Gave_Up or else
        (Flows (1).E.C.St in Time_Wait | Closed and then Flows (2).E.C.St in Time_Wait | Closed);
      Now := Now + 10;
      Timer (1);
      Timer (2);
   end loop;
   Check (not Gave_Up and then Flows (1).E.C.St = Time_Wait and then
          Flows (2).E.C.St = Time_Wait,
          "crossing FINs: the FIN retransmitted from CLOSING ends the close");

   --  A lost window update (RFC 9293 3.8.6.1): the receiver's window
   --  fills, it then reads everything, and the segment reopening the window
   --  is lost. Nothing is in flight, so no retransmission would bring the
   --  update: only the persist timer's probes do.
   Pool_Small.Initialize (P);
   Loss_Percent_Now := 0;
   Drop_Fin := False;
   Gave_Up := False;
   FL.Open_Passive (Flows (2), P, 2, MSS);
   FL.Open_Active (Flows (1), P, 1, ISS (1), MSS, Now);
   Transmit (1, (Seq_No => ISS (1), SYN => True, Window => Flows (1).E.C.Rcv_Wnd, others => <>), None);
   declare
      A : FL.Flow renames Flows (1);
      B : FL.Flow renames Flows (2);
      Total   : constant := 200;
      Pushed, Taken_Back, Probes : Natural := 0;
   begin
      while Pushed < Total and then not FL.Window_Blocked (A) loop
         FL.EP.Write (A.E, P, Sent_Data (Pushed + 1 .. Natural'Min (Pushed + 16, Total)), Accepted);
         Pushed := Pushed + Accepted;
         Pump (1);
         exit when Accepted = 0;
      end loop;
      Check (FL.Window_Blocked (A) and then A.Persisting,
             "zero window: the sender is blocked and persisting");
      --  The receiver drains everything; its window update is lost.
      loop
         FL.EP.Read (B.E, P, Out_Buf, Read_Got);
         Taken_Back := Taken_Back + Read_Got;
         exit when Read_Got = 0;
      end loop;
      Drop_Next := 1;
      Transmit (2, Header (B), None);
      for Tick in 1 .. 100_000 loop
         exit when Gave_Up or else (Taken_Back >= Pushed and then Pushed >= Total);
         Now := Now + 10;
         if Pushed < Total then
            FL.EP.Write (A.E, P, Sent_Data (Pushed + 1 .. Natural'Min (Pushed + 16, Total)),
                         Accepted);
            Pushed := Pushed + Accepted;
         end if;
         FL.EP.Read (B.E, P, Out_Buf, Read_Got);
         Taken_Back := Taken_Back + Read_Got;
         if Read_Got > 0 then
            Transmit (2, Header (B), None);
         end if;
         Pump (1);
         if A.Persisting and then Now >= A.Persist_At then
            declare
               Give_Up : Boolean;
            begin
               FL.Persist_Timeout (A, Now, Give_Up);
               if Give_Up then
                  Gave_Up := True;
               else
                  Probes := Probes + 1;
                  Transmit (1, (Seq_No => A.E.C.Snd_Una - 1, Ack_No => A.E.C.Rcv_Nxt,
                                ACK => True, Window => A.E.C.Rcv_Wnd, others => <>), None);
               end if;
            end;
         end if;
         Timer (1);
      end loop;
      Check (not Gave_Up and then Probes >= 1 and then Taken_Back = Total,
             "zero window: a probe recovers the lost update and every byte arrives");
      Put_Line ("zero window: probes" & Probes'Image & ", time (ms):" & Now'Image);
   end;
   Put_Line (if Failures = 0 then "TCP-FLOW-SIM: PASS" else "TCP-FLOW-SIM: FAIL");
end TCP_Flow_Sim;
