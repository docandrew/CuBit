------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with TCP_Limits;

package body TCP_Connection with SPARK_Mode is

   --  SEG.ACK acknowledges something in flight: SND.UNA < ACK <= SND.NXT.
   function Acks_In_Flight (C : Connection; Ack : Seq) return Boolean is
     (Distance (C.Snd_Una, Ack) in 1 .. Distance (C.Snd_Una, C.Snd_Nxt));

   --  An acknowledgement in flight leaves less in flight.
   procedure Lemma_Ack_Shrinks (Una, Nxt, Ack : Seq) with
     Ghost, Global => null,
     Pre  => Distance (Una, Ack) in 1 .. Distance (Una, Nxt) and then
             Distance (Una, Nxt) <= Maximum_Window,
     Post => Distance (Ack, Nxt) = Distance (Una, Nxt) - Distance (Una, Ack) and then
             Distance (Ack, Nxt) < Distance (Una, Nxt) and then
             Lt (Una, Ack) and then Le (Ack, Nxt);
   procedure Lemma_Ack_Shrinks (Una, Nxt, Ack : Seq) is null;

   --  CLOSED, LISTEN and SYN-SENT (3.10.7.1 - 3.10.7.3).
   procedure Arrive_Unsynchronized (C : in out Connection; S : Segment; New_ISS : Seq;
                                    O : in out Outcome)
   with
     Pre  => Valid (C) and then C.St not in Synchronized_State and then O.Count = 0,
     Post => Valid (C) and then Allowed (C'Old.St, C.St) and then O.Count = 0 and then
             C.Fin_Pending = C'Old.Fin_Pending and then C.Fin_Sent = C'Old.Fin_Sent and then
             (if C'Old.St = Closed then C.St = Closed) and then
             (if C'Old.St = Listen then C.St in Listen | Syn_Received) and then
             (if C'Old.St = Listen and then C.St = Syn_Received then
                C.Snd_Una = New_ISS and then C.Snd_Nxt = New_ISS + 1) and then
             (if C'Old.St = Syn_Sent and then C'Old.Snd_Nxt = C'Old.Snd_Una + 1 then
                C.Snd_Nxt = C'Old.Snd_Nxt and then
                (if C.St = Established then C.Snd_Una = C'Old.Snd_Nxt else C.Snd_Una = C'Old.Snd_Una))
   is
      Ack_OK : Boolean;
   begin
      case C.St is
         when Closed =>
            --  Answer anything but a RST with a RST.
            if not S.RST then
               if S.ACK then
                  O.Answer := Send_Reset;
                  O.Reset_Seq := S.Ack_No;
               else
                  O.Answer := Send_Reset_Ack;
                  O.Reset_Ack := S.Seq_No + S.Length +
                    (if S.SYN then 1 else 0) + (if S.FIN then 1 else 0);
               end if;
            end if;

         when Listen =>
            if S.RST then
               null;
            elsif S.ACK then
               O.Answer := Send_Reset;
               O.Reset_Seq := S.Ack_No;
            elsif S.SYN then
               C.IRS := S.Seq_No;
               C.Rcv_Nxt := S.Seq_No + 1;
               C.ISS := New_ISS;
               C.Snd_Una := New_ISS;
               C.Snd_Nxt := New_ISS + 1;
               C.Snd_Wnd := S.Window;
               C.Snd_Wl1 := S.Seq_No;
               C.Snd_Wl2 := New_ISS;
               C.Passive := True;
               C.St := Syn_Received;
               O.Answer := Send_Syn_Ack;
            end if;

         when Syn_Sent =>
            --  SND.UNA = ISS here, so this is ISS < ACK <= SND.NXT.
            Ack_OK := S.ACK and then Acks_In_Flight (C, S.Ack_No);
            if S.ACK and then not Ack_OK then
               if not S.RST then
                  O.Answer := Send_Reset;
                  O.Reset_Seq := S.Ack_No;
               end if;
            elsif S.RST then
               if Ack_OK then
                  C.St := Closed;
                  O.Happened := Refused;
               end if;
            elsif S.SYN then
               C.IRS := S.Seq_No;
               C.Rcv_Nxt := S.Seq_No + 1;
               if Ack_OK then
                  Lemma_Ack_Shrinks (C.Snd_Una, C.Snd_Nxt, S.Ack_No);
                  C.Snd_Una := S.Ack_No;
                  C.Snd_Wnd := S.Window;
                  C.Snd_Wl1 := S.Seq_No;
                  C.Snd_Wl2 := S.Ack_No;
                  C.St := Established;
                  O.Answer := Send_Ack;
                  O.Happened := Connected;
               else
                  --  Simultaneous open.
                  C.St := Syn_Received;
                  O.Answer := Send_Syn_Ack;
               end if;
            end if;

         when Synchronized_State =>
            null;
      end case;
   end Arrive_Unsynchronized;

   --  Fifth step of 3.10.7.4: the acknowledgement. Stop: the segment is
   --  finished with. Fin_Acked: our FIN is acknowledged.
   procedure Process_Ack (C : in out Connection; S : Segment; O : in out Outcome;
                          Stop, Fin_Acked : out Boolean)
   with
     Pre  => Valid (C) and then C.St in Synchronized_State and then S.ACK and then
             O.Count = 0,
     Post => Valid (C) and then O.Count = 0 and then
             C.Rcv_Nxt = C'Old.Rcv_Nxt and then C.Rcv_Wnd = C'Old.Rcv_Wnd and then
             C.Fin_Sent = C'Old.Fin_Sent and then C.Fin_Pending = C'Old.Fin_Pending and then
             (C.St = C'Old.St or else
              (C'Old.St = Syn_Received and then C.St = Established) or else
              (C'Old.St = Fin_Wait_1 and then C.St = Fin_Wait_2) or else
              (C'Old.St = Closing and then C.St = Time_Wait) or else
              (C'Old.St = Last_Ack and then C.St = Closed)) and then
             (if C.St = Closed then Stop) and then
             (if C'Old.St = Syn_Received and then C'Old.Snd_Nxt = C'Old.Snd_Una + 1 and then
                 C.St /= Syn_Received and then C.St /= Closed
              then C.Snd_Una = C'Old.Snd_Nxt) and then
             (if C'Old.St = Syn_Received and then C.St = Syn_Received then C.Snd_Una = C'Old.Snd_Una) and then
             Le (C'Old.Snd_Una, C.Snd_Una) and then Le (C.Snd_Una, C.Snd_Nxt) and then
             C.Snd_Nxt = C'Old.Snd_Nxt and then
             --  The window rule, stated on its own (RFC 9293 3.10.7.4): an
             --  ACK that is not refused in SYN-RECEIVED and does not
             --  acknowledge unsent data updates the window exactly when it
             --  passes the test against the updated SND.UNA, and nothing
             --  else touches the window.
             (if not (C'Old.St = Syn_Received and then C.St = Syn_Received) and then
                 Updates_Window ((C'Old with delta Snd_Una => C.Snd_Una), S)
              then C.Snd_Wnd = S.Window and then C.Snd_Wl1 = S.Seq_No and then
                   C.Snd_Wl2 = S.Ack_No
              else C.Snd_Wnd = C'Old.Snd_Wnd and then C.Snd_Wl1 = C'Old.Snd_Wl1 and then
                   C.Snd_Wl2 = C'Old.Snd_Wl2) and then
             --  Stated on its own: a segment older than the one that last
             --  set the window never changes it.
             (if Lt (S.Seq_No, C'Old.Snd_Wl1) then C.Snd_Wnd = C'Old.Snd_Wnd)
   is
   begin
      Stop := False;
      Fin_Acked := False;
      if C.St = Syn_Received then
         if Acks_In_Flight (C, S.Ack_No) then
            C.St := Established;
            O.Happened := Connected;
         else
            O.Answer := Send_Reset;
            O.Reset_Seq := S.Ack_No;
            Stop := True;
            return;
         end if;
      end if;
      if Acks_In_Flight (C, S.Ack_No) then
         Lemma_Ack_Shrinks (C.Snd_Una, C.Snd_Nxt, S.Ack_No);
         C.Snd_Una := S.Ack_No;
      elsif Distance (C.Snd_Una, S.Ack_No) > Distance (C.Snd_Una, C.Snd_Nxt)
        and then Lt (C.Snd_Nxt, S.Ack_No)
      then
         --  Acknowledges data never sent: ACK and drop.
         O.Answer := Send_Ack;
         Stop := True;
         return;
      end if;
      if Updates_Window (C, S) then
         C.Snd_Wnd := S.Window;
         C.Snd_Wl1 := S.Seq_No;
         C.Snd_Wl2 := S.Ack_No;
      end if;
      Fin_Acked := C.Fin_Sent and then C.Snd_Una = C.Snd_Nxt;
      case C.St is
         when Fin_Wait_1 =>
            if Fin_Acked then C.St := Fin_Wait_2; end if;
         when Closing =>
            if Fin_Acked then C.St := Time_Wait; end if;
         when Last_Ack =>
            if Fin_Acked then
               C.St := Closed;
               O.Happened := Closed_Fully;
               Stop := True;
            end if;
         when others =>
            null;
      end case;
   end Process_Ack;

   --  Seventh and eighth steps: segment text in order, then a FIN once
   --  everything before it has arrived.
   procedure Process_Text_And_Fin (C : in out Connection; S : Segment; Fin_Acked : Boolean;
                                   O : in out Outcome)
   with
     Pre  => Valid (C) and then C.St in Synchronized_State and then O.Count = 0,
     Post => Valid (C) and then
             C.Snd_Una = C'Old.Snd_Una and then C.Snd_Nxt = C'Old.Snd_Nxt and then
             C.Fin_Pending = C'Old.Fin_Pending and then C.Fin_Sent = C'Old.Fin_Sent and then
             (C.St = C'Old.St or else
              (C'Old.St = Established and then C.St = Close_Wait) or else
              (C'Old.St = Fin_Wait_1 and then C.St in Closing | Time_Wait) or else
              (C'Old.St = Fin_Wait_2 and then C.St = Time_Wait)) and then
             (if O.Count > 0 then
                C'Old.St in Data_State and then
                O.Deliver_First = C'Old.Rcv_Nxt and then
                O.Count <= C'Old.Rcv_Wnd and then
                O.Skip + O.Count <= S.Length) and then
             (C.Rcv_Nxt = C'Old.Rcv_Nxt + O.Count or else
              C.Rcv_Nxt = C'Old.Rcv_Nxt + O.Count + 1)
   is
      First : Seq;
      Skip, Count : Segment_Length;
   begin
      if S.Length > 0 and then C.St in Data_State and then
        Acceptable (S.Seq_No, S.Length, C.Rcv_Nxt, C.Rcv_Wnd)
      then
         Trim (S.Seq_No, S.Length, C.Rcv_Nxt, C.Rcv_Wnd, First, Skip, Count);
         if First = C.Rcv_Nxt then
            O.Deliver_First := First;
            O.Skip := Skip;
            O.Count := Count;
            C.Rcv_Nxt := C.Rcv_Nxt + Count;
         end if;
         O.Answer := Send_Ack;
      end if;

      if S.FIN then
         O.Answer := Send_Ack;
         if S.Seq_No + S.Length = C.Rcv_Nxt and then
           C.St in Established | Fin_Wait_1 | Fin_Wait_2
         then
            C.Rcv_Nxt := C.Rcv_Nxt + 1;
            if O.Happened = None then
               O.Happened := Peer_Closed;
            end if;
            case C.St is
               when Established => C.St := Close_Wait;
               when Fin_Wait_1 =>
                  C.St := (if Fin_Acked then Time_Wait else Closing);
               when Fin_Wait_2 => C.St := Time_Wait;
               when others => null;
            end case;
         end if;
      end if;
   end Process_Text_And_Fin;

   procedure Arrive (C : in out Connection; S : Segment; New_ISS : Seq;
                     O : out Outcome)
   is
      Stop, Fin_Acked : Boolean;
      Len : Segment_Length;
   begin
      O := (others => <>);
      if C.St not in Synchronized_State then
         Arrive_Unsynchronized (C, S, New_ISS, O);
         return;
      end if;

      --  3.10.7.4, synchronized states. An over-long segment cannot be
      --  counted with its SYN and FIN; drop it as unacceptable.
      if S.Length > Maximum_Window - TCP_Limits.Control_Octets then
         if not S.RST then
            O.Answer := Send_Ack;
         end if;
         return;
      end if;
      Len := S.Length + (if S.SYN then 1 else 0) + (if S.FIN then 1 else 0);

      --  First: sequence number acceptability.
      if not Acceptable (S.Seq_No, Len, C.Rcv_Nxt, C.Rcv_Wnd) then
         if not S.RST then
            O.Answer := Send_Ack;
            --  A retransmission ending exactly at RCV.NXT (typically the
            --  peer's FIN again) carries a current acknowledgement: take it,
            --  as Linux does (tcp_sequence accepts end_seq >= RCV.NXT), but
            --  none of its text or FIN. Otherwise crossing FINs wait in
            --  CLOSING for a FIN retransmission when the peer's ACK came
            --  only on such a segment.
            if S.ACK and then not S.SYN and then C.St /= Syn_Received and then
              Len > 0 and then S.Seq_No + Seq (Len) = C.Rcv_Nxt
            then
               Process_Ack (C, S, O, Stop, Fin_Acked);
               O.Answer := Send_Ack;
            end if;
         end if;
         return;
      end if;

      --  Second: RST (RFC 5961 3.2).
      if S.RST then
         if S.Seq_No = C.Rcv_Nxt then
            if C.St = Syn_Received and then C.Passive then
               C.St := Listen;
            else
               C.St := Closed;
               O.Happened := Reset;
            end if;
         else
            O.Answer := Send_Challenge_Ack;
         end if;
         return;
      end if;

      --  Fourth: SYN (RFC 5961 4.2).
      if S.SYN then
         O.Answer := Send_Challenge_Ack;
         return;
      end if;

      --  Fifth: ACK.
      if not S.ACK then
         return;
      end if;
      Process_Ack (C, S, O, Stop, Fin_Acked);
      if Stop then
         return;
      end if;

      Process_Text_And_Fin (C, S, Fin_Acked, O);
   end Arrive;

   procedure Open_Active (C : in out Connection; ISS : Seq; Window : Window_Size) is
   begin
      C := (St => Syn_Sent, Passive => False, ISS => ISS, IRS => 0,
            Snd_Una => ISS, Snd_Nxt => ISS + 1, Snd_Wnd => 0, Snd_Wl1 => 0, Snd_Wl2 => 0,
            Rcv_Nxt => 0, Rcv_Wnd => Window,
            Fin_Pending => False, Fin_Sent => False);
   end Open_Active;

   procedure Open_Passive (C : in out Connection; Window : Window_Size) is
   begin
      C := (St => Listen, Passive => True, ISS => 0, IRS => 0,
            Snd_Una => 0, Snd_Nxt => 0, Snd_Wnd => 0, Snd_Wl1 => 0, Snd_Wl2 => 0,
            Rcv_Nxt => 0, Rcv_Wnd => Window,
            Fin_Pending => False, Fin_Sent => False);
   end Open_Passive;

   procedure Close (C : in out Connection) is
   begin
      case C.St is
         when Listen | Syn_Sent =>
            C.St := Closed;
         when Syn_Received | Established =>
            C.St := Fin_Wait_1;
            C.Fin_Pending := True;
         when Close_Wait =>
            C.St := Last_Ack;
            C.Fin_Pending := True;
         when others =>
            null;
      end case;
   end Close;

   procedure Data_Sent (C : in out Connection; N : Segment_Length) is
   begin
      C.Snd_Nxt := C.Snd_Nxt + N;
   end Data_Sent;

   procedure Fin_Sent_Now (C : in out Connection) is
   begin
      C.Snd_Nxt := C.Snd_Nxt + 1;
      C.Fin_Sent := True;
   end Fin_Sent_Now;

   procedure Time_Wait_Expired (C : in out Connection) is
   begin
      C.St := Closed;
   end Time_Wait_Expired;
end TCP_Connection;
