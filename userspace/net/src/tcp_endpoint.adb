------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body TCP_Endpoint with SPARK_Mode is

   --  Counts below 2**30 add the same way as sequence numbers.
   procedure Lemma_Seq_Add (A, B : Natural) with
     Ghost, Global => null,
     Pre  => A <= 2 ** 30 and then B <= 2 ** 30 - A,
     Post => Seq (A + B) = Seq (A) + Seq (B);
   procedure Lemma_Seq_Add (A, B : Natural) is null;

   --  A point between two sequence numbers less than half the space apart
   --  is no further from the first than the second is.
   procedure Lemma_Between (X, A, Y : Seq) with
     Ghost, Global => null,
     Pre  => Le (X, A) and then Le (A, Y) and then Distance (X, Y) < Half,
     Post => Distance (X, A) <= Distance (X, Y);
   procedure Lemma_Between (X, A, Y : Seq) is null;

   procedure Open_Active (E : out Endpoint; P : Chunks.Pool; Me : Chunks.Owner_Id; ISS : Seq) is
   begin
      E.C := (others => <>);
      TCP_Connection.Open_Active (E.C, ISS, Seq (Receives.Capacity));
      Sends.Initialize (E.S, P, Me, ISS + 1);
      Receives.Initialize (E.R, 0);
      E.Rtx := ISS + 1;
   end Open_Active;

   procedure Open_Passive (E : out Endpoint; P : Chunks.Pool; Me : Chunks.Owner_Id) is
   begin
      E.C := (others => <>);
      TCP_Connection.Open_Passive (E.C, Seq (Receives.Capacity));
      Sends.Initialize (E.S, P, Me, 0);
      Receives.Initialize (E.R, 0);
      E.Rtx := 0;
   end Open_Passive;

   --  RCV.NXT and the window follow the receive queue.
   procedure Sync_Receive (C : in out Connection; R : Receives.Queue) with
     Pre  => C.St not in Closed | Listen | Syn_Sent,
     Post => C.Rcv_Wnd = Seq (Receives.Window (R)) and then
             C.Rcv_Nxt = Receives.Rcv_Nxt (R) + (if C.St in Peer_Closed_State then 1 else 0) and then
             C.St = C.St'Old and then
             C.Snd_Una = C.Snd_Una'Old and then C.Snd_Nxt = C.Snd_Nxt'Old and then
             C.Fin_Sent = C.Fin_Sent'Old and then C.Fin_Pending = C.Fin_Pending'Old and then
             C.Snd_Wnd = C.Snd_Wnd'Old and then C.ISS = C.ISS'Old and then
             C.IRS = C.IRS'Old and then C.Passive = C.Passive'Old
   is
   begin
      C.Rcv_Wnd := Seq (Receives.Window (R));
      C.Rcv_Nxt := Receives.Rcv_Nxt (R) + (if C.St in Peer_Closed_State then 1 else 0);
   end Sync_Receive;

   procedure Read (E : in out Endpoint; P : Chunks.Pool; Output : out Receives.Byte_Array;
                   Got : out Receives.Byte_Count)
   is
   begin
      Receives.Read (E.R, Output, Got);
      Sync_Receive (E.C, E.R);
   end Read;

   procedure Write (E : in out Endpoint; P : in out Chunks.Pool; Data : Sends.Byte_Array;
                    Accepted : out Sends.Byte_Count)
   is
   begin
      Sends.Push (E.S, P, Data, Accepted);
   end Write;

   procedure Close (E : in out Endpoint; P : Chunks.Pool) is
   begin
      TCP_Connection.Close (E.C);
   end Close;

   procedure Next_Segment (E : in out Endpoint; P : Chunks.Pool; Limit : Natural;
                           Data : out Sends.Byte_Array; First : out Seq;
                           Taken : out Sends.Byte_Count; FIN : out Boolean)
   is
      Queue_First : Seq;
   begin
      FIN := False;
      First := E.Rtx;
      if E.Rtx /= E.C.Snd_Nxt then
         --  Resending: data from the retransmission point, then the FIN.
         if E.C.Fin_Sent and then E.Rtx = E.C.Snd_Nxt - 1 then
            Data := [others => 0];
            Taken := 0;
            FIN := True;
            E.Rtx := E.C.Snd_Nxt;
         else
            pragma Assert (Sends.Una (E.S) = E.C.Snd_Una);
            Lemma_Between (E.C.Snd_Una, E.Rtx, Sends.Nxt (E.S));
            pragma Assert (Distance (Sends.Una (E.S), E.Rtx) <= Seq (Sends.Sent (E.S)));
            Sends.Peek (E.S, P, Natural (Distance (Sends.Una (E.S), E.Rtx)), Limit, Data, Taken);
            pragma Assert (E.Rtx + Seq (Taken) = Sends.Una (E.S) +
                             Seq (Natural (Distance (Sends.Una (E.S), E.Rtx)) + Taken));
            E.Rtx := E.Rtx + Seq (Taken);
            pragma Assert (Le (E.Rtx, Sends.Nxt (E.S)));
         end if;
         return;
      end if;
      if E.C.Fin_Sent then
         --  Everything, FIN included, has been sent.
         Data := [others => 0];
         Taken := 0;
         return;
      end if;
      declare
         Sent_Old : constant Sends.Byte_Count := Sends.Sent (E.S);
      begin
         Sends.Take (E.S, P, Limit, Data, Queue_First, Taken);
         pragma Assert (Queue_First = First);
         pragma Assert (Sends.Sent (E.S) = Sent_Old + Taken);
         Lemma_Seq_Add (Sent_Old, Taken);
         pragma Assert (Seq (Sends.Sent (E.S)) = Seq (Sent_Old) + Seq (Taken));
         TCP_Connection.Data_Sent (E.C, Seq (Taken));
         pragma Assert (E.C.Snd_Nxt = Sends.Una (E.S) + Seq (Sent_Old) + Seq (Taken));
      end;
      pragma Assert (Sends.Una (E.S) = E.C.Snd_Una and then Sends.Nxt (E.S) = E.C.Snd_Nxt);
      if E.C.Fin_Pending and then Sends.Sent (E.S) = Sends.Count (E.S) then
         TCP_Connection.Fin_Sent_Now (E.C);
         FIN := True;
         --  The FIN follows the data: SND.NXT is past SND.UNA by the queued
         --  bytes and one.
         pragma Assert (E.C.Snd_Nxt = Sends.Nxt (E.S) + 1);
         pragma Assert (Sends.Nxt (E.S) = Sends.Una (E.S) + Seq (Sends.Count (E.S)));
         pragma Assert (E.C.Snd_Nxt - E.C.Snd_Una = Seq (Sends.Count (E.S)) + 1);
         pragma Assert (E.C.Snd_Una /= E.C.Snd_Nxt);
      end if;
      E.Rtx := E.C.Snd_Nxt;
   end Next_Segment;


   --  After the state machine accepted an acknowledgement, the queue
   --  catches up: it lags the state machine by what was just acknowledged.
   procedure Sync_Ack (C : Connection; S : in out Sends.Queue; P : in out Chunks.Pool) with
     Pre  => TCP_Connection.Valid (C) and then Sends.Valid (S, P) and then
             C.St in Established .. Time_Wait and then
             (if C.Fin_Sent then C.Fin_Pending) and then
             Le (Sends.Una (S), C.Snd_Una) and then Le (C.Snd_Una, C.Snd_Nxt) and then
             (if C.Fin_Sent then
                Sends.Sent (S) = Sends.Count (S) and then C.Snd_Nxt = Sends.Nxt (S) + 1 and then
                Le (Sends.Una (S), Sends.Nxt (S))
              else Sends.Nxt (S) = C.Snd_Nxt),
     Post => Sends.Valid (S, P) and then Send_Consistent (C, S) and then
             Sends.Owner_Of (S) = Sends.Owner_Of (S'Old) and then
             Sends.Isolated (Sends.Owner_Of (S), P'Old, P)
   is
      A     : Seq;
      R     : Sends.Ack_Result;
      Freed : Natural;
   begin
      --  An acknowledged FIN is not in the queue.
      A := (if C.Fin_Sent and then C.Snd_Una = C.Snd_Nxt then C.Snd_Una - 1
            else C.Snd_Una);
      pragma Assert (Le (Sends.Una (S), A) and then Le (A, Sends.Nxt (S)));
      pragma Assert (Distance (Sends.Una (S), Sends.Nxt (S)) = Seq (Sends.Sent (S)));
      Lemma_Between (Sends.Una (S), A, Sends.Nxt (S));
      pragma Assert (Distance (Sends.Una (S), A) <= Seq (Sends.Sent (S)));
      Sends.Acknowledge (S, P, A, R, Freed);
      pragma Assert (Sends.Una (S) = A);
   end Sync_Ack;

   procedure Segment_Arrived (E : in out Endpoint; P : in out Chunks.Pool; S : Segment;
                              Payload : Receives.Byte_Array; New_ISS : Seq; O : out Outcome)
   is
      Old_St  : constant State := E.C.St;
      Old_Wnd : constant Seq := E.C.Rcv_Wnd;
      Old_Una : constant Seq := E.C.Snd_Una;
      Old_Nxt : constant Seq := E.C.Snd_Nxt;
      Me      : constant Chunks.Owner_Id := Sends.Owner_Of (E.S);
      Q_Una   : constant Seq := Sends.Una (E.S);
      Old_P   : constant Chunks.Pool := P with Ghost;
   begin
      Chunks.Lemma_Equal (Old_P, P);
      pragma Assert (if Old_St not in Closed | Listen | Syn_Sent then Old_Wnd = Seq (Receives.Window (E.R)));
      --  Where the queue stands relative to the state machine.
      pragma Assert
        (if Old_St in Syn_Sent | Syn_Received then Q_Una = Old_Una + 1 and then Old_Nxt = Old_Una + 1);
      pragma Assert
        (if Old_St in Established .. Time_Wait then
           (if E.C.Fin_Sent and then Old_Una = Old_Nxt then Q_Una + 1 = Old_Una else Q_Una = Old_Una));
      TCP_Connection.Arrive (E.C, S, New_ISS, O);
      pragma Assert (Sends.Isolated (Me, Old_P, P));
      if E.C.St = Closed and then Old_St /= Closed then
         --  Reset: the connection's chunks go back to the pool.
         Sends.Release_All (E.S, P);
         pragma Assert (Sends.Isolated (Me, Old_P, P));
         pragma Assert (Send_Consistent (E.C, E.S));
      elsif Old_St = Listen and then E.C.St = Syn_Received then
         --  A passive open: data will start after our SYN.
         Sends.Release_All (E.S, P);
         pragma Assert (Sends.Isolated (Me, Old_P, P));
         Sends.Initialize (E.S, P, Me, E.C.Snd_Una + 1);
         pragma Assert (Send_Consistent (E.C, E.S));
      elsif Old_St = Syn_Received and then E.C.St = Listen then
         Sends.Release_All (E.S, P);
         pragma Assert (Sends.Isolated (Me, Old_P, P));
         pragma Assert (Send_Consistent (E.C, E.S));
      elsif Old_St = Syn_Sent and then E.C.St = Established then
         --  Our SYN is acknowledged: data starts where the queue does.
         pragma Assert (E.C.Snd_Una = Old_Nxt and then Q_Una = E.C.Snd_Una);
         pragma Assert (Sends.Nxt (E.S) = E.C.Snd_Nxt);
         pragma Assert (Send_Consistent (E.C, E.S));
      elsif Old_St in Synchronized_State and then E.C.St in Established .. Time_Wait then
         --  The queue frees what the state machine accepted.
         pragma Assert (if Old_St = Syn_Received then E.C.Snd_Una = Old_Nxt and then Q_Una = E.C.Snd_Una);
         pragma Assert (E.C.Snd_Nxt = Old_Nxt and then Le (Old_Una, E.C.Snd_Una));
         pragma Assert (Le (Q_Una, E.C.Snd_Una));
         Sync_Ack (E.C, E.S, P);
         pragma Assert (Sends.Isolated (Me, Old_P, P));
         pragma Assert (Send_Consistent (E.C, E.S));
      else
         pragma Assert (Send_Consistent (E.C, E.S));
      end if;
      pragma Assert (Sends.Isolated (Me, Old_P, P));
      --  The retransmission point stays between SND.UNA and SND.NXT.
      if E.C.St in Established .. Time_Wait and then
        not (Le (E.C.Snd_Una, E.Rtx) and then Le (E.Rtx, E.C.Snd_Nxt))
      then
         E.Rtx := E.C.Snd_Una;
      end if;
      pragma Assert (Send_Consistent (E.C, E.S));
      pragma Assert (Consistent (E));

      --  The receive side, which leaves the send side as it is.
      declare
         St1  : constant State := E.C.St;
         Una1 : constant Seq := E.C.Snd_Una;
         Nxt1 : constant Seq := E.C.Snd_Nxt;
         Rtx1 : constant Seq := E.Rtx;
         FS1  : constant Boolean := E.C.Fin_Sent;
         FP1  : constant Boolean := E.C.Fin_Pending;
         Q_Una1  : constant Seq := Sends.Una (E.S);
         Q_Sent1 : constant Natural := Sends.Sent (E.S);
         Q_Count1 : constant Natural := Sends.Count (E.S);
      begin
      if E.C.St in Closed | Listen | Syn_Sent then
         null;
      elsif Old_St in Listen | Syn_Sent then
         --  The peer's SYN is in: its data starts at RCV.NXT.
         Receives.Initialize (E.R, E.C.Rcv_Nxt);
         Sync_Receive (E.C, E.R);
      else
         if O.Count > 0 then
            --  In-order text, at the queue's edge: it fits the window.
            pragma Assert (O.Count <= Old_Wnd and then Old_Wnd = Seq (Receives.Window (E.R)));
            pragma Assert (Natural (O.Count) <= Receives.Window (E.R));
            declare
               N    : constant Natural := Natural (O.Count);
               From : constant Natural := Natural (O.Skip);
            begin
               declare
                  Text : constant Receives.Byte_Array (1 .. N) := Payload (From + 1 .. From + N);
               begin
                  Receives.Insert (E.R, Receives.Ready (E.R), Text);
               end;
            end;
         elsif S.Length > 0 and then E.C.St in Data_State and then
           Acceptable (S.Seq_No, S.Length, Receives.Rcv_Nxt (E.R), Seq (Receives.Window (E.R)))
         then
            --  Out-of-order text, kept at its offset.
            declare
               First : Seq;
               Skip, Count : Segment_Length;
            begin
               Trim (S.Seq_No, S.Length, Receives.Rcv_Nxt (E.R), Seq (Receives.Window (E.R)),
                     First, Skip, Count);
               declare
                  N    : constant Natural := Natural (Count);
                  From : constant Natural := Natural (Skip);
               begin
                  declare
                     Text : constant Receives.Byte_Array (1 .. N) := Payload (From + 1 .. From + N);
                  begin
                     Receives.Insert
                       (E.R, Receives.Ready (E.R) + Natural (Distance (Receives.Rcv_Nxt (E.R), First)),
                        Text);
                  end;
               end;
            end;
         end if;
         Sync_Receive (E.C, E.R);
      end if;
      pragma Assert (E.C.St = St1 and then E.C.Snd_Una = Una1 and then E.C.Snd_Nxt = Nxt1 and then
                     E.Rtx = Rtx1 and then E.C.Fin_Sent = FS1 and then E.C.Fin_Pending = FP1);
      pragma Assert (Sends.Una (E.S) = Q_Una1 and then Sends.Sent (E.S) = Q_Sent1 and then
                     Sends.Count (E.S) = Q_Count1);
      pragma Assert (Send_Consistent (E.C, E.S));
      end;
      pragma Assert (Consistent (E));
      pragma Assert (Recv_Consistent (E));
      pragma Assert (Valid (E, P));
      --  Only what the postcondition needs, so that relating the ghost copy
      --  to P'Old is not lost in this long body's context (level 1).
      pragma Assert_And_Cut
        (Valid (E, P) and then Sends.Owner_Of (E.S) = Me and then
         Sends.Isolated (Me, Old_P, P) and then Allowed (Old_St, E.C.St) and then
         (if E.C.St = Closed and then Old_St /= Closed then Sends.Chunks_Held (E.S) = 0));
   end Segment_Arrived;

   procedure Timeout (E : in out Endpoint; P : Chunks.Pool) is
   begin
      if E.C.St in Established .. Time_Wait then
         E.Rtx := E.C.Snd_Una;
      end if;
   end Timeout;
end TCP_Endpoint;
