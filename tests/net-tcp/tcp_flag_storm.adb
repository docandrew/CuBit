--  Linux-hosted regression: flag combinations no legitimate stack sends
--  ("Christmas tree" segments: SYN, FIN, RST, ACK together, with or without
--  data) at a connection in every RFC 9293 state. Only SYN, ACK, FIN and
--  RST reach the state machine (TCP_Header parses URG, PSH, ECE, CWR and NS;
--  none of them changes state), so all 16 combinations of those four are
--  sent, with in-window, exact, stale and far sequence numbers and
--  acknowledgements of old, current and unsent data. -gnata also checks
--  every proved postcondition on each call.
with Ada.Text_IO;    use Ada.Text_IO;
with TCP_Sequence;   use TCP_Sequence;
with TCP_Connection; use TCP_Connection;

procedure TCP_Flag_Storm is
   Failures, Segments, Resets, Challenges, Moved : Natural := 0;

   procedure Check (OK : Boolean; Name : String) is
   begin
      if not OK then
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   function Seg (Seq_No, Ack_No : Seq; SYN, ACK, FIN, RST : Boolean := False;
                 Length : Seq := 0) return Segment
   is ((Seq_No => Seq_No, Ack_No => Ack_No, SYN => SYN, ACK => ACK, FIN => FIN,
        RST => RST, Length => Length, Window => 1000));

   O : Outcome;

   --  A (client, ISS 100) and B (server, ISS 5000) handshaken.
   procedure Handshake (A, B : out Connection) is
   begin
      A := (others => <>);
      B := (others => <>);
      Open_Active (A, 100, 1000);
      Open_Passive (B, 1000);
      Arrive (B, Seg (100, 0, SYN => True), 5000, O);
      Arrive (A, Seg (5000, 101, SYN => True, ACK => True), 0, O);
      Arrive (B, Seg (101, 5001, ACK => True), 0, O);
   end Handshake;

   type Connections is array (State) of Connection;
   Samples : Connections;

   procedure Build is
      A, B, C : Connection;
   begin
      Samples (Closed) := (others => <>);
      C := (others => <>); Open_Passive (C, 1000); Samples (Listen) := C;
      C := (others => <>); Open_Active (C, 100, 1000); Samples (Syn_Sent) := C;
      C := (others => <>); Open_Passive (C, 1000);
      Arrive (C, Seg (100, 0, SYN => True), 5000, O); Samples (Syn_Received) := C;
      Handshake (A, B); Samples (Established) := A;
      --  A closes first: FIN-WAIT-1, then FIN-WAIT-2, CLOSING, TIME-WAIT.
      Handshake (A, B);
      Close (A); Fin_Sent_Now (A); Samples (Fin_Wait_1) := A;
      C := A; Arrive (C, Seg (5001, 102, ACK => True), 0, O); Samples (Fin_Wait_2) := C;
      C := A; Arrive (C, Seg (5001, 101, ACK => True, FIN => True), 0, O); Samples (Closing) := C;
      C := Samples (Fin_Wait_2);
      Arrive (C, Seg (5001, 102, ACK => True, FIN => True), 0, O); Samples (Time_Wait) := C;
      --  B closes second: CLOSE-WAIT, then LAST-ACK.
      Handshake (A, B);
      Arrive (B, Seg (101, 5001, ACK => True, FIN => True), 0, O); Samples (Close_Wait) := B;
      C := B; Close (C); Fin_Sent_Now (C); Samples (Last_Ack) := C;
      for S in State loop
         Check (Samples (S).St = S, "reached " & S'Image);
      end loop;
   end Build;
begin
   Build;
   for S in State loop
      declare
         Base : constant Connection := Samples (S);
         Seqs : constant array (1 .. 6) of Seq :=
           [Base.Rcv_Nxt, Base.Rcv_Nxt + 1, Base.Rcv_Nxt - 1, Base.Rcv_Nxt + 500,
            Base.Rcv_Nxt + 70_000, 16#DEAD_BEEF#];
         Acks : constant array (1 .. 5) of Seq :=
           [Base.Snd_Una, Base.Snd_Nxt, Base.Snd_Nxt + 1, Base.Snd_Una - 1, 16#0BAD_F00D#];
      begin
         for Flags in 0 .. 15 loop
            for Q of Seqs loop
               for K of Acks loop
                  for Len in 0 .. 1 loop
                     declare
                        C   : Connection := Base;
                        SYN : constant Boolean := Flags mod 2 = 1;
                        ACK : constant Boolean := (Flags / 2) mod 2 = 1;
                        FIN : constant Boolean := (Flags / 4) mod 2 = 1;
                        RST : constant Boolean := (Flags / 8) mod 2 = 1;
                        X   : constant Segment :=
                          Seg (Q, K, SYN, ACK, FIN, RST, Length => Seq (Len * 10));
                     begin
                        Arrive (C, X, 9000, O);
                        Segments := Segments + 1;
                        Check (Allowed (S, C.St), "allowed transition from " & S'Image);
                        Check (Valid (C), "valid after " & S'Image);
                        if C.St /= S then
                           Moved := Moved + 1;
                        end if;
                        if O.Answer = Send_Challenge_Ack then
                           Challenges := Challenges + 1;
                        end if;
                        --  RFC 5961 3.2: a RST ends a synchronized connection
                        --  only exactly at RCV.NXT.
                        if S in Synchronized_State and then RST and then C.St /= S then
                           Check (Q = Base.Rcv_Nxt, "blind reset in " & S'Image);
                           Resets := Resets + 1;
                        end if;
                        --  RFC 5961 4.2: a SYN (without an exact RST) never
                        --  moves a synchronized connection.
                        if S in Synchronized_State and then SYN and then not RST then
                           Check (C.St = S, "SYN moved " & S'Image);
                        end if;
                        --  The full tree (SYN, ACK, FIN, RST) off RCV.NXT
                        --  changes nothing and delivers nothing.
                        if S in Synchronized_State and then SYN and then ACK and then FIN and then
                          RST and then Q /= Base.Rcv_Nxt
                        then
                           Check (C = Base and then O.Count = 0, "tree changed " & S'Image);
                        end if;
                        --  A listener never opens on a segment with RST or
                        --  ACK, and never delivers data.
                        if S = Listen then
                           Check (O.Count = 0, "listener delivered data");
                           Check (C.St = Listen or else (SYN and then not RST and then not ACK),
                                  "listener opened on " & Flags'Image);
                        end if;
                        --  A closed connection stays closed.
                        if S = Closed then
                           Check (C.St = Closed and then O.Count = 0, "closed connection moved");
                        end if;
                     end;
                  end loop;
               end loop;
            end loop;
         end loop;
      end;
   end loop;
   Put_Line ("segments:" & Segments'Image & ", state changes:" & Moved'Image &
             ", resets honoured (exact RCV.NXT only):" & Resets'Image &
             ", challenge ACKs:" & Challenges'Image);
   Put_Line (if Failures = 0 then "TCP-FLAG-STORM: PASS" else "TCP-FLAG-STORM: FAIL");
end TCP_Flag_Storm;
