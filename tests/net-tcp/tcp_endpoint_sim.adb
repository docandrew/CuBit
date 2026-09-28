--  Linux-hosted end-to-end check of TCP_Endpoint: two endpoints connected
--  by a simulated link that drops segments. The proofs cover each step;
--  this checks that the steps compose into a reliable byte stream.
with Ada.Text_IO;    use Ada.Text_IO;
with Interfaces;     use Interfaces;
with TCP_Sequence;   use TCP_Sequence;
with TCP_Connection; use TCP_Connection;
with Pool_Small;
with Send_Chunked_Small;
with Endpoint_Small;
with Receive_Queue_64;

procedure TCP_Endpoint_Sim is
   package EP renames Endpoint_Small;
   package RQ renames Receive_Queue_64;
   package SQ renames Send_Chunked_Small;
   use type Pool_Small.Byte_Array;

   Failures : Natural := 0;
   procedure Check (OK : Boolean; Name : String) is
   begin
      if not OK then
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   Window    : constant := 1000;
   Message   : constant := 200;           --  bytes A sends B
   Segment_Max : constant := 10;         --  bytes per segment
   --  About a quarter of data segments are lost, pseudo-randomly (a fixed
   --  seed, so runs repeat).
   Loss_Percent : constant := 25;
   Rand : Unsigned_32 := 12345;
   function Lost return Boolean is
   begin
      Rand := Rand * 1_103_515_245 + 12_345;
      return Shift_Right (Rand, 16) mod 100 < Loss_Percent;
   end Lost;

   P    : Pool_Small.Pool;
   A, B : EP.Endpoint;
   O    : Outcome;

   Sent_Data : Pool_Small.Byte_Array (1 .. Message) :=
     [for I in 1 .. Message => Unsigned_8 (I mod 251)];
   Written   : Natural := 0;              --  bytes A's application wrote
   Received  : Pool_Small.Byte_Array (1 .. Message) := [others => 0];
   Got       : Natural := 0;              --  bytes B delivered in order
   Data_Segments : Natural := 0;
   Timeouts  : Natural := 0;

   function Ack_From (E : EP.Endpoint) return Segment is
     ((Seq_No => E.C.Snd_Nxt, Ack_No => E.C.Rcv_Nxt, ACK => True, Window => E.C.Rcv_Wnd,
       others => <>));
   None : constant RQ.Byte_Array (1 .. 0) := [others => 0];
   Out_Buf : RQ.Byte_Array (1 .. 64);
   Read_Got : RQ.Byte_Count;

   --  B takes a data segment and keeps whatever it delivers.
   procedure Deliver_To_B (S : Segment; Payload : Pool_Small.Byte_Array) is
      Text : constant RQ.Byte_Array (1 .. Payload'Length) :=
        [for K in 1 .. Payload'Length => Payload (Payload'First + K - 1)];
   begin
      EP.Segment_Arrived (B, P, S, Text, 0, O);
      --  B's application reads whatever is in order.
      EP.Read (B, P, Out_Buf, Read_Got);
      for K in 1 .. Read_Got loop
         if Got < Message then
            Got := Got + 1;
            Received (Got) := Out_Buf (K);
         end if;
      end loop;
   end Deliver_To_B;

   Buffer : Pool_Small.Byte_Array (1 .. Segment_Max);
   First  : Seq;
   Taken  : SQ.Byte_Count;
   FIN    : Boolean;
   Accepted : SQ.Byte_Count;
   Idle   : Natural := 0;
   Before : Seq;
begin
   Pool_Small.Initialize (P);
   EP.Open_Passive (B, P, 2);
   EP.Open_Active (A, P, 1, 100);

   --  Handshake.
   EP.Segment_Arrived (B, P, (Seq_No => 100, SYN => True, Window => Window, others => <>), None, 5000, O);
   Check (B.C.St = Syn_Received and then O.Answer = Send_Syn_Ack, "B: SYN -> SYN-RECEIVED");
   EP.Segment_Arrived
     (A, P, (Seq_No => 5000, Ack_No => 101, SYN => True, ACK => True, Window => Window, others => <>),
      None, 0, O);
   Check (A.C.St = Established, "A: SYN-ACK -> ESTABLISHED");
   EP.Segment_Arrived (B, P, Ack_From (A), None, 0, O);
   Check (B.C.St = Established, "B: ACK -> ESTABLISHED");

   --  Data over a lossy link; A retransmits on timeout.
   for Tick in 1 .. 2000 loop
      exit when Got = Message and then SQ.Count (A.S) = 0;
      if Written < Message then
         EP.Write (A, P, Sent_Data (Written + 1 .. Natural'Min (Written + 16, Message)), Accepted);
         Written := Written + Accepted;
      end if;
      Before := A.C.Snd_Una;
      EP.Next_Segment (A, P, Segment_Max, Buffer, First, Taken, FIN);
      if Taken > 0 then
         Data_Segments := Data_Segments + 1;
         if not Lost then
            Deliver_To_B ((Seq_No => First, Ack_No => A.C.Rcv_Nxt, ACK => True,
                           Length => Seq (Taken), Window => Window, others => <>),
                          Buffer (1 .. Taken));
            EP.Segment_Arrived (A, P, Ack_From (B), None, 0, O);
         end if;
      end if;
      if A.C.Snd_Una = Before and then SQ.Sent (A.S) > 0 then
         Idle := Idle + 1;
         if Idle >= 3 then
            EP.Timeout (A, P);
            Timeouts := Timeouts + 1;
            Idle := 0;
         end if;
      else
         Idle := 0;
      end if;
   end loop;
   Check (Got = Message and then Received = Sent_Data, "B received every byte, in order");
   Check (Timeouts > 0, "losses were recovered by retransmission");
   Check (SQ.Count (A.S) = 0, "A's queue is empty once everything is acknowledged");

   --  Close: A first, then B.
   EP.Close (A, P);
   EP.Next_Segment (A, P, Segment_Max, Buffer, First, Taken, FIN);
   Check (FIN and then Taken = 0 and then A.C.St = Fin_Wait_1, "A sends its FIN");
   EP.Segment_Arrived (B, P, (Seq_No => First, Ack_No => A.C.Rcv_Nxt, ACK => True, FIN => True,
                              Window => Window, others => <>), None, 0, O);
   Check (B.C.St = Close_Wait, "B: FIN -> CLOSE-WAIT");
   EP.Segment_Arrived (A, P, Ack_From (B), None, 0, O);
   Check (A.C.St = Fin_Wait_2, "A: FIN acknowledged -> FIN-WAIT-2");
   EP.Close (B, P);
   EP.Next_Segment (B, P, Segment_Max, Buffer, First, Taken, FIN);
   Check (FIN and then B.C.St = Last_Ack, "B sends its FIN");
   EP.Segment_Arrived (A, P, (Seq_No => First, Ack_No => B.C.Rcv_Nxt, ACK => True, FIN => True,
                              Window => Window, others => <>), None, 0, O);
   Check (A.C.St = Time_Wait, "A: FIN -> TIME-WAIT");
   EP.Segment_Arrived (B, P, Ack_From (A), None, 0, O);
   Check (B.C.St = Closed, "B: last ACK -> CLOSED");
   Check (Pool_Small.Held (P, 2) = 0, "B's closed connection returned its chunks");

   Put_Line ("segments:" & Data_Segments'Image & ", timeouts:" & Timeouts'Image);
   Put_Line (if Failures = 0 then "TCP-ENDPOINT-SIM: PASS" else "TCP-ENDPOINT-SIM: FAIL");
end TCP_Endpoint_Sim;
