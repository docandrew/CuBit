------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A TCP connection's send queue: bytes the application has written and
--  the peer has not acknowledged (RFC 9293 3.3.1, SND.UNA .. SND.NXT ..).
--
--  Offsets count from SND.UNA: the first Sent bytes are in flight
--  (SND.UNA .. SND.NXT), the rest are queued but unsent. The contracts are
--  functional (proved, tests/net-tcp): a segment's bytes are exactly the
--  bytes written at those sequence numbers; an acknowledgement frees only
--  acknowledged bytes and never moves the rest; acknowledgements of data
--  never sent are refused; retransmission after Rewind resends identical
--  bytes. SYN and FIN are not stored here (they carry no data); the
--  connection accounts for their sequence numbers.
------------------------------------------------------------------------------
with Interfaces;   use Interfaces;
with TCP_Sequence; use TCP_Sequence;

with TCP_Limits;

generic
   Capacity : Positive;
package TCP_Send_Queue with SPARK_Mode is
   pragma Compile_Time_Error (Capacity > TCP_Limits.Maximum_Scaled_Window,
      "a send queue must stay below a quarter of the sequence space");

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   --  Bytes queued, and a byte's offset from SND.UNA.
   subtype Byte_Count  is Natural range 0 .. Capacity;
   subtype Byte_Offset is Byte_Count range 0 .. Capacity - 1;

   type Queue is private;

   function Una (Q : Queue) return Seq;              --  SND.UNA
   function Count (Q : Queue) return Byte_Count;     --  queued bytes
   function Sent (Q : Queue) return Byte_Count       --  bytes in flight
     with Post => Sent'Result <= Count (Q);
   function Nxt (Q : Queue) return Seq is            --  SND.NXT
     (Una (Q) + Seq (Sent (Q)));
   function Free (Q : Queue) return Byte_Count is (Capacity - Count (Q));

   --  The byte at offset I from SND.UNA.
   function Element (Q : Queue; I : Byte_Offset) return Unsigned_8
     with Pre => I < Count (Q);

   --  An empty queue whose first data byte will be Start (ISS + 1).
   procedure Initialize (Q : out Queue; Start : Seq) with
     Post => Una (Q) = Start and then Count (Q) = 0 and then Sent (Q) = 0;

   --  Queue as much of Data as fits.
   procedure Push (Q : in out Queue; Data : Byte_Array; Accepted : out Byte_Count) with
     Post => Accepted = Natural'Min (Data'Length, Free (Q'Old)) and then
             Count (Q) = Count (Q'Old) + Accepted and then
             Una (Q) = Una (Q'Old) and then Sent (Q) = Sent (Q'Old) and then
             (for all I in 0 .. Count (Q'Old) - 1 =>
                Element (Q, I) = Element (Q'Old, I)) and then
             (for all I in 0 .. Accepted - 1 =>
                Element (Q, Count (Q'Old) + I) = Data (Data'First + I));

   --  The next segment: up to Limit unsent bytes, starting at SND.NXT.
   --  Data (Data'First .. Data'First + Taken - 1) receives them.
   procedure Take (Q : in out Queue; Limit : Natural; Data : out Byte_Array;
                   First : out Seq; Taken : out Byte_Count) with
     Pre  => Data'Length >= Limit and then Data'First = 1,
     Post => Taken = Natural'Min (Limit, Count (Q'Old) - Sent (Q'Old)) and then
             First = Nxt (Q'Old) and then
             Sent (Q) = Sent (Q'Old) + Taken and then
             Una (Q) = Una (Q'Old) and then Count (Q) = Count (Q'Old) and then
             (for all I in 0 .. Count (Q) - 1 => Element (Q, I) = Element (Q'Old, I)) and then
             (for all K in 1 .. Taken =>
                Data (K) = Element (Q'Old, Sent (Q'Old) + K - 1));

   type Ack_Result is (Advanced, Duplicate, Old, Unsent_Data);

   --  An arriving acknowledgement (SEG.ACK). Within SND.UNA .. SND.NXT it
   --  frees the acknowledged bytes (Advanced, or Duplicate if it equals
   --  SND.UNA); before SND.UNA it is Old; beyond SND.NXT it acknowledges
   --  data never sent and is refused (RFC 9293 3.10.7.4: send an ACK, drop).
   procedure Acknowledge (Q : in out Queue; Ack : Seq; Result : out Ack_Result;
                          Freed : out Byte_Count) with
     Post =>
       (case Result is
          when Advanced | Duplicate =>
            Freed = Natural (Distance (Una (Q'Old), Ack)) and then
            Freed <= Sent (Q'Old) and then
            (Result = Duplicate) = (Freed = 0) and then
            Una (Q) = Ack and then
            Count (Q) = Count (Q'Old) - Freed and then
            Sent (Q) = Sent (Q'Old) - Freed and then
            (for all I in 0 .. Count (Q) - 1 =>
               Element (Q, I) = Element (Q'Old, I + Freed)),
          when Old | Unsent_Data =>
            Freed = 0 and then Q = Q'Old and then
            (Result = Unsent_Data) = Lt (Nxt (Q'Old), Ack));

   --  Retransmission (RFC 6298 5.4): resend from SND.UNA.
   procedure Rewind (Q : in out Queue) with
     Post => Sent (Q) = 0 and then Una (Q) = Una (Q'Old) and then
             Count (Q) = Count (Q'Old) and then
             (for all I in 0 .. Count (Q) - 1 => Element (Q, I) = Element (Q'Old, I));

private
   subtype Index is Byte_Offset;
   type Storage is array (Index) of Unsigned_8;

   type Queue is record
      Data   : Storage := [others => 0];
      Head   : Index := 0;                      --  storage index of SND.UNA
      Length    : Byte_Count := 0;
      In_Flight : Byte_Count := 0;
      Start  : Seq := 0;                        --  SND.UNA
   end record
     with Type_Invariant => In_Flight <= Length;

   function Una (Q : Queue) return Seq is (Q.Start);
   function Count (Q : Queue) return Byte_Count is (Q.Length);
   function Sent (Q : Queue) return Byte_Count is (Q.In_Flight);
   function Element (Q : Queue; I : Byte_Offset) return Unsigned_8 is
     (Q.Data ((Q.Head + I) mod Capacity));
end TCP_Send_Queue;
