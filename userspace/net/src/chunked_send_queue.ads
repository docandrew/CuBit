------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A TCP send queue whose bytes live in chunks from a shared pool
--  (docs/netstack-redesign.md, "The connection engine"). An idle queue
--  holds no chunks; chunks are allocated as the application writes and
--  released as the peer acknowledges.
--
--  The contract is TCP_Send_Queue's, with the pool as a parameter: a
--  segment's bytes are exactly those written at its sequence numbers, and
--  so on. In addition (proved, tests/net-tcp):
--  - isolation: nothing outside this queue's own chunks changes in the
--    pool: no other owner's chunks, bytes or counts;
--  - every chunk the queue holds is owned by it in the pool, once.
------------------------------------------------------------------------------
with Interfaces;   use Interfaces;
with TCP_Sequence; use TCP_Sequence;
with Chunk_Pool;

generic
   with package Chunks is new Chunk_Pool (<>);
   Max_Chunks : Positive;   --  chunks one queue may hold
package Chunked_Send_Queue with SPARK_Mode is

   Chunk_Bytes : constant Positive := Chunks.Chunk_Size;
   --  One chunk is kept in reserve for the offset of SND.UNA in the first.
   Capacity    : constant Natural := (Max_Chunks - 1) * Chunk_Bytes;

   pragma Compile_Time_Error (Max_Chunks < 2, "a queue needs at least two chunks");
   pragma Compile_Time_Error (Max_Chunks * Chunks.Chunk_Size > 2 ** 30,
                              "a send queue stays below 2**30 bytes");

   --  Bytes queued; an offset within one chunk; chunks held and a place
   --  in the chunk list; a byte's position from the first chunk's start.
   subtype Byte_Count   is Natural range 0 .. Capacity;
   subtype Chunk_Offset is Natural range 0 .. Chunk_Bytes - 1;
   subtype Held_Count   is Natural range 0 .. Max_Chunks;
   subtype Chunk_Slot   is Held_Count range 0 .. Max_Chunks - 1;
   subtype Position     is Natural range 0 .. Max_Chunks * Chunk_Bytes;

   subtype Byte_Array is Chunks.Byte_Array;
   use type Chunks.Maybe_Owner;
   use type Chunks.Block;

   type Queue is private;

   function Owner_Of (Q : Queue) return Chunks.Owner_Id;
   function Valid (Q : Queue; P : Chunks.Pool) return Boolean with Ghost;

   function Una (Q : Queue) return Seq;
   function Count (Q : Queue) return Byte_Count;
   function Sent (Q : Queue) return Byte_Count with Post => Sent'Result <= Count (Q);
   function Nxt (Q : Queue) return Seq is (Una (Q) + Seq (Sent (Q)));
   function Free (Q : Queue) return Byte_Count is (Capacity - Count (Q));
   function Chunks_Held (Q : Queue) return Held_Count;

   --  The byte at offset I from SND.UNA.
   function Element (Q : Queue; P : Chunks.Pool; I : Byte_Count) return Unsigned_8
     with Ghost, Pre => Valid (Q, P) and then I < Count (Q);

   --  Nothing of any other owner changed.
   function Isolated (Me : Chunks.Owner_Id; A, B : Chunks.Pool) return Boolean is
     ((for all C in Chunks.Chunk_Id =>
         (if Chunks.Owner (A, C) /= Me and then Chunks.Owner (A, C) /= Chunks.Free then
            Chunks.Owner (B, C) = Chunks.Owner (A, C) and then
            Chunks.Contents (B, C) = Chunks.Contents (A, C))) and then
      (for all O in 1 .. Chunks.Owner_Count =>
         (if O /= Me then Chunks.Held (B, O) = Chunks.Held (A, O))))
   with Ghost, Pre => Chunks.Valid (A) and then Chunks.Valid (B);

   procedure Initialize (Q : out Queue; P : Chunks.Pool; Me : Chunks.Owner_Id; Start : Seq) with
     Pre  => Chunks.Valid (P),
     Post => Valid (Q, P) and then Owner_Of (Q) = Me and then
             Una (Q) = Start and then Count (Q) = 0 and then Sent (Q) = 0 and then
             Chunks_Held (Q) = 0;

   --  Queue as much of Data as fits, and as the pool can hold.
   procedure Push (Q : in out Queue; P : in out Chunks.Pool; Data : Byte_Array;
                   Accepted : out Natural)
   with
     Pre  => Valid (Q, P) and then Data'Length <= Capacity and then Data'Last < Positive'Last,
     Post => Valid (Q, P) and then Owner_Of (Q) = Owner_Of (Q'Old) and then
             Isolated (Owner_Of (Q), P'Old, P) and then
             Accepted <= Natural'Min (Data'Length, Free (Q'Old)) and then
             Count (Q) = Count (Q'Old) + Accepted and then
             Una (Q) = Una (Q'Old) and then Sent (Q) = Sent (Q'Old) and then
             (for all I in 0 .. Count (Q'Old) - 1 =>
                Element (Q, P, I) = Element (Q'Old, P'Old, I)) and then
             (for all X in Count (Q'Old) .. Count (Q) - 1 =>
                Element (Q, P, X) = Data (Data'First + (X - Count (Q'Old))));

   --  The next segment: up to Limit unsent bytes, starting at SND.NXT.
   procedure Take (Q : in out Queue; P : Chunks.Pool; Limit : Natural; Data : out Byte_Array;
                   First : out Seq; Taken : out Natural)
   with
     Pre  => Valid (Q, P) and then Data'Length >= Limit and then Data'First = 1,
     Post => Valid (Q, P) and then Owner_Of (Q) = Owner_Of (Q'Old) and then
             Taken = Natural'Min (Limit, Count (Q'Old) - Sent (Q'Old)) and then
             First = Nxt (Q'Old) and then
             Sent (Q) = Sent (Q'Old) + Taken and then
             Una (Q) = Una (Q'Old) and then Count (Q) = Count (Q'Old) and then
             (for all I in 0 .. Count (Q) - 1 => Element (Q, P, I) = Element (Q'Old, P, I)) and then
             (for all K in 1 .. Taken => Data (K) = Element (Q'Old, P, Sent (Q'Old) + K - 1));

   type Ack_Result is (Advanced, Duplicate, Old, Unsent_Data);

   --  An arriving acknowledgement (SEG.ACK), as TCP_Send_Queue's; chunks
   --  it passes entirely go back to the pool.
   procedure Acknowledge (Q : in out Queue; P : in out Chunks.Pool; Ack : Seq;
                          Result : out Ack_Result; Freed : out Natural)
   with
     Pre  => Valid (Q, P),
     Post => Valid (Q, P) and then Owner_Of (Q) = Owner_Of (Q'Old) and then
             Isolated (Owner_Of (Q), P'Old, P) and then
       --  Taken exactly when it acknowledges data in flight.
       (Result in Advanced | Duplicate) = (Distance (Una (Q'Old), Ack) <= Seq (Sent (Q'Old))) and then
       (case Result is
          when Advanced | Duplicate =>
            Freed = Natural (Distance (Una (Q'Old), Ack)) and then
            Freed <= Sent (Q'Old) and then
            (Result = Duplicate) = (Freed = 0) and then
            Una (Q) = Ack and then
            Count (Q) = Count (Q'Old) - Freed and then
            Sent (Q) = Sent (Q'Old) - Freed and then
            (for all X in 0 .. Count (Q) - 1 =>
               Element (Q, P, X) = Element (Q'Old, P'Old, X + Freed)),
          when Old | Unsent_Data =>
            Freed = 0 and then Count (Q) = Count (Q'Old) and then
            Sent (Q) = Sent (Q'Old) and then Una (Q) = Una (Q'Old) and then
            (for all X in 0 .. Count (Q) - 1 => Element (Q, P, X) = Element (Q'Old, P'Old, X)) and then
            (Result = Unsent_Data) = Lt (Nxt (Q'Old), Ack));

   --  The connection is closed: every chunk goes back to the pool.
   procedure Release_All (Q : in out Queue; P : in out Chunks.Pool) with
     Pre  => Valid (Q, P),
     Post => Valid (Q, P) and then Owner_Of (Q) = Owner_Of (Q'Old) and then
             Count (Q) = 0 and then Chunks_Held (Q) = 0 and then
             Isolated (Owner_Of (Q), P'Old, P) and then
             Chunks.Held (P, Owner_Of (Q)) = Chunks.Held (P'Old, Owner_Of (Q)) - Chunks_Held (Q'Old);

   --  Retransmission: up to Limit bytes already sent, from Offset bytes
   --  past SND.UNA. The queue does not change.
   procedure Peek (Q : Queue; P : Chunks.Pool; Offset : Byte_Count; Limit : Natural;
                   Data : out Byte_Array; Taken : out Byte_Count)
   with
     Pre  => Valid (Q, P) and then Offset <= Sent (Q) and then
             Data'Length >= Limit and then Data'First = 1,
     Post => Taken = Natural'Min (Limit, Sent (Q) - Offset) and then
             (for all K in 1 .. Taken => Data (K) = Element (Q, P, Offset + K - 1));

private
   --  The chunks in order: Ids (0) holds SND.UNA.
   type Id_List is array (Chunk_Slot) of Chunks.Chunk_Id;

   type Queue is record
      Me        : Chunks.Owner_Id := 1;
      Ids       : Id_List := [others => 1];
      Held      : Held_Count := 0;
      Head_Off  : Chunk_Offset := 0;   --  SND.UNA within Ids (0)
      Length    : Byte_Count := 0;
      In_Flight : Byte_Count := 0;
      Start     : Seq := 0;
   end record;

   --  The queue owns every chunk it holds.
   function Owns_All (Q : Queue; P : Chunks.Pool) return Boolean is
     (for all K in 0 .. Q.Held - 1 => Chunks.Owner (P, Q.Ids (K)) = Q.Me)
   with Ghost;

   --  It holds each chunk once.
   function Distinct (Q : Queue) return Boolean is
     (for all K1 in 0 .. Q.Held - 1 =>
        (for all K2 in 0 .. Q.Held - 1 =>
           (if K1 /= K2 then Q.Ids (K1) /= Q.Ids (K2))))
   with Ghost;

   function Valid (Q : Queue; P : Chunks.Pool) return Boolean is
     (Chunks.Valid (P) and then
      Q.In_Flight <= Q.Length and then
      Q.Head_Off + Q.Length <= Q.Held * Chunk_Bytes and then
      Owns_All (Q, P) and then Distinct (Q));

   function Owner_Of (Q : Queue) return Chunks.Owner_Id is (Q.Me);
   function Una (Q : Queue) return Seq is (Q.Start);
   function Count (Q : Queue) return Byte_Count is (Q.Length);
   function Chunks_Held (Q : Queue) return Held_Count is (Q.Held);
   function Sent (Q : Queue) return Byte_Count is
     (if Q.In_Flight <= Q.Length then Q.In_Flight else Q.Length);

   function Element (Q : Queue; P : Chunks.Pool; I : Byte_Count) return Unsigned_8 is
     (Chunks.Data (P, Q.Ids ((Q.Head_Off + I) / Chunk_Bytes),
                   (Q.Head_Off + I) mod Chunk_Bytes + 1));
end Chunked_Send_Queue;
