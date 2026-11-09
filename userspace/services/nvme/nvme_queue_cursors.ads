------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Head, tail and phase arithmetic for one NVMe queue pair (NVMe 2.0, 3.3.1
--  "Circular Queues" and 4.6 "Completion Queue Entry").
--
--  A completion queue starts zeroed and the controller writes its first
--  pass of entries with phase tag 1, the next pass with 0, and so on. The
--  host's cursor is therefore a function of how many entries it has
--  consumed (Cursor_After): head = consumed mod depth, phase flips once per
--  pass. The driver consumes every posted entry, whatever it carries (its
--  own command, a late one, a foreign one), so the cursor never falls out
--  of step with the controller. Each consumed entry's head is published
--  through the CQ head doorbell.
--
--  Submission tails advance the same way; a queue is full when the tail is
--  one behind the head the controller last reported.
------------------------------------------------------------------------------
with NVMe_Protocol; use NVMe_Protocol;

package NVMe_Queue_Cursors with SPARK_Mode, Pure is

   --  Entries per queue (admin and I/O); CAP.MQES allows far more, the
   --  driver keeps at most Flight_Slots I/O commands outstanding.
   Queue_Depth : constant := 16;
   subtype Queue_Index is Natural range 0 .. Queue_Depth - 1;

   --  The controller's first pass over a zeroed queue (4.6).
   First_Pass_Phase : constant Phase_Tag := 1;

   type Cursor is record
      Head  : Queue_Index := 0;
      Phase : Phase_Tag := First_Pass_Phase;
   end record;

   Initial : constant Cursor := (Head => 0, Phase => First_Pass_Phase);

   --  The entry at Head holds a new completion.
   function Is_Posted (Position : Cursor; Status : Status_Field) return Boolean
   is (Phase_Of (Status) = Position.Phase);

   --  The cursor after consuming the entry at Head.
   function Advance (Position : Cursor) return Cursor
     with Post =>
       (if Position.Head = Queue_Index'Last
        then Advance'Result = (Head => 0, Phase => Position.Phase + 1)
        else Advance'Result = (Head  => Position.Head + 1,
                               Phase => Position.Phase));

   ---------------------------------------------------------------------------
   --  The model: the cursor after Consumed entries of a fresh queue.
   ---------------------------------------------------------------------------
   subtype Consumed_Count is Natural;

   function Cursor_After (Consumed : Consumed_Count) return Cursor is
     (Head  => Consumed mod Queue_Depth,
      Phase => First_Pass_Phase + Phase_Tag ((Consumed / Queue_Depth) mod 2));

   --  Advance consumes exactly one entry: it never skips or repeats one.
   procedure Lemma_Advance_Consumes_One (Consumed : Consumed_Count)
     with Ghost,
          Pre  => Consumed < Consumed_Count'Last,
          Post => Advance (Cursor_After (Consumed)) = Cursor_After (Consumed + 1);

   --  A full pass returns to the same slot with the opposite phase, so an
   --  entry left from the previous pass is never taken for a new one.
   procedure Lemma_Pass_Flips_Phase (Consumed : Consumed_Count)
     with Ghost,
          Pre  => Consumed <= Consumed_Count'Last - Queue_Depth,
          Post => Cursor_After (Consumed + Queue_Depth).Head =
                    Cursor_After (Consumed).Head
                  and then Cursor_After (Consumed + Queue_Depth).Phase /=
                    Cursor_After (Consumed).Phase;

   ---------------------------------------------------------------------------
   --  Submission queue tails.
   ---------------------------------------------------------------------------
   function Next_Tail (Tail : Queue_Index) return Queue_Index is
     (if Tail = Queue_Index'Last then 0 else Tail + 1);

   --  Entries the controller has not fetched yet: Tail - Head, mod depth.
   function Occupied (Tail, Head : Queue_Index) return Natural is
     ((Tail - Head + Queue_Depth) mod Queue_Depth)
     with Post => Occupied'Result < Queue_Depth;

   --  One slot always stays empty (3.3.1.5: full when Tail + 1 = Head).
   function Full (Tail, Head : Queue_Index) return Boolean is
     (Next_Tail (Tail) = Head);

   --  Fewer than Queue_Depth - 1 entries queued means room for one more.
   procedure Lemma_Room (Tail, Head : Queue_Index)
     with Ghost,
          Pre  => Occupied (Tail, Head) < Queue_Depth - 1,
          Post => not Full (Tail, Head);

end NVMe_Queue_Cursors;
