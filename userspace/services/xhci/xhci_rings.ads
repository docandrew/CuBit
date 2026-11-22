------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Producer and consumer cycle-state bookkeeping for xHCI rings.
--
--  @description
--  Every ring xhci.drv produces (the command ring, each endpoint's transfer
--  ring) and the event ring it consumes use this one model; the native
--  driver only stores what it says where it says.
--
--  Producer (xHCI 1.2, 4.9.2 and 4.11.5.1): Ring_Entries TRBs, the last a
--  Link TRB back to the start with Toggle Cycle set. Software writes a TRB
--  with its Producer Cycle State; after the TRB just before the Link TRB it
--  rewrites the Link TRB's cycle bit to the cycle it used on that pass (so
--  the controller follows it), then toggles its cycle state.
--
--  Consumer (4.9.4, 5.5.2.3): an event TRB belongs to software when its
--  cycle bit equals the Consumer Cycle State; at the end of the segment
--  (no Link TRB in event rings) the index wraps and the state toggles.
--  The dequeue pointer software writes back is the next TRB it has not
--  consumed. Software always writes ERDP with EHB (RW1C) when it finishes
--  servicing an interrupt, even when it found nothing to consume: the
--  interrupter sets EHB with IP, and does not assert another interrupt
--  while EHB stays set (4.17.2, 5.5.2.3.3).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package XHCI_Rings with SPARK_Mode => On, Pure is

   TRB_Bytes : constant := 16;

   type Cycle_Bit is mod 2;

   ---------------------------------------------------------------------------
   --  Producer rings
   ---------------------------------------------------------------------------
   Ring_Entries : constant := 64;
   subtype Ring_Index is Natural range 0 .. Ring_Entries - 1;
   Link_Index : constant Ring_Index := Ring_Entries - 1;
   subtype Enqueue_Index is Ring_Index range 0 .. Link_Index - 1;

   type Producer is record
      Tail  : Enqueue_Index := 0;
      Cycle : Cycle_Bit := 1;
   end record;

   Fresh_Producer : constant Producer := (Tail => 0, Cycle => 1);

   --  Where the next TRB goes and what to do after writing it.
   type Placement is record
      Index      : Enqueue_Index := 0;
      Cycle      : Cycle_Bit := 1;
      --  After this TRB, rewrite the Link TRB with Link_Cycle.
      Link_After : Boolean := False;
      Link_Cycle : Cycle_Bit := 1;
   end record;

   procedure Advance (P : in out Producer; Placed : out Placement)
   with Post =>
     Placed.Index = P'Old.Tail and then Placed.Cycle = P'Old.Cycle
     and then Placed.Link_After = (P'Old.Tail = Enqueue_Index'Last)
     and then
       (if Placed.Link_After then
          P.Tail = 0 and then P.Cycle = P'Old.Cycle + 1
          and then Placed.Link_Cycle = P'Old.Cycle
        else
          P.Tail = P'Old.Tail + 1 and then P.Cycle = P'Old.Cycle);

   --  The physical address of TRB Index of a ring at Base.
   function Address (Base : Unsigned_64; Index : Ring_Index) return Unsigned_64 is
     (Base + Unsigned_64 (Index) * TRB_Bytes)
   with Pre => Base <= Unsigned_64'Last - Unsigned_64 (Ring_Entries * TRB_Bytes);

   --  The Set TR Dequeue Pointer / CRCR value that restarts the consumer at
   --  P's next enqueue position: address with the Dequeue Cycle State in
   --  bit 0 (6.4.3.9, 5.4.5).
   function Restart_Pointer (Base : Unsigned_64; P : Producer) return Unsigned_64 is
     (Address (Base, P.Tail) or Unsigned_64 (P.Cycle))
   with Pre => Base mod TRB_Bytes = 0
     and then Base <= Unsigned_64'Last - Unsigned_64 (Ring_Entries * TRB_Bytes);

   --  Which TRB of a ring at Base an event's TRB pointer names, if any:
   --  inside the ring, TRB aligned, and not the Link TRB.
   function Names_TRB (Base, Pointer : Unsigned_64) return Boolean is
     (Pointer >= Base and then Pointer - Base < Unsigned_64 (Link_Index) * TRB_Bytes
      and then (Pointer - Base) mod TRB_Bytes = 0);
   function Index_Of (Base, Pointer : Unsigned_64) return Enqueue_Index is
     (Enqueue_Index ((Pointer - Base) / TRB_Bytes))
   with Pre => Names_TRB (Base, Pointer);

   ---------------------------------------------------------------------------
   --  The event ring (one segment)
   ---------------------------------------------------------------------------
   Event_Entries : constant := 64;
   subtype Event_Index is Natural range 0 .. Event_Entries - 1;

   type Consumer is record
      Head  : Event_Index := 0;
      Cycle : Cycle_Bit := 1;
   end record;

   Fresh_Consumer : constant Consumer := (Head => 0, Cycle => 1);

   --  The TRB at Head belongs to software.
   function Owned (C : Consumer; Control : Unsigned_32) return Boolean is
     (Cycle_Bit (Control and 1) = C.Cycle);

   procedure Consume (C : in out Consumer)
   with Post =>
     (if C'Old.Head = Event_Index'Last then
        C.Head = 0 and then C.Cycle = C'Old.Cycle + 1
      else
        C.Head = C'Old.Head + 1 and then C.Cycle = C'Old.Cycle);

   --  Whether a service pass writes ERDP (with EHB): when it consumed
   --  events, and ALWAYS when it services an interrupt, even with nothing
   --  to consume. The interrupter may set EHB after software already
   --  consumed the event that raised it (the event is written before IP and
   --  EHB are set); a driver that writes ERDP only per consumed event then
   --  leaves EHB set with an empty ring, and the controller raises no
   --  further interrupt: input works, then stops for good.
   function Must_Write_Dequeue (Consumed : Natural; Interrupted : Boolean) return Boolean is
     (Consumed > 0 or else Interrupted);

   --  ERDP: the next unconsumed TRB, with EHB written as one to clear it.
   EHB_Clear : constant Unsigned_64 := 2 ** 3;
   function Dequeue_Register (Base : Unsigned_64; C : Consumer) return Unsigned_64 is
     ((Base + Unsigned_64 (C.Head) * TRB_Bytes) or EHB_Clear)
   with Pre => Base mod 64 = 0
     and then Base <= Unsigned_64'Last - Unsigned_64 (Event_Entries * TRB_Bytes);

end XHCI_Rings;
