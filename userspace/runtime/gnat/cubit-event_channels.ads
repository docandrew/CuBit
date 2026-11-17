------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A one-way event channel (IPC-009, docs/async-rings.md): a producer's
--  fixed-size elements to one consumer through two regions the sides share
--  over grants (a CuBit.Channels duplex), the layout every one-way protocol
--  instantiates. CuBit.Request_Channels is the two-way sibling; here
--  nothing is answered.
--
--  @description
--  The producer owns: the producer writes only its region (the element ring,
--  its produced index, its room-waiting word and its overflow count) and the
--  consumer only its own (its consumed index and its waiting word); each
--  maps the other's read-only. Indices are CuBit.Slot_Rings' (proved): each
--  side keeps its own privately and accepts the peer's only if it moves
--  forward within the ring, so a peer writing garbage garbles only its own
--  traffic. Each side wakes the other by ringing its own region's grant
--  (the glue does: this unit only says when, Ring_Peer), and only when the
--  peer armed its waiting word, once per arming.
--
--  A full ring is back pressure: Push says so and nothing is overwritten.
--  The producer keeps what it could not push and arms for room; whatever
--  it must give up meanwhile it counts in its overflow word (Mark_Overflow)
--  and flags in the protocol's own elements, so the consumer sees every
--  loss. Nothing is dropped silently.
--
--  No system call here: the same code runs in a process and in a hosted
--  test. Take copies an element out of the producer's region before the
--  caller looks at it (rule 1 of docs/data-plane.md).
--
--  Region layout (byte offsets; each writer's word on its own line):
--    producer: 0 Produced, 64 Room_Waiting, 128 Overflows; page 1.. the ring
--    consumer: 0 Consumed, 64 Consumer_Waiting
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Slot_Rings;

generic
   type Element is private;
   Slot_Bits : Natural;    --  2 ** Slot_Bits slots
package CuBit.Event_Channels is

   package Rings is new CuBit.Slot_Rings (Element, Slot_Bits);

   Page_Bytes : constant := 4_096;
   Line_Bytes : constant := 64;

   Produced_At     : constant := 0;
   Room_Waiting_At : constant := 64;
   Overflows_At    : constant := 128;

   Consumed_At         : constant := 0;
   Consumer_Waiting_At : constant := 64;

   Ring_At : constant := Page_Bytes;
   Ring_Bytes : constant Natural := Rings.Ring'Size / 8;

   --  Pages of each region: the producer's header page and its ring; the
   --  consumer's one page.
   Producer_Pages : constant Positive := 1 + (Ring_Bytes + Page_Bytes - 1) / Page_Bytes;
   Consumer_Pages : constant Positive := 1;

   ---------------------------------------------------------------------------
   --  The producer's half.
   ---------------------------------------------------------------------------
   type Producer is limited private;

   procedure Attach_Producer (P : in out Producer; Own_Region, Consumer_Region : System.Address);
   procedure Detach_Producer (P : in out Producer);
   function Producer_Attached (P : Producer) return Boolean;

   --  Room for one more element (taking the consumer's consumed index).
   function Can_Push (P : in out Producer) return Boolean;
   --  Elements pushed and not yet taken, as far as the consumer said.
   function Unconsumed (P : Producer) return Natural;

   --  Write and publish one element. Pushed False: the ring is full (back
   --  pressure; nothing changed). Ring_Peer: the consumer sleeps and asked
   --  to be rung (once per arming).
   procedure Push (P : in out Producer; Item : Element; Pushed, Ring_Peer : out Boolean);

   --  Count one loss the producer had to accept (its own retention ran
   --  out while the ring was full); the consumer reads the count.
   procedure Mark_Overflow (P : in out Producer);
   function Overflows (P : Producer) return Unsigned_32;

   --  Before sleeping for room: arm. False: room appeared meanwhile.
   function Arm_For_Room (P : in out Producer) return Boolean;
   procedure Disarm_For_Room (P : Producer);

   ---------------------------------------------------------------------------
   --  The consumer's half.
   ---------------------------------------------------------------------------
   type Consumer is limited private;

   procedure Attach_Consumer (C : in out Consumer; Own_Region, Producer_Region : System.Address);
   procedure Detach_Consumer (C : in out Consumer);
   function Consumer_Attached (C : Consumer) return Boolean;

   --  An element waits (taking the producer's produced index).
   function Can_Take (C : in out Consumer) return Boolean;

   --  The oldest element, copied out of the producer's region, without
   --  taking it (the consumer merging several channels by time looks first).
   procedure Peek (C : in out Consumer; Item : out Element; Got : out Boolean);

   --  Copy the next element out and release its slot. Ring_Peer: the
   --  producer waits for room and asked to be rung.
   procedure Take (C : in out Consumer; Item : out Element; Got, Ring_Peer : out Boolean);

   --  The producer's overflow count as it last published it.
   function Peer_Overflows (C : Consumer) return Unsigned_32;

   --  Before sleeping for elements: arm. False: one arrived meanwhile.
   function Arm (C : in out Consumer) return Boolean;
   procedure Disarm (C : Consumer);

private
   type Producer is limited record
      Own, Peer : System.Address := System.Null_Address;
      Ring : Rings.Producer;
      Rung_Arming, Room_Arming : Unsigned_32 := 0;
      Lost : Unsigned_32 := 0;
   end record;
   type Consumer is limited record
      Own, Peer : System.Address := System.Null_Address;
      Ring : Rings.Consumer;
      Rung_Room, Arming : Unsigned_32 := 0;
   end record;
end CuBit.Event_Channels;
