------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The consumer of input source rings (DEV-002, IPC-009): the set of rings
--  Desktop reads, one per source device, as devices come and go.
--
--  @description
--  Each ring is a CuBit.Input_Rings consumer over the two regions its
--  channel mapped. Add gives the ring its source identity, chosen here and
--  never taken from anything the producer writes: a source cannot name
--  itself or another. Drain hands reports on in arrival order across the
--  rings: it compares only the rings' oldest reports, by their acquisition
--  stamps, so each source's own order is kept exactly. Every element is
--  copied out of the producer's region once and the copy is what is
--  delivered (CuBit.Event_Channels.Take). A producer waiting for room is
--  woken through Wake_Producer.
--
--  No system call here (the caller's formals make them): the same code
--  runs in Desktop and in the hosted test (tests/input-rings).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System; use type System.Address;
with CuBit.Input;

generic
   --  The most rings at once (a named ceiling: sources per driver times
   --  the input drivers a machine has).
   Capacity : Positive;
   --  Ring Slot's producer waits for room: wake it (its channel's doorbell).
   with procedure Wake_Producer (Slot : Positive);
   --  One report, from the source Source, in arrival order.
   with procedure Deliver (Words : CuBit.Input.Report_Words; Source : Unsigned_64);
package Input_Source_Set is

   subtype Slot_Number is Positive range 1 .. Capacity;

   --  The first source identity a ring gets; each ring a new one, never
   --  reused while Desktop runs (event-lane sources are process words).
   Ring_Source_Base : constant Unsigned_64 := 16#5249_4E47_0000_0000#;

   function Active (Slot : Slot_Number) return Boolean;

   --  A slot no ring holds (0: the set is full).
   function Free_Slot return Natural;

   --  A new ring in Slot over its regions: Desktop's own (the consumer's
   --  page) and the producer's (mapped read-only). Source: its identity
   --  (0 when identities ran out: nothing was added).
   procedure Add
     (Slot : Slot_Number; Own_Region, Producer_Region : System.Address; Source : out Unsigned_64)
   with Pre => not Active (Slot) and then Own_Region /= System.Null_Address
               and then Producer_Region /= System.Null_Address;

   --  Deliver everything the ring in Slot still holds, in its order (before
   --  Remove: what a source sent before it went away still counts, a
   --  keyboard's releases as it was unplugged).
   procedure Drain_Slot (Slot : Slot_Number);

   --  The ring in Slot is gone (its channel ended): nothing more is read.
   procedure Remove (Slot : Slot_Number);

   function Source_Of (Slot : Slot_Number) return Unsigned_64;
   function Count return Natural;

   --  Deliver up to Budget reports, oldest acquisition first across rings.
   procedure Drain (Budget : Positive; Delivered : out Natural);

   --  Before Desktop sleeps: every ring armed. False: a report arrived
   --  meanwhile (drain first; the rings armed so far stay armed).
   function Quiet return Boolean;
   --  After waking.
   procedure Disarm_All;

   --  Some ring holds a report (reading only: nothing is armed or taken).
   function Waiting return Boolean;

   --  Retention overflows the producer of Slot has published.
   function Producer_Overflows (Slot : Slot_Number) return Unsigned_32;

end Input_Source_Set;
