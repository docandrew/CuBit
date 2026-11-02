with Interfaces; use Interfaces;

--  One controller-owned dispatcher, bounded mailboxes per device/endpoint.
--  The native adapter is the sole producer and consumer. This is not a lock-
--  free queue for sharing between CPUs. Do not let class drivers independently
--  dequeue the hardware event ring: each could eat another driver's events.
--  The mailboxes cover the slots the controller enabled (its MaxSlotsEn,
--  sized from HCSPARAMS1 MaxSlots), up to the protocol's 255 slot IDs.
package XHCI_Completions with SPARK_Mode => On is
   PROTOCOL_MAX_SLOTS : constant := 255;
   Queue_Depth : constant := 8;
   subtype Slot_Number is Natural range 0 .. PROTOCOL_MAX_SLOTS;
   subtype Endpoint_Number is Natural range 0 .. 31;
   --  (0, 0) is the controller command mailbox, not a USB endpoint.
   type Event is record
      ParameterLo : Unsigned_32 := 0;
      ParameterHi : Unsigned_32 := 0;
      Status : Unsigned_32 := 0;
      Control : Unsigned_32 := 0;
   end record with Convention => C, Size => 128;
   for Event use record
      ParameterLo at 0 range 0 .. 31;
      ParameterHi at 4 range 0 .. 31;
      Status at 8 range 0 .. 31;
      Control at 12 range 0 .. 31;
   end record;

   --  Slots 1 .. Slots (and the command mailbox, slot 0).
   type Mailboxes (Slots : Slot_Number) is private;
   type Route_Result is (Queued, Not_A_Completion, Invalid_Target, Queue_Full);
   procedure Clear (State : out Mailboxes);
   --  A Transfer Event for a slot past State.Slots is Invalid_Target.
   procedure Route
     (State : in out Mailboxes; Item : Event; Result : out Route_Result);
   procedure Take
     (State : in out Mailboxes; Slot : Slot_Number;
      Endpoint : Endpoint_Number; Item : out Event; Found : out Boolean)
   with Pre => Slot <= State.Slots;
   function Pending
     (State : Mailboxes; Slot : Slot_Number; Endpoint : Endpoint_Number)
      return Natural
   with Pre => Slot <= State.Slots;
   --  Route would queue Item (or ignore it): never Queue_Full. The event
   --  ring owner leaves an event in the ring rather than overflow a
   --  mailbox, and drains it once the consumer has taken its backlog.
   function Has_Room (State : Mailboxes; Item : Event) return Boolean;
   --  A disabled slot's stale completions go with it (hotplug detach).
   procedure Clear_Slot (State : in out Mailboxes; Slot : Slot_Number)
   with Pre => Slot <= State.Slots,
        Post => (for all E in Endpoint_Number => Pending (State, Slot, E) = 0);
private
   subtype Queue_Position is Positive range 1 .. Queue_Depth;
   subtype Queue_Count is Natural range 0 .. Queue_Depth;
   type Event_Array is array (Queue_Position) of Event;
   type Mailbox is record
      Items : Event_Array := [others => (others => 0)];
      Head, Tail : Queue_Position := Queue_Position'First;
      Count : Queue_Count := 0;
   end record;
   type Mailbox_Array is array (Slot_Number range <>, Endpoint_Number range <>) of Mailbox;
   type Mailboxes (Slots : Slot_Number) is record
      Queues : Mailbox_Array (0 .. Slots, Endpoint_Number);
   end record;
end XHCI_Completions;
