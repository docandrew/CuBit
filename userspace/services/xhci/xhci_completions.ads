with Interfaces; use Interfaces;

--  One controller-owned dispatcher, bounded mailboxes per device/endpoint.
--  The native adapter is the sole producer and consumer. This is not a lock-
--  free queue for sharing between CPUs. Do not let class drivers independently
--  dequeue the hardware event ring: each could eat another driver's events.
package XHCI_Completions with SPARK_Mode => On is
   Maximum_Slots : constant := 8;
   Queue_Depth : constant := 8;
   subtype Slot_Number is Natural range 0 .. Maximum_Slots;
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

   type Mailboxes is private;
   type Route_Result is (Queued, Not_A_Completion, Invalid_Target, Queue_Full);
   procedure Clear (State : out Mailboxes);
   procedure Route
     (State : in out Mailboxes; Item : Event; Result : out Route_Result);
   procedure Take
     (State : in out Mailboxes; Slot : Slot_Number;
      Endpoint : Endpoint_Number; Item : out Event; Found : out Boolean);
   function Pending
     (State : Mailboxes; Slot : Slot_Number; Endpoint : Endpoint_Number)
      return Natural;
private
   subtype Queue_Position is Positive range 1 .. Queue_Depth;
   subtype Queue_Count is Natural range 0 .. Queue_Depth;
   type Event_Array is array (Queue_Position) of Event;
   type Mailbox is record
      Items : Event_Array := [others => (others => 0)];
      Head, Tail : Queue_Position := Queue_Position'First;
      Count : Queue_Count := 0;
   end record;
   type Mailbox_Array is array (Slot_Number, Endpoint_Number) of Mailbox;
   type Mailboxes is record
      Queues : Mailbox_Array;
   end record;
end XHCI_Completions;
