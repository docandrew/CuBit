with Interfaces; use Interfaces;

-- One CPU-local expiry slot. Owner includes process lifetime identity in the
-- adapter. No clock, IRQ, priority or authority operations belong in this ADT.
generic
   type Owner is private;
   No_Owner : Owner;
   Max_Ticket : Unsigned_64 := Unsigned_64'Last;
package Deadline_Ownership with Pure, SPARK_Mode is
   subtype Tick is Unsigned_64;
   subtype Ticket is Unsigned_64 range 0 .. Max_Ticket;
   type Snapshot is record
      Active : Boolean := False;
      Identity : Owner := No_Owner;
      Deadline : Tick := 0;
      Serial : Ticket := 0;
   end record;
   type State is private;
   function View (S : State) return Snapshot;
   type Outcome is (Nothing_Due, Expired, Stale_Owner);

   -- Each successful arm replaces the slot, invalidating earlier tickets.
   -- Exhaustion is explicit and does not wrap/reuse a ticket or disturb state.
   procedure Arm (S : in out State; Identity : Owner; At_Tick : Tick;
                  Issued : out Ticket; Accepted : out Boolean)
     with Post =>
       Accepted = (Identity /= No_Owner and View (S'Old).Serial < Max_Ticket) and then
       (if Accepted then
          View (S) = (True, Identity, At_Tick, View (S'Old).Serial + 1) and
          Issued = View (S).Serial
        else View (S) = View (S'Old) and Issued = 0);

   procedure Cancel (S : in out State; Identity : Owner; Issued : Ticket;
                     Cancelled : out Boolean)
     with Post =>
       Cancelled = (View (S'Old).Active and View (S'Old).Identity = Identity and
                    View (S'Old).Serial = Issued) and then
       View (S).Identity = View (S'Old).Identity and then
       View (S).Deadline = View (S'Old).Deadline and then
       View (S).Serial = View (S'Old).Serial and then
       View (S).Active = (View (S'Old).Active and not Cancelled);

   -- A late/stale hardware vector carries no authority. Recheck the current
   -- slot and owner; consume due state BEFORE the adapter can yield/rearm.
   procedure Poll (S : in out State; Now : Tick; Current : Owner;
                   Result : out Outcome; Lateness : out Tick)
     with Post =>
       View (S).Identity = View (S'Old).Identity and then
       View (S).Deadline = View (S'Old).Deadline and then
       View (S).Serial = View (S'Old).Serial and then
       (if View (S'Old).Active and Now >= View (S'Old).Deadline then
          not View (S).Active and
          Lateness = Now - View (S'Old).Deadline and
          Result = (if Current = View (S'Old).Identity then Expired else Stale_Owner)
        else View (S).Active = View (S'Old).Active and
          Result = Nothing_Due and Lateness = 0);

   -- Multiplex this expiry with a CPU-owned clock/quantum deadline. An arm or
   -- cancellation must not reset that independent clock deadline.
   function Next_Interrupt (S : State; Clock_Deadline : Tick) return Tick
     with Post => Next_Interrupt'Result =
       (if View (S).Active then Tick'Min (View (S).Deadline, Clock_Deadline)
        else Clock_Deadline);
private
   type State is record
      Data : Snapshot;
   end record;
   function View (S : State) return Snapshot is (S.Data);
end Deadline_Ownership;
