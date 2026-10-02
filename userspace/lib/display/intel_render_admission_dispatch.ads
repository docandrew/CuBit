with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Capability_Grants;
with Intel_Render_Admission;
with Intel_Render_Admission_Native;

-- Single-owner startup dispatcher component. Reserve an exclusive completion
-- token range across all dispatchers in the process. The caller owns source
-- slots and destination reservations for these retained lifetimes.
generic
   Capacity : Positive := 16;
   First_Token : Unsigned_64;
   Last_Token : Unsigned_64;
package Intel_Render_Admission_Dispatch is
   subtype Ticket is Natural range 0 .. Capacity;
   type Dispatcher is limited private;
   procedure Start
     (Object : in out Dispatcher;
      Target : CuBit.Capability_Grants.Recipient;
      Source, Application_Source, Destination : CuBit.Messages.CapabilitySlot;
      Now, Deadline : Unsigned_64; ID : out Ticket);
   -- At most one submission/delegation per call (plus read-only inspection).
   -- Time is monotonic in caller units.
   -- Deadline cancels admission, not its pending IPC or remote reservation.
   procedure Step (Object : in out Dispatcher; Now : Unsigned_64);
   -- Central event-loop integration: drain/routably handle completions and
   -- requests, then Step fairly. Do not sleep while Runnable. Otherwise wait
   -- for activity until Next_Deadline (U64'Last means no broker deadline).
   -- Clock values use the same monotonic units supplied to Start/Step/Complete.
   -- A timeout cancels only once: an outstanding receipt stays retained but
   -- must not cause a busy loop on an already-expired deadline.
   function Runnable (Object : Dispatcher) return Boolean;
   function Next_Deadline (Object : Dispatcher) return Unsigned_64;
   procedure Complete
     (Object : in out Dispatcher; Receipt : CuBit.Messages.CompletionEntry;
      Now : Unsigned_64; Consumed : out Boolean);
   procedure Cancel (Object : in out Dispatcher; ID : Ticket);
   function State (Object : Dispatcher; ID : Ticket)
     return Intel_Render_Admission.Phase;
   -- Tickets and slot reservations are never recycled by this component.
   -- Abort is NOT backing retirement. Capacity/token exhaustion fails closed;
   -- reclamation needs a separately confirmed retirement protocol.
private
   type Entry_Record is limited record
      Request : Intel_Render_Admission_Native.Broker_Request;
      Identity, Deadline, Token_Base : Unsigned_64 := 0;
      Destination : CuBit.Messages.CapabilitySlot := 0;
      Deadline_Observed : Boolean := False;
   end record;
   type Entry_Array is array (Positive range <>) of Entry_Record;
   type Dispatcher is limited record
      Entries : Entry_Array (1 .. Capacity);
      Used : Ticket := 0;
      Cursor : Positive range 1 .. Capacity := 1;
      Next_Token : Unsigned_64 := First_Token;
      Last_Time : Unsigned_64 := 0;
      Clock_Failed, Tokens_Exhausted : Boolean := False;
   end record;
end Intel_Render_Admission_Dispatch;
