with Interfaces; use Interfaces;
with Intel_GPU_Broker_Request;
package Intel_GPU_Broker_Launches with SPARK_Mode is
   Capacity : constant := Intel_GPU_Broker_Request.Source_Slot'Last -
     Intel_GPU_Broker_Request.Source_Slot'First + 1;
   subtype Ticket is Natural range 0 .. Capacity;
   type Phase is (Absent, Pending, Reply_Ready, Reply_Taken, Acknowledged, Retained);
   -- Acknowledged records a delivered admission reply, not current GPU health
   -- or continuing session authority; those remain the driver's responsibility.
   type Outcome is (Admitted, Rejected, Uncertain);
   type Ledger is limited private;
   function State (Object : Ledger; ID : Ticket) return Phase;
   function Nonce (Object : Ledger; ID : Ticket) return Unsigned_64;
   function Identity (Object : Ledger; ID : Ticket) return Unsigned_64;
   function Result (Object : Ledger; ID : Ticket) return Outcome;
   function Abort_Required (Object : Ledger; ID : Ticket) return Boolean;
   function Count (Object : Ledger) return Ticket;
   function Can_Reserve
     (Object : Ledger; Request : Intel_GPU_Broker_Request.Decoded;
      Captured : Unsigned_64) return Boolean;
   -- Input must already have passed envelope authentication. Captured is
   -- obtained from the actual source capability, never a request PID.
   -- Replays, source reuse for another incarnation, and destination aliases
   -- are rejected. Entries are never recycled, even after failure/retirement.
   procedure Reserve
     (Object : in out Ledger; Request : Intel_GPU_Broker_Request.Decoded;
      Captured : Unsigned_64; ID : out Ticket)
     with Post =>
       ((ID /= 0) = Can_Reserve (Object, Request, Captured)'Old) and
       Count (Object) = Count (Object)'Old + (if ID /= 0 then 1 else 0) and
       (if ID /= 0 then State (Object, ID) = Pending and
         Identity (Object, ID) = Captured and Nonce (Object, ID) /= 0);
   -- Called only from the corresponding authenticated admission completion.
   -- This does not authorize a reply yet; the native adapter must retain the
   -- launcher's saved reply capability independently of the application cap.
   procedure Finish (Object : in out Ledger; ID : Ticket; Value : Outcome);
   -- Exactly one reply attempt. False leaves the adapter no permission to send.
   procedure Take_Reply (Object : in out Ledger; ID : Ticket; Taken : out Boolean)
     with Post => (Taken = (State (Object, ID)'Old = Reply_Ready)) and
       (if Taken then State (Object, ID) = Reply_Taken);
   -- A failed success reply must cancel admission, not leave an orphan active
   -- session. No retry/reclamation follows ambiguous delivery. Retained means
   -- bookkeeping retained, NOT GPU/grant quiescence or revoked kernel caps.
   procedure Delivered (Object : in out Ledger; ID : Ticket; Sent : Boolean)
     with Post =>
       (State (Object, ID)'Old /= Reply_Taken or
        ((State (Object, ID) = Acknowledged) =
          (Sent and Result (Object, ID) = Admitted))) and
       (State (Object, ID)'Old = Reply_Taken or
        State (Object, ID) = State (Object, ID)'Old) and
       (State (Object, ID)'Old /= Reply_Taken or
        Abort_Required (Object, ID) = (not Sent and Result (Object, ID) = Admitted));
private
   type Entry_Record is record
      Current : Phase := Absent;
      Captured, Request_Nonce : Unsigned_64 := 0;
      Source : Intel_GPU_Broker_Request.Source_Slot := 40;
      Destination : Intel_GPU_Broker_Request.Destination_Slot := 0;
      Completion : Outcome := Uncertain;
      Must_Abort : Boolean := False;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Entry_Record;
   type Ledger is limited record
      Items : Entries;
      Used : Ticket := 0;
   end record;
end Intel_GPU_Broker_Launches;
