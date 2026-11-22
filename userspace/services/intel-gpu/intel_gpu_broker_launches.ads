with Interfaces; use Interfaces;
with Intel_GPU_Broker_Request;
package Intel_GPU_Broker_Launches with SPARK_Mode is
   -- Admissions one launcher may have outstanding or retained.
   Capacity : constant := 16;
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
     (Object : Ledger; Request : Intel_GPU_Broker_Request.Decoded) return Boolean;
   -- Input must already have passed envelope authentication (the launcher's
   -- kernel sender and stamp). The identity is the launcher's child; a
   -- replayed nonce and a second admission for the same child are rejected.
   -- Entries are never recycled, even after failure/retirement.
   procedure Reserve
     (Object : in out Ledger; Request : Intel_GPU_Broker_Request.Decoded;
      ID : out Ticket)
     with Post =>
       ((ID /= 0) = Can_Reserve (Object, Request)'Old) and
       Count (Object) = Count (Object)'Old + (if ID /= 0 then 1 else 0) and
       (if ID /= 0 then
          State (Object, ID) = Pending and then Request.Valid and then
          Identity (Object, ID) = Request.Identity and then Nonce (Object, ID) /= 0);
   -- Called only from the corresponding authenticated admission completion.
   -- This does not authorize a reply yet; the native adapter must retain the
   -- launcher's saved reply handle until it answers.
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
      Completion : Outcome := Uncertain;
      Must_Abort : Boolean := False;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Entry_Record;
   type Ledger is limited record
      Items : Entries;
      Used : Ticket := 0;
   end record;
end Intel_GPU_Broker_Launches;
