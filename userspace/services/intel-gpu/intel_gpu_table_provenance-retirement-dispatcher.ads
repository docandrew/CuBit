with System;
generic
   with procedure Submit (Session, Ticket : Unsigned_64; Accepted : out Boolean);
   with procedure Poll
     (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean);
package Intel_GPU_Table_Provenance.Retirement.Dispatcher is
   type Controller is limited private;
   type State is (Unused, Running, Done, Failed);
   procedure Start
     (Object : in out Controller; Tables : in out Ledger;
      Session, Generation : Unsigned_64; Accepted : out Boolean);
   procedure Step (Object : in out Controller; Tables : in out Ledger);
   function Status (Object : Controller) return State;
   -- One table group per controller. Step invokes at most one Submit or Poll,
   -- or one bounded ledger sweep. Poll must be nonblocking and route only the
   -- exact allocation-generation receipt; the parent's Release_Confirmed is
   -- still required before references clear. Pending is not permission to
   -- resubmit. Unknown outcomes permanently fail this group, retaining all
   -- unacknowledged references. Caller retains Tables and backing throughout.
   -- Done means the ledger is swept, not reopened. Explicit generation-changing
   -- Reopen remains the caller's separate operation after dropping cached IDs.
   -- The controller is bound to this limited ledger's stable address; it must
   -- not be relocated during retirement. Callbacks are serialized/non-reentrant.
   -- Poll's backend owns deadline handling and reports ambiguous expiry as
   -- Failed, never as an acknowledgment or permission to retry.
private
   type Controller is limited record
      Phase : State := Unused;
      Owner, Epoch, Ticket : Unsigned_64 := 0;
      Ledger_Address : System.Address := System.Null_Address;
   end record;
end Intel_GPU_Table_Provenance.Retirement.Dispatcher;
