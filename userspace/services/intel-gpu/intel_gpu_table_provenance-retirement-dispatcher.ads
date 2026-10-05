with System;
generic
   with procedure Prepare_Ticket
     (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean);
   with procedure Submit (Session, Ticket : Unsigned_64; Accepted : out Boolean);
   with procedure Poll
     (Session, Ticket : Unsigned_64; Complete, Failed : out Boolean);
   with procedure Finalize_Ticket
     (Session, Ticket : Unsigned_64; Accepted : out Boolean);
package Intel_GPU_Table_Provenance.Retirement.Dispatcher is
   type Controller is limited private;
   type State is (Unused, Running, Done, Failed);
   procedure Start
     (Object : in out Controller; Tables : in out Ledger;
      Session, Generation : Unsigned_64; Accepted : out Boolean;
      Last_Ticket : Unsigned_64 := 0);
   procedure Step (Object : in out Controller; Tables : in out Ledger);
   function Status (Object : Controller) return State;
   procedure Reopen
     (Object : in out Controller; Tables : in out Ledger; Accepted : out Boolean);
   -- Only after Done: reopen the exact swept ledger, advance its generation,
   -- then reset this controller for a subsequent group. Failed work never resets.
   -- Prepare_Ticket performs bounded read-only release preflight. It may yield
   -- with both outputs False; Failed wins over Complete. No Submit is called
   -- until a successful preparation and a subsequent exclusion recheck.
   -- The backend must keep those proofs stable under Context_Released until
   -- release completes. Preparation never authorizes clearing references.
   -- One table group per controller. Step invokes at most one Prepare_Ticket,
   -- Submit, Poll or
   -- Finalize_Ticket (after the exact acknowledged ticket is fully swept),
   -- or one bounded ledger sweep. Poll must be nonblocking and route only the
   -- exact allocation-generation receipt; the parent's Release_Confirmed is
   -- still required before references clear. Pending is not permission to
   -- resubmit. Unknown outcomes permanently fail this group, retaining all
   -- unacknowledged references. Caller retains Tables and backing throughout.
   -- Done means the ledger is swept, not reopened. Explicit generation-changing
   -- Reopen remains the caller's separate operation after dropping cached IDs.
   -- The controller is bound to this limited ledger's stable address; it must
   -- not be relocated during retirement. Callbacks are serialized/non-reentrant.
   -- Prepare/Submit/Poll must not advance or replace the ledger transaction;
   -- its owner, epoch, phase and pending ticket are revalidated on return.
   -- Poll's backend owns deadline handling and reports ambiguous expiry as
   -- Failed, never as an acknowledgment or permission to retry.
   -- Finalize_Ticket retires driver-private retained metadata for that exact
   -- allocation, not another physical release. It may fail; no subsequent
   -- ticket or retry is dispatched. Caller must not reuse cached provenance
   -- identities until the group's generation-changing Reopen succeeds.
private
   type Controller is limited record
      Phase : State := Unused;
      Owner, Epoch, Ticket : Unsigned_64 := 0;
      Prepared, Submitted : Boolean := False;
      Ledger_Address : System.Address := System.Null_Address;
   end record;
end Intel_GPU_Table_Provenance.Retirement.Dispatcher;
