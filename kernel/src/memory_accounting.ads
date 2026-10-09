with Interfaces;
with Process_Memory_Budget;

-- Kernel-only adapter. Process/capability authorization is the caller's duty.
-- All account state has one leaf ledger lock; no buddy operation under it.
package Memory_Accounting is
   subtype Identity is Interfaces.Unsigned_64;
   -- Single-threaded startup only, before any charges are bound. Installs the
   -- sole physical refund callback; called by Process.setup before accounts.
   procedure Initialize (OK : out Boolean);
   procedure Open (Owner : out Identity; OK : out Boolean);
   procedure Close (Owner : Identity; OK : out Boolean);
   procedure Adopt (Owner, Limit_Pages : Identity; OK : out Boolean);
   procedure Reserve
     (Owner : Identity; Kind : Process_Memory_Budget.Charge_Kind;
      Pages : Identity; Charge : out Identity; OK : out Boolean);
   -- Only for reservations never handed to the physical allocator's charge
   -- callback. A failed map AFTER binding must not call this again.
   procedure Cancel_Unbound (Charge, Pages : Identity; OK : out Boolean);
   procedure Inspect
     (Owner : Identity; Live : out Boolean; Pages, Limit_Pages : out Identity;
      OK : out Boolean);
   procedure Metadata_Status
     (Stored, Growing, Retiring, Limit : out Identity; Faulted : out Boolean);
end Memory_Accounting;
