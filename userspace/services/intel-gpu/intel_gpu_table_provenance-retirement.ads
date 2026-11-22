generic
   with function Context_Released (Session : Unsigned_64) return Boolean;
   with function May_Release (Session, Ticket : Unsigned_64) return Boolean;
   with function Release_Confirmed (Session, Ticket : Unsigned_64) return Boolean;
package Intel_GPU_Table_Provenance.Retirement is
   procedure Start (Object : in out Ledger; Session, Expected_Generation : Unsigned_64;
                    Accepted : out Boolean; Last_Ticket : Unsigned_64 := 0);
   -- Optional exact combined-parent ticket: visit it only after all other
   -- allocation groups are acknowledged and swept. This is ordering ONLY;
   -- its context/ring/scratch consumers must independently satisfy May_Release.
   -- Parent references remain retained on any earlier failure. Zero preserves
   -- normal ledger order; an absent Last_Ticket grants no new release authority.
   procedure Reopen (Object : in out Ledger; Session, Expected_Generation : Unsigned_64;
                     Accepted : out Boolean);
   procedure Recycle_Confirmed
     (Object : in out Ledger; Session, Expected_Generation, Ticket : Unsigned_64;
      Accepted : out Boolean);
   -- Adapter for a single allocation with at most64 records whose exact
   -- supervisor retirement receipt is already present. Never dispatches a
   -- release request. Multi-ticket/larger ledgers use the stepped group API.
   -- Only a fully acknowledged/swept generation may be reused. Reopen retains
   -- CPU metadata capacity, advances generation without wrapping, and rejects
   -- stale reuse attempts. Caller must discard old-generation cached references;
   -- lookups and installs must carry the generation captured with their IDs.
   procedure Step (Object : in out Ledger);
   function Phase (Object : Ledger) return Retirement_Phase;
   function Pending_Ticket (Object : Ledger) return Unsigned_64;
   procedure Take_Request
     (Object : in out Ledger; Session : Unsigned_64;
      Ticket : out Unsigned_64; Accepted : out Boolean);
   procedure Acknowledge
     (Object : in out Ledger; Session, Ticket : Unsigned_64; Accepted : out Boolean);
   -- Context_Released includes engine/context/address/TLB retirement and CPU
   -- grant clearance; May_Release excludes every other allocation consumer.
   -- Take_Request consumes Request_Ready once and returns one exact generation
   -- ticket for submission. Once Awaiting_Ack, it cannot emit the request again.
   -- If sending fails, retain/quarantine; never synthesize acknowledgement.
   -- Ack must match both ticket and authenticated supervisor completion.
   -- Revalidate Context_Released after allocation/receipt callbacks; a true
   -- callback result must not conceal exclusion lost during that callback.
   -- Step visits at most64 records. No backing is freed here. References clear
   -- only after acknowledgement; failure retains all unacknowledged references.
   -- Access stays disabled until explicit generation-changing Reopen succeeds.
end Intel_GPU_Table_Provenance.Retirement;
