with Interfaces; use Interfaces;
with CuBit.Async_Requests;

-- Single-owner transaction for startup's nonblocking GPU broker. The caller
-- retains the captured Recipient and selected GPU source capability.
-- This core neither grants capabilities nor authenticates IPC receipts.
package Intel_Render_Admission with SPARK_Mode is
   type Phase is (Idle, Reserve_Ready, Reserve_Pending, Delegate_Ready,
     Activate_Ready, Activate_Pending, Abort_Ready, Abort_Pending,
     Active, Failed, Quarantined);
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Transaction is limited private;
   function State (Item : Transaction) return Phase;
   function Identity (Item : Transaction) return Unsigned_64;
   function Session (Item : Transaction) return Unsigned_64;
   function Cancelled (Item : Transaction) return Boolean;
   procedure Start (Item : in out Transaction; Captured : Unsigned_64);
   -- Ready request payload for label0A21, length4, flags/reserved0.
   function Request (Item : Transaction) return Words
     with Pre => State (Item) in Reserve_Ready | Activate_Ready | Abort_Ready;
   -- Before queue submission. ID must be unique across the broker, not just
   -- this transaction. A false return means no submission may be attempted.
   procedure Prepare (Item : in out Transaction; ID : Unsigned_64;
                      Accepted : out Boolean);
   procedure Submitted (Item : in out Transaction; Accepted : Boolean)
     with Pre => State (Item) in Reserve_Pending | Activate_Pending |
       Abort_Pending;
   -- Call only for a kernel-authenticated receipt on the retained endpoint.
   -- Envelope_OK includes exact label/length/flags/reserved checks. Malformed
   -- receipts are consumed but never treated as successful remote cleanup.
   procedure Complete (Item : in out Transaction; ID : Unsigned_64;
     Envelope_OK : Boolean; Reply : Words; Consumed : out Boolean);
   -- Report success only after both endpoint delegations, using the originally
   -- captured identities. A rejected installation still needs
   -- the reserved GPU session aborted; installed caps are not revoked here.
   procedure Delegated (Item : in out Transaction; Installed : Boolean)
     with Pre => State (Item) = Delegate_Ready,
       Post => Identity (Item) = Identity (Item)'Old and
         Session (Item) = Session (Item)'Old and
         State (Item) = (if Installed and not Cancelled (Item) then
                          Activate_Ready else Abort_Ready);
   -- Cancellation is irreversible. Pending replies must still be drained;
   -- late success can request Abort, never activation. No timeout frees state.
   procedure Cancel (Item : in out Transaction)
     with Post => Cancelled (Item) and State (Item) /= Active and
       Identity (Item) = Identity (Item)'Old and
       Session (Item) = Session (Item)'Old;
private
   type Transaction is limited record
      Current : Phase := Idle;
      Target, Tag : Unsigned_64 := 0;
      Stopped : Boolean := False;
      Pending : CuBit.Async_Requests.Tracker;
   end record;
   function State (Item : Transaction) return Phase is (Item.Current);
   function Identity (Item : Transaction) return Unsigned_64 is (Item.Target);
   function Session (Item : Transaction) return Unsigned_64 is (Item.Tag);
   function Cancelled (Item : Transaction) return Boolean is (Item.Stopped);
end Intel_Render_Admission;
