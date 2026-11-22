with Interfaces;
-- One instance per designated, serialized display-power owner. This is a
-- lifetime coordinator, not an MMIO implementation or a concurrency lock.
generic
   -- Enumeration order must be a topological ordering: ancestors first.
   type Well is (<>);
   -- Required must be ancestor-closed, with a platform-validated selection.
   with function Selection_Valid (Bits : Interfaces.Unsigned_64) return Boolean;
   -- Callback establishes a hardware reference (including DC/fuse/workaround
   -- handling). Added=False means a retained inherited request: never clear
   -- it on release. Failure may have written hardware; no rollback is assumed.
   -- The backend must validate a stable inherited baseline: removing any
   -- Added request must not withdraw power relied on by an inherited consumer.
   -- A raw request/state snapshot alone is insufficient for this contract.
   with procedure Hold_Well (Item : Well; Added, Success : out Boolean);
   -- Always release software bookkeeping. Added=False MUST NOT clear the
   -- inherited hardware request; it is not permission to disable that well.
   with procedure Drop_Well (Item : Well; Added : Boolean; Success : out Boolean);
package Intel_GPU_Display_Lease is
   type State_Kind is (Idle, Held, Faulted);
   function State return State_Kind;
   function Retained return Interfaces.Unsigned_64;
   function Uncertain return Interfaces.Unsigned_64;
   procedure Acquire (Required : Interfaces.Unsigned_64; Success : out Boolean);
   procedure Release (Success : out Boolean);
   -- No reset/rebind operation. A failed callback permanently quarantines
   -- this instance. Native callbacks must be bounded and non-raising; the
   -- pre-callback latch also prevents reuse if an exception escapes.
end Intel_GPU_Display_Lease;
