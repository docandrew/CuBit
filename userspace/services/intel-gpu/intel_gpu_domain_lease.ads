generic
   type Domain is (<>);
   -- Callbacks implement one bounded hardware-domain handshake. A failed
   -- acquisition may leave that domain uncertain; do not report success on
   -- timeout merely because a cleanup write was attempted. Callbacks must not
   -- raise exceptions: report hardware/clock failures through Success instead,
   -- so cleanup can continue for every previously acquired domain.
   with procedure Acquire_Domain (Item : Domain; Success : out Boolean);
   with procedure Release_Domain (Item : Domain; Success : out Boolean);
package Intel_GPU_Domain_Lease is
   type Selection is array (Domain) of Boolean;
   type Ownership_State is (Idle, Held, Faulted);
   type Lease is limited private;
   function State (Object : Lease) return Ownership_State;
   function Uncertain (Object : Lease) return Selection;
   procedure Acquire (Object : in out Lease; Required : Selection; Success : out Boolean);
   procedure Release (Object : in out Lease; Success : out Boolean);
   -- One serialized owner, validated platform selection, no concurrent calls.
   -- Faulted leases are never reusable, even after successful partial cleanup.
private
   type Lease is limited record
      Current : Ownership_State := Idle;
      Owned : Selection := [others => False];
      Unknown : Selection := [others => False];
   end record;
end Intel_GPU_Domain_Lease;
