-- Install already-owned contiguous backing. Unlike Heap_Growth, ownership is
-- one extent, not one allocation per page. Caller holds registry/address locks
-- and keeps the reservation registered throughout all callbacks.
generic
   with procedure Map_Page (Index : Natural; Success : out Boolean);
   -- False must mean no leaf was installed for that index. Page-table storage
   -- may remain owned by the address space, but the backing extent stays held.
   with procedure Unmap_Page (Index : Natural; Success : out Boolean);
   with procedure Synchronize (Success : out Boolean);
   with procedure Release_Extent (Success : out Boolean);
package Region_Install is
   type Phase is (Fresh, Quarantined, Mapped, Released);
   type Attempt is limited private;
   function State (Object : Attempt) return Phase;
   procedure Apply (Object : in out Attempt; Pages : Positive);
   -- Mapped: caller may commit reservation. Released: caller may finish its
   -- retirement. Quarantined: never recycle backing/handle; cleanup incomplete
   -- or callback raised. No automatic retry. Fresh after no callbacks only.
private
   type Attempt is limited record
      Current : Phase := Fresh;
   end record;
end Region_Install;
