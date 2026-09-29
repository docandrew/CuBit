-- Retire an already-owned region, never arbitrary heap or device mappings.
-- Caller holds address-space/registry locks, has entered Retiring, and excludes
-- new grants/aliases/fault remapping. Authorized includes those obligations
-- and absence of outstanding pins. Callbacks report completed operations.
generic
   with procedure Unmap_Page (Index : Natural; Success : out Boolean);
   with procedure Synchronize (Success : out Boolean);
   with procedure Release_Extent (Success : out Boolean);
package Region_Release is
   type Phase is (Fresh, Quarantined, Released);
   type Attempt is limited private;
   function State (Object : Attempt) return Phase;
   procedure Apply (Object : in out Attempt; Pages : Positive;
                    Authorized : Boolean);
   -- Only Released permits Finish_Retirement in the registry. Any failed or
   -- raised callback latches Quarantined: retain metadata and never retry free.
private
   type Attempt is limited record
      Current : Phase := Fresh;
   end record;
end Region_Release;
