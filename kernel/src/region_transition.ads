-- Permission transition for an exclusively owned, unaliased region.
-- Caller serializes faults, grants, threads and teardown and validates policy.
-- Callbacks report actual completion, not merely queued work. Invalidate must
-- acknowledge every relevant CPU. A failed attempt leaves the region unusable.
-- Initial object represents an already installed RW/NX region. Revoke removes
-- user access; Install sets either RX or RW/NX, never W+X. These are adapter
-- obligations, not properties established by this callback-only controller.
generic
   with procedure Revoke (Success : out Boolean);
   with procedure Invalidate (Success : out Boolean);
   with procedure Install (Executable : Boolean; Success : out Boolean);
package Region_Transition is
   type Permission is (Writable, Executable);
   type Phase is (Stable, Quarantined);
   type Region is limited private;
   function State (Object : Region) return Phase;
   function Current (Object : Region) return Permission;
   -- Last committed mode only; not evidence of access while quarantined.
   procedure Change (Object : in out Region; Target : Permission;
                     Authorized : Boolean; Success : out Boolean);
private
   type Region is limited record
      Status : Phase := Stable;
      Mode : Permission := Writable;
   end record;
end Region_Transition;
