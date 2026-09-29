-- Low-level adapter only: no authority is conferred by this interface.
package Virtmem.Regions with SPARK_Mode => Off is
   type Access_Mode is (Inaccessible, Read_Only, Read_Write, Read_Execute);
   -- Validates retained physical identity even for an inaccessible leaf.
   function Matches (Root : P4; Address : VirtAddress;
                     Expected_Frame : PFN) return Boolean;
   -- Caller owns the frame exclusively, holds the address-space lock and
   -- excludes fault remapping/aliases/teardown. Expected_Frame is retained
   -- even when a leaf is nonpresent. Only 4KiB normal user RAM is admitted.
   -- Installation requires a nonpresent leaf; caller must have completed
   -- acknowledged shootdown after revocation BEFORE calling installation.
   -- Success says a PTE was changed, NOT that TLB invalidation completed.
   procedure Set_Access (Root : P4; Address : VirtAddress;
                         Expected_Frame : PFN; Mode : Access_Mode;
                         Success : out Boolean);
end Virtmem.Regions;
