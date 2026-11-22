with Memory_Accounting;
with System;

-- Kernel-only allocation adapter. Caller authenticates the owner, serializes
-- publication and prevents access to the page until Allocate succeeds.
-- Do not call while holding the accounting ledger or buddy allocator lock.
package Charged_Metadata_Pages is
   procedure Allocate
     (Owner : Memory_Accounting.Identity; Page : out System.Address;
      OK : out Boolean);
   -- Caller has detached all metadata references/readers. This is not a GPU
   -- retirement operation and must never be used for GPU backing or slices.
   procedure Release (Page : System.Address);
end Charged_Metadata_Pages;
