with Owner_Record_Slabs;
with Charged_Metadata_Pages;
with DMA_Retirement_Steps;
package Process.DMA is
   -- Caller holds target mailbox, keeping the incarnation admitted/stable.
   type Ticket is private;
   procedure Reserve
     (PID : ProcessID; Order : Natural; Retained : Boolean;
      Item : out Ticket; Success : out Boolean);
   -- Domain: the IOMMU domain the block was mapped into (0: none).
   procedure Commit (Item : in out Ticket; Physical : Unsigned_64; Domain : Unsigned_64);
   procedure Cancel (Item : in out Ticket);
   function Has_Records (PID : ProcessID) return Boolean;
   -- Queue under grantLock; notify only after releasing grantLock.
   procedure Enqueue (PID : ProcessID);
   procedure Take_Ready (PID : out ProcessID);
   procedure Finish_Step (PID : ProcessID; Complete : Boolean);
   -- Caller establishes stopped CPU execution, retired translations and no
   -- acquired grants. Keep the PID unreusable until Complete is true.
   -- A block in an IOMMU domain is unmapped and its IOTLB invalidation
   -- waited for before any frame is freed (DMA_Remapping.Unmap_Grant);
   -- a retained block stays mapped (it is never freed). An invalidation
   -- that does not complete poisons the record: backing quarantined.
   procedure Retire_Step
     (PID : ProcessID; CPU_And_Grants_Retired : Boolean; Complete : out Boolean);
private
   procedure Release_Owner (Physical_Page, Owner : Unsigned_64);
   procedure Free_Block (Physical : Unsigned_64; Order : Natural);
   package Retirement is new DMA_Retirement_Steps (Release_Owner, Free_Block);
   -- A slab (its header and this many nodes) must fit one 4 KiB page
   -- (Owner_Record_Slabs.Open refuses otherwise): a node is the allocation
   -- record (48 bytes since the IOMMU domain, IOMMU-001) and 24 of links.
   Records_Per_Metadata_Page : constant Positive := 48;
   package Records is new Owner_Record_Slabs
     (Retirement.Allocation, Records_Per_Metadata_Page,
      Charged_Metadata_Pages.Allocate, Charged_Metadata_Pages.Release);
   type Ticket is record
      Ref : Records.Reference := null;
   end record;
end Process.DMA;
