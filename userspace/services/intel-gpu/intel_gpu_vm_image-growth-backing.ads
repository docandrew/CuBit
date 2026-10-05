generic
   with function Owned_Table (DMA : Unsigned_64) return Boolean;
package Intel_GPU_VM_Image.Growth.Backing is
   type Link is record
      Parent_DMA, Child_DMA, Expected, Value, Fill : Unsigned_64 := 0;
      Index : Intel_GPU_ADLN_PPGTT.Table_Index := 0;
   end record;
   type Links is array (Positive range <>) of Link;
   generic
      with function Read_Page (Ordinal : Positive) return Unsigned_64;
      with procedure Emit
        (Ordinal : Positive; Item : Link; Topology : Node; Accepted : out Boolean);
   procedure Resolve_Into
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count, Output_Capacity : Natural; Accepted : out Boolean);
   -- Private staging only: a rejected emission can leave an uncommitted
   -- prefix. Stable inputs and serialized non-reentrant callbacks required.
   generic
      with function Read_Page (Ordinal : Positive) return Unsigned_64;
   procedure Resolve_From_Pages
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      Page_Count : Natural; Output : out Links; Accepted : out Boolean);
   -- Trusted stable retained input, possibly read more than once. No input
   -- array retained; same complete alias/topology/ownership preflight.
   procedure Resolve
     (Source : Image; GPU, Bytes, Retained_Root : Unsigned_64;
      New_Pages : Data_Pages; Output : out Links; Accepted : out Boolean);
   -- Pure preparation, no publication. Owned_Table must authenticate retained
   -- page-table backing in this VM, not merely test address geometry. Caller
   -- serializes source/backing lifetime. Retained_Root is trusted context state,
   -- never a client address. New pages are distinct from ALL reserved/source
   -- tables, mapped data, scratch and retained root. No hidden aliases allowed.
   -- Fill is the fallback word for all512 child entries. Expected is the
   -- parent fallback; writer must compare it before publishing Value, then
   -- flush/read back. This plan is not a TLB completion or commit receipt.
end Intel_GPU_VM_Image.Growth.Backing;
