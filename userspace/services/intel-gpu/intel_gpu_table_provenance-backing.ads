with Intel_GPU_Buffer_Reply;
generic
   with function Owner_Ready return Boolean;
   with function Ticket_Session (Ticket : Unsigned_64) return Unsigned_64;
   with procedure Select_Slice
     (Session, Ticket : Unsigned_64;
      Selected : out Intel_GPU_Buffer_Reply.Backing;
      Allocation_Offset : out Unsigned_64; Accepted : out Boolean);
package Intel_GPU_Table_Provenance.Backing is
   procedure Resolve_Owned_Page
     (Session, Ticket, Offset : Unsigned_64;
      CPU, DMA : out Unsigned_64; Accepted : out Boolean);
   -- Offset is relative to the complete allocation, not the selected slice.
   -- Select_Slice authenticates the table role and returns retained geometry.
   -- Caller serializes allocation lifetime; callbacks must not reenter or
   -- release the selected backing. This performs no memory or MMIO access.
end Intel_GPU_Table_Provenance.Backing;
