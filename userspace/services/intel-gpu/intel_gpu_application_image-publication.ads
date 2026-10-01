with Intel_GPU_GGTT_Reservations;
generic
   with function Range_Allowed (First, Bytes : Unsigned_64) return Boolean;
   with procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                            Success : out Boolean);
   with procedure Write_PTE (Index, Value : Unsigned_64; Success : out Boolean);
   with procedure Invalidate (Success : out Boolean);
package Intel_GPU_Application_Image.Publication is
   type Result is (Rejected, Mapping_Failed, Quarantined, Published);
   -- One serialized attempt on fresh retained backing. Uses the SAME ledger
   -- as other GGTT publishers; failure never releases claims or backing.
   -- Caller holds exclusive device/VA ownership and excludes retained scanout.
   -- Does not register a GuC context or authorize any application request.
   procedure Publish
     (Object : in out State; Source : VM.Image; Backing : Tables.Mappings;
      Allocation : Intel_GPU_Buffer_Reply.Backing;
      Reservations : in out Intel_GPU_GGTT_Reservations.Ledger;
      Status : out Result);
   function GPU_Address (Object : State) return Unsigned_64;
end Intel_GPU_Application_Image.Publication;
