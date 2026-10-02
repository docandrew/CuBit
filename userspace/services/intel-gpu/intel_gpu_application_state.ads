with Interfaces;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Render_Sessions;
with Intel_GPU_VM_Image;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Application_Lifetime;
package Intel_GPU_Application_State is
   -- Native service-owned storage. Keep the multi-megabyte offline VM images
   -- at library scope, not on the service thread's stack. Serialized access;
   -- no client may choose an index without authenticated session resolution.
   Table_Pages : constant := 64;
   package VM is new Intel_GPU_VM_Image (Table_Pages);
   type Context_Record is limited record
      Attempted : Boolean := False;
      -- Exact supervisor slot/generation ticket, captured before allocation.
      -- Retain through cancellation/failure; address equality is not identity.
      Parent_Ticket : Interfaces.Unsigned_64 := 0;
      Parent, Context, Tables, Scratch : Intel_GPU_Buffer_Reply.Backing;
      Source : VM.Image;
      Life : Intel_GPU_Application_Lifetime.Phase := Intel_GPU_Application_Lifetime.Empty;
   end record;
   type Context_Array is array
     (1 .. Intel_GPU_Render_Sessions.Capacity) of Context_Record;
   Items : Context_Array;
   -- Each allocator ticket is used once. Retain old generations at library
   -- scope even after publication, failure, or session retirement.
   type Update_Record is limited record
      Tables : Intel_GPU_Buffer_Reply.Backing;
      Candidate : VM.Image;
   end record;
   type Update_Array is array (Intel_GPU_Buffer_Backing.Slot) of Update_Record;
   Updates : Update_Array;
end Intel_GPU_Application_State;
