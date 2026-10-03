with Interfaces;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Render_Sessions;
with Intel_GPU_VM_Image;
with Intel_GPU_Record_Store;
with Intel_GPU_Application_Lifetime;
with Intel_GPU_Table_Provenance;
package Intel_GPU_Application_State is
   -- Native service-owned storage. Keep the multi-megabyte offline VM images
   -- at library scope, not on the service thread's stack. Serialized access;
   -- no client may choose an index without authenticated session resolution.
   Table_Pages : constant := 64;
   Bootstrap_Table_Mirrors : constant := 4;
   package VM is new Intel_GPU_VM_Image (Table_Pages, Bootstrap_Table_Mirrors);
   Insertion : VM.Insertion_Receipt;
   Growth : VM.Growth_Receipt;
   type Table_References is array (VM.Page_Number) of Natural;
   -- Image ordinals are NOT provenance IDs. Growth can adopt ledger record65
   -- at image position5 while initial reserved records5..64 remain retained.
   -- Zero is unresolved. IDs are scoped by the accompanying ledger generation.
   type Context_Record is limited record
      Attempted : Boolean := False;
      -- Exact supervisor slot/generation ticket, captured before allocation.
      -- Retain through cancellation/failure; address equality is not identity.
      Parent_Ticket : Interfaces.Unsigned_64 := 0;
      Parent, Context, Tables, Scratch : Intel_GPU_Buffer_Reply.Backing;
      Source : VM.Image;
      Table_Owners : Intel_GPU_Table_Provenance.Ledger;
      Table_Generation : Interfaces.Unsigned_64 := 1;
      Table_IDs : Table_References := [others => 0];
      Life : Intel_GPU_Application_Lifetime.Phase := Intel_GPU_Application_Lifetime.Empty;
   end record;
   type Context_Array is array
     (1 .. Intel_GPU_Render_Sessions.Capacity) of Context_Record;
   Items : Context_Array;
   -- Retain each image across publication/failure until explicit retirement.
   -- Image storage is independent of the allocation-ticket namespace.
   type Update_Record is limited record
      Tables : Intel_GPU_Buffer_Reply.Backing;
      Candidate : VM.Image;
      Table_Owners : Intel_GPU_Table_Provenance.Ledger;
      Table_Generation : Interfaces.Unsigned_64 := 1;
      Table_IDs : Table_References := [others => 0];
   end record;
   type Update_Access is access all Update_Record;
   pragma No_Strict_Aliasing (Update_Access);
   function Update_Storage_Bytes return Interfaces.Unsigned_64;
   -- Trusted committed writable CPU memory only, disjoint from metadata index,
   -- BOs, device mappings and every other live object. Initialize one image
   -- using Ada defaults, then publish its stable reference. No heap allocation
   -- or GPU mapping. Caller retains the complete span for service lifetime.
   procedure Install_Fresh_Update
     (Index : Positive; Base, Bytes : Interfaces.Unsigned_64;
      Accepted : out Boolean);
   Bootstrap_Updates : constant Positive := 16;
   function Update_Capacity return Positive;
   function Updates (Index : Positive) return Update_Access;
   function Has_Update (Index : Positive) return Boolean;
   -- Trusted stable committed CPU metadata. Does not allocate VM images.
   procedure Extend_Update_Index
     (Base, Bytes : Interfaces.Unsigned_64; Accepted : out Boolean);
   -- Owner provides a default-initialized image retained for the service
   -- lifetime. No app pointers. Never replace an installed image, alias an
   -- existing one, or use this operation as retirement/reuse authority.
   procedure Install_Update
     (Index : Positive; Item : Update_Access; Accepted : out Boolean);
private
   type Update_Array is array (1 .. Bootstrap_Updates) of aliased Update_Record;
   Inline_Updates : Update_Array;
   package Update_References is new Intel_GPU_Record_Store
     (Update_Access, null, Bootstrap_Updates);
   References : Update_References.Store;
end Intel_GPU_Application_State;
