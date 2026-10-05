with Interfaces;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Render_Sessions;
with Intel_GPU_VM_Image;
with Intel_GPU_Record_Store;
with Intel_GPU_Application_Lifetime;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_References;
with Intel_GPU_Metadata_Ranges;
package Intel_GPU_Application_State is
   -- Native service-owned storage. Keep the multi-megabyte offline VM images
   -- at library scope, not on the service thread's stack. Serialized access;
   -- no client may choose an index without authenticated session resolution.
   Table_Pages : constant := 64;
   Bootstrap_Table_Mirrors : constant := 4;
   package VM is new Intel_GPU_VM_Image
     (Table_Pages, Bootstrap_Table_Mirrors, Bootstrap_Insertion_Words => 512,
      Bootstrap_Growth_Links => 2, Bootstrap_Descriptors => 4);
   Insertion : VM.Insertion_Receipt;
   package Table_References is new Intel_GPU_Table_References (Table_Pages);
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
      Growth : VM.Growth_Receipt;
      Table_Owners : Intel_GPU_Table_Provenance.Ledger;
      Table_Generation : Interfaces.Unsigned_64 := 1;
      Table_IDs : Table_References.Map;
      Life : Intel_GPU_Application_Lifetime.Phase := Intel_GPU_Application_Lifetime.Empty;
   end record;
   type Context_Array is array
     (1 .. Intel_GPU_Render_Sessions.Capacity) of Context_Record;
   Items : Context_Array;
   -- Retain each image across publication/failure until explicit retirement.
   -- Image storage is independent of the allocation-ticket namespace.
   type Update_Record is limited record
      -- Intrusive CPU-storage registration, retained with this object. Not
      -- GPU mapping authority and never reset during candidate retirement.
      Metadata_Range : aliased Intel_GPU_Metadata_Ranges.Node;
      Tables : Intel_GPU_Buffer_Reply.Backing;
      Candidate : VM.Image;
      Table_Owners : Intel_GPU_Table_Provenance.Ledger;
      Table_Generation : Interfaces.Unsigned_64 := 1;
      Table_IDs : Table_References.Map;
   end record;
   type Update_Access is access all Update_Record;
   pragma No_Strict_Aliasing (Update_Access);
   function Update_Storage_Bytes return Interfaces.Unsigned_64;
   -- Trusted committed writable CPU memory only, disjoint from metadata index,
   -- BOs, device mappings and every other live object. Initialize one image
   -- using Ada defaults, then publish its stable reference. No heap allocation
   -- or GPU mapping. Caller retains the complete span for service lifetime.
   type Installation_Phase is (Idle, Checking, Publishing, Complete, Failed);
   type Installation is limited private;
   function State (Object : Installation) return Installation_Phase;
   function Checked (Object : Installation) return Natural;
   procedure Begin_Fresh_Update
     (Object : in out Installation; Index : Positive;
      Base, Bytes : Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure Step_Fresh_Update (Object : in out Installation);
   -- At most64 range-search probes per check; no empty index slots visited.
   -- A registry mutation invalidates the
   -- attempt; initialization/publication occur only after all checks pass.
   -- Failed/complete attempts never replay. Publication inserts the retained
   -- range node with bounded AVL rebalancing, independently of namespace size.
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
   type Installation is limited record
      Phase : Installation_Phase := Idle;
      Index : Positive := 1;
      Probes : Natural := 0;
      Base, Bytes, Epoch : Interfaces.Unsigned_64 := 0;
   end record;
   Registry_Epoch : Interfaces.Unsigned_64 := 1;
   Ranges : Intel_GPU_Metadata_Ranges.Tree;
   Ranges_Ready : Boolean := False;
   type Update_Array is array (1 .. Bootstrap_Updates) of aliased Update_Record;
   Inline_Updates : Update_Array;
   package Update_References is new Intel_GPU_Record_Store
     (Update_Access, null, Bootstrap_Updates);
   References : Update_References.Store;
end Intel_GPU_Application_State;
