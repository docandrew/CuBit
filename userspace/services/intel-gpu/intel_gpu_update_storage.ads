with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Retained_Store;
with Intel_GPU_Application_State;
generic
   with package Storage is new Intel_GPU_Metadata_Arena (<>);
   with function Owner_Ready return Boolean;
package Intel_GPU_Update_Storage is
   type Pool is limited private;
   procedure Request
     (Object : in out Pool; Index : Positive; Byte_Quota : Unsigned_64;
      Accepted : out Boolean; Tables : Positive := Intel_GPU_Application_State.Table_Pages);
   procedure Step (Object : in out Pool);
   function Pending (Object : Pool) return Boolean;
   function Ready (Object : Pool) return Boolean;
   function Charged_Bytes (Object : Pool) return Unsigned_64;
   function Table_Metadata_Bytes return Unsigned_64;
   function Mirror_Metadata_Bytes return Unsigned_64;
   -- CPU-only metadata, not GPU backing. Each image retains independent
   -- reservations and arena state. Index, state records and region commits
   -- share one fixed backing budget; no release/reuse on uncertain failure.
private
   type Budget is limited record
      Limit, Used : Unsigned_64 := 0;
   end record;
   function Charge (Account : in out Budget; Bytes : Unsigned_64) return Boolean;
   type Region_Kind is (Image, Ledger, References, Descriptors, Mirrors);
   type Arena_Array is array (Region_Kind) of Storage.Arena;
   type Image_Regions is limited record
      Items : Arena_Array;
      Fresh : Intel_GPU_Application_State.Installation;
   end record;
   package Records is new Intel_GPU_Retained_Store
     (Image_Regions, Storage, Owner_Ready, Budget, Charge);
   type Phase is (Idle, Registry, Checking, Opening, Requesting, Committing,
                  Publishing, Installing, Failed);
   type Pool is limited record
      State : Phase := Idle;
      Requested : Boolean := False;
      Account : aliased Budget;
      Images : Records.Store;
      Current : Records.Element_Access := null;
      Kind : Region_Kind := Image;
      Index, Target : Positive := 1;
      Generation : Unsigned_64 := 1;
   end record;
end Intel_GPU_Update_Storage;
