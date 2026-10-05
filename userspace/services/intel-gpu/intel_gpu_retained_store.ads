with Interfaces; use Interfaces;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Record_Store;
with System;
generic
   -- Passive records only: no task/protected/controlled components or
   -- allocating/fallible default expressions. Defaults must perform bounded
   -- initialization; placement storage is retained after the local view ends.
   type Element is limited private;
   with package Storage is new Intel_GPU_Metadata_Arena (<>);
   with function Owner_Ready return Boolean;
   type Budget is limited private;
   -- Local atomic accounting against the owner's shared committed-backing
   -- budget (also used by independent per-image arenas). Not an IPC operation:
   -- False has no effect; True reserves Bytes conservatively, without refund
   -- on uncertain commit/initialization failure. It may revoke the owner.
   with function Charge (Account : in out Budget; Bytes : Unsigned_64) return Boolean;
   Bootstrap_Count : Positive := 16;
package Intel_GPU_Retained_Store is
   -- Supervisor-owned CPU metadata only; no app addresses or GPU BOs. The
   -- owner authenticates indices and serializes access, including returned
   -- pointers. A pointer is stable storage, not independent authority.
   type Element_Access is access all Element;
   pragma No_Strict_Aliasing (Element_Access);
   function Element_Bytes return Unsigned_64;
   type Phase is (Idle, Checking, Opening_Index, Requesting_Index,
                  Committing_Index, Publishing_Index, Opening_Elements,
                  Requesting_Element, Committing_Element, Installing, Failed);
   type Store is limited private;
   function State (Object : Store) return Phase;
   function Charged_Bytes (Object : Store) return Unsigned_64;
   function Lookup (Object : Store; Index : Positive) return Element_Access;
   procedure Request
     (Object : in out Store; Account : not null access Budget; Index : Positive;
      Index_Byte_Quota, Element_Byte_Quota : Unsigned_64; Accepted : out Boolean);
   -- Virtual quotas are fixed at first admission, independently of backing.
   -- Sparse identities allocate only requested elements. Index growth commits
   -- a bounded prefix; existing element addresses never move or get replaced.
   procedure Step (Object : in out Store; Account : not null access Budget);
   -- Account identity is fixed on first accepted Request. The owner must keep
   -- this budget object alive and at the same address for the store lifetime;
   -- substituting an account cannot bypass charges already retained elsewhere.
   -- No deletion/reuse: all initialized records and uncertain backing remain
   -- retained. A future release requires a separate proven retirement gate.
private
   package Pointers is new Intel_GPU_Record_Store
     (Element_Access, null, Bootstrap_Count);
   type Store is limited record
      Status : Phase := Idle;
      Target : Positive := 1;
      Index_Limit, Element_Limit, Index_Wanted, Element_Offset : Unsigned_64 := 0;
      Charged : Unsigned_64 := 0;
      Budget_Address : System.Address := System.Null_Address;
      Index_Arena, Element_Arena : Storage.Arena;
      Items : Pointers.Store;
   end record;
end Intel_GPU_Retained_Store;
