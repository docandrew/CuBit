with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT;
generic
   Bootstrap_Tables : Positive := 4;
   Table_Quota : Positive;
package Intel_GPU_VM_Table_Store is
   pragma Compile_Time_Error (Bootstrap_Tables > Table_Quota, "bootstrap exceeds table quota");
   subtype Table_ID is Positive range 1 .. Table_Quota;
   type Page is array (Intel_GPU_ADLN_PPGTT.Table_Index) of Unsigned_64;
   type Store is limited private;
   function Capacity (Object : Store) return Positive;
   function Word (Object : Store; Table : Table_ID;
                  Index : Intel_GPU_ADLN_PPGTT.Table_Index) return Unsigned_64
     with Pre => Table <= Capacity (Object);
   procedure Set_Word (Object : in out Store; Table : Table_ID;
                       Index : Intel_GPU_ADLN_PPGTT.Table_Index; Value : Unsigned_64)
     with Pre => Table <= Capacity (Object);
   procedure Clear (Object : in out Store; Table : Table_ID)
     with Pre => Table <= Capacity (Object);
   procedure Copy_Page (Target : in out Store; Target_ID : Table_ID;
                        Source : Store; Source_ID : Table_ID)
     with Pre => Target_ID <= Capacity (Target) and Source_ID <= Capacity (Source);
   procedure Extend (Object : in out Store; Base, Bytes : Unsigned_64;
                     Accepted : out Boolean);
   -- Trusted, committed writable CPU metadata only. Caller authenticates and
   -- retains a disjoint stable reservation for this store's lifetime. No app
   -- pointers, GPU backing, MMIO, or authority inferred from an address.
   -- Increment <=64KiB; only the new suffix is zeroed. Existing indices/words
   -- never move. Quota is a limit, not eager allocation. No heap or exceptions.
private
   type Inline_Pages is array (1 .. Bootstrap_Tables) of Page;
   type Store is limited record
      Inline : Inline_Pages := [others => [others => 0]];
      Base, Bytes : Unsigned_64 := 0;
      Available : Positive := Bootstrap_Tables;
   end record;
end Intel_GPU_VM_Table_Store;
