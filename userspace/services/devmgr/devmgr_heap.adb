------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Devmgr_Heap - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

package body Devmgr_Heap is

   function CuAlloc_Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
   with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Item : Unsigned_64)
   with Import, Convention => C, External_Name => "cualloc_free";

   function Allocate (Bytes, Alignment : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (CuAlloc_Allocate (Bytes, Alignment))));

   procedure Release (Block : System.Address) is
   begin
      CuAlloc_Free (Unsigned_64 (To_Integer (Block)));
   end Release;

end Devmgr_Heap;
