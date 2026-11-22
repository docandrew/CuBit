------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package body CuBit.CuAlloc_Pool is

   use System.Storage_Elements;

   --  CuAlloc's C entry points (CuAlloc_Native): the same objects as the
   --  libc malloc family, System.Memory and Rust's GlobalAlloc use.
   function CuAlloc_Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free_Sized (Item, Bytes, Alignment : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free_sized";

   No_Block : constant Unsigned_64 := 0;

   overriding procedure Allocate
     (Pool                     : in out CuAlloc_Storage_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
      Item : constant Unsigned_64 :=
        CuAlloc_Allocate (Unsigned_64 (Size_In_Storage_Elements), Unsigned_64 (Alignment));
   begin
      if Item = No_Block then
         raise Storage_Error;
      end if;
      Storage_Address := To_Address (Integer_Address (Item));
   end Allocate;

   overriding procedure Deallocate
     (Pool                     : in out CuAlloc_Storage_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Pool);
   begin
      CuAlloc_Free_Sized
        (Unsigned_64 (To_Integer (Storage_Address)), Unsigned_64 (Size_In_Storage_Elements),
         Unsigned_64 (Alignment));
   end Deallocate;

   overriding function Storage_Size
     (Pool : CuAlloc_Storage_Pool) return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return Storage_Count'Last;
   end Storage_Size;

end CuBit.CuAlloc_Pool;
