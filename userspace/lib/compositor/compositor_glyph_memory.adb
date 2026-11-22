with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
package body Compositor_Glyph_Memory with SPARK_Mode => Off is
   use type System.Address;
   function CuAlloc_Allocate (Bytes, Align : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";
   procedure Reserve (Size : Positive; Pixels : out System.Address; Capacity : out Natural) is
   begin
      Pixels := To_Address (Integer_Address (CuAlloc_Allocate (Unsigned_64 (Size), Alignment)));
      Capacity := (if Pixels = System.Null_Address then 0 else Size);
   end Reserve;
   procedure Release (Pixels : in out System.Address) is
   begin
      if Pixels /= System.Null_Address then
         CuAlloc_Free (Unsigned_64 (To_Integer (Pixels)));
         Pixels := System.Null_Address;
      end if;
   end Release;
end Compositor_Glyph_Memory;
