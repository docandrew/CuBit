pragma Ada_2022;
with Interfaces.C;

package body Test_Heap is

   function C_Aligned_Alloc (Alignment, Bytes : Interfaces.C.size_t) return System.Address
   with Import, Convention => C, External_Name => "aligned_alloc";
   procedure C_Free (Block : System.Address)
   with Import, Convention => C, External_Name => "free";

   --  aligned_alloc wants a size that is a multiple of the alignment.
   function Allocate (Bytes, Alignment : Unsigned_64) return System.Address is
      Rounded : constant Unsigned_64 := (Bytes + Alignment - 1) / Alignment * Alignment;
   begin
      if Bytes > Budget then
         Refusals := Refusals + 1;
         return System.Null_Address;
      end if;
      return C_Aligned_Alloc (Interfaces.C.size_t (Alignment), Interfaces.C.size_t (Rounded));
   end Allocate;

   procedure Release (Block : System.Address) is
   begin
      C_Free (Block);
   end Release;

end Test_Heap;
