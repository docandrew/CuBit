package body CuAlloc_Host is
   function C_Aligned_Alloc (Alignment, Bytes : Unsigned_64) return Unsigned_64
   with Import, Convention => C, External_Name => "aligned_alloc";
   procedure C_Free (Block : Unsigned_64)
   with Import, Convention => C, External_Name => "free";

   --  aligned_alloc wants a size that is a multiple of the alignment.
   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
     (C_Aligned_Alloc (Alignment, (Bytes + Alignment - 1) / Alignment * Alignment));

   procedure Free (Block : Unsigned_64) is
   begin
      C_Free (Block);
   end Free;
end CuAlloc_Host;
