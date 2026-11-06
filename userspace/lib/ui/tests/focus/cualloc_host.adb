package body CuAlloc_Host is
   use type Interfaces.C.size_t;
   Minimum_Alignment : constant := 16;
   function Aligned_Alloc (Alignment, Bytes : Interfaces.C.size_t) return System.Address
     with Import, Convention => C, External_Name => "aligned_alloc";
   procedure C_Free (Item : System.Address)
     with Import, Convention => C, External_Name => "free";

   function Allocate (Bytes, Alignment : Interfaces.C.size_t) return System.Address is
      A : constant Interfaces.C.size_t :=
        (if Alignment < Minimum_Alignment then Minimum_Alignment else Alignment);
   begin
      return Aligned_Alloc (A, (Bytes + A - 1) / A * A);
   end Allocate;

   procedure Free (Item : System.Address) is
   begin
      C_Free (Item);
   end Free;
end CuAlloc_Host;
