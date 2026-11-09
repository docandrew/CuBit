------------------------------------------------------------------------------
--  The request arithmetic of CuAlloc's C adapters (the libc malloc family,
--  CuBit.Libc_Memory), proved in SPARK (tests/cualloc/run.sh): an element
--  count times an element size only when the product fits, and the
--  alignment rules of aligned_alloc, memalign and posix_memalign. CuAlloc
--  itself refuses alignments that are not powers of two or above its
--  maximum, and sizes above its maximum request, whatever an adapter
--  passes (Ada's System.Memory and Rust's GlobalAlloc pass sizes and
--  alignments straight through).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuAlloc_Requests with SPARK_Mode, Pure is
   --  malloc's alignment: max_align_t.
   Fundamental_Alignment : constant := 16;
   Pointer_Bytes : constant := Standard'Address_Size / 8;

   --  Count elements of Size bytes, in 128 bits: never wraps (one
   --  64 x 64 -> 128 multiply).
   function Wide_Product (Count, Size : Unsigned_64) return Unsigned_128 is
     (Unsigned_128 (Count) * Unsigned_128 (Size))
   with
     Post => Wide_Product'Result <= Unsigned_128 (Unsigned_64'Last) * Unsigned_128 (Unsigned_64'Last);

   --  They fit in 64 bits (calloc, reallocarray).
   function Product_Fits (Count, Size : Unsigned_64) return Boolean is
     (Wide_Product (Count, Size) <= Unsigned_128 (Unsigned_64'Last));

   --  Their bytes: the exact product.
   function Product (Count, Size : Unsigned_64) return Unsigned_64 is
     (Unsigned_64 (Wide_Product (Count, Size)))
   with
     Pre => Product_Fits (Count, Size),
     Post => Unsigned_128 (Product'Result) = Unsigned_128 (Count) * Unsigned_128 (Size);

   --  A power of two: one bit set.
   function Power_Of_Two (Value : Unsigned_64) return Boolean is
     (Value /= 0 and then (Value and (Value - 1)) = 0)
   with
     Post => Power_Of_Two'Result = (for some K in 0 .. 63 => Value = 2 ** K);

   --  posix_memalign's rule: a power of two and a multiple of a pointer.
   function Posix_Alignment (Value : Unsigned_64) return Boolean is
     (Power_Of_Two (Value) and then Value mod Pointer_Bytes = 0);

   --  What CuAlloc is asked for: at least malloc's alignment.
   function Request_Alignment (Value : Unsigned_64) return Unsigned_64 is
     (Unsigned_64'Max (Value, Fundamental_Alignment))
   with
     Post => Request_Alignment'Result >= Fundamental_Alignment
       and then (if Power_Of_Two (Value) then Power_Of_Two (Request_Alignment'Result));
end CuAlloc_Requests;
