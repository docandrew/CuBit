with Interfaces;
-- Size policy only; backing storage is assigned by Heap_Slabs.
package Heap_Classes with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_32;
   type Size_Class is (Bytes_16, Bytes_32, Bytes_64, Bytes_128, Bytes_256,
                       Bytes_512, Bytes_1024, Bytes_2048, Bytes_4096);
   subtype Request_Size is Positive range 1 .. 4_096;
   subtype Block_Size is Positive range 16 .. 4_096;
   function Stride (Class : Size_Class) return Block_Size is
     (case Class is
        when Bytes_16 => 16, when Bytes_32 => 32, when Bytes_64 => 64,
        when Bytes_128 => 128, when Bytes_256 => 256, when Bytes_512 => 512,
        when Bytes_1024 => 1_024, when Bytes_2048 => 2_048,
        when Bytes_4096 => 4_096);
   function Class_For (Size : Request_Size) return Size_Class with Inline_Always,
     Post => Stride (Class_For'Result) >= Size and then
       (if Class_For'Result /= Size_Class'First then
          Stride (Size_Class'Pred (Class_For'Result)) < Size);

   -- Slab-local arithmetic includes the one-past-end capacity calculation.
   -- Fixed-point scaling is exact because all current strides divide 65536.
   subtype Geometry_Offset is Natural range 0 .. 65_536;
   function Quotient (Relative : Geometry_Offset; Class : Size_Class) return Natural
     with Inline_Always, Post => Quotient'Result = Relative / Stride (Class);
   function Aligned (Relative : Natural; Class : Size_Class) return Boolean is
     ((Interfaces.Unsigned_32 (Relative) and (Interfaces.Unsigned_32 (Stride (Class)) - 1)) = 0)
     with Inline_Always, Post => Aligned'Result = (Relative mod Stride (Class) = 0);
end Heap_Classes;
