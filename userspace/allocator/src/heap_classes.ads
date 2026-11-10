with Interfaces;
-- Size policy only; backing storage is assigned by Heap_Slabs.
package Heap_Classes with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_32;
   type Size_Class is (Bytes_16, Bytes_32, Bytes_64, Bytes_128, Bytes_256,
                       Bytes_512, Bytes_1024, Bytes_2048, Bytes_4096);
   subtype Request_Size is Positive range 1 .. 4_096;
   subtype Block_Size is Positive range 16 .. 4_096;
   -- Each class's stride and its base-2 logarithm, as tables: one load on
   -- the hot paths, the same code for every class (no per-class branch).
   subtype Class_Shift is Natural range 4 .. 12;
   type Stride_Table is array (Size_Class) of Block_Size;
   type Shift_Table is array (Size_Class) of Class_Shift;
   Strides : constant Stride_Table :=
     [Bytes_16 => 16, Bytes_32 => 32, Bytes_64 => 64, Bytes_128 => 128, Bytes_256 => 256,
      Bytes_512 => 512, Bytes_1024 => 1_024, Bytes_2048 => 2_048, Bytes_4096 => 4_096];
   Shifts : constant Shift_Table :=
     [Bytes_16 => 4, Bytes_32 => 5, Bytes_64 => 6, Bytes_128 => 7, Bytes_256 => 8,
      Bytes_512 => 9, Bytes_1024 => 10, Bytes_2048 => 11, Bytes_4096 => 12];
   function Stride (Class : Size_Class) return Block_Size is (Strides (Class));
   function Class_For (Size : Request_Size) return Size_Class with Inline_Always,
     Post => Stride (Class_For'Result) >= Size and then
       (if Class_For'Result /= Size_Class'First then
          Stride (Size_Class'Pred (Class_For'Result)) < Size);

   -- Slab-local arithmetic includes the one-past-end capacity calculation.
   -- Exact because every stride is a power of two dividing 65536.
   subtype Geometry_Offset is Natural range 0 .. 65_536;
   function Quotient (Relative : Geometry_Offset; Class : Size_Class) return Natural
     with Inline_Always, Post => Quotient'Result = Relative / Stride (Class);
   -- Item's byte offset in its slab: Item * Stride (Class), as a shift.
   function Scale (Item : Natural; Class : Size_Class) return Natural
     with Inline_Always, Pre => Item < 4_096, Post => Scale'Result = Item * Stride (Class);
   function Aligned (Relative : Natural; Class : Size_Class) return Boolean is
     ((Interfaces.Unsigned_32 (Relative) and (Interfaces.Unsigned_32 (Stride (Class)) - 1)) = 0)
     with Inline_Always, Post => Aligned'Result = (Relative mod Stride (Class) = 0);
end Heap_Classes;
