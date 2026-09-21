package body Heap_Classes with SPARK_Mode is
   -- Each entry describes a 16-byte request bucket. A small read-only table
   -- avoids several unpredictable comparisons on mixed-size request traces.
   type Bucket is range 1 .. 256;
   Classes : constant array (Bucket) of Size_Class :=
     [1 => Bytes_16, 2 => Bytes_32, 3 .. 4 => Bytes_64,
      5 .. 8 => Bytes_128, 9 .. 16 => Bytes_256, 17 .. 32 => Bytes_512,
      33 .. 64 => Bytes_1024, 65 .. 128 => Bytes_2048, 129 .. 256 => Bytes_4096];
   function Class_For (Size : Request_Size) return Size_Class is
     (Classes (Bucket ((Size - 1) / 16 + 1)));

   subtype Scale_Factor is Positive range 16 .. 4_096;
   function Scale_For (Class : Size_Class) return Scale_Factor is
     (case Class is
        when Bytes_16 => 4_096, when Bytes_32 => 2_048, when Bytes_64 => 1_024,
        when Bytes_128 => 512, when Bytes_256 => 256, when Bytes_512 => 128,
        when Bytes_1024 => 64, when Bytes_2048 => 32, when Bytes_4096 => 16);
   function Quotient (Relative : Geometry_Offset; Class : Size_Class) return Natural is
   begin
      -- The bounded operands keep the product below 2**32. Unsigned division
      -- avoids signed-rounding corrections; the public contract stays integer.
      return Natural ((Interfaces.Unsigned_32 (Relative) *
                       Interfaces.Unsigned_32 (Scale_For (Class))) / 65_536);
   end Quotient;
end Heap_Classes;
