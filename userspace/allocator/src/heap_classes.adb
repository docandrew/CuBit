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

   function Quotient (Relative : Geometry_Offset; Class : Size_Class) return Natural is
   begin
      -- Every stride is a power of two: one constant divisor per class of a
      -- non-negative operand, so the compiler emits a shift and the provers
      -- see linear arithmetic (a shared scale-and-divide form left the
      -- contract unproved).
      case Class is
         when Bytes_16 => return Relative / 16;
         when Bytes_32 => return Relative / 32;
         when Bytes_64 => return Relative / 64;
         when Bytes_128 => return Relative / 128;
         when Bytes_256 => return Relative / 256;
         when Bytes_512 => return Relative / 512;
         when Bytes_1024 => return Relative / 1_024;
         when Bytes_2048 => return Relative / 2_048;
         when Bytes_4096 => return Relative / 4_096;
      end case;
   end Quotient;
end Heap_Classes;
