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
      -- One shift by the class's table entry (masked to the word, so no
      -- out-of-range shift branch): no per-class branch or jump table.
      R : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Relative);
      Q : constant Interfaces.Unsigned_32 := Interfaces.Shift_Right (R, Natural (Interfaces.Unsigned_32 (Shifts (Class)) and 31));
   begin
      -- Proof only (no code): in each class the shift is its stride's division.
      case Class is
         when Bytes_16 => pragma Assert (Q = R / 16 and then Stride (Class) = 16 and then Natural (Q) = Relative / 16);
         when Bytes_32 => pragma Assert (Q = R / 32 and then Stride (Class) = 32 and then Natural (Q) = Relative / 32);
         when Bytes_64 => pragma Assert (Q = R / 64 and then Stride (Class) = 64 and then Natural (Q) = Relative / 64);
         when Bytes_128 => pragma Assert (Q = R / 128 and then Stride (Class) = 128 and then Natural (Q) = Relative / 128);
         when Bytes_256 => pragma Assert (Q = R / 256 and then Stride (Class) = 256 and then Natural (Q) = Relative / 256);
         when Bytes_512 => pragma Assert (Q = R / 512 and then Stride (Class) = 512 and then Natural (Q) = Relative / 512);
         when Bytes_1024 => pragma Assert (Q = R / 1_024 and then Stride (Class) = 1_024 and then Natural (Q) = Relative / 1_024);
         when Bytes_2048 => pragma Assert (Q = R / 2_048 and then Stride (Class) = 2_048 and then Natural (Q) = Relative / 2_048);
         when Bytes_4096 => pragma Assert (Q = R / 4_096 and then Stride (Class) = 4_096 and then Natural (Q) = Relative / 4_096);
      end case;
      return Natural (Q);
   end Quotient;

   function Scale (Item : Natural; Class : Size_Class) return Natural is
      U : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Item);
      V : constant Interfaces.Unsigned_32 := Interfaces.Shift_Left (U, Natural (Interfaces.Unsigned_32 (Shifts (Class)) and 31));
   begin
      -- Proof only (no code): in each class the shift is its stride's product.
      case Class is
         when Bytes_16 => pragma Assert (V = U * 16 and then Stride (Class) = 16 and then Natural (V) = Item * 16);
         when Bytes_32 => pragma Assert (V = U * 32 and then Stride (Class) = 32 and then Natural (V) = Item * 32);
         when Bytes_64 => pragma Assert (V = U * 64 and then Stride (Class) = 64 and then Natural (V) = Item * 64);
         when Bytes_128 => pragma Assert (V = U * 128 and then Stride (Class) = 128 and then Natural (V) = Item * 128);
         when Bytes_256 => pragma Assert (V = U * 256 and then Stride (Class) = 256 and then Natural (V) = Item * 256);
         when Bytes_512 => pragma Assert (V = U * 512 and then Stride (Class) = 512 and then Natural (V) = Item * 512);
         when Bytes_1024 => pragma Assert (V = U * 1_024 and then Stride (Class) = 1_024 and then Natural (V) = Item * 1_024);
         when Bytes_2048 => pragma Assert (V = U * 2_048 and then Stride (Class) = 2_048 and then Natural (V) = Item * 2_048);
         when Bytes_4096 => pragma Assert (V = U * 4_096 and then Stride (Class) = 4_096 and then Natural (V) = Item * 4_096);
      end case;
      return Natural (V);
   end Scale;
end Heap_Classes;
