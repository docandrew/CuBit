-- Size policy only; backing storage is assigned by Heap_Slabs.
-- Classes: 16-byte steps to 64, then four per power of two to 4096 (the
-- largest rounding is 25 %, under 12.5 % on average for uniform sizes).
-- A stride need not be a power of two: a slab offset's slot number and
-- its alignment come from one multiplication by the class's reciprocal
-- (Divide), proved exact for every offset of a slab.
package Heap_Classes with SPARK_Mode, Pure is
   type Size_Class is
     (Bytes_16, Bytes_32, Bytes_48, Bytes_64, Bytes_80, Bytes_96, Bytes_112,
      Bytes_128, Bytes_160, Bytes_192, Bytes_224, Bytes_256, Bytes_320, Bytes_384,
      Bytes_448, Bytes_512, Bytes_640, Bytes_768, Bytes_896, Bytes_1024, Bytes_1280,
      Bytes_1536, Bytes_1792, Bytes_2048, Bytes_2560, Bytes_3072, Bytes_3584, Bytes_4096);
   subtype Request_Size is Positive range 1 .. 4_096;
   subtype Block_Size is Positive range 16 .. 4_096;
   -- The blocks of one class in a 64 KiB slab.
   subtype Slot_Total is Positive range 16 .. 4_096;
   -- ceil (2 ** 32 / stride): the class's reciprocal.
   Reciprocal_Shift : constant := 32;
   subtype Magic_Number is Positive range 2 ** 20 .. 2 ** 28;
   -- Each class's stride, slots per slab and reciprocal, as tables: one
   -- load on the hot paths, the same code for every class.
   type Stride_Table is array (Size_Class) of Block_Size;
   type Slot_Table is array (Size_Class) of Slot_Total;
   type Magic_Table is array (Size_Class) of Magic_Number;
   Strides : constant Stride_Table :=
     [Bytes_16 => 16, Bytes_32 => 32, Bytes_48 => 48, Bytes_64 => 64, Bytes_80 => 80, Bytes_96 => 96,
      Bytes_112 => 112, Bytes_128 => 128, Bytes_160 => 160, Bytes_192 => 192, Bytes_224 => 224, Bytes_256 => 256,
      Bytes_320 => 320, Bytes_384 => 384, Bytes_448 => 448, Bytes_512 => 512, Bytes_640 => 640, Bytes_768 => 768,
      Bytes_896 => 896, Bytes_1024 => 1_024, Bytes_1280 => 1_280, Bytes_1536 => 1_536, Bytes_1792 => 1_792, Bytes_2048 => 2_048,
      Bytes_2560 => 2_560, Bytes_3072 => 3_072, Bytes_3584 => 3_584, Bytes_4096 => 4_096];
   Slot_Counts : constant Slot_Table :=
     [Bytes_16 => 4_096, Bytes_32 => 2_048, Bytes_48 => 1_365, Bytes_64 => 1_024, Bytes_80 => 819, Bytes_96 => 682,
      Bytes_112 => 585, Bytes_128 => 512, Bytes_160 => 409, Bytes_192 => 341, Bytes_224 => 292, Bytes_256 => 256,
      Bytes_320 => 204, Bytes_384 => 170, Bytes_448 => 146, Bytes_512 => 128, Bytes_640 => 102, Bytes_768 => 85,
      Bytes_896 => 73, Bytes_1024 => 64, Bytes_1280 => 51, Bytes_1536 => 42, Bytes_1792 => 36, Bytes_2048 => 32,
      Bytes_2560 => 25, Bytes_3072 => 21, Bytes_3584 => 18, Bytes_4096 => 16];
   Magics : constant Magic_Table :=
     [Bytes_16 => 268_435_456, Bytes_32 => 134_217_728, Bytes_48 => 89_478_486, Bytes_64 => 67_108_864,
      Bytes_80 => 53_687_092, Bytes_96 => 44_739_243, Bytes_112 => 38_347_923, Bytes_128 => 33_554_432,
      Bytes_160 => 26_843_546, Bytes_192 => 22_369_622, Bytes_224 => 19_173_962, Bytes_256 => 16_777_216,
      Bytes_320 => 13_421_773, Bytes_384 => 11_184_811, Bytes_448 => 9_586_981, Bytes_512 => 8_388_608,
      Bytes_640 => 6_710_887, Bytes_768 => 5_592_406, Bytes_896 => 4_793_491, Bytes_1024 => 4_194_304,
      Bytes_1280 => 3_355_444, Bytes_1536 => 2_796_203, Bytes_1792 => 2_396_746, Bytes_2048 => 2_097_152,
      Bytes_2560 => 1_677_722, Bytes_3072 => 1_398_102, Bytes_3584 => 1_198_373, Bytes_4096 => 1_048_576];
   function Stride (Class : Size_Class) return Block_Size is (Strides (Class));
   -- A 16-byte multiple: every block is 16-byte aligned.
   procedure Prove_Stride (Class : Size_Class) with Ghost, Post => Stride (Class) mod 16 = 0;
   function Class_For (Size : Request_Size) return Size_Class with Inline_Always,
     Post => Stride (Class_For'Result) >= Size and then
       (if Class_For'Result /= Size_Class'First then
          Stride (Size_Class'Pred (Class_For'Result)) < Size);

   -- Slab-local arithmetic includes the one-past-end capacity calculation.
   subtype Geometry_Offset is Natural range 0 .. 65_536;
   -- The blocks of Class a slab holds: 65536 / stride.
   function Slots (Class : Size_Class) return Slot_Total with Inline_Always,
     Post => Slots'Result = 65_536 / Stride (Class);
   -- Relative's block number in its slab (Relative / stride) and whether
   -- it is a block's start (Relative mod stride = 0): one multiplication.
   procedure Divide (Relative : Geometry_Offset; Class : Size_Class; Quotient : out Natural;
                     Exact : out Boolean) with Inline_Always,
     Post => Quotient = Relative / Stride (Class) and then Exact = (Relative mod Stride (Class) = 0);
   -- Item's byte offset in its slab: Item * Stride (Class), a block that
   -- ends within the slab, 16-byte aligned.
   function Scale (Item : Natural; Class : Size_Class) return Natural
     with Inline_Always, Pre => Item < Slots (Class),
     Post => Scale'Result = Item * Stride (Class) and then Scale'Result mod 16 = 0
       and then Scale'Result + Stride (Class) <= 65_536;
   -- Block Item's start is a multiple of the stride, Item strides in.
   procedure Prove_Scale (Item : Natural; Class : Size_Class) with Ghost,
     Pre => Item < Slots (Class),
     Post => Scale (Item, Class) mod Stride (Class) = 0 and then Scale (Item, Class) / Stride (Class) = Item;
   -- A multiple of the stride within the slab's blocks starts one: it is a
   -- 16-byte multiple and Scale gives it back.
   procedure Prove_Start (Relative : Geometry_Offset; Class : Size_Class) with Ghost,
     Pre => Relative mod Stride (Class) = 0 and then Relative / Stride (Class) < Slots (Class),
     Post => Relative mod 16 = 0 and then Scale (Relative / Stride (Class), Class) = Relative
       and then Relative + Stride (Class) <= 65_536;
   -- Blocks follow one another: a later block starts after an earlier one ends.
   procedure Prove_Order (I, J : Natural; Class : Size_Class) with Ghost,
     Pre => I < J and then J < Slots (Class),
     Post => Scale (I, Class) + Stride (Class) <= Scale (J, Class);
end Heap_Classes;
