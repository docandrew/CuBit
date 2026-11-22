with Heap_Classes; use Heap_Classes;
-- One slab's blocks, all of one class: exact accounting, no payload links.
-- A live bit per 16-byte granule of the slab, set only at a live block's
-- first granule: a release needs no division (the bit at an offset says
-- whether a live block starts there, so an interior or unaligned offset
-- finds it clear), and the bits mean the same for every class. Free
-- blocks are cached as byte offsets: an allocation needs no
-- multiplication. The class rides in the pool: one cache line holds what
-- a release or an allocation reads besides its bitmap word.
package Heap_Bitmap with SPARK_Mode, Pure is
   Slab_Bytes : constant := 65_536;
   Granule_Bytes : constant := 16;
   Max_Slots : constant := Slab_Bytes / Granule_Bytes;
   -- A block of the slab, by number: block Item starts at
   -- Scale (Item - 1, Class).
   subtype Slot is Positive range 1 .. Max_Slots;
   subtype Count is Natural range 0 .. Max_Slots;
   -- A byte offset in the slab.
   subtype Local is Natural range 0 .. Slab_Bytes - 1;
   type Pool is private;
   function Valid (State : Pool) return Boolean with Ghost;
   function Class (State : Pool) return Size_Class with Inline_Always;
   function Capacity (State : Pool) return Slot with Inline_Always;
   function Used (State : Pool) return Count with Inline_Always;
   -- Block Item is live.
   function Live (State : Pool; Item : Slot) return Boolean;
   -- Position is where one of the slab's blocks starts (a multiple of the
   -- stride, below the last block's end); Item_Of names that block.
   function Starts (State : Pool; Position : Local) return Boolean is
     (Position mod Stride (Class (State)) = 0 and then Position / Stride (Class (State)) < Capacity (State));
   -- A block is ready for Take: cached, or never used.
   function Ready (State : Pool) return Boolean with Ghost;
   function Item_Of (State : Pool; Position : Local) return Slot is
     (Position / Stride (Class (State)) + 1)
     with Pre => Starts (State, Position);

   procedure Initialize (State : out Pool; Kind : Size_Class) with
     Post => Valid (State) and then Class (State) = Kind and then Capacity (State) = Slots (Kind)
       and then Used (State) = 0 and then (for all I in Slot => not Live (State, I));
   -- A free block: from the cache, the never-used blocks, or a scan.
   procedure Allocate (State : in out Pool; Position : out Local; Success : out Boolean) with Inline_Always,
     Pre => Valid (State),
     Post => Valid (State) and then Class (State) = Class (State'Old) and then
       Capacity (State) = Capacity (State'Old) and then
       Success = (Used (State'Old) < Capacity (State'Old)) and then
       (if Success then Starts (State, Position) and then Position mod Granule_Bytes = 0 and then
          Position + Stride (Class (State)) <= Slab_Bytes and then
          Used (State) = Used (State'Old) + 1 and then
          not Live (State'Old, Item_Of (State, Position)) and then Live (State, Item_Of (State, Position)) and then
          (for all I in Slot => (if I /= Item_Of (State, Position) then Live (State, I) = Live (State'Old, I)))
        else State = State'Old);
   -- Allocate from the free-block cache, or else the never-used blocks (in
   -- order), only: no scan, no call. Fails (State unchanged) when both are
   -- empty, even if free blocks remain: the caller then takes Allocate.
   -- Untouched: the block lies wholly above every byte the slab's blocks
   -- ever reached (its clean mark, kept across class changes): its memory
   -- is as the slab's backing was given, which the caller may know to be
   -- zero (not a proved property of memory: of the mark only).
   procedure Take (State : in out Pool; Position : out Local; Success, Untouched : out Boolean) with Inline_Always,
     Pre => Valid (State),
     Post => Valid (State) and then Class (State) = Class (State'Old) and then
       Capacity (State) = Capacity (State'Old) and then Success = Ready (State'Old) and then
       (if Success then Used (State'Old) < Capacity (State'Old) and then
          Starts (State, Position) and then Position mod Granule_Bytes = 0 and then
          Position + Stride (Class (State)) <= Slab_Bytes and then
          Used (State) = Used (State'Old) + 1 and then
          not Live (State'Old, Item_Of (State, Position)) and then Live (State, Item_Of (State, Position)) and then
          (for all I in Slot => (if I /= Item_Of (State, Position) then Live (State, I) = Live (State'Old, I)))
        else State = State'Old);
   -- Release the block starting at Position. Anything else (an offset
   -- inside a block, unaligned, or of a free block) is refused, State
   -- unchanged.
   procedure Release (State : in out Pool; Position : Local; Success : out Boolean) with Inline_Always,
     Pre => Valid (State),
     Post => Valid (State) and then Class (State) = Class (State'Old) and then
       Capacity (State) = Capacity (State'Old) and then
       Success = (Starts (State'Old, Position) and then Live (State'Old, Item_Of (State'Old, Position))) and then
       (if Success then Used (State) = Used (State'Old) - 1 and then
          not Live (State, Item_Of (State, Position)) and then
          (for all I in Slot => (if I /= Item_Of (State, Position) then Live (State, I) = Live (State'Old, I)))
        else State = State'Old);
   -- A live block starts at Position, as the bits say (Is_Live's result
   -- when the pool is valid; no precondition, for readers that do not
   -- hold the pool).
   function Live_Start (State : Pool; Position : Local) return Boolean with Inline_Always;
   -- A live block starts at Position: one bitmap word read.
   function Is_Live (State : Pool; Position : Local) return Boolean with Inline_Always,
     Pre => Valid (State),
     Post => Is_Live'Result = (Starts (State, Position) and then Live (State, Item_Of (State, Position)));

   -- The lifecycle gate: no class change while even one live block remains.
   procedure Reconfigure (State : in out Pool; Kind : Size_Class; Success : out Boolean) with
     Pre => Valid (State),
     Post => Valid (State) and then Success = (Used (State'Old) = 0) and then
       (if Success then Class (State) = Kind and then Capacity (State) = Slots (Kind) and then Used (State) = 0
          and then (for all I in Slot => not Live (State'Old, I) and then not Live (State, I))
        else State = State'Old);
private
   Bits_Per_Word : constant := 64;
   subtype Word_Index is Natural range 0 .. Max_Slots / Bits_Per_Word - 1;
   subtype Bit_Index is Natural range 0 .. Bits_Per_Word - 1;
   type Word_Bits is array (Bit_Index) of Boolean with Pack, Size => 64;
   type Bitmap is array (Word_Index) of Word_Bits;
   -- A granule of the slab, 1-based: granule G holds bytes
   -- (G - 1) * 16 .. G * 16 - 1.
   subtype Granule is Positive range 1 .. Max_Slots;
   -- Conversion is bounded by Granule; unsigned division/remainder avoids
   -- signed negative-index correction in unchecked release code.
   function Word_Of (G : Granule) return Word_Index with Inline_Always,
     Post => Word_Of'Result = (G - 1) / Bits_Per_Word;
   function Bit_Of (G : Granule) return Bit_Index with Inline_Always,
     Post => Bit_Of'Result = (G - 1) mod Bits_Per_Word;
   function Bit (Bits : Bitmap; G : Granule) return Boolean is
     (Bits (Word_Of (G)) (Bit_Of (G))) with Inline_Always;
   -- Block Item's first granule.
   function Grain (C : Size_Class; Item : Slot) return Granule is
     (Scale (Item - 1, C) / Granule_Bytes + 1)
     with Pre => Item <= Slots (C);
   -- A cached free block: its byte offset.
   type Stored_Offset is range 0 .. Slab_Bytes - Granule_Bytes with Size => 16;
   -- As many cached free blocks as fit the 1 KiB record beside the bitmap
   -- and the counters: a refill (a bitmap scan) is needed at most once per
   -- Cache_Size allocations of reused blocks.
   Cache_Size : constant := 248;
   subtype Cache_Count is Natural range 0 .. Cache_Size;
   subtype Cache_Index is Positive range 1 .. Cache_Size;
   type Free_Stack is array (Cache_Index) of Stored_Offset with Component_Size => 16;
   -- Handed: the blocks handed out in order so far (the never-used
   -- blocks are those numbered above it: free and uncached, so a fresh
   -- slab hands out blocks in order with no scan).
   -- The counters, the class and the cache first: an allocation or a
   -- release touches the record's first cache line(s) and one bitmap word.
   -- Clean: no block ever reached a byte from it up (since the slab's
   -- backing was given; class changes keep it).
   -- All zero is a valid empty pool (of the first class): fresh backing,
   -- which reads as zero, needs no initialization.
   subtype Clean_Mark is Natural range 0 .. Slab_Bytes;
   type Pool is record
      Occupied : Count;
      Cached : Cache_Count;
      Handed : Count;
      Kind : Size_Class;
      Clean : Clean_Mark;
      Free : Free_Stack;
      Bits : Bitmap;
   end record with Object_Size => 1_024 * 8;
   for Pool use record
      Occupied at 0 range 0 .. 15;
      Cached at 2 range 0 .. 15;
      Handed at 4 range 0 .. 15;
      Kind at 6 range 0 .. 7;
      Clean at 8 range 0 .. 31;
      Free at 16 range 0 .. Cache_Size * 16 - 1;
      Bits at 512 range 0 .. Max_Slots - 1;
   end record;
   -- The class's block count, and the first never-used block.
   function Limit_Of (State : Pool) return Slot is (Slots (State.Kind)) with Inline_Always;
   function Fresh_Of (State : Pool) return Positive is (State.Handed + 1) with Inline_Always;
   -- The live blocks among the first Prefix.
   function Population (Bits : Bitmap; C : Size_Class; Prefix : Count) return Count is
     (if Prefix = 0 then 0 else Population (Bits, C, Prefix - 1) +
        (if Bit (Bits, Grain (C, Prefix)) then 1 else 0)) with Ghost,
     Pre => Prefix <= Slots (C),
     Post => Population'Result <= Prefix,
     Subprogram_Variant => (Decreases => Prefix);
   function Valid (State : Pool) return Boolean is
     (State.Handed <= Limit_Of (State) and then
      State.Occupied = Population (State.Bits, State.Kind, Limit_Of (State)) and then
      -- A set bit is a block's first granule, the block below the fresh ones.
      (for all G in Granule =>
         (if Bit (State.Bits, G) then
            ((G - 1) * Granule_Bytes) mod Stride (State.Kind) = 0 and then
            ((G - 1) * Granule_Bytes) / Stride (State.Kind) + 1 < Fresh_Of (State))) and then
      -- A cached block is a free block below the fresh ones, cached once.
      (for all I in 1 .. State.Cached =>
         Natural (State.Free (I)) mod Stride (State.Kind) = 0 and then
         Natural (State.Free (I)) mod Granule_Bytes = 0 and then
         Natural (State.Free (I)) / Stride (State.Kind) + 1 < Fresh_Of (State) and then
         not Bit (State.Bits, Natural (State.Free (I)) / Granule_Bytes + 1) and then
         (for all J in 1 .. I - 1 => State.Free (I) /= State.Free (J))));
   function Live (State : Pool; Item : Slot) return Boolean is
     (Item <= Slots (State.Kind) and then Bit (State.Bits, Grain (State.Kind, Item)));
   function Class (State : Pool) return Size_Class is (State.Kind);
   function Live_Start (State : Pool; Position : Local) return Boolean is
     (Position mod Granule_Bytes = 0 and then Bit (State.Bits, Position / Granule_Bytes + 1));
   function Ready (State : Pool) return Boolean is (State.Cached > 0 or else State.Handed < Limit_Of (State));
   function Capacity (State : Pool) return Slot is (Limit_Of (State));
   function Used (State : Pool) return Count is (State.Occupied);
end Heap_Bitmap;
