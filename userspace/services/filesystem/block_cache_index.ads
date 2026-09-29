with Interfaces; use Interfaces;

--  Bookkeeping for the filesystem service's block cache: which slot holds
--  which (volume, block) key, and CLOCK replacement within a set. Ext2 uses
--  it write-through today, so no block is ever dirty; the dirty flags,
--  dependency classes and ordered write-back selection are the bookkeeping a
--  journal needs to make it write-back. Block contents live beside it in
--  Ext2; this unit never touches them or the device.
--
--  The cache is set-associative: a key can only live in the Ways slots of
--  its set, so a lookup inspects Ways slots and is still complete.
--
--  The quantified contracts are proved by GNATprove at level 1. They are not
--  evaluated at run time (linear or quadratic scans), even in hosted tests
--  built with assertions; those tests check the resulting behaviour.
package Block_Cache_Index with Pure, SPARK_Mode is
   pragma Assertion_Policy (Pre => Ignore, Post => Ignore, Ghost => Ignore,
                            Loop_Invariant => Ignore, Assert => Ignore);

   Ways : constant := 8;
   Sets : constant := 1024;
   --  8,192 slots of up to 4 KiB: 32 MiB of blocks.
   Capacity : constant := Sets * Ways;
   subtype Slot_Index is Natural range 0 .. Capacity - 1;
   subtype Set_Index is Natural range 0 .. Sets - 1;
   subtype Way_Index is Natural range 0 .. Ways - 1;

   --  A volume is identified by its block-device endpoint slot.
   type Block_Key is record
      Volume : Unsigned_64;
      Block  : Unsigned_32;
   end record;

   --  Write-back order: a class is written (and, on a flushable device,
   --  fenced) before any later one. Blocks' contents first; then allocation
   --  marks (bitmaps, group and superblock counts), so no pointer can reach
   --  the disk before the block it names is marked allocated; then pointer
   --  blocks leaf to root; then inodes; then the directory blocks naming
   --  them. Frees are the reverse and are not ordered here: a block is freed
   --  in the cache only after the flush that made its detachment durable.
   type Block_Class is
     (File_Data, Allocation_Metadata, Leaf_Pointers, Middle_Pointers,
      Root_Pointers, Inode_Table, Directory_Data);

   type Key_Array is array (Slot_Index) of Block_Key;
   type Flag_Array is array (Slot_Index) of Boolean;
   type Class_Array is array (Slot_Index) of Block_Class;
   type Hand_Array is array (Set_Index) of Way_Index;
   type Index is record
      Keys : Key_Array;
      Used, Dirty : Flag_Array;
      Referenced : Flag_Array; -- CLOCK second-chance bits
      Classes : Class_Array;
      Hands : Hand_Array;
   end record;

   Empty : constant Index :=
     (Keys => [others => (Volume => 0, Block => 0)],
      Used | Dirty | Referenced => [others => False],
      Classes => [others => File_Data], Hands => [others => 0]);

   Volume_Stride : constant := 257; -- spreads volumes' equal block numbers

   function Set_Of (Key : Block_Key) return Set_Index is
     (Set_Index ((Unsigned_64 (Key.Block) + Key.Volume * Volume_Stride) mod Sets));

   function Slot_Of (Set : Set_Index; Way : Way_Index) return Slot_Index is
     (Set * Ways + Way);

   function Holds (Table : Index; Slot : Slot_Index; Key : Block_Key)
      return Boolean is (Table.Used (Slot) and then Table.Keys (Slot) = Key);

   function Contains (Table : Index; Key : Block_Key) return Boolean is
     (for some Slot in Slot_Index => Holds (Table, Slot, Key))
     with Ghost;

   --  Every used slot lies in its key's set.
   function Placed (Table : Index) return Boolean is
     (for all Slot in Slot_Index =>
        (if Table.Used (Slot) then Set_Of (Table.Keys (Slot)) = Slot / Ways))
     with Ghost;

   --  No key is held by two slots.
   function Unique (Table : Index) return Boolean is
     (for all A in Slot_Index =>
        (for all B in Slot_Index =>
           (if A /= B and then Table.Used (A) and then Table.Used (B)
            then Table.Keys (A) /= Table.Keys (B))))
     with Ghost;

   --  Only dirty slots hold unwritten data; free slots are never dirty.
   function Consistent (Table : Index) return Boolean is
     (for all Slot in Slot_Index =>
        (if Table.Dirty (Slot) then Table.Used (Slot)))
     with Ghost;

   --  Every dirty block of Before is still held, dirty, in After.
   function Keeps_Dirty (Before, After : Index) return Boolean is
     (for all Slot in Slot_Index =>
        (if Before.Used (Slot) and then Before.Dirty (Slot) then
           After.Used (Slot) and then After.Dirty (Slot) and then
           After.Keys (Slot) = Before.Keys (Slot) and then
           After.Classes (Slot) = Before.Classes (Slot)))
     with Ghost;

   function Valid (Table : Index) return Boolean is
     (Placed (Table) and then Unique (Table) and then Consistent (Table))
     with Ghost;

   --  A hit is exactly the slot cached for this key; it becomes recently used.
   procedure Find
     (Table : in out Index; Key : Block_Key;
      Found : out Boolean; Slot : out Slot_Index)
     with Pre => Placed (Table),
          Post =>
            Found = Contains (Table'Old, Key) and then
            (if Found then Holds (Table, Slot, Key)) and then
            Table.Keys = Table'Old.Keys and then Table.Used = Table'Old.Used and then
            Table.Dirty = Table'Old.Dirty and then
            Table.Classes = Table'Old.Classes;

   --  Assign a clean slot in the key's set to a key not yet cached: a free
   --  way if any, otherwise the CLOCK victim among clean ways. Found is False
   --  (and nothing changes) when every way holds a dirty block: the caller
   --  must write back first. A dirty block is never evicted. (Found is True
   --  whenever any way is clean; only reference bits and the hand move.)
   procedure Claim
     (Table : in out Index; Key : Block_Key;
      Found : out Boolean; Slot : out Slot_Index)
     with Pre => Valid (Table) and then not Contains (Table, Key),
          Post =>
            Valid (Table) and then Keeps_Dirty (Table'Old, Table) and then
            (if Found then
               Holds (Table, Slot, Key) and then not Table.Dirty (Slot) and then
               not Table'Old.Dirty (Slot) and then
               (for all Other in Slot_Index =>
                  (if Other /= Slot then
                     Table.Used (Other) = Table'Old.Used (Other) and then
                     Table.Keys (Other) = Table'Old.Keys (Other)))
             else
               Table.Keys = Table'Old.Keys and then Table.Used = Table'Old.Used and then
               Table.Dirty = Table'Old.Dirty and then
               Table.Classes = Table'Old.Classes);

   procedure Mark_Dirty
     (Table : in out Index; Slot : Slot_Index; Class : Block_Class)
     with Pre => Valid (Table) and then Table.Used (Slot),
          Post =>
            Valid (Table) and then Table.Dirty (Slot) and then Table.Classes (Slot) = Class and then
            Table.Keys = Table'Old.Keys and then Table.Used = Table'Old.Used and then
            (for all Other in Slot_Index =>
               (if Other /= Slot then
                  Table.Dirty (Other) = Table'Old.Dirty (Other) and then
                  Table.Classes (Other) = Table'Old.Classes (Other)));

   --  Only after the device acknowledged this slot's write-back.
   procedure Mark_Clean (Table : in out Index; Slot : Slot_Index)
     with Pre => Valid (Table),
          Post =>
            Valid (Table) and then not Table.Dirty (Slot) and then
            Table.Keys = Table'Old.Keys and then Table.Used = Table'Old.Used and then
            (for all Other in Slot_Index =>
               (if Other /= Slot then Table.Dirty (Other) = Table'Old.Dirty (Other)));

   --  Drop a clean entry (a failed fill). Dirty blocks cannot be forgotten.
   procedure Forget (Table : in out Index; Key : Block_Key)
     with Pre => Valid (Table) and then
                 (for all Slot in Slot_Index =>
                    (if Holds (Table, Slot, Key) then not Table.Dirty (Slot))),
          Post => Valid (Table) and then not Contains (Table, Key) and then
                  Keeps_Dirty (Table'Old, Table);

   --  End of a volume's lifetime (re-admission of its endpoint): every
   --  entry, dirty or not, belonged to the previous session and is dropped.
   --  This is the only operation that removes a dirty block without its
   --  write-back.
   procedure Discard_Volume (Table : in out Index; Volume : Unsigned_64)
     with Pre => Valid (Table),
          Post =>
            Valid (Table) and then
            (for all Slot in Slot_Index =>
               (if Table.Used (Slot) then Table.Keys (Slot).Volume /= Volume)) and then
            (for all Slot in Slot_Index =>
               (if Table'Old.Used (Slot) and then
                   Table'Old.Keys (Slot).Volume /= Volume
                then Table.Used (Slot) and then Table.Keys (Slot) = Table'Old.Keys (Slot)
                     and then Table.Dirty (Slot) = Table'Old.Dirty (Slot)
                     and then Table.Classes (Slot) = Table'Old.Classes (Slot)));

   --  Is any block of the volume dirty?
   function Has_Dirty (Table : Index; Volume : Unsigned_64) return Boolean is
     (for some Slot in Slot_Index =>
        Table.Used (Slot) and then Table.Dirty (Slot) and then
        Table.Keys (Slot).Volume = Volume);

   --  The earliest class with a dirty block of the volume: writing back
   --  every block of the returned class before any later one respects the
   --  dependency order.
   procedure Lowest_Dirty_Class
     (Table : Index; Volume : Unsigned_64;
      Found : out Boolean; Class : out Block_Class)
     with Post =>
       Found = Has_Dirty (Table, Volume) and then
       (if Found then
          (for some Slot in Slot_Index =>
             Table.Used (Slot) and then Table.Dirty (Slot) and then
             Table.Keys (Slot).Volume = Volume and then
             Table.Classes (Slot) = Class) and then
          (for all Slot in Slot_Index =>
             (if Table.Used (Slot) and then Table.Dirty (Slot) and then
                 Table.Keys (Slot).Volume = Volume
              then Table.Classes (Slot) >= Class)));
end Block_Cache_Index;
