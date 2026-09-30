with Interfaces; use Interfaces;

--  The filesystem service's name cache (Linux's dcache): which inode a
--  (volume, directory inode, name) resolves to, or that it resolves to
--  nothing (a negative entry). Names up to Maximum_Name_Bytes are cached;
--  longer ones always go to the directory. Ext2 forgets an entry before
--  every change to that name, a directory's entries when it is removed,
--  and a volume's entries whenever it is admitted (journal replay may
--  have changed any directory).
--
--  The cache is set-associative with round-robin replacement: a key can
--  only live in the Ways slots of its set, so a lookup inspects Ways slots
--  and is still complete. The quantified contracts are proved at level 1
--  and not evaluated at run time.
package Dentry_Cache with Pure, SPARK_Mode is
   pragma Assertion_Policy (Pre => Ignore, Post => Ignore, Ghost => Ignore,
                            Loop_Invariant => Ignore, Assert => Ignore);

   Ways : constant := 4;
   Sets : constant := 2048;
   Capacity : constant := Sets * Ways;
   subtype Slot_Index is Natural range 0 .. Capacity - 1;
   subtype Set_Index is Natural range 0 .. Sets - 1;
   subtype Way_Index is Natural range 0 .. Ways - 1;

   Maximum_Name_Bytes : constant := 64;
   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Name_Text is String (1 .. Maximum_Name_Bytes);

   --  A volume is identified by its block-device endpoint slot. The name
   --  is padded with NUL (never part of an ext2 name). Set is the key's
   --  hash, computed once by Make_Key.
   type Name_Key is record
      Volume : Unsigned_64;
      Parent : Unsigned_32;
      Length : Name_Length;
      Name   : Name_Text;
      Set    : Set_Index;
   end record;

   function Cacheable (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_Name_Bytes);

   function Make_Key (Volume : Unsigned_64; Parent : Unsigned_32; Name : String)
      return Name_Key
     with Pre => Cacheable (Name),
          Post => Make_Key'Result.Volume = Volume and then
                  Make_Key'Result.Parent = Parent and then
                  Make_Key'Result.Length = Name'Length;

   --  Zero: a negative entry (the name is absent from the directory).
   No_Inode : constant Unsigned_32 := 0;

   type Key_Array is array (Slot_Index) of Name_Key;
   type Inode_Array is array (Slot_Index) of Unsigned_32;
   type Flag_Array is array (Slot_Index) of Boolean;
   type Hand_Array is array (Set_Index) of Way_Index;

   --  Complete directories: every name in them is cached, so a name that
   --  is not cached is absent. Ext2 marks a directory complete after
   --  indexing all of it; losing any of its entries except by Forget (a
   --  name that is gone) makes it incomplete again.
   Complete_Capacity : constant := 256;
   subtype Complete_Index is Natural range 0 .. Complete_Capacity - 1;
   type Directory_Id is record
      Volume : Unsigned_64;
      Parent : Unsigned_32;
   end record;
   type Directory_Array is array (Complete_Index) of Directory_Id;
   type Complete_Flags is array (Complete_Index) of Boolean;

   type Table is record
      Keys : Key_Array;
      Inodes : Inode_Array;
      Used : Flag_Array;
      Hands : Hand_Array;
      Directories : Directory_Array;
      Complete : Complete_Flags;
      Complete_Hand : Complete_Index;
   end record;


   function Is_Complete (Cache : Table; Volume : Unsigned_64; Parent : Unsigned_32)
      return Boolean is
     (for some Index in Complete_Index =>
        Cache.Complete (Index) and then
        Cache.Directories (Index) = (Volume => Volume, Parent => Parent));

   --  No entry of a directory complete in After was lost since Before.
   function Keeps_Complete (Before, After : Table) return Boolean is
     (for all Slot in Slot_Index =>
        (if Before.Used (Slot) and then
            Is_Complete (After, Before.Keys (Slot).Volume, Before.Keys (Slot).Parent)
         then After.Used (Slot) and then After.Keys (Slot) = Before.Keys (Slot)))
     with Ghost;

   --  Completeness is only ever granted by Mark_Complete.
   function No_New_Complete (Before, After : Table) return Boolean is
     (for all Index in Complete_Index =>
        (if After.Complete (Index) then
           Is_Complete (Before, After.Directories (Index).Volume,
                        After.Directories (Index).Parent)))
     with Ghost;

   function Set_Of (Key : Name_Key) return Set_Index is (Key.Set);

   function Slot_Of (Set : Set_Index; Way : Way_Index) return Slot_Index is
     (Set * Ways + Way);

   function Holds (Cache : Table; Slot : Slot_Index; Key : Name_Key)
      return Boolean is (Cache.Used (Slot) and then Cache.Keys (Slot) = Key);

   function Contains (Cache : Table; Key : Name_Key) return Boolean is
     (for some Slot in Slot_Index => Holds (Cache, Slot, Key))
     with Ghost;

   --  Every used slot lies in its key's set, and no key is held twice.
   function Valid (Cache : Table) return Boolean is
     ((for all Slot in Slot_Index =>
         (if Cache.Used (Slot) then Set_Of (Cache.Keys (Slot)) = Slot / Ways)) and then
      (for all A in Slot_Index =>
         (for all B in Slot_Index =>
            (if A /= B and then Cache.Used (A) and then Cache.Used (B)
             then Cache.Keys (A) /= Cache.Keys (B)))))
     with Ghost;

   --  An empty cache, initialized in place.
   procedure Clear (Cache : out Table)
     with Post => Valid (Cache) and then
                  (for all Slot in Slot_Index => not Cache.Used (Slot)) and then
                  (for all Index in Complete_Index => not Cache.Complete (Index));

   --  A hit reports exactly what was cached for the key.
   procedure Find
     (Cache : Table; Key : Name_Key; Found : out Boolean;
      Inode : out Unsigned_32)
     with Pre => Valid (Cache),
          Post =>
            Found = Contains (Cache, Key) and then
            (if Found then
               (for some Slot in Slot_Index =>
                  Holds (Cache, Slot, Key) and then Cache.Inodes (Slot) = Inode));

   --  Record what Key resolves to (replacing any entry for it). Replacing
   --  another key's entry makes that key's directory incomplete; Displaced
   --  then names it (Displaced_From), so an indexing pass can tell.
   procedure Insert
     (Cache : in out Table; Key : Name_Key; Inode : Unsigned_32;
      Displaced : out Boolean; Displaced_From : out Directory_Id)
     with Pre => Valid (Cache),
          Post => Valid (Cache) and then
            Keeps_Complete (Cache'Old, Cache) and then
            No_New_Complete (Cache'Old, Cache) and then
            (for some Slot in Slot_Index =>
               Holds (Cache, Slot, Key) and then Cache.Inodes (Slot) = Inode);

   --  Declare every name of a directory cached.
   procedure Mark_Complete
     (Cache : in out Table; Volume : Unsigned_64; Parent : Unsigned_32)
     with Post => Is_Complete (Cache, Volume, Parent) and then
                  Cache.Keys = Cache'Old.Keys and then Cache.Used = Cache'Old.Used and then
                  Cache.Inodes = Cache'Old.Inodes;

   procedure Mark_Incomplete
     (Cache : in out Table; Volume : Unsigned_64; Parent : Unsigned_32)
     with Post => not Is_Complete (Cache, Volume, Parent) and then
                  No_New_Complete (Cache'Old, Cache) and then
                  Cache.Keys = Cache'Old.Keys and then Cache.Used = Cache'Old.Used and then
                  Cache.Inodes = Cache'Old.Inodes;

   --  After Forget, Key is not cached (the next lookup reads the directory).
   procedure Forget (Cache : in out Table; Key : Name_Key)
     with Pre => Valid (Cache),
          Post => Valid (Cache) and then not Contains (Cache, Key);

   --  Every entry of one directory, or of one volume.
   procedure Forget_Directory
     (Cache : in out Table; Volume : Unsigned_64; Parent : Unsigned_32)
     with Pre => Valid (Cache),
          Post => Valid (Cache) and then not Is_Complete (Cache, Volume, Parent) and then
            (for all Slot in Slot_Index =>
               (if Cache.Used (Slot) then
                  Cache.Keys (Slot).Volume /= Volume or else
                  Cache.Keys (Slot).Parent /= Parent));

   procedure Discard_Volume (Cache : in out Table; Volume : Unsigned_64)
     with Pre => Valid (Cache),
          Post => Valid (Cache) and then
            (for all Index in Complete_Index =>
               (if Cache.Complete (Index) then
                  Cache.Directories (Index).Volume /= Volume)) and then
            (for all Slot in Slot_Index =>
               (if Cache.Used (Slot) then Cache.Keys (Slot).Volume /= Volume));
end Dentry_Cache;
