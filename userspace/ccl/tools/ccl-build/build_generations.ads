with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Interfaces;
with Build_Store;
with Storehouse;
with Storehouse.Profiles;

--  Generations and roots (docs/ccl-build.md, "Generations and garbage
--  collection").
--
--  A profile is a named sequence of generations in the Storehouse:
--
--     profiles/<profile>/<n>        generation n: immutable, read-only
--     profiles/<profile>/current    the current generation's number
--
--  A generation is a root set: the components' outputs by identity, in
--  the order built, and the parameters that produced them (the declarations
--  read, stored as entries, and the pinned toolchain and library trees).
--  Numbers only grow. "current" is one small file replaced by rename, so a
--  switch is atomic on any filesystem with rename (CuBit's ext3 included;
--  no symbolic links). Switching back keeps every generation: rollback is
--  a switch to the generation before the current one.
--
--  Root records (roots/checkouts/<key>/<executable>) are a checkout's last
--  build of a component: every step's outputs, so a rebuild after garbage
--  collection is answered from the store, and the pinned trees it used.
package Build_Generations is
   use Ada.Strings.Unbounded;
   use Storehouse;
   use Storehouse.Profiles;

   Generation_Error : exception;

   --  One stored entry a root keeps. Name: an output's path in the build,
   --  a declaration's path in the tree, or a pinned tree's name.
   type Item is record
      Name : Unbounded_String;
      Kind : Entry_Kind;
      Identity : Digest;
   end record;
   package Item_Vectors is new Ada.Containers.Vectors (Positive, Item);

   type Component is record
      Name : Unbounded_String;            --  the component's name
      Outputs : Item_Vectors.Vector;      --  what it delivers
   end record;
   package Component_Vectors is new Ada.Containers.Vectors (Positive, Component);

   type Content is record
      Parameters : Item_Vectors.Vector;
      Components : Component_Vectors.Vector;
   end record;
   --  The identity of a generation's content (not its number or time):
   --  two generations with equal identities install the same thing.
   function Identity (C : Content) return Digest;

   subtype Seconds is Interfaces.Unsigned_64;
   type Generation is record
      Number : Generation_Number;
      Created : Seconds;                  --  since the epoch; for people
      Value : Content;
   end record;

   package Number_Vectors is new Ada.Containers.Vectors (Positive, Generation_Number);

   --  The profiles in Store, and a profile's generations, ascending.
   package Name_Vectors renames Build_Store.Name_Vectors;
   function Profiles (Store : Build_Store.Store) return Name_Vectors.Vector;
   function Numbers (Store : Build_Store.Store; Profile : String) return Number_Vectors.Vector
   with Pre => Is_Profile_Name (Profile);
   --  0 when the profile has no generation yet.
   function Current (Store : Build_Store.Store; Profile : String) return Generation_Count
   with Pre => Is_Profile_Name (Profile);
   function Read (Store : Build_Store.Store; Profile : String; Number : Generation_Number) return Generation
   with Pre => Is_Profile_Name (Profile);

   --  Makes Value the profile's next generation and switches to it, unless
   --  it equals the current one (Added False, Number the current one).
   --  Every entry it names must be in the store.
   procedure Add
     (Store : Build_Store.Store; Profile : String; Value : Content;
      Number : out Generation_Number; Added : out Boolean)
   with Pre => Is_Profile_Name (Profile);
   --  Makes generation Number current; refused unless every entry it
   --  names is in the store and intact.
   procedure Switch (Store : Build_Store.Store; Profile : String; Number : Generation_Number)
   with Pre => Is_Profile_Name (Profile);
   --  Switches to the newest generation older than the current one.
   procedure Rollback (Store : Build_Store.Store; Profile : String; Number : out Generation_Number)
   with Pre => Is_Profile_Name (Profile);
   --  Deletes every generation but the newest Keep and the current one.
   --  For garbage collection, which then no longer keeps their entries.
   procedure Delete_Old
     (Store : Build_Store.Store; Profile : String; Keep : Generation_Number; Deleted : out Natural)
   with Pre => Is_Profile_Name (Profile);

   --  Root records. Decode_Root also reads the first version, which had
   --  no pins.
   function Encode_Root (Products, Pins : Item_Vectors.Vector) return Byte_Array;
   procedure Decode_Root (Data : Byte_Array; Products, Pins : out Item_Vectors.Vector);

end Build_Generations;
