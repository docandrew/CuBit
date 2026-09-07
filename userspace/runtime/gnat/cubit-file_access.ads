------------------------------------------------------------------------------
--  CuBit per-application filesystem policy. This is not POSIX mode checking:
--  identities and policy installation are mediated by the filesystem service.
--  This pure unit only decodes bounded scopes and decides requested rights.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.File_Access with SPARK_Mode => On is
   Maximum_Prefix_Bytes : constant := 64;
   Maximum_Entries : constant := 16;
   Wire_Entry_Bytes : constant := 72;
   subtype Wire_Index is
     Positive range 1 .. Maximum_Entries * Wire_Entry_Bytes;
   type Wire_Bytes is array (Wire_Index range <>) of Unsigned_8;

   --  Preserve the manifest's existing four bit positions. Execute_Objects
   --  is represented, but image admission remains a separate procmgr decision.
   type Access_Right is
     (Read_Objects, Write_Objects, Execute_Objects, Create_Objects);
   type Rights_Set is array (Access_Right) of Boolean;
   No_Rights : constant Rights_Set := [others => False];
   All_Rights : constant Rights_Set := [others => True];

   function Valid_Rights (Raw : Unsigned_8) return Boolean is (Raw <= 15);
   function Rights_From_Wire (Raw : Unsigned_8) return Rights_Set;
   function Includes (Granted, Requested : Rights_Set) return Boolean is
     (for all Right in Access_Right =>
        (not Requested (Right) or else Granted (Right)));

   type Policy is private;
   function Is_Empty (Item : Policy) return Boolean with Ghost;
   function Valid_Path (Name : String) return Boolean;
   function Scope_Matches (Scope, Name : String) return Boolean;
   function Allows
     (Item : Policy; Name : String; Requested : Rights_Set) return Boolean;

   --  Default construction and Clear deny everything. A trusted bootstrap
   --  wildcard is explicit; an empty/malformed serialized policy never grants.
   procedure Clear (Item : out Policy) with Post => Is_Empty (Item);
   procedure Allow_All_For_Bootstrap (Item : out Policy);
   procedure Decode
     (Data : Wire_Bytes; Item : out Policy; Success : out Boolean)
     with Post => (if not Success then Is_Empty (Item));

private
   type Scope_Entry is record
      Prefix : String (1 .. Maximum_Prefix_Bytes) := [others => ' '];
      Length : Natural range 0 .. Maximum_Prefix_Bytes := 0;
      Rights : Rights_Set := No_Rights;
   end record;
   type Scope_Array is array (1 .. Maximum_Entries) of Scope_Entry;
   type Policy is record
      Entries : Scope_Array;
      Count : Natural range 0 .. Maximum_Entries := 0;
   end record;
   function Is_Empty (Item : Policy) return Boolean is (Item.Count = 0);
end CuBit.File_Access;
