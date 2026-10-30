------------------------------------------------------------------------------
--  CuBit per-application filesystem policy. This is not POSIX mode checking:
--  identities and policy installation are mediated by the filesystem service.
--  This pure unit only decodes bounded scopes and decides requested rights.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.File_Access with SPARK_Mode => On is
   --  A scope may name any path the service takes
   --  (CuBit.Directory_Paths.Maximum_Bytes).
   Maximum_Prefix_Bytes : constant := 256;
   Maximum_Entries : constant := 16;
   --  Per entry: rights, the prefix length (u16, little-endian), five zero
   --  bytes, then the prefix.
   Wire_Header_Bytes : constant := 8;
   Wire_Entry_Bytes : constant := Wire_Header_Bytes + Maximum_Prefix_Bytes;
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

   --  Its entries, as Decode takes them (Queue_List_Scopes,
   --  docs/filesystem-protocol-v2.md step 6): Count entries of
   --  Wire_Entry_Bytes from Data'First, each prefix zero-padded. The
   --  bootstrap wildcard is one entry with an empty prefix and all rights.
   function Entry_Count (Item : Policy) return Natural
     with Post => Entry_Count'Result <= Maximum_Entries;
   function Rights_To_Wire (Rights : Rights_Set) return Unsigned_8
     with Post => Valid_Rights (Rights_To_Wire'Result);
   --  (Rights_From_Wire (Rights_To_Wire (R)) = R: tested, all 16 sets.)
   procedure Encode (Item : Policy; Data : out Wire_Bytes; Count : out Natural)
     with Pre  => Data'First = 1 and then Data'Length = Maximum_Entries * Wire_Entry_Bytes,
          Post => Count = Entry_Count (Item);

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
   function Entry_Count (Item : Policy) return Natural is (Item.Count);
end CuBit.File_Access;
