with Interfaces; use Interfaces;
with Files_Limits; use Files_Limits;

--  One folder's entries, in the order they arrived (docs/files-app.md, "Data
--  model"). An entry's Entry_Id is its arrival slot and never changes while
--  the listing lives; names are packed into one byte arena. Each entry keeps
--  a sort prefix: its first case-folded bytes up to the first digit, so most
--  name comparisons are one integer compare (Files_Order). Pure policy,
--  proved (tests/files-app/files_proof.gpr).
package Files_Listing with SPARK_Mode is
   type Entry_Kind is (Unknown_Kind, File_Kind, Directory_Kind, Link_Kind);
   --  Bytes in a file; milliseconds since the Unix epoch; the volume's
   --  identity for the object (volume << 32 | inode), NO_OBJECT if it gave
   --  none. Descriptive only: none of these is authority.
   type Byte_Size is new Unsigned_64;
   type Time_Ms is new Unsigned_64;
   type Object_Identity is new Unsigned_64;
   NO_OBJECT : constant Object_Identity := 0;
   type Sort_Prefix is new Unsigned_64;
   --  Bytes of the prefix, and what a digit becomes in it: digits sort
   --  before letters, so every name with a digit at a position compares
   --  there as '0' and ties fall to the full natural comparison.
   PREFIX_BYTES : constant := 8;
   DIGIT_MARK : constant Unsigned_8 := Character'Pos ('0');

   type Name_Bytes is array (Positive range <>) of Unsigned_8;

   --  What arrived about one entry, before it is stored.
   type Entry_Facts is record
      Kind : Entry_Kind := Unknown_Kind;
      Size : Byte_Size := 0;
      Size_Known : Boolean := False;
      Modified : Time_Ms := 0;
      Modified_Known : Boolean := False;
      --  The metadata change time (ctime: ext2 keeps no creation time).
      Changed : Time_Ms := 0;
      Changed_Known : Boolean := False;
      --  Stored permission and type bits, owner and group (descriptive).
      Mode : Unsigned_32 := 0;
      Mode_Known : Boolean := False;
      Owner, Group : Unsigned_32 := 0;
      Owner_Known : Boolean := False;
      Object : Object_Identity := NO_OBJECT;
   end record;

   type Entry_Info is record
      Facts : Entry_Facts;
      Name_First : Arena_Index := 1;
      Length : Name_Length := 0;
      --  Where the extension starts within the name (after its last '.'),
      --  0 for none: a leading dot (".profile") is not an extension.
      Extension_At : Name_Length := 0;
      Prefix : Sort_Prefix := 0;
   end record;
   type Entry_Table is array (Entry_Id range <>) of Entry_Info;
   type Arena is array (Arena_Index range <>) of Unsigned_8;

   type Listing (Capacity : Entry_Capacity; Arena_Bytes : Arena_Capacity) is record
      Count : Entry_Count := 0;
      Used : Arena_Count := 0;
      --  Every entry has arrived (the service's END page was seen).
      Complete : Boolean := False;
      Entries : Entry_Table (1 .. Capacity);
      Names : Arena (1 .. Arena_Bytes);
   end record;

   function Valid (L : Listing) return Boolean is
     (L.Count <= L.Capacity and then L.Used <= L.Arena_Bytes);

   --  Room for one more entry with a name of Length bytes.
   function Has_Room (L : Listing; Length : Name_Length) return Boolean is
     (Valid (L) and then L.Count < L.Capacity and then Length <= L.Arena_Bytes - L.Used);

   procedure Clear (L : in out Listing)
     with Post => Valid (L) and then L.Count = 0 and then L.Used = 0 and then not L.Complete;

   procedure Append (L : in out Listing; Name : Name_Bytes; Facts : Entry_Facts)
     with Pre => Name'Length in 1 .. MAXIMUM_NAME_BYTES and then Has_Room (L, Name'Length),
          Post => Valid (L) and then L.Count = L.Count'Old + 1 and then
                  L.Used = L.Used'Old + Name'Length and then L.Complete = L.Complete'Old;

   --  Readers. An Id beyond the listing, or a position beyond the name,
   --  reads as nothing: callers never index the arena themselves.
   function Is_Entry (L : Listing; Id : Entry_Id) return Boolean is
     (Valid (L) and then Id <= L.Count);
   function Length (L : Listing; Id : Entry_Id) return Name_Length is
     (if Is_Entry (L, Id) then L.Entries (Id).Length else 0);
   function Byte_At (L : Listing; Id : Entry_Id; Position : Name_Position) return Unsigned_8;
   function Name (L : Listing; Id : Entry_Id) return String
     with Post => Name'Result'First = 1 and then Name'Result'Length = Length (L, Id);
   function Facts (L : Listing; Id : Entry_Id) return Entry_Facts is
     (if Is_Entry (L, Id) then L.Entries (Id).Facts else (others => <>));
   function Kind (L : Listing; Id : Entry_Id) return Entry_Kind is (Facts (L, Id).Kind);
   function Prefix (L : Listing; Id : Entry_Id) return Sort_Prefix is
     (if Is_Entry (L, Id) then L.Entries (Id).Prefix else 0);
   function Extension_At (L : Listing; Id : Entry_Id) return Name_Length is
     (if Is_Entry (L, Id) then L.Entries (Id).Extension_At else 0);

   --  ASCII case folding (lower case: '_' then sorts before letters, as
   --  Explorer does); other bytes, UTF-8 included, compare as they are.
   function Fold (B : Unsigned_8) return Unsigned_8 is
     (if B in Character'Pos ('A') .. Character'Pos ('Z') then B + (Character'Pos ('a') - Character'Pos ('A'))
      else B);
   function Is_Digit (B : Unsigned_8) return Boolean is
     (B in Character'Pos ('0') .. Character'Pos ('9'));
   function Prefix_Of (Name : Name_Bytes) return Sort_Prefix;
end Files_Listing;
