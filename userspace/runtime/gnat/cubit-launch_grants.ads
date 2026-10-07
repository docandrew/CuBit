------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Delegated places in a launch request (docs/self-hosting.md, item 4;
--  docs/process-arguments.md): filesystem names a launcher hands the child
--  it starts, each with rights, on top of what the child's own manifest
--  requests. procmgr checks every entry against what the launcher holds
--  (CuBit.Launch_Authority.Scope_Covered) and installs it for the child;
--  an entry the launcher does not hold refuses the whole launch.
--
--  Delegation is authority, unlike the launch block's arguments, so it
--  travels beside the block in the request and never reaches the child.
--
--  Layout, little-endian: u16 version (1), u16 count (1 .. 16), then per
--  entry u8 rights (Read 1, Write 2, Create 8: the filesystem's access
--  bits), u16 name length (1 .. 256), and the name: a qualified CuBit name
--  ('@', a volume, then components), at most 256 bytes, no NUL.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Launch_Grants with Pure, SPARK_Mode is

   Version       : constant := 1;
   Header_Bytes  : constant := 4;
   Maximum_Grants : constant := 16;
   --  CuBit.Launch_Authority.Maximum_Prefix_Bytes: procmgr's access entries,
   --  any path the filesystem service takes.
   Maximum_Name_Bytes : constant := 256;
   Entry_Header_Bytes : constant := 3;
   Maximum_Bytes : constant :=
     Header_Bytes + Maximum_Grants * (Entry_Header_Bytes + Maximum_Name_Bytes);

   Read_Right   : constant Unsigned_8 := 1;
   Write_Right  : constant Unsigned_8 := 2;
   Create_Right : constant Unsigned_8 := 8;
   All_Rights   : constant Unsigned_8 := Read_Right or Write_Right or Create_Right;

   subtype Grant_Count is Natural range 0 .. Maximum_Grants;
   subtype Byte_Count is Natural range 0 .. Maximum_Bytes;
   subtype Name_Length is Positive range 1 .. Maximum_Name_Bytes;

   type Bytes is array (Positive range <>) of Unsigned_8;

   --  A name a grant may carry: qualified, printable, no NUL.
   function Valid_Name (Name : String) return Boolean is
     (Name'Length in Name_Length
      and then Name (Name'First) = '@'
      and then (for all C of Name => C in ' ' .. '~'));

   function Valid_Rights (Rights : Unsigned_8) return Boolean is
     (Rights /= 0 and then (Rights and not All_Rights) = 0);

   --  An entry's name length (u16 at Position + 1).
   function Length_At (Item : Bytes; Position : Positive) return Natural is
     (Natural (Item (Position + 1)) + 256 * Natural (Item (Position + 2)))
   with Pre => Item'First = 1 and then Position <= Maximum_Bytes
               and then Position + 2 <= Item'Last;

   --  Entries from Position on, Remaining of them, end exactly at the end.
   function Entries_Valid
     (Item : Bytes; Position : Positive; Remaining : Grant_Count)
      return Boolean
   is (if Remaining = 0 then Position = Item'Last + 1
       else Position + 2 <= Item'Last
            and then Valid_Rights (Item (Position))
            and then Length_At (Item, Position) in Name_Length
            and then Length_At (Item, Position) <= Item'Last - Position - 2
            and then Item (Position + 3) = Character'Pos ('@')
            and then (for all K in Position + 3 ..
                                   Position + 2 + Length_At (Item, Position) =>
                        Item (K) in 32 .. 126)
            and then Entries_Valid
              (Item, Position + 3 + Length_At (Item, Position), Remaining - 1))
   with Ghost,
        Pre => Item'First = 1 and then Item'Length <= Maximum_Bytes
               and then Position <= Item'Last + 1,
        Subprogram_Variant => (Decreases => Remaining);

   function Count_Of (Item : Bytes) return Natural is
     (Natural (Item (3)) + 256 * Natural (Item (4)))
   with Pre => Item'First = 1 and then Item'Length >= Header_Bytes;

   function Header_Valid (Item : Bytes) return Boolean is
     (Item'Length in Header_Bytes .. Maximum_Bytes
      and then Item (1) = Version and then Item (2) = 0
      and then Count_Of (Item) in 1 .. Maximum_Grants)
   with Pre => Item'First = 1;

   function Well_Formed (Item : Bytes) return Boolean is
     (Header_Valid (Item)
      and then Entries_Valid (Item, Header_Bytes + 1, Count_Of (Item)))
   with Ghost, Pre => Item'First = 1;

   function Valid (Item : Bytes) return Boolean
   with Pre  => Item'First = 1,
        Post => Valid'Result = Well_Formed (Item);

   --  Walk a valid region: Position starts at Header_Bytes + 1; each call
   --  yields one entry's rights and its name's bounds within Item.
   procedure Next
     (Item : Bytes; Position : in out Positive; Rights : out Unsigned_8;
      Name_First, Name_Last : out Positive)
   with Pre  => Item'First = 1 and then Item'Length <= Maximum_Bytes
                and then Position <= Maximum_Bytes
                and then Position + 2 <= Item'Last
                and then Length_At (Item, Position) in Name_Length
                and then Length_At (Item, Position) <= Item'Last - Position - 2,
        Post => Rights = Item (Position'Old)
                and then Name_First = Position'Old + 3
                and then Name_Last = Position'Old + 2 + Length_At (Item, Position'Old)
                and then Name_Last <= Item'Last
                and then Position = Name_Last + 1;

   --  OP_LAUNCH's fourth word: the priority in the low half; in the high
   --  half, the length of the grants region (after the name and the launch
   --  block in the lent grant) in bits 32 .. 47, and the length of the connector
   --  ring table after it (CuBit.Outlet_Rings) in bits 48 .. 63. Zero grant
   --  bytes: no delegation; zero ring bytes: no launcher-owned rings.
   pragma Compile_Time_Error (Maximum_Bytes > 16#FFFF#, "grants length exceeds 16 bits");
   function Request_Word
     (Priority : Unsigned_32; Grant_Bytes : Byte_Count; Ring_Bytes : Natural := 0)
     return Unsigned_64 is
     (Unsigned_64 (Priority) or Shift_Left (Unsigned_64 (Grant_Bytes), 32)
      or Shift_Left (Unsigned_64 (Ring_Bytes), 48))
   with Pre => Ring_Bytes <= 16#FFFF#;
   function Request_Priority (Word : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Word and 16#FFFF_FFFF#));
   function Request_Grant_Bytes (Word : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Word, 32) and 16#FFFF#);
   function Request_Ring_Bytes (Word : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Word, 48));
   function Grant_Bytes_Valid (Length : Unsigned_64) return Boolean is
     (Length = 0 or else Length in Header_Bytes .. Maximum_Bytes);

   ---------------------------------------------------------------------------
   --  OP_DELEGATED_PLACES: the places delegated to the requester when it was
   --  launched (not its own manifest's scopes), as a region, so that it can
   --  pass exactly those on to the programs it starts (the libc's
   --  posix_spawn; docs/self-hosting.md, decision D2). Seeing one's own
   --  grants needs no authority. The requester lends procmgr a writable
   --  grant of Maximum_Bytes:
   --    words (0) grant reference (CuBit.Grant_References wire form)
   --  Reply OK: words (0) = the region's length written there (0: none).
   ---------------------------------------------------------------------------
   Places_Operation     : constant := 16#010B#;
   Places_Request_Words : constant := 1;

   --  Building a region.
   type Builder is private;

   procedure Start (B : out Builder)
   with Post => Count (B) = 0;

   function Count (B : Builder) return Grant_Count;

   procedure Add (B : in out Builder; Rights : Unsigned_8; Name : String;
                  Added : out Boolean)
   with Pre  => Name'First = 1,
        Post => Added = (Count (B'Old) < Maximum_Grants
                         and then Valid_Rights (Rights) and then Valid_Name (Name))
                and then Count (B) = Count (B'Old) + (if Added then 1 else 0);

   --  The region: empty when nothing was added. (procmgr validates every
   --  region it receives; tests/launch-grants checks the builder's output
   --  with Valid.)
   procedure Finish (B : Builder; Region : out Bytes; Length : out Byte_Count)
   with Pre  => Region'First = 1 and then Region'Length = Maximum_Bytes,
        Post => (if Count (B) = 0 then Length = 0 else Length >= Header_Bytes);

private
   type Builder is record
      Data  : Bytes (1 .. Maximum_Bytes) := [others => 0];
      Used  : Byte_Count := Header_Bytes;
      Grants : Grant_Count := 0;
   end record
     with Predicate => Used >= Header_Bytes
                       and then Used <= Header_Bytes +
                                        Grants * (Entry_Header_Bytes + Maximum_Name_Bytes);

   function Count (B : Builder) return Grant_Count is (B.Grants);
end CuBit.Launch_Grants;
