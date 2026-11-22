------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Directory.Page.V2 (docs/filesystem-protocol-v2.md step 3): one 4 KiB page
--  of a directory listing, packed: each entry carries its name and what the
--  volume records about it (kind, size, times, mode, links, owner, object
--  identity), so a listing needs no stat per entry. The service writes pages
--  with Start, Append and Finish; clients copy a page out of shared memory
--  and then check it (Check) before using any entry (Get).
--
--  @description
--  Layout, little-endian, all offsets in bytes:
--    Header (Header_Bytes): version (u16), header bytes (u16), entry count
--      (u16), bytes used (u16, header and entries), flags (u32, Page_End),
--      reserved (u32, zero), resume token (u64), change stamp (u64).
--    Entries from Header_Bytes on, each Record_Bytes (its name length) long
--      (8-byte multiples): record bytes (u16), name length (u8), kind (u8),
--      valid bits (u32), object (u64), size (u64), modified, changed and
--      accessed (u64 milliseconds since the Unix epoch), mode, links,
--      owner and group (u32 each), then the name.
--  The resume token is opaque: handed back (Queue_Seek_Directory), it
--  resumes the listing at the next entry. The change stamp is the service's
--  namespace generation when the page was read.
--
--  Proved (tests/directory-pages, level 2): no run-time errors for any page
--  contents or offset; Append keeps the writer within the page and in step
--  with the bytes it wrote. Tested: every page the writer makes is accepted
--  by Check and reads back the same entries; bad pages are refused.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Directory_Pages with Pure, SPARK_Mode is

   Page_Bytes         : constant := 4_096;
   Version            : constant := 2;
   Header_Bytes       : constant := 32;
   Fixed_Entry_Bytes  : constant := 64;
   Record_Alignment   : constant := 8;
   Maximum_Name_Bytes : constant := 255;

   --  Header fields.
   Version_At      : constant := 0;
   Header_Bytes_At : constant := 2;
   Count_At        : constant := 4;
   Used_At         : constant := 6;
   Flags_At        : constant := 8;
   Reserved_At     : constant := 12;
   Resume_At       : constant := 16;
   Stamp_At        : constant := 24;
   --  Flags: the page's last entry is the directory's last.
   Page_End : constant := 1;

   --  Entry fields.
   Record_Bytes_At : constant := 0;
   Name_Length_At  : constant := 2;
   Kind_At         : constant := 3;
   Valid_At        : constant := 4;
   Object_At       : constant := 8;
   Size_At         : constant := 16;
   Modified_At     : constant := 24;
   Changed_At      : constant := 32;
   Accessed_At     : constant := 40;
   Mode_At         : constant := 48;
   Links_At        : constant := 52;
   Owner_At        : constant := 56;
   Group_At        : constant := 60;
   Name_At         : constant := Fixed_Entry_Bytes;

   --  Kinds (the wire's u8).
   Kind_Unknown   : constant := 0;
   Kind_File      : constant := 1;
   Kind_Directory : constant := 2;
   Kind_Symlink   : constant := 3;
   Last_Kind      : constant := Kind_Symlink;

   --  Valid bits: which facts the volume filled (others are zero).
   Valid_Size   : constant := 1;
   Valid_Times  : constant := 2;
   Valid_Mode   : constant := 4;
   Valid_Links  : constant := 8;
   Valid_Owner  : constant := 16;
   Valid_Object : constant := 32;

   Largest_Record  : constant := Fixed_Entry_Bytes + Maximum_Name_Bytes + 1;   --  320
   Smallest_Record : constant := Fixed_Entry_Bytes + Record_Alignment;          --  72
   Maximum_Entries : constant := (Page_Bytes - Header_Bytes) / Smallest_Record;  --  56

   subtype Page_Index is Natural range 0 .. Page_Bytes - 1;
   type Page is array (Page_Index) of Unsigned_8;

   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Name_Index is Positive range 1 .. Maximum_Name_Bytes;
   type Name_Bytes is array (Name_Index) of Unsigned_8;
   subtype Entry_Count is Natural range 0 .. Maximum_Entries;
   subtype Used_Bytes is Natural range Header_Bytes .. Page_Bytes;
   subtype Record_Size is Positive range Smallest_Record .. Largest_Record;
   --  Where an entry may start (within the entries' part of a page).
   subtype Entry_Offset is Natural range Header_Bytes .. Page_Bytes;

   type Facts is record
      Kind     : Unsigned_8 := Kind_Unknown;
      Valid    : Unsigned_32 := 0;
      Object   : Unsigned_64 := 0;
      Size     : Unsigned_64 := 0;
      Modified : Unsigned_64 := 0;
      Changed  : Unsigned_64 := 0;
      Accessed : Unsigned_64 := 0;
      Mode     : Unsigned_32 := 0;
      Links    : Unsigned_32 := 0;
      Owner    : Unsigned_32 := 0;
      Group    : Unsigned_32 := 0;
   end record;

   --  An entry's record: the fixed fields and its name, rounded up to 8.
   function Record_Bytes (Length : Name_Length) return Record_Size is
     ((Fixed_Entry_Bytes + Length + Record_Alignment - 1) / Record_Alignment * Record_Alignment)
   with Pre => Length >= 1;

   --  A name a listing may hold: 1 .. 255 bytes, no '/' and no NUL, and
   --  neither "." nor "..".
   function Valid_Name (Name : Name_Bytes; Length : Name_Length) return Boolean is
     (Length >= 1 and then
      (for all I in 1 .. Length => Name (I) /= 0 and then Name (I) /= Character'Pos ('/'))
      and then not (Length = 1 and then Name (1) = Character'Pos ('.'))
      and then not (Length = 2 and then Name (1) = Character'Pos ('.')
                    and then Name (2) = Character'Pos ('.')));

   ---------------------------------------------------------------------------
   --  Writing (the service).
   ---------------------------------------------------------------------------
   type Writer is record
      Count : Entry_Count := 0;
      Used  : Used_Bytes := Header_Bytes;
   end record;

   procedure Start (P : out Page; W : out Writer)
   with Post => W.Count = 0 and then W.Used = Header_Bytes;

   function Fits (W : Writer; Length : Name_Length) return Boolean is
     (Length >= 1 and then W.Count < Maximum_Entries
      and then W.Used <= Page_Bytes - Record_Bytes (Length));

   procedure Append
     (P : in out Page; W : in out Writer; Item : Facts; Name : Name_Bytes; Length : Name_Length)
   with Pre  => Fits (W, Length),
        Post => W.Count = W'Old.Count + 1 and then
                W.Used = W'Old.Used + Record_Bytes (Length);

   procedure Finish
     (P : in out Page; W : Writer; Ended : Boolean; Resume, Stamp : Unsigned_64);

   ---------------------------------------------------------------------------
   --  Reading (clients; the page is a private copy, never shared memory).
   ---------------------------------------------------------------------------

   --  The entry at Offset, checked on its own: its record lies within
   --  Limit (the page's bytes used) and its fields agree. Next: the
   --  following entry's offset. OK False: malformed (nothing else is
   --  meaningful then).
   procedure Get
     (P : Page; Offset : Natural; Limit : Natural; Item : out Facts;
      Name : out Name_Bytes; Length : out Name_Length; Next : out Natural;
      OK : out Boolean)
   with Post => (if OK then Length >= 1 and then Next = Offset + Record_Bytes (Length)
                   and then Next <= Limit and then Next <= Page_Bytes);

   --  Whether the page is a well-formed Directory.Page.V2: version and
   --  header size, Count entries walking from Header_Bytes, each well-formed
   --  (Get), with a valid name and a known kind, ending exactly at Used,
   --  and zeroed reserved fields. Only a Valid page's entries are used.
   procedure Check
     (P : Page; Valid : out Boolean; Count : out Entry_Count; Used : out Used_Bytes;
      Ended : out Boolean; Resume, Stamp : out Unsigned_64);

end CuBit.Directory_Pages;
