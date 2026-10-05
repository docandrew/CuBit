------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The bookkeeping of the libc's file page cache (docs/filesystem-data-
--  plane.md, "Read delegations"; docs/c-removal.md): which file and page
--  each cache slot holds, the hash chains that find them, and the clock
--  that picks a slot to reuse. The page bytes themselves are the caller's
--  (one 4 KiB buffer per slot).
--
--  @description
--  A file is cached by inode (volume << 32 | inode) and version: when the
--  service reports a new version, the file's epoch moves on and its pages
--  go stale at once, to be reused before the cache grows. Links are a slot
--  plus one, zero ending a chain, so a zeroed Cache is an empty one.
--
--  Proved (tests/libc-ada): every index and link stays in its table, every
--  walk ends (bounded by the table size), and Find returns only a page of
--  the asked file, page number and current epoch.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Libc_File_Cache with Pure, SPARK_Mode is

   Maximum_Pages : constant := 32_768;        --  128 MiB of 4 KiB pages
   Page_Buckets  : constant := 65_536;        --  a power of two
   Cached_Files  : constant := 4_096;
   File_Buckets  : constant := 8_192;         --  a power of two
   --  Stale pages looked for before the cache grows; after a probe finds
   --  none, probing pauses until an epoch moves again.
   Stale_Probe_Pages : constant := 16;

   subtype Page_Slot is Natural range 0 .. Maximum_Pages - 1;
   subtype Page_Count is Natural range 0 .. Maximum_Pages;
   subtype Page_Link is Natural range 0 .. Maximum_Pages;     --  slot + 1
   subtype File_Slot is Natural range 0 .. Cached_Files - 1;
   subtype File_Count is Natural range 0 .. Cached_Files;
   subtype File_Link is Natural range 0 .. Cached_Files;      --  slot + 1
   subtype Page_Bucket is Natural range 0 .. Page_Buckets - 1;
   subtype File_Bucket is Natural range 0 .. File_Buckets - 1;

   No_Link : constant := 0;

   type File_Entry is record
      Inode   : Unsigned_64 := 0;       --  0: unused
      Version : Unsigned_64 := 0;       --  the version its pages hold
      Epoch   : Unsigned_32 := 0;       --  pages of other epochs are stale
      Next    : File_Link := No_Link;   --  hash chain
   end record;

   type Page_Entry is record
      File       : File_Link := No_Link;   --  the file + 1; 0: free
      Epoch      : Unsigned_32 := 0;
      Page       : Unsigned_64 := 0;       --  page number within the file
      Next       : Page_Link := No_Link;   --  hash chain
      Referenced : Boolean := False;
   end record;

   type File_Table is array (File_Slot) of File_Entry;
   type File_Heads is array (File_Bucket) of File_Link;
   type Page_Table is array (Page_Slot) of Page_Entry;
   type Page_Heads is array (Page_Bucket) of Page_Link;

   type Cache is record
      Files          : File_Table;
      File_Used      : File_Count := 0;
      File_Clock     : File_Slot := 0;
      File_Chains    : File_Heads;
      Pages          : Page_Table;
      --  Slots with a page buffer (the caller grows the buffers).
      Pages_Backed   : Page_Count := 0;
      Page_Clock     : Page_Slot := 0;
      Page_Chains    : Page_Heads;
      Stale_Possible : Boolean := False;
   end record;

   --  Files an open handle uses (never given to another inode).
   type File_Uses is array (File_Slot) of Boolean;

   function Is_Current (C : Cache; S : Page_Slot) return Boolean is
     (C.Pages (S).File /= No_Link
      and then C.Pages (S).Epoch = C.Files (C.Pages (S).File - 1).Epoch);

   --  The slot holding File's page Page at the file's current epoch,
   --  marked referenced; No_Link if none.
   procedure Find (C : in out Cache; File : File_Slot; Page : Unsigned_64;
                   Found : out Page_Link)
   with Post => (if Found /= No_Link then
                   C.Pages (Found - 1).File = File + 1
                   and then C.Pages (Found - 1).Page = Page
                   and then Is_Current (C, Found - 1));

   --  The cached file for Inode at Version (pages of another version go
   --  stale). A new one takes a free entry, or one In_Use leaves free;
   --  Found is False only if every entry is in use.
   procedure File_For
     (C : in out Cache; Inode, Version : Unsigned_64; In_Use : File_Uses;
      File : out File_Slot; Found : out Boolean)
   with Post => (if Found then C.Files (File).Inode = Inode
                   and then C.Files (File).Version = Version);

   --  A slot for File's page Page: stale, a new backed one (Can_Grow: the
   --  caller has a buffer for slot Pages_Backed), or taken from the clock.
   --  The slot is then File's page, current and referenced.
   procedure New_Page
     (C : in out Cache; File : File_Slot; Page : Unsigned_64; Can_Grow : Boolean;
      Slot : out Page_Link)
   with Post => (if Slot /= No_Link then
                   Slot - 1 < C.Pages_Backed
                   and then C.Pages (Slot - 1).File = File + 1
                   and then C.Pages (Slot - 1).Page = Page
                   and then Is_Current (C, Slot - 1));

   --  A new version of File (its own write moved it on): its pages have it.
   procedure Set_Version (C : in out Cache; File : File_Slot; Version : Unsigned_64);

end CuBit.Libc_File_Cache;
