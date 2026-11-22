------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's map of its buffered writes (docs/filesystem-data-plane.md,
--  "Write delegations"; docs/c-removal.md): which dirty-arena entry holds a
--  handle's page, by an open-addressed table over (handle tag, page).
--
--  @description
--  The arena's entries are shared with the filesystem service, which frees
--  an entry (sequence 0) when it takes the page; the map turns such an
--  entry into a tombstone when it next meets it. Entries are passed in as
--  a snapshot, so nothing the service writes can move an index out of
--  range. Proved (tests/libc-ada): every probe stays in the map, every
--  entry index in the arena, and the counts in their bounds.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Filesystem_Queues;

package CuBit.Libc_Dirty_Map with Pure, SPARK_Mode is

   package FQ renames CuBit.Filesystem_Queues;

   subtype Entry_Index is Natural range 0 .. FQ.Dirty_Entries - 1;

   --  One dirty-arena entry (FQ.Dirty_*_At).
   type Dirty_Entry is record
      Sequence : Unsigned_32;       --  0 free, odd being written, even held
      Tag      : Unsigned_32;       --  the handle's slot and generation bits
      Page     : Unsigned_32;       --  page within the file
      Start    : Unsigned_16;
      Stop     : Unsigned_16;
   end record;
   for Dirty_Entry use record
      Sequence at FQ.Dirty_Sequence_At range 0 .. 31;
      Tag      at FQ.Dirty_Slot_At     range 0 .. 31;
      Page     at FQ.Dirty_Page_At     range 0 .. 31;
      Start    at FQ.Dirty_Start_At    range 0 .. 15;
      Stop     at FQ.Dirty_Stop_At     range 0 .. 15;
   end record;
   for Dirty_Entry'Size use FQ.Dirty_Entry_Bytes * 8;
   type Entry_Table is array (Entry_Index) of Dirty_Entry;

   Map_Slots : constant := 2 * FQ.Dirty_Entries;     --  a power of two
   subtype Map_Index is Natural range 0 .. Map_Slots - 1;
   --  0 empty, -1 a tombstone (a taken entry), else the entry + 1.
   subtype Map_Value is Integer range -1 .. FQ.Dirty_Entries;
   Empty : constant Map_Value := 0;
   Tombstone : constant Map_Value := -1;
   type Map_Values is array (Map_Index) of Map_Value;

   type Map is record
      Values     : Map_Values := [others => Empty];
      Tombstones : Natural range 0 .. Map_Slots := 0;
      Used       : Natural range 0 .. FQ.Dirty_Entries := 0;  --  as of our last look
   end record;

   --  A handle's tag (FQ.Dirty_Slot_At): its slot, and the low bits of its
   --  generation above it.
   function Tag_Of (Handle : Unsigned_64; Slot : Natural) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Shift_Right (Handle, 32) and (2 ** FQ.Tag_Generation_Bits - 1)),
                  FQ.Tag_Slot_Bits)
      or Unsigned_32 (Slot mod 2 ** FQ.Tag_Slot_Bits));

   --  The entry holding (Tag, Page) as we left it, or -1; a free one met on
   --  the way becomes a tombstone.
   procedure Find (M : in out Map; Entries : Entry_Table; Tag, Page : Unsigned_32;
                   Found : out Integer)
   with Post => Found in -1 .. FQ.Dirty_Entries - 1;

   procedure Remember (M : in out Map; Tag, Page : Unsigned_32; E : Entry_Index);

   --  The map of the entries still held in the arena.
   procedure Rebuild (M : out Map; Entries : Entry_Table);

end CuBit.Libc_Dirty_Map;
