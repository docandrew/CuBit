------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Filesystem change events (docs/filesystem-protocol-v2.md step 4): the
--  records the filesystem service puts in a client's event ring, one per
--  change a watch of the client sees, and the Rescan_Needed record that
--  stands in for events the ring had no room for.
--
--  @description
--  A record, little-endian: watch (u32), kind (u8), flags (u8), name length
--  (u16), object (u64), cookie (u64: a rename's two records share it, else
--  0), stamp (u64: the service's namespace generation after the change),
--  then the name: the changed entry's path relative to the watched folder
--  (one component for a folder watch, one or more for a subtree watch).
--  Rescan_Needed and Watch_Ended carry no name.
--
--  Proved (tests/filesystem-events, level 2): no run-time errors for any
--  bytes; Decode accepts only well-formed records. Tested: Decode reads
--  back what Encode wrote.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Filesystem_Events with Pure, SPARK_Mode is

   Header_Bytes : constant := 32;
   --  A relative path is at most a full path (CuBit.Directory_Paths).
   Maximum_Name_Bytes : constant := 4_096;
   Largest_Record : constant := Header_Bytes + Maximum_Name_Bytes;
   --  Watches one client may hold.
   Maximum_Watches : constant := 16;

   Watch_At       : constant := 0;
   Kind_At        : constant := 4;
   Flags_At       : constant := 5;
   Name_Length_At : constant := 6;
   Object_At      : constant := 8;
   Cookie_At      : constant := 16;
   Stamp_At       : constant := 24;
   Name_At        : constant := Header_Bytes;

   type Event_Kind is
     (Created, Removed, Renamed_From, Renamed_To, Modified,
      --  The ring had no room for some of this watch's events: they are
      --  not coming. Rescan the folder (or subtree), then go on.
      Rescan_Needed,
      --  The watched folder is gone (removed, or moved: the watch named it
      --  by its path). No more records for this watch.
      Watch_Ended);
   for Event_Kind use
     (Created => 1, Removed => 2, Renamed_From => 3, Renamed_To => 4, Modified => 5,
      Rescan_Needed => 6, Watch_Ended => 7);
   Kind_Codes : constant array (Event_Kind) of Unsigned_8 :=
     [Created => 1, Removed => 2, Renamed_From => 3, Renamed_To => 4, Modified => 5,
      Rescan_Needed => 6, Watch_Ended => 7];

   --  Flags.
   Is_Directory : constant := 1;

   subtype Watch_Number is Positive range 1 .. Maximum_Watches;
   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Record_Length is Natural range Header_Bytes .. Largest_Record;
   subtype Record_Index is Natural range 0 .. Largest_Record - 1;
   type Record_Bytes is array (Record_Index) of Unsigned_8;
   subtype Name_Index is Positive range 1 .. Maximum_Name_Bytes;
   type Name_Bytes is array (Name_Index) of Unsigned_8;

   type Event is record
      Watch  : Watch_Number := 1;
      Kind   : Event_Kind := Rescan_Needed;
      Flags  : Unsigned_8 := 0;
      Object : Unsigned_64 := 0;
      Cookie : Unsigned_64 := 0;
      Stamp  : Unsigned_64 := 0;
   end record;

   --  Kinds that name an entry.
   function Named (Kind : Event_Kind) return Boolean is
     (Kind in Created | Removed | Renamed_From | Renamed_To | Modified);

   Slash : constant := Character'Pos ('/');
   Dot   : constant := Character'Pos ('.');

   --  A relative path: nonempty, no NUL, no empty, "." or ".." component
   --  (so no leading, trailing or doubled '/').
   function Valid_Relative (Name : Name_Bytes; Length : Name_Length) return Boolean;

   procedure Encode
     (Item : Event; Name : Name_Bytes; Length : Name_Length;
      Into : out Record_Bytes; Used : out Record_Length)
   with Pre  => (if Named (Item.Kind) then Valid_Relative (Name, Length) else Length = 0),
        Post => Used = Header_Bytes + Length;

   procedure Decode
     (From : Record_Bytes; Used : Natural; Item : out Event;
      Name : out Name_Bytes; Length : out Name_Length; OK : out Boolean)
   with Post => (if OK then Used = Header_Bytes + Length and then
                   (if Named (Item.Kind) then Valid_Relative (Name, Length) else Length = 0));

end CuBit.Filesystem_Events;
