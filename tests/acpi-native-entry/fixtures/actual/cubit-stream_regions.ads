------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A CuBit.Stream_Rings region in memory (docs/ccl-streams.md, "The ring
--  underneath"): the control page's words, the order a producer publishes
--  in, and a reader's checked copy. CuBit.Streams, the libc's outlets and
--  launchers reading a child's outlet all go through here, so they agree.
--
--  @description
--  Publishing: OLDEST (after Make_Room), a fence, the record, a fence,
--  PRODUCED. A reader takes PRODUCED and OLDEST, copies the record, and
--  keeps it only if OLDEST has not passed its start meanwhile
--  (Stream_Rings.Intact): bytes change only after OLDEST says so.
--
--  The ring's size always comes from the pages declared for the region,
--  never from the region itself, which its producer can write.
--
--  Needs no Ada run-time library (the libc uses it).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Channel_Rings;
with CuBit.Stream_Rings;

package CuBit.Stream_Regions is

   package Rings renames CuBit.Channel_Rings;
   package SR renames CuBit.Stream_Rings;

   --  A connector's declared pages, as a ring's: clamped to 1 .. 255.
   function Declared (Pages : Natural) return SR.Declared_Pages is
     (if Pages < 1 then 1 elsif Pages > SR.Declared_Pages'Last then SR.Declared_Pages'Last
      else Pages);

   --  The region's bytes for Pages declared pages.
   function Region_Bytes (Pages : Natural) return Unsigned_64 is
     (Unsigned_64 (SR.Region_Pages (Declared (Pages))) * SR.PAGE_BYTES);

   --  An empty region at Base: control page zeroed, its element type set.
   procedure Initialize (Base : Unsigned_64; Element : Unsigned_16);

   --  A producer for the region at Base (Pages declared), from where its
   --  words stand: a ring this process was lent and now produces into.
   function Writer_Of (Base : Unsigned_64; Pages : Natural) return Rings.Producer;

   --  Put Length bytes at Data as one record, evicting the oldest ones as
   --  needed. False: too large for the ring (nothing written).
   function Write
     (Base : Unsigned_64; Writer : in out Rings.Producer;
      Data : System.Address; Length : Natural) return Boolean;

   --  The next record from Cursor into Buffer (at most Maximum bytes; the
   --  rest of a longer one is dropped): its length, 0 when none is ready.
   --  A record overwritten while copied is dropped and the cursor resumes
   --  at OLDEST.
   function Read
     (Base : Unsigned_64; Size : Rings.Ring_Size; Cursor : in out Rings.Index;
      Buffer : System.Address; Maximum : Natural) return Natural;

   --  The region owner's read in place (a launcher reading its child's
   --  outlet): Read from the cursor kept in the control page.
   function Read_Owned
     (Base : Unsigned_64; Pages : Natural; Buffer : System.Address;
      Maximum : Natural) return Natural;

   function Produced (Base : Unsigned_64) return Rings.Index;
   function Element (Base : Unsigned_64) return Unsigned_16;

end CuBit.Stream_Regions;
