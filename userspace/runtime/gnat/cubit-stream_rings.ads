------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Broadcast stream rings (docs/ccl-streams.md, "The ring underneath"): one
--  producer, any number of readers, and a producer that never waits for a
--  reader. Program outlets (unix.stdout, typed outlets) use them.
--
--  @description
--  Records are CuBit.Datagram_Rings records in a CuBit.Channel_Rings byte
--  ring. The producer's state is a Channel_Rings.Producer whose consumed
--  index is OLDEST: the start of the oldest whole record still in the ring.
--  To make room it moves OLDEST past whole records (Publish), so a slow
--  reader loses the oldest records rather than holding the producer.
--
--  A reader keeps its own cursor (readers' grants are read-only). Read takes
--  the record at the cursor from a snapshot of PRODUCED and OLDEST; a cursor
--  OLDEST has passed resumes at OLDEST and reports the loss. The producer
--  may overwrite a record while a reader copies it, so after copying, the
--  reader checks Intact against fresh indices and discards a record that is
--  no longer whole.
--
--  The region: a control page (one word per cache line: PRODUCED, OLDEST,
--  ENDED), then the data ring. The adapters publish OLDEST before writing
--  bytes and PRODUCED after; that ordering, like the region's mapping, is
--  theirs.
--
--  Proved (tests/channel-rings): every access stays in the ring and the
--  caller's buffers, indices stay valid, eviction moves over whole records
--  (or empties the ring when it finds a malformed one), a record is put
--  whole or not at all, and Publish terminates.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Rings; use CuBit.Channel_Rings;
with CuBit.Datagram_Rings;

package CuBit.Stream_Rings with Pure, SPARK_Mode is

   PAGE_BYTES : constant := 4_096;
   CONTROL_BYTES : constant := PAGE_BYTES;
   PRODUCED_OFFSET : constant := 0;
   OLDEST_OFFSET   : constant := 64;
   ENDED_OFFSET    : constant := 128;
   --  The cursor of the region's owner when it reads in place (a launcher
   --  reading its child's outlet); producers and other readers ignore it.
   OWNER_CURSOR_OFFSET : constant := 192;
   --  The stream's element type (a CuBit.Streams.TypeTag), set once when
   --  the ring is made.
   ELEMENT_OFFSET : constant := 256;
   ELEMENT_RAW_BYTES : constant := 16#0000#;
   ELEMENT_TEXT_LINE : constant := 16#0001#;

   --  A connector declares its ring in pages (1 .. 255). The data ring is
   --  the largest power of two of those pages, after the control page.
   subtype Declared_Pages is Positive range 1 .. 255;
   function Ring_Pages (Declared : Declared_Pages) return Positive is
     (if Declared >= 128 then 128 elsif Declared >= 64 then 64
      elsif Declared >= 32 then 32 elsif Declared >= 16 then 16
      elsif Declared >= 8 then 8 elsif Declared >= 4 then 4
      elsif Declared >= 2 then 2 else 1)
   with Post => Ring_Pages'Result <= Declared and then Ring_Pages'Result <= 128;
   function Ring_Bytes (Declared : Declared_Pages) return Ring_Size is
     (Ring_Pages (Declared) * PAGE_BYTES);
   --  The whole region: the control page and the ring.
   function Region_Pages (Declared : Declared_Pages) return Positive is
     (1 + Ring_Pages (Declared));

   --  The ring bytes a record of Length payload bytes takes.
   function Record_Bytes (Length : Datagram_Rings.Payload_Length) return Positive
     renames Datagram_Rings.Record_Bytes;

   --  The largest record a ring of Size takes: half of it, so that a record
   --  always fits once older ones are evicted, wherever the ring stands.
   function Fits (Size : Ring_Size; Length : Natural) return Boolean is
     (Length <= Datagram_Rings.Maximum_Payload
      and then Record_Bytes (Length) <= Size / 2);

   --  The largest payload a ring of Size takes in one record; a writer
   --  splits longer data.
   function Largest_Payload (Size : Ring_Size) return Natural is
     (Natural'Min (Datagram_Rings.Maximum_Payload, Size / 2 - Datagram_Rings.Header_Bytes))
   with Post => Largest_Payload'Result > 0 and then Fits (Size, Largest_Payload'Result);

   --  The producer: Produced is PRODUCED, Consumed (P) is OLDEST.
   function New_Writer (Size : Ring_Size) return Producer is
     (New_Producer (Size));

   --  Whether Datagram_Rings.Put takes a record of Needed ring bytes now:
   --  the free bytes from PRODUCED to the end, or (padding the end) the free
   --  bytes from the start of the ring.
   function Room (P : Producer; Needed : Positive) return Boolean
   with Pre => Valid (P);

   type Publish_Result is (Published, Too_Large);

   --  Move OLDEST past as many old records as it takes (Evicted bytes) for
   --  a record of Length payload bytes to fit, without writing the ring.
   --  The adapters publish the new OLDEST before Put changes those bytes.
   --  Too_Large: the record cannot go in; the caller drops P.
   procedure Make_Room
     (P : in out Producer; Ring : Bytes; Length : Natural;
      Evicted : out Natural; Result : out Publish_Result)
   with
     Pre  => Valid (P) and then Ring'First = 0 and then Ring'Last = P.Size - 1,
     Post => Valid (P) and then P.Size = P'Old.Size
             and then P.Produced = P'Old.Produced
             and then (if Result = Published then Fits (P.Size, Length))
             and then (if not Fits (P'Old.Size, Length)
                       then Result = Too_Large and then P = P'Old and then Evicted = 0);

   --  Put Data as one record, first moving OLDEST past as many old records
   --  as it takes (Make_Room). Too_Large: nothing is written.
   procedure Publish
     (P : in out Producer; Ring : in out Bytes; Data : Bytes;
      Evicted : out Natural; Result : out Publish_Result)
   with
     Pre  => Valid (P)
             and then Ring'First = 0 and then Ring'Last = P.Size - 1
             and then Data'Last < Natural'Last,
     Post => Valid (P) and then P.Size = P'Old.Size
             and then (if not Fits (P'Old.Size, Data'Length)
                       then Result = Too_Large and then P = P'Old and then Evicted = 0);

   type Read_Result is (Taken, Empty, Malformed);

   --  The record at Cursor, given PRODUCED and OLDEST as last read. Lost:
   --  OLDEST had passed Cursor (or the indices made no sense for it), so the
   --  read resumed at OLDEST. Cursor moves past what was read.
   procedure Read
     (Cursor : in out Index; Size : Ring_Size; Ring : Bytes;
      Produced, Oldest : Index; Into : in out Bytes;
      Length : out Natural; Truncated, Lost : out Boolean;
      Result : out Read_Result)
   with
     Pre  => Ring'First = 0 and then Ring'Last = Size - 1
             and then Into'Last < Natural'Last,
     Post => Length <= Into'Length
             and then (if Result /= Taken then Length = 0 and then not Truncated);

   --  Whether the record read from Start is still whole by PRODUCED and
   --  OLDEST as they are now: OLDEST has not passed Start.
   function Intact (Start, Produced, Oldest : Index) return Boolean is
     (Distance (Start, Produced) <= Distance (Oldest, Produced));

end CuBit.Stream_Rings;
