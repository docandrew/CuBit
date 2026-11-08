------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A read-only Bible image (APP-005): one translation compiled at build
--  time by tools/build_bible.py into fixed-width little-endian records
--  (layout in that tool's header comment): the books, chapters and verses,
--  each verse's segments (headings, paragraphs, poetry lines) in one UTF-8
--  text table, and a search index of every word to the verses holding it.
--  Nothing here parses text.
--
--  Open checks the header once: the magic, the format version, the number
--  of book slots, and that every table lies inside the image. Index then
--  reads the books and chapters into a Versification and every chapter's
--  first verse row, checking each against the tables. Each record read
--  afterwards checks the references it carries before handing them out, so
--  a damaged image yields Valid => False on the record concerned, never an
--  access outside the image.
--
--  Proved (SPARK, level 2): no run-time errors on any image (the units are
--  compiled -gnatp), and every reference a valid record returns lies inside
--  its table or the text.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Bible_Canon; use Bible_Canon;
with Bible_References; use Bible_References;

package Bible_Images with SPARK_Mode, Pure is

   --  Offsets on disk are 32-bit; Open refuses an image larger than this.
   MAXIMUM_IMAGE_BYTES : constant := 2 ** 31 - 2;
   subtype Image_Size is Natural range 0 .. MAXIMUM_IMAGE_BYTES;
   subtype Image_Offset is Image_Size range 0 .. MAXIMUM_IMAGE_BYTES - 1;
   type Byte_Array is array (Image_Offset range <>) of Unsigned_8;

   --  The longest text run tools/build_bible.py writes (one segment; the
   --  Greek Esther's royal letters are near 4 KiB); longer is damage.
   MAXIMUM_SEGMENT_BYTES : constant := 8_192;
   subtype Segment_Length is Natural range 0 .. MAXIMUM_SEGMENT_BYTES;
   --  Segments of one verse (headings and lines).
   MAXIMUM_VERSE_SEGMENTS : constant := 64;
   subtype Segment_Count is Natural range 0 .. MAXIMUM_VERSE_SEGMENTS;
   --  A search key (Bible_Search.MAXIMUM_KEY).
   MAXIMUM_KEY_BYTES : constant := 32;
   subtype Key_Length is Natural range 0 .. MAXIMUM_KEY_BYTES;
   --  An about text (the licence statement is the longest).
   MAXIMUM_ABOUT_BYTES : constant := 4_096;

   type Section is (Books, Chapters, Verses, Segments, Words, Postings, Keys, Text, About);

   type Width_Table is array (Section) of Positive;
   WIDTH : constant Width_Table :=
     [Books => 8, Chapters => 8, Verses => 8, Segments => 8, Words => 16, Postings => 4, Keys => 1,
      Text => 1, About => 8];

   --  2: the about table names the translation's language (APP-005 v1.1).
   FORMAT_VERSION : constant := 2;
   HEADER_BYTES : constant := 8 + 4 + 4 + 8 * 9;

   type Extent is record
      Offset : Image_Size := 0;
      Count  : Image_Size := 0;
   end record;
   type Extent_Array is array (Section) of Extent;

   type Layout is record
      Length   : Image_Size := 0;
      Sections : Extent_Array := [others => (0, 0)];
   end record;

   --  Every table lies inside the image.
   function Fits (L : Layout) return Boolean is
     (for all S in Section =>
        L.Sections (S).Offset <= L.Length
        and then Long_Long_Integer (L.Sections (S).Count) * Long_Long_Integer (WIDTH (S))
                 <= Long_Long_Integer (L.Length - L.Sections (S).Offset));

   type Open_Status is (Opened, Too_Short, Too_Large, Bad_Magic, Wrong_Version, Wrong_Canon, Bad_Table);

   procedure Open (Data : Byte_Array; Result : out Layout; Status : out Open_Status)
     with Pre  => Data'First = 0,
          Post => (if Status = Opened then Fits (Result) and then Result.Length = Data'Length
                   and then Result.Sections (Books).Count = BOOK_COUNT);

   --  The image size a header declares (the about table is last), from the
   --  first HEADER_BYTES of a file: a reader allocates once, then fills.
   procedure Declared_Size (Header : Byte_Array; Size : out Image_Size; Status : out Open_Status)
     with Pre => Header'First = 0 and then Header'Length >= HEADER_BYTES;

   --  An image to read: its layout matches Data.
   function Usable (Data : Byte_Array; L : Layout) return Boolean is
     (Data'First = 0 and then L.Length = Data'Length and then Fits (L)
      and then L.Sections (Books).Count = BOOK_COUNT);

   function Count (L : Layout; S : Section) return Image_Size is (L.Sections (S).Count);

   ---------------------------------------------------------------------------
   --  Books, chapters and verses.
   ---------------------------------------------------------------------------
   type Chapter_Starts is array (Chapter_Slot) of Image_Size;

   --  The translation's versification, and where each chapter's verses are:
   --  chapter C of book B starts at verse row First_Verse (Before (B) + C).
   type Book_Index is record
      Table       : Versification;
      First_Verse : Chapter_Starts := [others => 0];
   end record;

   function Indexed (L : Layout; I : Book_Index) return Boolean is
     (Valid (I.Table)
      and then (for all S in Chapter_Slot =>
                  I.First_Verse (S) <= Count (L, Verses)
                  and then Image_Size (I.Table.Verses (S)) <= Count (L, Verses) - I.First_Verse (S)));

   type Index_Status is (Index_Read, Bad_Book, Bad_Chapter, Too_Many_Chapters);

   procedure Index (Data : Byte_Array; L : Layout; Result : out Book_Index; Status : out Index_Status)
     with Pre  => Usable (Data, L),
          Post => Indexed (L, Result);

   --  The verse row of a reference the translation numbers.
   function Verse_Row (L : Layout; I : Book_Index; Item : Reference) return Image_Size is
     (I.First_Verse (I.Table.Before (Item.Book) + Item.Chapter) + Item.Verse - 1)
   with Pre  => Indexed (L, I) and then Exists (I.Table, Item),
        Post => Verse_Row'Result < Count (L, Verses);

   --  The reference of verse row V (Valid False if no chapter holds it).
   procedure Locate (L : Layout; I : Book_Index; V : Image_Size; Item : out Reference; Found : out Boolean)
     with Pre  => Indexed (L, I),
          Post => (if Found then Exists (I.Table, Item) and then Verse_Row (L, I, Item) = V);

   ---------------------------------------------------------------------------
   --  Records.
   ---------------------------------------------------------------------------

   --  A slice of the image, absolute.
   type Text_Slice is record
      First  : Image_Size := 0;
      Length : Image_Size := 0;
   end record;
   function Inside (Data : Byte_Array; T : Text_Slice) return Boolean is
     (T.First <= Data'Length and then T.Length <= Data'Length - T.First);

   --  How a run of text sits on the page. On disk 0 .. 9 in this order.
   type Segment_Kind is
     (Prose,             --  continues the paragraph or line before it
      Paragraph,         --  starts a paragraph (first line indented)
      Flush_Paragraph,   --  starts a paragraph at the margin
      Poetry_1,          --  starts a poetry line, indented one step
      Poetry_2,          --  ... two steps
      Poetry_3,          --  ... three steps
      Section_Heading,   --  a heading above the verse (not verse text)
      Major_Heading,     --  a major division ("BOOK II" in the Psalms)
      Psalm_Title,       --  a psalm's superscription ("A Psalm of David.")
      Prologue);         --  prose before a chapter's first verse (Sirach)

   function Is_Verse_Text (Kind : Segment_Kind) return Boolean is (Kind in Prose .. Poetry_3);

   type Verse_Record is record
      Valid         : Boolean := False;
      First_Segment : Image_Size := 0;
      Segments      : Segment_Count := 0;   --  0: the translation has no text here
   end record;

   type Segment_Record is record
      Valid        : Boolean := False;
      Kind         : Segment_Kind := Prose;
      Stanza_Break : Boolean := False;      --  a blank line before it
      --  The words of Christ (USFM \wj): red letters where the reader wants.
      Words_Of_Christ : Boolean := False;
      --  No space between it and the run before (a style changed mid-line).
      Joined       : Boolean := False;
      Content      : Text_Slice;
   end record;

   type Word_Record is record
      Valid         : Boolean := False;
      Key           : Text_Slice;
      First_Posting : Image_Size := 0;
      Postings      : Image_Size := 0;
   end record;

   function Read_Verse (Data : Byte_Array; L : Layout; V : Image_Size) return Verse_Record
     with Pre  => Usable (Data, L) and then V < Count (L, Verses),
          Post => (if Read_Verse'Result.Valid then
                     Read_Verse'Result.First_Segment <= Count (L, Segments)
                     and then Image_Size (Read_Verse'Result.Segments)
                              <= Count (L, Segments) - Read_Verse'Result.First_Segment);

   function Read_Segment (Data : Byte_Array; L : Layout; S : Image_Size) return Segment_Record
     with Pre  => Usable (Data, L) and then S < Count (L, Segments),
          Post => (if Read_Segment'Result.Valid then
                     Inside (Data, Read_Segment'Result.Content)
                     and then Read_Segment'Result.Content.Length <= MAXIMUM_SEGMENT_BYTES);

   function Read_Word (Data : Byte_Array; L : Layout; W : Image_Size) return Word_Record
     with Pre  => Usable (Data, L) and then W < Count (L, Words),
          Post => (if Read_Word'Result.Valid then
                     Inside (Data, Read_Word'Result.Key)
                     and then Read_Word'Result.Key.Length <= MAXIMUM_KEY_BYTES
                     and then Read_Word'Result.First_Posting <= Count (L, Postings)
                     and then Read_Word'Result.Postings
                              <= Count (L, Postings) - Read_Word'Result.First_Posting);

   --  Posting R: a verse row (Valid False if out of range).
   procedure Read_Posting (Data : Byte_Array; L : Layout; R : Image_Size; Verse : out Image_Size;
                           Valid : out Boolean)
     with Pre  => Usable (Data, L) and then R < Count (L, Postings),
          Post => (if Valid then Verse < Count (L, Verses));

   --  What the about table says of the translation: its language as an ISO
   --  639-1 code ("la") and an English name ("Latin").
   type About_Kind is (Abbreviation, Title, Licence, Language_Code, Language_Name);

   procedure Read_About (Data : Byte_Array; L : Layout; Kind : About_Kind; Content : out Text_Slice;
                         Valid : out Boolean)
     with Pre  => Usable (Data, L),
          Post => (if Valid then Inside (Data, Content) and then Content.Length <= MAXIMUM_ABOUT_BYTES);

end Bible_Images;
