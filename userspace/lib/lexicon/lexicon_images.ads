with Interfaces; use Interfaces;

--  The read-only lexicon image (docs/lexicon.md, "Image format"): WordNet
--  senses and SCOWL spellings compiled at build time by
--  tools/build_lexicon.py into fixed-width little-endian records in sorted,
--  indexed tables plus one text blob. Nothing here parses text.
--
--  Open checks the header once: the magic, the format version, and that
--  every table lies inside the image. Each record read afterwards checks
--  the references it carries (a text slice, a range in another table)
--  before handing them out, so a damaged image yields Valid => False on
--  the record concerned, never an access outside the image.
--
--  Proved: no run-time errors (the units are compiled -gnatp), and every
--  reference a valid record returns lies inside its table or the text.
package Lexicon_Images with SPARK_Mode, Pure is

   --  Offsets on disk are 32-bit; Open refuses an image larger than this.
   MAXIMUM_IMAGE_BYTES : constant := 2 ** 31 - 2;
   subtype Image_Size is Natural range 0 .. MAXIMUM_IMAGE_BYTES;
   subtype Image_Offset is Image_Size range 0 .. MAXIMUM_IMAGE_BYTES - 1;
   type Byte_Array is array (Image_Offset range <>) of Unsigned_8;

   --  Table rows (each table's own index type, so they cannot be mixed).
   type Word_Id is range 0 .. MAXIMUM_IMAGE_BYTES;
   type Sense_Id is range 0 .. MAXIMUM_IMAGE_BYTES;
   type Synset_Id is range 0 .. MAXIMUM_IMAGE_BYTES;
   type Row is range 0 .. MAXIMUM_IMAGE_BYTES;

   --  Key and display spellings are at most 255 bytes (one byte on disk).
   MAXIMUM_WORD_BYTES : constant := 255;
   subtype Word_Length is Natural range 0 .. MAXIMUM_WORD_BYTES;

   --  The longest gloss or example (tools/build_lexicon.py refuses longer
   --  ones; Read_Synset and Read_Example treat them as damage). WordNet
   --  3.0's longest gloss is 505 bytes.
   MAXIMUM_TEXT_ITEM_BYTES : constant := 1_024;
   subtype Text_Item_Length is Natural range 0 .. MAXIMUM_TEXT_ITEM_BYTES;

   type Section is (Words, Senses, Synsets, Members, Relations, Antonyms, Examples, Exceptions,
                    Text);

   --  Bytes per record of each table (docs/lexicon.md); Text is bytes.
   type Width_Table is array (Section) of Positive;
   WIDTH : constant Width_Table :=
     [Words => 12, Senses => 18, Synsets => 23, Members => 4, Relations => 5, Antonyms => 4,
      Examples => 6, Exceptions => 9, Text => 1];

   HEADER_BYTES : constant := 8 + 4 + 8 * 9;
   FORMAT_VERSION : constant := 1;

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

   type Open_Status is (Opened, Too_Short, Too_Large, Bad_Magic, Wrong_Version, Bad_Table);

   procedure Open (Data : Byte_Array; Result : out Layout; Status : out Open_Status)
     with Pre  => Data'First = 0,
          Post => (if Status = Opened then Fits (Result) and then Result.Length = Data'Length);

   --  The image size a header declares (the text table is last), from the
   --  first HEADER_BYTES of a file: a reader allocates once, then fills.
   procedure Declared_Size (Header : Byte_Array; Size : out Image_Size; Status : out Open_Status)
     with Pre => Header'First = 0 and then Header'Length >= HEADER_BYTES;

   --  An image to read: its layout matches Data.
   function Usable (Data : Byte_Array; L : Layout) return Boolean is
     (Data'First = 0 and then L.Length = Data'Length and then Fits (L));

   function Count (L : Layout; S : Section) return Image_Size is (L.Sections (S).Count);
   function Word_Count (L : Layout) return Image_Size is (Count (L, Words));

   --  A slice of the text blob, absolute in the image.
   type Text_Slice is record
      First  : Image_Size := 0;
      Length : Image_Size := 0;
   end record;
   function Inside (Data : Byte_Array; T : Text_Slice) return Boolean is
     (T.First <= Data'Length and then T.Length <= Data'Length - T.First);

   --  The parts of speech; on disk 0 .. 3 in this order.
   type Part_Of_Speech is (Noun, Verb, Adjective, Adverb);

   --  The relations kept (docs/lexicon.md); on disk 0 .. 3 in this order.
   type Relation_Kind is (Broader, Narrower, Similar, See_Also);

   --  How common a spelling is: the smallest SCOWL size listing it
   --  (10 most common .. 60), or WORDNET_ONLY for words only WordNet has.
   type Commonness is range 0 .. 255;
   WORDNET_ONLY : constant Commonness := 70;

   type Word_Record is record
      Valid       : Boolean := False;
      Key         : Text_Slice;
      Band        : Commonness := WORDNET_ONLY;
      First_Sense : Sense_Id := 0;
      Sense_Count : Image_Size := 0;
   end record;

   type Sense_Record is record
      Valid         : Boolean := False;
      Synset        : Synset_Id := 0;
      Word          : Word_Id := 0;
      Display       : Text_Slice;
      First_Antonym : Row := 0;
      Antonym_Count : Image_Size := 0;
   end record;

   type Synset_Record is record
      Valid          : Boolean := False;
      Part           : Part_Of_Speech := Noun;
      Gloss          : Text_Slice;
      First_Example  : Row := 0;
      Example_Count  : Image_Size := 0;
      First_Member   : Row := 0;
      Member_Count   : Image_Size := 0;
      First_Relation : Row := 0;
      Relation_Count : Image_Size := 0;
   end record;

   type Relation_Record is record
      Valid  : Boolean := False;
      Kind   : Relation_Kind := Broader;
      Target : Synset_Id := 0;
   end record;

   type Exception_Record is record
      Valid : Boolean := False;
      Key   : Text_Slice;
      Base  : Word_Id := 0;
   end record;

   function Read_Word (Data : Byte_Array; L : Layout; W : Word_Id) return Word_Record
     with Pre  => Usable (Data, L) and then Image_Size (W) < Count (L, Words),
          Post => (if Read_Word'Result.Valid then
                     Inside (Data, Read_Word'Result.Key)
                     and then Read_Word'Result.Key.Length <= MAXIMUM_WORD_BYTES
                     and then Image_Size (Read_Word'Result.First_Sense) <= Count (L, Senses)
                     and then Read_Word'Result.Sense_Count
                              <= Count (L, Senses) - Image_Size (Read_Word'Result.First_Sense));

   function Read_Sense (Data : Byte_Array; L : Layout; S : Sense_Id) return Sense_Record
     with Pre  => Usable (Data, L) and then Image_Size (S) < Count (L, Senses),
          Post => (if Read_Sense'Result.Valid then
                     Inside (Data, Read_Sense'Result.Display)
                     and then Read_Sense'Result.Display.Length <= MAXIMUM_WORD_BYTES
                     and then Image_Size (Read_Sense'Result.Synset) < Count (L, Synsets)
                     and then Image_Size (Read_Sense'Result.Word) < Count (L, Words)
                     and then Image_Size (Read_Sense'Result.First_Antonym) <= Count (L, Antonyms)
                     and then Read_Sense'Result.Antonym_Count
                              <= Count (L, Antonyms) - Image_Size (Read_Sense'Result.First_Antonym));

   function Read_Synset (Data : Byte_Array; L : Layout; S : Synset_Id) return Synset_Record
     with Pre  => Usable (Data, L) and then Image_Size (S) < Count (L, Synsets),
          Post => (if Read_Synset'Result.Valid then
                     Inside (Data, Read_Synset'Result.Gloss)
                     and then Read_Synset'Result.Gloss.Length <= MAXIMUM_TEXT_ITEM_BYTES
                     and then Image_Size (Read_Synset'Result.First_Example) <= Count (L, Examples)
                     and then Read_Synset'Result.Example_Count
                              <= Count (L, Examples) - Image_Size (Read_Synset'Result.First_Example)
                     and then Image_Size (Read_Synset'Result.First_Member) <= Count (L, Members)
                     and then Read_Synset'Result.Member_Count
                              <= Count (L, Members) - Image_Size (Read_Synset'Result.First_Member)
                     and then Image_Size (Read_Synset'Result.First_Relation) <= Count (L, Relations)
                     and then Read_Synset'Result.Relation_Count
                              <= Count (L, Relations) - Image_Size (Read_Synset'Result.First_Relation));

   --  Row R of Members: the sense it lists (Valid False if out of range).
   procedure Read_Member (Data : Byte_Array; L : Layout; R : Row; Sense : out Sense_Id; Valid : out Boolean)
     with Pre  => Usable (Data, L) and then Image_Size (R) < Count (L, Members),
          Post => (if Valid then Image_Size (Sense) < Count (L, Senses));

   procedure Read_Antonym (Data : Byte_Array; L : Layout; R : Row; Sense : out Sense_Id; Valid : out Boolean)
     with Pre  => Usable (Data, L) and then Image_Size (R) < Count (L, Antonyms),
          Post => (if Valid then Image_Size (Sense) < Count (L, Senses));

   function Read_Relation (Data : Byte_Array; L : Layout; R : Row) return Relation_Record
     with Pre  => Usable (Data, L) and then Image_Size (R) < Count (L, Relations),
          Post => (if Read_Relation'Result.Valid then
                     Image_Size (Read_Relation'Result.Target) < Count (L, Synsets));

   procedure Read_Example (Data : Byte_Array; L : Layout; R : Row; Slice : out Text_Slice; Valid : out Boolean)
     with Pre  => Usable (Data, L) and then Image_Size (R) < Count (L, Examples),
          Post => (if Valid then Inside (Data, Slice) and then Slice.Length <= MAXIMUM_TEXT_ITEM_BYTES);

   function Read_Exception (Data : Byte_Array; L : Layout; R : Row) return Exception_Record
     with Pre  => Usable (Data, L) and then Image_Size (R) < Count (L, Exceptions),
          Post => (if Read_Exception'Result.Valid then
                     Inside (Data, Read_Exception'Result.Key)
                     and then Read_Exception'Result.Key.Length <= MAXIMUM_WORD_BYTES
                     and then Image_Size (Read_Exception'Result.Base) < Count (L, Words));

end Lexicon_Images;
