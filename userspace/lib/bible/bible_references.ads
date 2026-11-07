------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Typed Bible references (APP-005): a Reference is a book, a chapter and
--  a verse; a Passage is a range from one Reference to another, in reading
--  order. Chapters and verses are named subtypes bounded by the largest
--  numbers any supported translation uses (tools/build_bible.py refuses a
--  source past them: a Capacity error, not a silent truncation).
--
--  Whether a reference exists is a question for a translation:
--  versifications differ (Malachi 4 in the King James Version is
--  Malachi 3:19-24 in Hebrew-based ones). A Versification is one
--  translation's table, read from its image (Bible_Images).
------------------------------------------------------------------------------
with Bible_Canon; use Bible_Canon;

package Bible_References with SPARK_Mode, Pure is

   --  Psalms has 150 chapters; Psalm 119 has 176 verses.
   MAXIMUM_CHAPTER : constant := 150;
   MAXIMUM_VERSE   : constant := 176;
   subtype Chapter_Count is Natural range 0 .. MAXIMUM_CHAPTER;
   subtype Chapter_Number is Chapter_Count range 1 .. MAXIMUM_CHAPTER;
   subtype Verse_Count is Natural range 0 .. MAXIMUM_VERSE;
   subtype Verse_Number is Verse_Count range 1 .. MAXIMUM_VERSE;

   type Reference is record
      Book    : Bible_Canon.Book := Genesis;
      Chapter : Chapter_Number := 1;
      Verse   : Verse_Number := 1;
   end record;

   --  Reading order: by book, then chapter, then verse.
   function Before_Or_Same (Left, Right : Reference) return Boolean is
     (Left.Book < Right.Book
      or else (Left.Book = Right.Book
               and then (Left.Chapter < Right.Chapter
                         or else (Left.Chapter = Right.Chapter and then Left.Verse <= Right.Verse))));

   type Passage is record
      From, To : Reference;
   end record;

   function Ordered (Item : Passage) return Boolean is (Before_Or_Same (Item.From, Item.To));

   ---------------------------------------------------------------------------
   --  Versification: one translation's books, chapters and verses.
   ---------------------------------------------------------------------------

   --  Every chapter of every book of one translation (the Revised Standard
   --  Version with all deuterocanonical books has about 1,400).
   MAXIMUM_CHAPTERS : constant := 2_048;
   subtype Chapter_Total is Natural range 0 .. MAXIMUM_CHAPTERS;
   subtype Chapter_Slot is Chapter_Total range 1 .. MAXIMUM_CHAPTERS;

   type Book_Chapters is array (Book) of Chapter_Count;
   type Book_Offsets is array (Book) of Chapter_Total;
   type Chapter_Verses is array (Chapter_Slot) of Verse_Count;

   --  Book B has Chapters (B) chapters (0: not in this translation); its
   --  chapter C has Verses (Before (B) + C) verses.
   type Versification is record
      Chapters : Book_Chapters := [others => 0];
      Before   : Book_Offsets := [others => 0];
      Verses   : Chapter_Verses := [others => 0];
   end record;

   function Valid (Table : Versification) return Boolean is
     (for all B in Book => Table.Before (B) <= MAXIMUM_CHAPTERS - Table.Chapters (B));

   function Has_Book (Table : Versification; Item : Book) return Boolean is (Table.Chapters (Item) > 0);

   function Verses_In (Table : Versification; Item : Book; Chapter : Chapter_Number) return Verse_Count is
     (Table.Verses (Table.Before (Item) + Chapter))
   with Pre => Valid (Table) and then Chapter <= Table.Chapters (Item);

   --  The reference names a verse this translation numbers.
   function Exists (Table : Versification; Item : Reference) return Boolean is
     (Item.Chapter <= Table.Chapters (Item.Book)
      and then Item.Verse <= Verses_In (Table, Item.Book, Item.Chapter))
   with Pre => Valid (Table);

   function Exists (Table : Versification; Item : Passage) return Boolean is
     (Exists (Table, Item.From) and then Exists (Table, Item.To) and then Ordered (Item))
   with Pre => Valid (Table);

   --  A chapter or verse number in decimal, without a sign or space.
   MAXIMUM_NUMBER_IMAGE : constant := 3;
   function Number_Image (Value : Natural) return String
     with Pre  => Value <= 999,
          Post => Number_Image'Result'First = 1 and then Number_Image'Result'Length in 1 .. MAXIMUM_NUMBER_IMAGE;

   --  "John 3:16", "John 3:16-18", "John 3:16-4:2", "John 3" (a whole
   --  chapter of Table), "John 3-4". Image of a Passage is how the app and
   --  CCL show it; Bible_Parser reads every form back.
   MAXIMUM_IMAGE : constant := MAXIMUM_NAME + 16;
   function Image (Item : Reference) return String
     with Post => Image'Result'Length <= MAXIMUM_IMAGE;
   function Image (Table : Versification; Item : Passage) return String
     with Pre => Valid (Table), Post => Image'Result'Length <= MAXIMUM_IMAGE;

end Bible_References;
