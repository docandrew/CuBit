------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Text to a typed Passage (APP-005): "John 3:16", "Jn 3:16-18",
--  "1 Cor 13", "Ps 23", "Tobit 1:1", "John 3:16-4:2", "John 3-4", "Jude 5",
--  "Obadiah". Search boxes and links feed it, so its input is untrusted:
--  proved free of run-time errors (SPARK, level 2), any byte sequence ends
--  in a typed result.
--
--  Grammar (case-insensitive, spaces optional around separators):
--
--    passage  ::= book [ chapter [ sep verse ] [ dash end ] ]
--    end      ::= number [ sep number ]
--    book     ::= [ ordinal ] name          ordinal: 1-4, I-IV, First..Fourth, 1st..4th
--    sep      ::= ":" | "."                  dash: "-", en dash or em dash (UTF-8)
--
--  A book is a full name, a common abbreviation ("Jn", "1 Cor", "Ps",
--  "Ecclus"), a Latin name or abbreviation as the Vulgate has them
--  ("Ioannes", "Io", "Psalmi", "Apocalypsis"), or any prefix of these that
--  names exactly one book. Periods and spaces inside the name are ignored
--  ("1 Cor.", "Song of Songs"). The Latin names follow the Clementine
--  Vulgate: 1-4 Regum are 1-2 Samuel and 1-2 Kings, 1-2 Esdrae Ezra and
--  Nehemiah, 3-4 Esdrae 1-2 Esdras.
--
--  Numbers are checked against a translation's versification:
--  - a book alone is the whole book, "John 3" the whole chapter;
--  - in a one-chapter book a lone number is a verse ("Jude 5" is Jude
--    1:5), as readers write it;
--  - "Psalm 151" is the book Psalm_151 where the translation has it as a
--    separate book;
--  - an unknown or ambiguous book, a chapter or verse past the
--    translation's last, and a range that runs backwards are typed errors
--    that name what was wrong.
------------------------------------------------------------------------------
with Bible_Canon; use Bible_Canon;
with Bible_References; use Bible_References;

package Bible_Parser with SPARK_Mode, Pure is

   --  Longer text is refused (Text_Too_Long), never truncated: no
   --  reference is near this long.
   MAXIMUM_TEXT : constant := 256;

   --  The largest number read; more digits are Number_Too_Large.
   MAXIMUM_NUMBER : constant := 999;
   subtype Number is Natural range 0 .. MAXIMUM_NUMBER;

   type Status is
     (Parsed,                   --  Passage holds the passage
      Empty_Text,               --  nothing but spaces
      Text_Too_Long,            --  longer than MAXIMUM_TEXT
      Unknown_Book,             --  no book has this name (Book_First .. Book_Last)
      Ambiguous_Book,           --  a prefix of several books' names
      Malformed,                --  unexpected text at Position
      Number_Too_Large,         --  a number at Position longer than MAXIMUM_NUMBER
      Book_Not_In_Translation,  --  Book is not in this translation
      Chapter_Out_Of_Range,     --  Book has no chapter Value; it has Limit
      Verse_Out_Of_Range,       --  Book Chapter has no verse Value; it has Limit
      Reversed_Range);          --  the range ends before it starts

   type Result is record
      Outcome    : Status := Empty_Text;
      Passage    : Bible_References.Passage;
      --  The book, once named (every status from Book_Not_In_Translation on).
      Book       : Bible_Canon.Book := Genesis;
      --  The text taken as the book's name (Unknown_Book, Ambiguous_Book).
      Book_First : Natural := 0;
      Book_Last  : Natural := 0;
      --  Where Malformed or Number_Too_Large text starts.
      Position   : Natural := 0;
      --  The out-of-range number, the translation's last, and (for a verse)
      --  the chapter.
      Value      : Number := 0;
      Limit      : Number := 0;
      Chapter    : Number := 0;
   end record;

   procedure Parse (Text : String; Table : Versification; Item : out Result)
     with Pre  => Valid (Table),
          Post => (if Item.Outcome = Parsed then Exists (Table, Item.Passage))
                  and then (if Item.Outcome in Unknown_Book | Ambiguous_Book
                            then Item.Book_First in Text'Range and then Item.Book_Last in Text'Range
                                 and then Item.Book_First <= Item.Book_Last)
                  and then (if Item.Outcome in Malformed | Number_Too_Large
                            then Item.Position in Text'Range);

   --  The book a name alone names ("Jn", "1 Cor", "song of songs"), as
   --  Parse matches it: Found False for none or several.
   procedure Find_Book (Name : String; Item : out Book; Found, Ambiguous : out Boolean)
     with Pre => Name'Length <= MAXIMUM_TEXT;

   --  A one-line description of a failed result, for status bars and CCL
   --  ("No book is called ""Jhon""").
   function Describe (Text : String; Item : Result) return String
     with Pre => Item.Outcome /= Parsed;

end Bible_Parser;
