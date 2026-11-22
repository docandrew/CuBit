with Lexicon_Images; use Lexicon_Images;

--  Finding words in a lexicon image (docs/lexicon.md, "Lookup"): the
--  normalized key, exact search, the base forms an inflected spelling may
--  come from (WordNet's exception lists and suffix rules), spelling checks
--  and near spellings.
--
--  Keys are ASCII lower case with single spaces between words (WordNet's
--  multi-word lemmas). Case folding is ASCII only: the image holds ASCII
--  keys (tools/build_lexicon.py drops the rest).
--
--  Proved: no run-time errors; every Word_Id returned is a row of the
--  image's word table.
package Lexicon_Lookup with SPARK_Mode is

   subtype Key_Length is Word_Length;
   subtype Key_Index is Key_Length range 1 .. MAXIMUM_WORD_BYTES;
   type Key is record
      Length : Key_Length := 0;
      Text   : String (Key_Index) := [others => ' '];
   end record;

   type Normal_Status is (Normalized, Empty, Too_Long);

   --  Input trimmed, ASCII letters in lower case, runs of spaces, tabs and
   --  underscores as one space, a typographic apostrophe (U+2019) as '.
   procedure Normalize (Input : String; Result : out Key; Status : out Normal_Status)
     with Pre  => Input'Last < Positive'Last,
          Post => (if Status = Normalized then Result.Length > 0);

   --  The word whose key is K.
   procedure Find (Data : Byte_Array; L : Layout; K : Key; Found : out Boolean; W : out Word_Id)
     with Pre  => Usable (Data, L),
          Post => (if Found then Image_Size (W) < Word_Count (L));

   --  The word's key and how many senses it has (0 if unreadable).
   function Sense_Count (Data : Byte_Array; L : Layout; W : Word_Id) return Image_Size
     with Pre => Usable (Data, L) and then Image_Size (W) < Word_Count (L);

   type Word_List is array (Positive range <>) of Word_Id;

   --  The words with senses K may be a form of, at most Into'Length: K
   --  itself first, then exception-list bases, then suffix-rule bases
   --  (running -> run, geese -> goose), each once.
   procedure Base_Forms (Data : Byte_Array; L : Layout; K : Key; Into : out Word_List; Count : out Natural)
     with Pre  => Usable (Data, L) and then Into'First = 1,
          Post => Count <= Into'Length
                  and then (for all I in 1 .. Count => Image_Size (Into (I)) < Word_Count (L));

   --  A correct spelling: a listed word, an inflection of one, or one with a
   --  possessive ending ('s, s').
   function Known (Data : Byte_Array; L : Layout; K : Key) return Boolean
     with Pre => Usable (Data, L);

   --  Edits (insert, delete, substitute, swap adjacent) between spellings.
   MAXIMUM_DISTANCE : constant := 3;
   subtype Edit_Distance is Natural range 0 .. MAXIMUM_DISTANCE;

   type Near_Spelling is record
      Word     : Word_Id := 0;
      Distance : Edit_Distance := 0;
      --  The same letters with two neighbours swapped ("teh", "the"): the
      --  likeliest slip of all, ranked first among equals.
      Swap     : Boolean := False;
      Band     : Commonness := WORDNET_ONLY;
   end record;
   type Near_Spelling_List is array (Positive range <>) of Near_Spelling;

   --  Near spellings of K within Within edits, at most Into'Length, nearest
   --  first, then swaps, then most common. Candidates share K's first letter (the
   --  prefix bound: one sorted range of the word table).
   procedure Suggest
     (Data : Byte_Array; L : Layout; K : Key; Within : Edit_Distance; Into : out Near_Spelling_List;
      Count : out Natural)
     with Pre  => Usable (Data, L) and then Into'First = 1,
          Post => Count <= Into'Length
                  and then (for all I in 1 .. Count => Image_Size (Into (I).Word) < Word_Count (L));

   --  A spelling in a text: its first byte and length.
   type Span is record
      First  : Positive := 1;
      Length : Key_Length := 0;
   end record;
   type Span_List is array (Positive range <>) of Span;

   --  The unknown words of Text from From on, at most Into'Length. Words
   --  are runs of letters with inner apostrophes or hyphens; a hyphenated
   --  word is right if it is listed or each part is. Runs with digits or
   --  other non-ASCII bytes, and single letters, are not judged. Next: where
   --  checking stopped (Text'Last + 1 when the whole text was checked).
   procedure Check_Text
     (Data : Byte_Array; L : Layout; Text : String; From : Positive; Into : out Span_List;
      Count : out Natural; Next : out Positive)
     with Pre  => Usable (Data, L) and then Into'First = 1 and then Text'Last < Positive'Last
                  and then Text'Last >= 0 and then From >= Text'First,
          Post => Count <= Into'Length and then Next >= From
                  and then (for all I in 1 .. Count =>
                              Into (I).First >= Text'First and then Into (I).Length > 0
                              and then Into (I).First <= Text'Last - Into (I).Length + 1);

   --  The key text of word W (its slice of the image).
   function Key_Of (Data : Byte_Array; L : Layout; W : Word_Id) return Text_Slice
     with Pre  => Usable (Data, L) and then Image_Size (W) < Word_Count (L),
          Post => Inside (Data, Key_Of'Result) and then Key_Of'Result.Length <= MAXIMUM_WORD_BYTES;

end Lexicon_Lookup;
