with Interfaces; use Interfaces;
with Lexicon_Images; use Lexicon_Images;
with Lexicon_Lookup;
with Lexicon_Wire; use Lexicon_Wire;

--  The lexicon's operations, answered from an image into one page of
--  Lexicon_Wire items (docs/lexicon.md, "Operations"). Every face uses
--  these: lexicon.svc for its channel clients (the Dictionary app, the CCL
--  console, any app granted the lexicon service), and hosted tests.
--
--    Define     a word -> its entries: each headword (the word itself if it
--               has senses, then the words it is a form of), each sense's
--               part of speech, definition, examples, synonyms, antonyms.
--               Not found: the Query and near spellings.
--    Thesaurus  as Define, plus related (similar, see also), broader and
--               narrower words for each sense.
--    Check      a text -> its misspellings, each with near spellings; a
--               whole paragraph per call. Resumes from a byte offset.
--    Suggest    a word -> near spellings.
--
--  Define and Thesaurus resume from an item number (Continue_At); Check
--  from a byte offset in the text (Checked_Upto).
--
--  Proved: no run-time errors; Used never exceeds the page.
package Lexicon_Answers with SPARK_Mode is

   type Operation is (Define, Thesaurus, Check, Suggest);

   --  The headwords one query considers: the word and the words it may be
   --  a form of. WordNet 3.0 lists at most 3 bases for one inflection; the
   --  suffix rules add at most one per rule ending.
   MAXIMUM_FORMS : constant := 16;

   --  How far near spellings may be (edits).
   SUGGESTION_DISTANCE : constant Lexicon_Lookup.Edit_Distance := 2;

   --  Near spellings per unknown word are ranked in a list on the stack;
   --  callers ask for fewer (a menu shows a handful).
   MAXIMUM_SUGGESTIONS : constant := 64;
   subtype Suggestion_Count is Natural range 0 .. MAXIMUM_SUGGESTIONS;

   --  Answer Op for Text from Resume (an item number for Define and
   --  Thesaurus, a 0-based byte offset into Text for Check), with at most
   --  Suggestions near spellings per unknown word.
   procedure Answer
     (Data : Byte_Array; L : Layout; Op : Operation; Text : String; Resume : Unsigned_32;
      Suggestions : Suggestion_Count; Into : out Answer_Bytes; Used : out Natural)
     with Pre  => Usable (Data, L) and then Into'First = 0 and then Into'Length >= MINIMUM_PAGE_BYTES
                  and then Into'Length < Natural'Last and then Text'Last < Positive'Last,
          Post => Used <= Into'Length;

end Lexicon_Answers;
