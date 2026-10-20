--  Hosted helpers for the lexicon tests: the image in memory and answers
--  decoded into lists of items (Linux only; the core never uses these).
with Ada.Containers.Indefinite_Vectors;
with Interfaces; use Interfaces;
with Lexicon_Answers;
with Lexicon_Images; use Lexicon_Images;
with Lexicon_Wire; use Lexicon_Wire;

package Lexicon_Support is

   type Image_Access is access Byte_Array;

   --  The image file at Path (LEXICON_IMAGE overrides the default).
   function Load (Path : String) return Image_Access;

   type Decoded (Length : Natural) is record
      Kind   : Item_Kind;
      Number : Unsigned_32;   --  numeric payloads
      Span_Length : Unsigned_16;
      Text   : String (1 .. Length);
   end record;
   package Item_Lists is new Ada.Containers.Indefinite_Vectors (Positive, Decoded);

   --  Every item of one page; Damaged if the page did not decode.
   procedure Decode (Page : Answer_Bytes; Into : in out Item_Lists.Vector; Damaged : out Boolean);

   --  All pages of Op on Text, page size Page_Bytes, concatenated without
   --  the Continue_At / intermediate Checked_Upto items. Pages: how many.
   procedure Ask_All
     (Data : Byte_Array; L : Layout; Op : Lexicon_Answers.Operation; Text : String;
      Page_Bytes : Positive; Suggestions : Lexicon_Answers.Suggestion_Count;
      Into : out Item_Lists.Vector; Pages : out Natural);

   function Has (Items : Item_Lists.Vector; Kind : Item_Kind; Text : String) return Boolean;
   function Count_Items (Items : Item_Lists.Vector; Kind : Item_Kind) return Natural;
   function Texts (Items : Item_Lists.Vector; Kind : Item_Kind) return String;

end Lexicon_Support;
