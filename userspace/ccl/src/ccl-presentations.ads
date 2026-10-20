with CCL.Image_Store;
with CCL.Language;
with CCL.Literal_Tables;
with CCL.Types.Shapes;

--  How a result is shown, decided once for every front end: the native
--  console, the Workbench and the Observatory (over Control_Wire) all
--  render this description, so they present a value the same way.
--  - Failure: the diagnostic (CCL.Sessions.Result_Value_Image).
--  - Text: the value as text (its type from Result_Type_Image).
--  - Table: a record, or a list of them; the cells of its literal under
--    its row shape.
--  - Picture: an Image (interfaces/image.schema), by its stored pixels.
--  - Gallery: a list of Images, side by side.
--  - Article: a lexicon entry (CCL.Interfaces.Lexicon: a Definition,
--    Thesaurus_Result or Synonyms) as a dictionary page: the headword, its
--    senses numbered under their parts of speech, examples, and word lists
--    whose words link to their own entries (docs/lexicon.md).
package CCL.Presentations is
   type Form is (Failure, Text, Table, Picture, Gallery, Article);
   --  A gallery shows at most the rows a table does.
   Maximum_Gallery : constant := CCL.Literal_Tables.Maximum_Rows;
   subtype Gallery_Count is Natural range 0 .. Maximum_Gallery;
   type Picture_Entry is record
      Image : CCL.Image_Store.Image_Id := CCL.Image_Store.No_Image;
      Width, Height : Natural := 0;
   end record;
   type Picture_Array is array (1 .. Maximum_Gallery) of Picture_Entry;
   --  An article: lines of text and links, each a span of the result's
   --  literal (a string's contents, its escapes as written).
   type Literal_Span is record
      First : Positive := 1;
      Last  : Natural := 0;
   end record;
   type Line_Kind is
     (Headword,    --  the entry's word
      Heading,     --  a part of speech, before the senses that have it
      Numbered,    --  a sense: its number and definition
      Example,     --  a usage example of the sense above
      Words,       --  a labelled list of linked words (Role)
      Missing);    --  the word is not in the lexicon (near spellings follow)
   type Word_Role is (Synonyms, Antonyms, Related, Broader, Forms, Suggestions);
   --  The command a link runs: the one that made the article.
   type Article_Command is (Dict_Command, Thesaurus_Command, Synonym_Command);
   type Part_Name is (Noun, Verb, Adjective, Adverb);
   type Article_Line is record
      Kind   : Line_Kind := Headword;
      Text   : Literal_Span;          --  Headword, Numbered, Example, Missing
      Number : Natural := 0;          --  Numbered
      Part   : Part_Name := Noun;     --  Heading
      Role   : Word_Role := Synonyms; --  Words
      First_Word : Positive := 1;     --  Words: Article_Words (First_Word ..
      Word_Count : Natural := 0;      --    First_Word + Word_Count - 1)
   end record;
   --  Every line and word takes at least a few literal bytes.
   Maximum_Article_Lines : constant := 256;
   Maximum_Article_Words : constant := 512;
   subtype Article_Line_Count is Natural range 0 .. Maximum_Article_Lines;
   subtype Article_Word_Count is Natural range 0 .. Maximum_Article_Words;
   type Article_Line_Array is array (1 .. Maximum_Article_Lines) of Article_Line;
   type Article_Word_Array is array (1 .. Maximum_Article_Words) of Literal_Span;
   type Article_View is record
      Command : Article_Command := Dict_Command;
      Lines : Article_Line_Array;
      Line_Count : Article_Line_Count := 0;
      Words : Article_Word_Array;
      Word_Count : Article_Word_Count := 0;
      --  Senses shown (First_Shown .. First_Shown + Shown - 1) of Total;
      --  all zero for a synonym list or a near-spelling answer.
      First_Shown, Shown, Total : Natural := 0;
   end record;

   type Presentation is record
      Kind : Form := Failure;
      Shape : CCL.Types.Shapes.Row_Shape;
      Cells : CCL.Literal_Tables.Table;
      Image : CCL.Image_Store.Image_Id := CCL.Image_Store.No_Image;
      Image_Width, Image_Height : Natural := 0;
      --  Gallery: its pictures, in order; Cells.Total counts them all.
      Pictures : Picture_Array := [others => (others => <>)];
      Picture_Count : Gallery_Count := 0;
      --  Article: the entry's lines.
      Page : Article_View;
   end record;
   function Span_Text (Outcome : CCL.Language.Interpretation_Result; Span : Literal_Span) return String;
   procedure Describe
     (Outcome : CCL.Language.Interpretation_Result; Result : out Presentation);
   --  A cell's text in the result's literal.
   function Cell_Text
     (Outcome : CCL.Language.Interpretation_Result; Item : Presentation;
      Row : CCL.Literal_Tables.Row_Index; Column : CCL.Types.Component_Index) return String;
end CCL.Presentations;
