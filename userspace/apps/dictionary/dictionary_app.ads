------------------------------------------------------------------------------
--  The native Dictionary and Thesaurus apps (docs/lexicon.md): one program
--  image per Apps-menu entry, both this view (Dictionary_Views) over the
--  lexicon service (Lexicon_Client, the manifest's "lexicon" endpoint).
------------------------------------------------------------------------------
with Dictionary_Views;
with Interfaces;
with Lexicon_Answers;
with Lexicon_Channel;
with Lexicon_Wire;

package Dictionary_App is
   procedure Ask
     (Op : Lexicon_Answers.Operation; Text : String; Resume : Interfaces.Unsigned_32;
      Suggestions : Lexicon_Answers.Suggestion_Count; Into : out Lexicon_Wire.Answer_Bytes;
      Used : out Natural; Available : out Boolean);
   package Views is new Dictionary_Views (Ask, Lexicon_Channel.PAGE_BYTES);

   --  Open the window in Start mode and run until it closes.
   procedure Run (Start : Views.Mode);
end Dictionary_App;
