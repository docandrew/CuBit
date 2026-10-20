------------------------------------------------------------------------------
--  The native Dictionary app (docs/lexicon.md), with Dictionary and
--  Thesaurus modes: this view (Dictionary_Views) over the lexicon service
--  (Lexicon_Client, the manifest's "lexicon" endpoint).
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

   --  Open the window in Dictionary mode and run until it closes.
   procedure Run;
end Dictionary_App;
