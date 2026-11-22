with Interfaces;
with Lexicon_Answers;
with Lexicon_Wire;

--  Where a CCL host's lexicon answers come from (docs/lexicon.md): natively
--  the lexicon service over a Lexicon_Channel channel (native/), on Linux
--  the image file named by CCL_LEXICON_IMAGE (tools/ccl-ui-preview/host/).
--  Both give the same pages: Lexicon_Answers writes them either way.
package CCL_Lexicon_Source is

   --  One answer page (Lexicon_Channel.PAGE_BYTES, the service's page).
   PAGE_BYTES : constant := 4_084;
   subtype Page is Lexicon_Wire.Answer_Bytes (0 .. PAGE_BYTES - 1);

   type Ask_Status is (Answered, Unavailable, Failed);

   --  One page of Op on Text from Resume (Lexicon_Answers.Answer).
   procedure Ask
     (Op : Lexicon_Answers.Operation; Text : String; Resume : Interfaces.Unsigned_32;
      Suggestions : Lexicon_Answers.Suggestion_Count; Into : out Page; Used : out Natural;
      Status : out Ask_Status);

   --  Why Ask said Unavailable, and what would fix it.
   function Unavailable_Reason return String;
   function Unavailable_Remedy return String;

end CCL_Lexicon_Source;
