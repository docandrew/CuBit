--  Native: the lexicon service (lexicon.svc) through Lexicon_Client, with
--  the program's "lexicon" endpoint.
with CCL_Native_Bindings;
with Lexicon_Channel;
with Lexicon_Client;

package body CCL_Lexicon_Source is
   use Interfaces;
   use type Lexicon_Client.Ask_Status;
   use type Lexicon_Client.Unavailable_Reason;

   pragma Compile_Time_Error (PAGE_BYTES /= Lexicon_Channel.PAGE_BYTES, "a page is the service's page");

   procedure Ask
     (Op : Lexicon_Answers.Operation; Text : String; Resume : Unsigned_32;
      Suggestions : Lexicon_Answers.Suggestion_Count; Into : out Page; Used : out Natural;
      Status : out Ask_Status)
   is
      Asked : Lexicon_Client.Ask_Status;
   begin
      Lexicon_Client.Ask (CCL_Native_Bindings.Lexicon, Op, Text, Resume, Suggestions, Into, Used, Asked);
      Status := (case Asked is
                    when Lexicon_Client.Answered => Answered,
                    when Lexicon_Client.Unavailable => Unavailable,
                    when Lexicon_Client.Failed => Failed);
   end Ask;

   function Unavailable_Reason return String is
     (case Lexicon_Client.Last_Reason is
         when Lexicon_Client.Not_Granted => "this program was not granted the lexicon service",
         when Lexicon_Client.Not_Opened => "the lexicon service did not open a channel",
         when Lexicon_Client.No_Image => "the lexicon service has no lexicon image");
   function Unavailable_Remedy return String is
     (case Lexicon_Client.Last_Reason is
         when Lexicon_Client.Not_Granted =>
            "the program's manifest must request the lexicon service (service ""lexicon"" ""lexicon"")",
         when Lexicon_Client.Not_Opened =>
            "start lexicon.svc (init profile: (start ""lexicon.svc"" (priority 4)))",
         when Lexicon_Client.No_Image =>
            "install the cubit-lexicon asset package (Assets/cubit-lexicon/1/english.lexicon)");
end CCL_Lexicon_Source;
