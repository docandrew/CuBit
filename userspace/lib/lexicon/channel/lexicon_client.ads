------------------------------------------------------------------------------
--  A client of the lexicon service (docs/lexicon.md, "Using the lexicon from
--  another program"): one Lexicon_Channel channel, opened on first use with
--  the program's "lexicon" endpoint and reopened after it ends. Ask is one
--  request and its answer page. Every native face uses this: the Dictionary
--  and Thesaurus apps, the CCL hosts, and any app granted the service.
--
--  One client per process; Ask is called from one thread at a time.
------------------------------------------------------------------------------
with Interfaces;
with CuBit.Handles;
with Lexicon_Answers;
with Lexicon_Channel;

package Lexicon_Client is

   type Ask_Status is (Answered, Unavailable, Failed);

   --  Explicit waits: the service answers in microseconds; these bound a
   --  service that is busy or gone, never the normal case.
   OPEN_DEADLINE_MS   : constant := 2_000;
   ANSWER_DEADLINE_MS : constant := 2_000;

   --  One page of Op on Text from Resume (Lexicon_Answers.Answer), from the
   --  service behind Endpoint.
   procedure Ask
     (Endpoint : CuBit.Handles.Endpoint; Op : Lexicon_Answers.Operation; Text : String;
      Resume : Interfaces.Unsigned_32; Suggestions : Lexicon_Answers.Suggestion_Count;
      Into : out Lexicon_Channel.Page; Used : out Natural; Status : out Ask_Status);

   --  Why the last Ask was Unavailable (for a message to the person).
   type Unavailable_Reason is (Not_Granted, Not_Opened, No_Image);
   function Last_Reason return Unavailable_Reason;

end Lexicon_Client;
