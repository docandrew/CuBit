with CuBit.Channel_Clients;
with CuBit.Channel_Protocol;
with CuBit.Deadlines.Clock;

package body Lexicon_Client is
   use Interfaces;
   package LC renames Lexicon_Channel;
   package Clients is new CuBit.Channel_Clients (LC.RC, LC.Element);
   use type Clients.Open_Outcome, Clients.Submit_Result, Clients.Wait_Outcome;

   Channel : Clients.Client;
   Reason : Unavailable_Reason := Not_Opened;

   function Connected (Endpoint : CuBit.Handles.Endpoint) return Boolean is
      Refusal : CuBit.Channel_Protocol.Open_Refusal;
      Outcome : Clients.Open_Outcome;
   begin
      if Clients.Is_Open (Channel) then
         return True;
      end if;
      if not CuBit.Handles.Valid (Endpoint) then
         Reason := Not_Granted;
         return False;
      end if;
      Clients.Open (Channel, Endpoint, CuBit.Deadlines.Clock.After (OPEN_DEADLINE_MS), Outcome, Refusal);
      Reason := Not_Opened;
      return Outcome = Clients.Connected;
   end Connected;

   procedure Ask
     (Endpoint : CuBit.Handles.Endpoint; Op : Lexicon_Answers.Operation; Text : String;
      Resume : Unsigned_32; Suggestions : Lexicon_Answers.Suggestion_Count;
      Into : out LC.Page; Used : out Natural; Status : out Ask_Status)
   is
      Item : LC.Request;
      Number : Clients.Tag;
      Submitted : Clients.Submit_Result;
      Outcome : Clients.Wait_Outcome;
      Answer : LC.Result;
   begin
      Into := [others => 0];
      Used := 0;
      if Text'Length > LC.REQUEST_TEXT_BYTES then
         Status := Failed;
         return;
      end if;
      if not Connected (Endpoint) then
         Status := Unavailable;
         return;
      end if;
      Item.Operation := Lexicon_Answers.Operation'Pos (Op);
      Item.Suggestions := Unsigned_8 (Suggestions);
      Item.Length := Unsigned_16 (Text'Length);
      Item.Resume := Resume;
      for I in 1 .. Text'Length loop
         Item.Text (I) := Text (Text'First + I - 1);
      end loop;
      Clients.Submit (Channel, Item, Number, Submitted);
      if Submitted /= Clients.Queued then
         Clients.Close (Channel);
         Status := Failed;
         return;
      end if;
      Clients.Wait (Channel, Number, CuBit.Deadlines.Clock.After (ANSWER_DEADLINE_MS), Answer, Outcome);
      if Outcome /= Clients.Answered then
         --  Gone or stuck: the next Ask opens a fresh channel.
         Clients.Close (Channel);
         Status := Failed;
         return;
      end if;
      if Answer.Status = LC.Answer_Status'Enum_Rep (LC.No_Lexicon) then
         Reason := No_Image;
         Status := Unavailable;
         return;
      elsif Answer.Status /= LC.Answer_Status'Enum_Rep (LC.Answered) or else Natural (Answer.Length) > LC.PAGE_BYTES then
         Status := Failed;
         return;
      end if;
      Used := Natural (Answer.Length);
      Into (0 .. Used - 1) := Answer.Bytes (0 .. Used - 1);
      Status := Answered;
   end Ask;

   function Last_Reason return Unavailable_Reason is (Reason);
end Lexicon_Client;
