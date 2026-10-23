------------------------------------------------------------------------------
--  lexicon.svc (docs/lexicon.md): the dictionary, thesaurus and spelling
--  service. It registers the "lexicon" role, reads the lexicon image once,
--  and serves Lexicon_Channel channels: each request is one
--  Lexicon_Answers operation, answered with one page of Lexicon_Wire items.
--  Every client (the Dictionary app, CCL hosts, any app granted the
--  service) uses the same channel, so there is no app-private API.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Servers;
with CuBit.Control_Events;
with CuBit.Deadlines;
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_Events;
with Lexicon_Answers;
with Lexicon_Channel;
with Lexicon_Images;
with Lexicon_Loading;
with Startup;

procedure Main is
   use ASCII;
   package LC renames Lexicon_Channel;
   package LA renames Lexicon_Answers;
   use type Lexicon_Loading.Image_Access;

   --  Every process procmgr handed the service may open channels: the
   --  manifest's request is the authority; limits are the offer's.
   function Admit (Offer : Positive; From : Process_ID; Badge : Unsigned_64) return Boolean is
     (Offer = 1 and then From /= No_Process);
   function Can_Serve_Another (Offer : Positive) return Boolean is (Offer = 1);

   package Servers is new CuBit.Channel_Servers
     (RC => LC.RC, Offer_Count => 1, Admit => Admit, Can_Serve_Another => Can_Serve_Another);

   --  Requests served per channel per turn before the next channel's.
   BUDGET : constant := 8;
   --  Reading the image at startup: one deadline for the whole read.
   LOAD_BUDGET_MS : constant := 30_000;
   REPLY_UNKNOWN : constant Unsigned_32 := 16#F001#;

   S : Servers.Server;
   Data : Lexicon_Loading.Image_Access;
   Layout : Lexicon_Images.Layout;
   Loaded : Lexicon_Loading.Load_Result;
   Logger : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Served : Unsigned_64 := 0;

   procedure Log (Text : String) is
      Value : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make (Text);
      Submitted : Boolean;
   begin
      debugPrint (Text & LF);
      if Value.Success then
         CuBit.Logging.Emit (Logger, Value.Value, Submitted);
      end if;
   end Log;

   --  One request, copied out of the client's region: validate, answer.
   function Answer_Of (Item : LC.Request) return LC.Result is
      Reply : LC.Result;
      Used : Natural;
   begin
      if Data = null then
         Reply.Status := LC.Answer_Status'Enum_Rep (LC.No_Lexicon);
         return Reply;
      end if;
      if Item.Operation > LA.Operation'Pos (LA.Operation'Last)
        or else Natural (Item.Length) > LC.REQUEST_TEXT_BYTES
        or else Natural (Item.Suggestions) > LA.MAXIMUM_SUGGESTIONS
      then
         Reply.Status := LC.Answer_Status'Enum_Rep (LC.Malformed);
         return Reply;
      end if;
      declare
         Text : String (1 .. Natural (Item.Length));
      begin
         for I in Text'Range loop
            Text (I) := Item.Text (I);
         end loop;
         LA.Answer (Data.all, Layout, LA.Operation'Val (Item.Operation), Text, Item.Resume,
                    Natural (Item.Suggestions), Reply.Bytes, Used);
      end;
      Reply.Status := LC.Answer_Status'Enum_Rep (LC.Answered);
      Reply.Length := Unsigned_16 (Used);
      return Reply;
   end Answer_Of;

   procedure Serve (I : Servers.Index) is
      Item : LC.RC.Queues.Submission;
      Got : Boolean;
   begin
      for K in 1 .. BUDGET loop
         Servers.Take (S, I, Item, Got);
         exit when not Got;
         Servers.Complete (S, I, Item.Tag, Answer_Of (Item.Item));
         Served := Served + 1;
      end loop;
      Servers.Served (S, I);
   end Serve;

   procedure Take_Events is
      Event : CuBit.Control_Events.Event;
      Found, Ended : Boolean;
      I : Servers.Index;
   begin
      loop
         CuBit.Process_Events.Next (Event, Found);
         exit when not Found;
         Servers.Handle_Event (S, Event, Ended, I);
      end loop;
   end Take_Events;

   Configured, Found, Is_Open, Opened, Ended : Boolean;
   I : Servers.Index;
   Reply_Msg : Message;
   Woke : Servers.Wake := (Kind => Servers.Work_Waiting, others => <>);
   Ignore : Unsigned_64;
begin
   debugPrint ("lexicon: starting" & LF);
   Servers.Configure
     (S, [1 => (Element => LC.Element,
                Per_Client => Startup.Channels.Com_Cubit_Lexicon_Per_Client,
                Total => Startup.Channels.Com_Cubit_Lexicon_Total,
                Service => Startup.Channels.Com_Cubit_Lexicon_Class)], Configured);
   if not Configured then
      debugPrint ("lexicon: no memory for the channel table" & LF);
      return;
   end if;
   Ignore := registerDriver (LC.Service_Role, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      debugPrint ("lexicon: registration failed" & LF);
      return;
   end if;
   debugPrint ("lexicon: registered" & LF);
   Lexicon_Loading.Load (LOAD_BUDGET_MS, Data, Layout, Loaded);
   Log ("lexicon: image " & Lexicon_Loading.Describe (Loaded) &
        (if Data /= null then "," & Lexicon_Images.Word_Count (Layout)'Image & " words" else ""));
   loop
      case Woke.Kind is
         when Servers.Message_Received =>
            if Woke.From = No_Process then
               --  A kernel event: a channel ended (grant notices).
               declare
                  Event : constant CuBit.Control_Events.Event :=
                    CuBit.Control_Events.Decode (Woke.Msg.tag.label, Woke.Msg.tag.length,
                                                 Woke.Msg.words (0), Woke.Msg.words (1), Woke.Msg.words (2));
               begin
                  Servers.Handle_Event (S, Event, Ended, I);
               end;
            else
               Servers.Handle_Open (S, Woke.From, Woke.Msg, Is_Open, Reply_Msg, Opened, I);
               if not Is_Open then
                  Reply_Msg := NULL_MESSAGE;
                  Reply_Msg.tag := (label => REPLY_UNKNOWN, length => 1, protocol => 0, ipc => <>,
                                    reserved => 0);
                  Reply_Msg.words (0) := Unsigned_64 (Woke.Msg.tag.label);
               end if;
               Ignore := reply (Woke.From, Reply_Msg);
            end if;
         when others =>
            null;
      end case;
      Take_Events;
      Servers.Refresh (S);
      loop
         Servers.Next (S, Found, I);
         exit when not Found;
         Serve (I);
      end loop;
      --  No idle deadline: every request, open and channel end wakes it.
      Servers.Sleep (S, CuBit.Deadlines.Forever, Woke);
   end loop;
end Main;
