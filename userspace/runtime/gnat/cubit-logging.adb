pragma Ada_2022;
with CuBit.Messages;
with CuBit.Channel_Protocol;
with CuBit.Published_Clock;
with CuBit.Log_Publish_Rings;
with CuBit.Log_Streams;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Last_Words;
package body CuBit.Logging is
   use type CuBit.Messages.Call_Ending;
   use CuBit.Messages;
   use CuBit.Log_Protocol;
   package Logs renames CuBit.Log_Records;
   package Channels renames CuBit.Channels;
   package Publish_Rings renames CuBit.Log_Publish_Rings;
   use type Channels.Put_Result;
   use type Channels.Open_Result;

   function Request (Op : Operation) return Message;
   function Reply_Status (Value : Message) return Status;
   procedure Drop (Item : in out Publisher);
   function Valid_Minimum (Value : Message) return Boolean;

   --  The publisher Publish_Now and Announce use: one per process, its
   --  channel open to logstore for the process's life.
   Announcer : Publisher;

   --  Last words (CuBit.Last_Words, LOG-004): the logstore endpoint of the
   --  first channel this process opened, kept as a value, so the
   --  last-chance handler can publish through the Announcer whatever
   --  became of the publisher that opened it. The handler waits at most
   --  Last_Words_Wait_Ms to open the channel and again for logstore to take
   --  the record: logstore never calls back into a dying program, and a
   --  stuck logstore cannot keep it from exiting.
   Last_Words_Slot : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Last_Words_Wait_Ms : constant := 200;
   procedure Last_Words (Text : String; Line : Natural);

   --  Open the channel to logstore, once.
   procedure Open_Channel
     (Item : in out Publisher;
      Deadline : Unsigned_64 := CuBit.Messages.Wait_Forever);
   procedure Open_Channel
     (Item : in out Publisher;
      Deadline : Unsigned_64 := CuBit.Messages.Wait_Forever)
   is
      Result : Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      if not CuBit.Handles.Valid (Item.Slot) then
         Item.State := Disabled;
         return;
      end if;
      Channels.Open (Item.Slot, Publish_Rings.CONTRACT, Channels.Producing, Item.Link, Result,
                     Ignore_Refusal, Deadline => Deadline);
      Item.State := (if Result = Channels.Opened then Ready else Disabled);
      if Item.State = Ready and then not CuBit.Handles.Valid (Last_Words_Slot) then
         Last_Words_Slot := Item.Slot;
         CuBit.Last_Words.Register (Last_Words'Access);
      end if;
   end Open_Channel;

   --  "unhandled exception at main.adb:14" as an Error, with its line as a
   --  field, through the Announcer (opened now if it never was).
   procedure Last_Words (Text : String; Line : Natural) is
      Made : Logs.Decoded := Logs.Make (Text, Logs.Error);
      Submitted, Drained : Boolean;
   begin
      if not Made.Success then
         return;
      end if;
      if Line > 0 then
         declare
            With_Line : constant Logs.Decoded :=
              Logs.With_Field (Made.Value, "exception.line", Logs.Unsigned_Integer,
                               Unsigned_64 (Line));
         begin
            if With_Line.Success then
               Made := With_Line;
            end if;
         end;
      end if;
      if Announcer.State = Uninitialized then
         if not CuBit.Handles.Valid (Announcer.Slot) then
            Announcer.Slot := Last_Words_Slot;
         end if;
         Open_Channel (Announcer, CuBit.Messages.Deadline_After (Last_Words_Wait_Ms));
      end if;
      Emit (Announcer, Made.Value, Submitted);
      if Submitted then
         Flush (Announcer, Drained, Wait_Ms => Last_Words_Wait_Ms);
      end if;
   end Last_Words;

   function Bound_To (Target : Logstore_Handles.Endpoint) return Publisher is
     (Slot => Logstore_Handles.Handle (Target), others => <>);

   function Bound_To (Target : Logstore_Handles.Optional_Endpoint) return Publisher is
     (Slot => (if Target.Present then Logstore_Handles.Handle (Target.Value)
               else CuBit.Handles.No_Endpoint),
      others => <>);

   function Awaiting_Grant return Publisher is (Pending => True, others => <>);

   procedure Attach (Item : in out Publisher; Target : Logstore_Handles.Endpoint) is
   begin
      Item.Slot := Logstore_Handles.Handle (Target);
      Item.Pending := False;
   end Attach;

   function Bound_To (Target : Observer_Handles.Endpoint) return Reader is
     (Slot => Observer_Handles.Handle (Target), others => <>);

   function Bound_To (Target : Observer_Handles.Optional_Endpoint) return Reader is
     (Slot => (if Target.Present then Observer_Handles.Handle (Target.Value)
               else CuBit.Handles.No_Endpoint),
      others => <>);

   procedure Publish_Now
     (Value : Logs.Log_Record; Result : out Status; Kept_From : out Logs.Severity;
      Target : Logstore_Handles.Endpoint)
   is
      Submitted, Drained : Boolean;
   begin
      if not CuBit.Handles.Valid (Announcer.Slot) then
         Announcer.Slot := Logstore_Handles.Handle (Target);
      end if;
      Result := Unavailable;
      Kept_From := Logs.Trace;
      Emit (Announcer, Value, Submitted);
      if not Submitted then
         return;
      end if;
      Flush (Announcer, Drained);
      Kept_From := Minimum (Announcer);
      if Drained then
         Result := (if Logs."<=" (Kept_From, Logs.Level (Value)) then OK else Below_Minimum);
      end if;
   end Publish_Now;

   procedure Announce
     (Text : String; Published : out Boolean; Target : Logstore_Handles.Endpoint;
      Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information)
   is
      Value : constant Logs.Decoded := Logs.Make (Text, Level);
      Result : Status;
      Kept_From : Logs.Severity;
   begin
      Published := False;
      if Value.Success then
         Publish_Now (Value.Value, Result, Kept_From, Target);
         Published := Result = OK;
      end if;
   end Announce;

   function Request (Op : Operation) return Message is
      Value : Message := NULL_MESSAGE;
   begin
      Value.tag := (label => Operation'Enum_Rep (Op), length => 4,
                    protocol => 0, ipc => <>, reserved => 0);
      return Value;
   end Request;
   function Reply_Status (Value : Message) return Status is
   begin
      if Value.tag.length = 4 and Value.tag.protocol = 0 and
        Value.tag.reserved = 0
      then
         for Candidate in Status loop
            if Value.tag.label = Status'Enum_Rep (Candidate) then
               return Candidate;
            end if;
         end loop;
      end if;
      return Unavailable;
   end Reply_Status;
   procedure Drop (Item : in out Publisher) is
   begin
      if Item.Loss < Unsigned_64'Last then
         Item.Loss := Item.Loss + 1;
      end if;
   end Drop;
   function Dropped (Item : Publisher) return Unsigned_64 is (Item.Loss);
   function Minimum (Item : Publisher) return Logs.Severity is
     (if Item.State = Ready
        and then Channels.Consumer_Word (Item.Link, Publish_Rings.Minimum_Word)
                   <= Logs.Severity'Pos (Logs.Severity'Last)
      then Logs.Severity'Val (Channels.Consumer_Word (Item.Link, Publish_Rings.Minimum_Word))
      else Logs.Trace);
   function Wanted (Item : Publisher; Level : Logs.Severity) return Boolean is
     (Logs."<=" (Minimum (Item), Level));

   --  A minimum carried in a reply's first word, if it is one.
   function Valid_Minimum (Value : Message) return Boolean is
     (Value.words (0) <= Logs.Severity'Pos (Logs.Severity'Last) and then Value.words (1 .. 3) = [0, 0, 0]);

   procedure Set_Minimum
     (Level : Logs.Severity; Previous : out Logs.Severity; Result : out Status;
      Target : CuBit.Handles.Endpoint)
   is
      Msg : Message := Request (CuBit.Log_Protocol.Set_Minimum);
      Tag : MessageTag;
   begin
      Msg.words (0) := Logs.Severity'Pos (Level);
      Tag := capCall (Target, Msg, CuBit.Messages.Wait_Forever);
      Result := (if CuBit.Messages.Ending_Of (Tag) /= CuBit.Messages.Call_Answered
                 then Unavailable else Reply_Status (Msg));
      Previous := Level;
      if Result = OK then
         if Valid_Minimum (Msg) then
            Previous := Logs.Severity'Val (Msg.words (0));
         else
            Result := Invalid_Request;
         end if;
      end if;
   end Set_Minimum;

   procedure Get_Minimum
     (Level : out Logs.Severity; Result : out Status;
      Target : CuBit.Handles.Endpoint)
   is
      Msg : Message := Request (CuBit.Log_Protocol.Get_Minimum);
      Tag : MessageTag;
   begin
      Tag := capCall (Target, Msg, CuBit.Messages.Wait_Forever);
      Result := (if CuBit.Messages.Ending_Of (Tag) /= CuBit.Messages.Call_Answered
                 then Unavailable else Reply_Status (Msg));
      Level := Logs.Trace;
      if Result = OK then
         if Valid_Minimum (Msg) then
            Level := Logs.Severity'Val (Msg.words (0));
         else
            Result := Invalid_Request;
         end if;
      end if;
   end Get_Minimum;
   procedure Emit
     (Item : in out Publisher; Value : Logs.Log_Record;
      Submitted : out Boolean)
   is
      Bytes : Logs.Wire_Buffer;
      Used : Logs.Wire_Count;
      Result : Channels.Put_Result;
   begin
      Submitted := False;
      if Item.Pending or else Item.Disconnecting or else Item.State = Disabled then
         Drop (Item);
         return;
      end if;
      if Item.State = Uninitialized then
         Open_Channel (Item);
         if Item.State /= Ready then
            Drop (Item);
            return;
         end if;
      end if;
      Logs.Encode (Value, Bytes, Used);
      --  Shed when full: logstore reports the channel's count as a gap.
      Channels.Put (Item.Link, Bytes'Address, Natural (Used), Result);
      Submitted := Result = Channels.Put;
      if not Submitted then
         Drop (Item);
      end if;
   end Emit;

   procedure Flush (Item : in out Publisher; Drained : out Boolean; Wait_Ms : Natural := 200) is
      Waited : Natural := 0;
      Ignore : Unsigned_64;
   begin
      if Item.State /= Ready then
         Drained := Item.State /= Disabled;
         return;
      end if;
      Drained := Channels.All_Taken (Item.Link);
      if Drained then
         return;
      end if;
      Channels.Kick (Item.Link);
      while Waited < Wait_Ms loop
         if Channels.All_Taken (Item.Link) then
            Drained := True;
            return;
         end if;
         Ignore := syscall (SYSCALL_SLEEP, 1);
         Waited := Waited + 1;
      end loop;
   end Flush;

   procedure List_Sources
     (Item : Reader; Skip : Natural; Page : out Source_Page;
      Written : out CuBit.Log_Sources.Page_Count; Total : out Natural;
      Result : out Status)
   is
      package LS renames CuBit.Log_Sources;
      type Lent_Page is array (0 .. LS.Page_Bytes - 1) of Unsigned_8
        with Alignment => LS.Page_Bytes;
      Lent : aliased Lent_Page := [others => 0];
      Loan : CuBit.Memory_Grants.Grant_Reference;
      Granted, Revoked, Accepted : Boolean;
      Msg : Message := Request (CuBit.Log_Protocol.List_Sources);
      Tag : MessageTag;
      Bytes : LS.Entry_Buffer;
   begin
      Page := [others => (others => <>)];
      Written := 0;
      Total := 0;
      Result := Unavailable;
      if not CuBit.Handles.Valid (Item.Slot) then
         return;
      end if;
      CuBit.Memory_Grants.Create_Via_Capability (Item.Slot, Lent'Address, 1, True, Loan, Granted);
      if not Granted then
         return;
      end if;
      Msg.words := [CuBit.Grant_References.Encode (Loan), Unsigned_64 (Skip), 0, 0];
      Tag := capCall (Item.Slot, Msg, CuBit.Messages.Wait_Forever);
      CuBit.Memory_Grants.Revoke (Loan, Revoked);
      Result := (if CuBit.Messages.Ending_Of (Tag) /= CuBit.Messages.Call_Answered
                 then Unavailable else Reply_Status (Msg));
      if Result /= OK then
         return;
      end if;
      if Msg.words (0) > LS.Page_Entries or else Msg.words (2 .. 3) /= [0, 0] then
         Result := Invalid_Request;
         return;
      end if;
      Total := Natural (Unsigned_64'Min (Msg.words (1), Unsigned_64 (Natural'Last)));
      for Position in 1 .. Natural (Msg.words (0)) loop
         for I in Bytes'Range loop
            Bytes (I) := Lent (LS.Entry_Offset (Position) + I);
         end loop;
         LS.Decode (Bytes, Page (Position), Accepted);
         if not Accepted then
            Result := Invalid_Request;
            return;
         end if;
         Written := Position;
      end loop;
   end List_Sources;

   procedure Find_Sources
     (Item : Reader; Wanted : String; Found : out Source_Entries; Count : out Natural;
      Result : out Status)
   is
      Page : Source_Page;
      Written : CuBit.Log_Sources.Page_Count;
      Total, Skip : Natural := 0;
      Seen : Boolean;
   begin
      Found := [others => (others => <>)];
      Count := 0;
      loop
         List_Sources (Item, Skip, Page, Written, Total, Result);
         exit when Result /= OK or else Written = 0;
         for Position in 1 .. Written loop
            Seen := False;
            for K in Found'First .. Found'First + Count - 1 loop
               Seen := Seen or else Found (K).Who.Source = Page (Position).Who.Source;
            end loop;
            if not Seen and then Count < Found'Length
              and then CuBit.Log_Sources.Answers_To (Page (Position).Who, Wanted)
            then
               Found (Found'First + Count) := Page (Position);
               Count := Count + 1;
            end if;
         end loop;
         Skip := Skip + Written;
         exit when Skip >= Total;
      end loop;
   end Find_Sources;

   procedure Disconnect (Item : in out Publisher; Done : out Boolean) is
   begin
      --  logstore drains what is left when the channel ends; the pages are
      --  freed once it has let go of them (CuBit.Channels.Close).
      if Item.State = Ready then
         Channels.Close (Item.Link);
      end if;
      Item.Disconnecting := True;
      Item.State := Disabled;
      Done := True;
   end Disconnect;

   procedure Subscribe
     (Item : in out Reader; Result : out Status;
      Minimum : Logs.Severity := Logs.Trace;
      Source : Unsigned_64 := Every_Source;
      Limit_Ms : Unsigned_64 := No_Limit) is
      Msg : Message := Request (CuBit.Log_Protocol.Subscribe);
      Tag : MessageTag;
      Opened : Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
      function Deadline return Unsigned_64 is
        (if Limit_Ms = No_Limit then CuBit.Messages.Wait_Forever
         else CuBit.Messages.Deadline_After (Limit_Ms));
   begin
      Result := Unavailable;
      Item.Limit_Ms := Limit_Ms;
      if not Item.Link.Active then
         Channels.Open (Item.Slot, CuBit.Log_Streams.CONTRACT, Channels.Consuming, Item.Link,
                        Opened, Ignore_Refusal, Deadline => Deadline);
         if Opened /= Channels.Opened then
            return;
         end if;
      end if;
      Msg.words := [Logs.Severity'Pos (Minimum), Source, Item.Link.Peer_Number, 0];
      Tag := capCall (Item.Slot, Msg, Deadline);
      Result := (if CuBit.Messages.Ending_Of (Tag) /= CuBit.Messages.Call_Answered
                 then Unavailable else Reply_Status (Msg));
      --  The reply confirms the handle and the source filter applied.
      if Result = OK and then Msg.words (0) /= 0 and then
        Msg.words (1) = Source and then Msg.words (2 .. 3) = [0, 0]
      then
         Item.Subscription := Msg.words (0);
         Item.Minimum := Minimum;
         Item.Source := Source;
         Item.Renewed_Ms := CuBit.Published_Clock.Milliseconds;
      else
         if Result = OK then
            Result := Invalid_Request;
         end if;
         Item.Subscription := 0;
      end if;
   end Subscribe;

   procedure Read_Next
     (Item : in out Reader; Value : out Event; Lost : out Unsigned_64;
      Result : out Status) is
      Entry_Bytes : CuBit.Log_Streams.Entry_Buffer := [others => 0];
      Length : Natural;
      Valid : Boolean;
      Taken : Channels.Take_Result;
      Kind : CuBit.Log_Streams.Entry_Kind;
      Now : Unsigned_64;
      Renewal : Status;
      use type Channels.Take_Result;
      use type CuBit.Log_Streams.Entry_Kind;
   begin
      Value := (others => <>);
      Lost := 0;
      Result := Unavailable;
      if Item.Subscription = 0 then
         return;
      end if;
      Channels.Take (Item.Link, Entry_Bytes'Address, Entry_Bytes'Length, Length, Taken);
      if Taken = Channels.Empty then
         Result := Empty;
         --  Nothing to read: a good moment to keep the subscription alive.
         Now := CuBit.Published_Clock.Milliseconds;
         if Now >= Item.Renewed_Ms + RENEW_MS then
            Subscribe (Item, Renewal, Item.Minimum, Item.Source, Item.Limit_Ms);
         end if;
         return;
      end if;
      if Taken /= Channels.Taken then
         Result := Invalid_Request;
         return;
      end if;
      CuBit.Log_Streams.Decode (Entry_Bytes, Length, Kind, Value, Lost, Valid);
      Result := (if not Valid then Invalid_Request elsif Kind = CuBit.Log_Streams.Gap_Entry then Gap else OK);
   end Read_Next;

   procedure Close (Item : in out Reader; Result : out Status) is
      Msg : Message := Request (CuBit.Log_Protocol.Close);
      Tag : MessageTag;
   begin
      Msg.words (0) := Item.Subscription;
      Tag := capCall (Item.Slot, Msg, CuBit.Messages.Wait_Forever);
      Result := (if CuBit.Messages.Ending_Of (Tag) /= CuBit.Messages.Call_Answered
                 then Unavailable else Reply_Status (Msg));
      if Result = OK or Result = Denied then
         --  The channel goes too; a later Subscribe opens a fresh one.
         Item.Subscription := 0;
         Channels.Close (Item.Link);
      end if;
   end Close;
end CuBit.Logging;
