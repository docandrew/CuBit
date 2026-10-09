pragma Ada_2022;
package body CuBit.Metrics is
   use CuBit.Messages;
   use type Protocol.Status;
   package Batches renames CuBit.Metric_Batches;
   package Grants renames CuBit.Memory_Grants;

   One_Page : constant := 1;

   function Request (Op : Protocol.Operation) return Message;
   function Reply_Status (Value : Message) return Protocol.Status;
   function Add (Left, Right : Unsigned_64) return Unsigned_64;

   function Request (Op : Protocol.Operation) return Message is
      Value : Message := NULL_MESSAGE;
   begin
      Value.tag := (label => Protocol.Operation'Enum_Rep (Op),
                    length => Protocol.Message_Words, flags => 0,
                    reserved => 0);
      return Value;
   end Request;

   function Reply_Status (Value : Message) return Protocol.Status is
   begin
      if Value.tag.length = Protocol.Message_Words and
        Value.tag.flags = 0 and Value.tag.reserved = 0
      then
         for Candidate in Protocol.Status loop
            if Value.tag.label = Protocol.Status'Enum_Rep (Candidate) then
               return Candidate;
            end if;
         end loop;
      end if;
      return Protocol.Unavailable;
   end Reply_Status;

   function Add (Left, Right : Unsigned_64) return Unsigned_64 is
     (if Right > Unsigned_64'Last - Left then Unsigned_64'Last
      else Left + Right);

   function Dropped (Item : Publisher) return Unsigned_64 is
     (Batches.Dropped (Item.State));
   function Rejected (Item : Publisher) return Unsigned_64 is
     (Item.Refused);
   function Disabled (Item : Publisher) return Boolean is (Item.Off);
   function Has_Room (Item : Publisher) return Boolean is
     (Batches.Has_Room (Item.State));

   procedure Put
     (Item : in out Publisher; Value : Records.Metric_Record;
      Accepted : out Boolean) is
   begin
      Batches.Append (Item.State, Item.Pages, Value, Accepted);
   end Put;

   function Has_Group_Room (Item : Publisher) return Boolean is
     (not Item.Off and then Batches.Has_Group_Room (Item.State));

   procedure Put_Group
     (Item : in out Publisher; Values : Records.Trace_Group;
      Accepted : out Boolean) is
   begin
      if Item.Off then
         Item.Refused := Add (Item.Refused, 4);
         Accepted := False;
         return;
      end if;
      Batches.Append_Group (Item.State, Item.Pages, Values, Accepted);
   end Put_Group;

   procedure Flush
     (Item : in out Publisher; Token : Unsigned_64; Submitted : out Boolean)
   is
      Sealed, Created : Boolean;
      Page : Batches.Page_Id;
      Bytes : Unsigned_64;
      Msg : Message := Request (Protocol.Publish_Batch);
   begin
      Submitted := False;
      if Item.Off then
         return;
      end if;
      Batches.Seal (Item.State, Item.Pages, Sealed, Page, Bytes);
      if not Sealed then
         return;
      end if;
      if not Item.Has_Grant (Page) then
         Grants.Create_Via_Capability
           (Item.Slot, Item.Pages (Page)'Address, One_Page, False,
            Item.Grants (Page), Created);
         Item.Has_Grant (Page) := Created;
      end if;
      if Item.Has_Grant (Page) then
         Msg.words := [Item.Grants (Page).slot, Item.Grants (Page).generation,
                       Bytes, 0];
         Submitted := capSubmit (Item.Slot, Msg, Token);
      end if;
      if Submitted then
         Item.Tokens (Page) := Token;
      else
         --  Never submitted: the page is ours again, but stop publishing.
         Item.Refused := Add (Item.Refused,
           Unsigned_64 (Batches.Used (Item.State, Page)));
         Batches.Complete (Item.State, Page);
         Item.Off := True;
      end if;
   end Flush;

   procedure Complete
     (Item : in out Publisher; Completion : CompletionEntry;
      Handled : out Boolean)
   is
      Result : Protocol.Status;
   begin
      Handled := False;
      for Page in Batches.Page_Id loop
         if Batches.In_Flight (Item.State, Page) and then
           Item.Tokens (Page) = Completion.token
         then
            Handled := True;
            Result := Reply_Status (Completion.msg);
            if Completion.status /= COMPLETION_OK or else
              Result = Protocol.Unavailable
            then
               --  Ambiguous: the page may still be acquired. Never reuse it.
               Item.Off := True;
               return;
            end if;
            if Result = Protocol.OK then
               Item.Refused := Add (Item.Refused, Completion.msg.words (1));
            else
               --  Definite refusal; the service returned any acquisition
               --  before replying.
               Item.Refused := Add (Item.Refused,
                 Unsigned_64 (Batches.Used (Item.State, Page)));
               if Result = Protocol.Denied then
                  Item.Off := True;
               end if;
            end if;
            Batches.Complete (Item.State, Page);
            return;
         end if;
      end loop;
   end Complete;

   procedure Disconnect (Item : in out Publisher; Done : out Boolean) is
      Accepted : Boolean;
   begin
      Item.Off := True;
      for Page in Batches.Page_Id loop
         if Item.Has_Grant (Page) then
            Grants.Revoke (Item.Grants (Page), Accepted);
            if Grants.Retirement_Confirmed (Item.Grants (Page)) then
               Item.Has_Grant (Page) := False;
            end if;
         end if;
      end loop;
      Done := True;
      for Page in Batches.Page_Id loop
         if Item.Has_Grant (Page) or else Batches.In_Flight (Item.State, Page)
         then
            Done := False;
         end if;
      end loop;
   end Disconnect;

   procedure Query
     (Item : in out Observer; Cursor : Unsigned_64;
      Rows : out Protocol.Summary_Page; Written : out Protocol.Row_Count;
      Next : out Unsigned_64; Result : out Protocol.Status)
   is
      Msg : Message := Request (Protocol.Query_Summaries);
      Tag : MessageTag;
   begin
      Rows := [others => [others => 0]];
      Written := 0;
      Next := Cursor;
      Result := Protocol.Unavailable;
      if not Item.Has_Grant then
         Grants.Create_Via_Capability
           (Item.Slot, Item.Page'Address, One_Page, True, Item.Grant,
            Item.Has_Grant);
         if not Item.Has_Grant then
            return;
         end if;
      end if;
      Msg.words := [Cursor, Item.Grant.slot, Item.Grant.generation,
                    Records.Page_Bytes];
      Tag := capCall (Item.Slot, Msg, CuBit.Messages.Wait_Forever);
      Result := (if Tag.label = 0 then Protocol.Unavailable
                 else Reply_Status (Msg));
      if Result /= Protocol.OK then
         return;
      end if;
      if Msg.words (0) > Protocol.Rows_Per_Page or else
        Msg.words (1) < Cursor or else Msg.words (3) /= 0
      then
         Result := Protocol.Invalid_Request;
         return;
      end if;
      Written := Protocol.Row_Count (Msg.words (0));
      Next := Msg.words (1);
      Rows := Protocol.Summary_Page (Item.Page);
   end Query;
end CuBit.Metrics;
