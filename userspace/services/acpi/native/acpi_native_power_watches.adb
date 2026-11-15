pragma Ada_2022;
with CuBit.Handles.IPC;
package body ACPI_Native_Power_Watches is
   package H renames CuBit.Handles;
   package IPC renames CuBit.Handles.IPC;
   package M renames CuBit.Messages;
   use type Interfaces.Unsigned_32;
   procedure Encode (ID : Interfaces.Unsigned_64; Status : E.Watch_Status;
      Press_Count : Interfaces.Unsigned_64; Source : E.Press_Source;
      Reply : out M.Message) is
   begin
      Reply := M.NULL_MESSAGE;
      Reply.tag.label := E.Button_Watch;
      Reply.tag.length := E.Watch_Reply_Words;
      Reply.words (E.Reply_Request_Word) := ID;
      Reply.words (E.Watch_Status_Word) := Interfaces.Unsigned_64 (E.Watch_Status'Enum_Rep (Status));
      Reply.words (E.Count_Word) := Press_Count;
      Reply.words (E.Source_Word) := Interfaces.Unsigned_64 (E.Press_Source'Enum_Rep (Source));
   end Encode;
   procedure Save_Current (Token : out H.Reply; Saved : out Boolean) is
      Result : constant IPC.Saved_Reply := IPC.Save_Reply;
   begin
      Saved := Result.Saved;
      Token := (if Result.Saved then Result.Handle else H.No_Reply);
   end Save_Current;
   procedure Answer_Saved (Token : in out H.Reply; ID : Interfaces.Unsigned_64;
      Status : E.Watch_Status; Press_Count : Interfaces.Unsigned_64;
      Source : E.Press_Source; Delivered : out Boolean) is
      Reply : M.Message;
   begin
      Encode (ID, Status, Press_Count, Source, Reply);
      Delivered := IPC.Answer (Token, Reply);
      -- Answer consumes the saved kernel authority even if delivery failed.
      Token := H.No_Reply;
   end Answer_Saved;
   procedure Mark_Present (Item : in out State) is
   begin Policy.Set_Availability (Item.Watches, Policy.Present); end Mark_Present;
   procedure Mark_Absent (Item : in out State) is
   begin Policy.Set_Availability (Item.Watches, Policy.Absent); end Mark_Absent;
   procedure Mark_Unavailable (Item : in out State) is
   begin Policy.Set_Availability (Item.Watches, Policy.Unobserved); end Mark_Unavailable;
   procedure Record_Press (Item : in out State; Source : Actual_Source) is
   begin Policy.Record_Press (Item.Watches, Source); end Record_Press;
   procedure Begin_Stop (Item : in out State) is
   begin Policy.Begin_Stop (Item.Watches); end Begin_Stop;
   function Retirement_Requested (Item : State) return Boolean is
     (Policy.Retirement_Requested (Item.Watches));
   function Pending (Item : State) return Boolean is (Policy.Runnable (Item.Watches));
   function Quiet (Item : State) return Boolean is (Policy.Quiet (Item.Watches));
   function Count (Item : State) return Interfaces.Unsigned_64 is
     (Policy.Observe (Item.Watches).Count);
   procedure Observe_Presses (Item : State; Count : out Interfaces.Unsigned_64;
      Source : out E.Press_Source) is
      Facts : constant Policy.Snapshot := Policy.Observe (Item.Watches);
   begin Count := Facts.Count; Source := Facts.Source; end Observe_Presses;
   procedure Advance (Item : in out State; Owner_Quiet : Boolean; Result : out Step_Outcome) is
      Progress : Policy.Delivery;
   begin
      Policy.Advance (Item.Watches, Owner_Quiet, Progress);
      Result := (case Progress is
        when Policy.No_Attempt => Idle,
        when Policy.Answered => Replied,
        when Policy.Caller_Gone => Caller_Gone);
   end Advance;
   procedure Dispatch (Item : in out State; Request : M.Message; Owner_Quiet : Boolean;
      Reply : out M.Message; Handled, Reply_Now : out Boolean) is
      Response : Policy.Response;
      Deferred : Boolean;
      Ignored_Previous : Policy.Delivery;
   begin
      Reply := M.NULL_MESSAGE; Reply_Now := True;
      Handled := Request.tag.label = E.Button_Watch;
      if not Handled then return; end if;
      Policy.Watch (Item.Watches,
        (Authority => Request.authorityTag,
         Length => Interfaces.Unsigned_64 (Request.tag.length),
         ID => Request.words (E.Request_Word), Seen => Request.words (E.Seen_Word),
         Flags => Interfaces.Unsigned_64 (M.Flag_Byte (Request.tag)),
         Reserved => Interfaces.Unsigned_64 (Request.tag.reserved)),
        Owner_Quiet, Response, Deferred, Ignored_Previous);
      Reply_Now := not Deferred;
      if Reply_Now then Encode (Response.ID, Response.Status, Response.Count, Response.Source, Reply); end if;
   end Dispatch;
end ACPI_Native_Power_Watches;
