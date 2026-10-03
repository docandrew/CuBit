pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Datagram_Rings;
package body CuBit.Logging is
   use CuBit.Messages;
   use CuBit.Log_Protocol;
   package Logs renames CuBit.Log_Records;
   package Grants renames CuBit.Memory_Grants;

   function Request (Op : Operation) return Message;
   function Reply_Status (Value : Message) return Status;
   procedure Drop (Item : in out Publisher);
   function Valid_Minimum (Value : Message) return Boolean;

   --  Announce's page stays granted to logstore for the process's life, so
   --  later announcements reuse it.
   Announce_Page : Transfer_Page := [others => 0];
   Announce_Grant : Grants.Grant_Reference;
   Announce_Granted : Boolean := False;

   procedure Publish_Now
     (Value : Logs.Log_Record; Result : out Status; Kept_From : out Logs.Severity)
   is
      Bytes : Logs.Wire_Buffer;
      Used : Logs.Wire_Count;
      Msg : Message := Request (Publish);
      Tag : MessageTag;
   begin
      Kept_From := Logs.Trace;
      Result := Unavailable;
      if not Announce_Granted then
         Grants.Create_Via_Capability
           (Publisher_Slot, Announce_Page'Address, 1, False, Announce_Grant, Announce_Granted);
         if not Announce_Granted then
            return;
         end if;
      end if;
      Logs.Encode (Value, Bytes, Used);
      for I in Bytes'Range loop
         Announce_Page (I) := Bytes (I);
      end loop;
      Msg.words := [Announce_Grant.slot, Announce_Grant.generation, Unsigned_64 (Used), 0];
      Tag := capCall (Publisher_Slot, Msg);
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
      if Result in OK | Below_Minimum and then Valid_Minimum (Msg) then
         Kept_From := Logs.Severity'Val (Msg.words (0));
      end if;
   end Publish_Now;

   procedure Announce
     (Text : String; Published : out Boolean;
      Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information)
   is
      Value : constant Logs.Decoded := Logs.Make (Text, Level);
      Result : Status;
      Kept_From : Logs.Severity;
   begin
      Published := False;
      if Value.Success then
         Publish_Now (Value.Value, Result, Kept_From);
         Published := Result = OK;
      end if;
   end Announce;

   function Request (Op : Operation) return Message is
      Value : Message := NULL_MESSAGE;
   begin
      Value.tag := (label => Operation'Enum_Rep (Op), length => 4,
                    flags => 0, reserved => 0);
      return Value;
   end Request;
   function Reply_Status (Value : Message) return Status is
   begin
      if Value.tag.length = 4 and Value.tag.flags = 0 and
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
   function Minimum (Item : Publisher) return Logs.Severity is (Item.Kept_From);
   function Wanted (Item : Publisher; Level : Logs.Severity) return Boolean is
     (Logs."<=" (Item.Kept_From, Level));

   --  A minimum carried in a reply's first word, if it is one.
   function Valid_Minimum (Value : Message) return Boolean is
     (Value.words (0) <= Logs.Severity'Pos (Logs.Severity'Last) and then Value.words (1 .. 3) = [0, 0, 0]);

   procedure Set_Minimum
     (Level : Logs.Severity; Previous : out Logs.Severity; Result : out Status;
      Slot : CuBit.Messages.CapabilitySlot := Control_Slot)
   is
      Msg : Message := Request (CuBit.Log_Protocol.Set_Minimum);
      Tag : MessageTag;
   begin
      Msg.words (0) := Logs.Severity'Pos (Level);
      Tag := capCall (Slot, Msg);
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
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
      Slot : CuBit.Messages.CapabilitySlot := Observer_Slot)
   is
      Msg : Message := Request (CuBit.Log_Protocol.Get_Minimum);
      Tag : MessageTag;
   begin
      Tag := capCall (Slot, Msg);
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
      Level := Logs.Trace;
      if Result = OK then
         if Valid_Minimum (Msg) then
            Level := Logs.Severity'Val (Msg.words (0));
         else
            Result := Invalid_Request;
         end if;
      end if;
   end Get_Minimum;
   function Pending (Item : Publisher) return Boolean is
     (Item.State = In_Flight);

   procedure Emit
     (Item : in out Publisher; Value : Logs.Log_Record;
      Token : Unsigned_64; Submitted : out Boolean) is
      Bytes : Logs.Wire_Buffer;
      Used : Logs.Wire_Count;
      Created : Boolean;
      Msg : Message := Request (Publish);
   begin
      Submitted := False;
      if Item.Disconnecting or else Item.State in In_Flight | Disabled then
         Drop (Item);
         return;
      end if;
      if Item.State = Uninitialized then
         Grants.Create_Via_Capability
           (Item.Slot, Item.Page'Address, 1, False, Item.Grant, Created);
         if not Created then
            Item.State := Disabled;
            Drop (Item);
            return;
         end if;
         Item.State := Ready;
         Item.Has_Grant := True;
      end if;
      Logs.Encode (Value, Bytes, Used);
      for I in Bytes'Range loop
         Item.Page (I) := Bytes (I);
      end loop;
      Msg.words := [Item.Grant.slot, Item.Grant.generation,
                    Unsigned_64 (Used), 0];
      Submitted := capSubmit (Item.Slot, Msg, Token);
      if Submitted then
         Item.State := In_Flight;
         Item.Token := Token;
      else
         Item.State := Disabled;
         Drop (Item);
      end if;
   end Emit;

   procedure Complete
     (Item : in out Publisher; Completion : CompletionEntry;
      Handled : out Boolean) is
   begin
      Handled := Item.State = In_Flight and then
        Completion.token = Item.Token;
      if not Handled then
         return;
      end if;
      if Completion.status = COMPLETION_OK and then
        Reply_Status (Completion.msg) = Rate_Limited and then
        Completion.msg.words = [0, 0, 0, 0]
      then
         Item.State := Ready;
         Drop (Item);
      elsif Completion.status = COMPLETION_OK and then
        Reply_Status (Completion.msg) in OK | Below_Minimum and then
        Completion.msg.words (1 .. 3) = [0, 0, 0] and then
        Completion.msg.words (0) <= Logs.Severity'Pos (Logs.Severity'Last)
      then
         --  Kept, or discarded under logstore's minimum: either way delivered.
         Item.State := Ready;
         Item.Kept_From := Logs.Severity'Val (Completion.msg.words (0));
      else
         --  Do not recycle the page after an ambiguous/failed acquisition.
         Item.State := Disabled;
         Drop (Item);
      end if;
   end Complete;

   procedure Disconnect (Item : in out Publisher; Done : out Boolean) is
      Accepted : Boolean;
   begin
      Item.Disconnecting := True;
      if Item.Has_Grant then
         --  Repetition is harmless, including already-retired references.
         --  Accepted alone is deliberately not used as the release condition.
         Grants.Revoke (Item.Grant, Accepted);
         if Grants.Retirement_Confirmed (Item.Grant) then
            Item.Has_Grant := False;
         end if;
      end if;
      Done := not Item.Has_Grant and then not Pending (Item);
      if Done then
         Item.State := Disabled;
      end if;
   end Disconnect;

   --  The stream's shared indices, each written by one side only.
   function Shared_Index (Item : Reader; Offset : Natural) return CuBit.Channel_Rings.Index;
   procedure Set_Shared_Index (Item : Reader; Offset : Natural; Value : CuBit.Channel_Rings.Index);
   function Shared_Index (Item : Reader; Offset : Natural) return CuBit.Channel_Rings.Index is
      Value : CuBit.Channel_Rings.Index with Import, Volatile,
        Address => Item.Region'Address + System.Storage_Elements.Storage_Offset (Offset);
   begin
      return Value;
   end Shared_Index;
   procedure Set_Shared_Index (Item : Reader; Offset : Natural; Value : CuBit.Channel_Rings.Index) is
      Target : CuBit.Channel_Rings.Index with Import, Volatile,
        Address => Item.Region'Address + System.Storage_Elements.Storage_Offset (Offset);
   begin
      Target := Value;
   end Set_Shared_Index;

   procedure Subscribe
     (Item : in out Reader; Result : out Status;
      Minimum : Logs.Severity := Logs.Trace;
      Source : Unsigned_64 := Every_Source) is
      Msg : Message := Request (CuBit.Log_Protocol.Subscribe);
      Tag : MessageTag;
      Fresh : constant Boolean := Item.Subscription = 0;
   begin
      Result := Unavailable;
      if not Item.Has_Grant then
         Grants.Create_Via_Capability
           (Item.Slot, Item.Region'Address, CuBit.Log_Streams.STREAM_PAGES, True, Item.Grant, Item.Has_Grant);
         if not Item.Has_Grant then
            return;
         end if;
      end if;
      if Fresh then
         --  A new stream starts empty at index zero on both sides.
         Set_Shared_Index (Item, CuBit.Log_Streams.PRODUCED_OFFSET, 0);
         Set_Shared_Index (Item, CuBit.Log_Streams.CONSUMED_OFFSET, 0);
         Item.Consumer := CuBit.Channel_Rings.New_Consumer (CuBit.Log_Streams.RING_BYTES);
      end if;
      Msg.words := [Logs.Severity'Pos (Minimum), Source, Item.Grant.slot, Item.Grant.generation];
      Tag := capCall (Item.Slot, Msg);
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
      --  The reply confirms the handle and the source filter applied.
      if Result = OK and then Msg.words (0) /= 0 and then
        Msg.words (1) = Source and then Msg.words (2 .. 3) = [0, 0]
      then
         Item.Subscription := Msg.words (0);
         Item.Minimum := Minimum;
         Item.Source := Source;
         Item.Renewed_Ms := syscall (SYSCALL_GETTIME);
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
      Ring : CuBit.Channel_Rings.Bytes (0 .. CuBit.Log_Streams.RING_BYTES - 1)
        with Import, Address => Item.Region'Address +
          System.Storage_Elements.Storage_Offset (CuBit.Log_Streams.CONTROL_BYTES);
      Entry_Bytes : CuBit.Log_Streams.Entry_Buffer := [others => 0];
      Length : Natural;
      Truncated, Accepted, Valid : Boolean;
      Taken : CuBit.Datagram_Rings.Take_Result;
      Kind : CuBit.Log_Streams.Entry_Kind;
      Now : Unsigned_64;
      Renewal : Status;
      use type CuBit.Datagram_Rings.Take_Result;
      use type CuBit.Log_Streams.Entry_Kind;
   begin
      Value := (others => <>);
      Lost := 0;
      Result := Unavailable;
      if Item.Subscription = 0 then
         return;
      end if;
      CuBit.Channel_Rings.Accept_Produced
        (Item.Consumer, Shared_Index (Item, CuBit.Log_Streams.PRODUCED_OFFSET), Accepted);
      if not Accepted then
         Result := Invalid_Request;
         return;
      end if;
      CuBit.Datagram_Rings.Take (Item.Consumer, Ring, Entry_Bytes, Length, Truncated, Taken);
      if Taken = CuBit.Datagram_Rings.Empty then
         Result := Empty;
         --  Nothing to read: a good moment to keep the subscription alive.
         Now := syscall (SYSCALL_GETTIME);
         if Now >= Item.Renewed_Ms + RENEW_MS then
            Subscribe (Item, Renewal, Item.Minimum, Item.Source);
         end if;
         return;
      end if;
      Set_Shared_Index (Item, CuBit.Log_Streams.CONSUMED_OFFSET, Item.Consumer.Consumed);
      if Taken /= CuBit.Datagram_Rings.Taken or else Truncated then
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
      Tag := capCall (Item.Slot, Msg);
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
      if Result = OK or Result = Denied then
         --  logstore returned the region before replying; a later Subscribe
         --  starts a fresh stream in it.
         Item.Subscription := 0;
      end if;
   end Close;
end CuBit.Logging;
