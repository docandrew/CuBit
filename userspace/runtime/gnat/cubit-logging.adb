pragma Ada_2022;
package body CuBit.Logging is
   use CuBit.Messages;
   use CuBit.Log_Protocol;
   package Logs renames CuBit.Log_Records;
   package Grants renames CuBit.Memory_Grants;

   function Request (Op : Operation) return Message;
   function Reply_Status (Value : Message) return Status;
   procedure Drop (Item : in out Publisher);

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
      if Item.State in In_Flight | Disabled then
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
        Reply_Status (Completion.msg) in OK | Rate_Limited and then
        Completion.msg.words = [0, 0, 0, 0]
      then
         Item.State := Ready;
         if Reply_Status (Completion.msg) = Rate_Limited then
            Drop (Item);
         end if;
      else
         --  Do not recycle the page after an ambiguous/failed acquisition.
         Item.State := Disabled;
         Drop (Item);
      end if;
   end Complete;

   procedure Subscribe (Item : in out Reader; Result : out Status) is
      Msg : Message := Request (CuBit.Log_Protocol.Subscribe);
      Tag : MessageTag;
   begin
      Tag := capCall (Item.Slot, Msg);
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
      if Result = OK and then Msg.words (0) /= 0 and then
        Msg.words (1 .. 3) = [0, 0, 0]
      then
         Item.Subscription := Msg.words (0);
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
      Msg : Message := Request (CuBit.Log_Protocol.Read_Next);
      Tag : MessageTag;
      Bytes : Logs.Wire_Buffer;
      Decoded : Logs.Decoded;
   begin
      Value := (others => <>);
      Lost := 0;
      Result := Unavailable;
      if Item.Subscription = 0 then
         return;
      end if;
      if not Item.Has_Grant then
         Grants.Create_Via_Capability
           (Item.Slot, Item.Page'Address, 1, True, Item.Grant, Item.Has_Grant);
         if not Item.Has_Grant then
            return;
         end if;
      end if;
      Msg.words := [Item.Subscription, Item.Grant.slot, Item.Grant.generation,
                    Unsigned_64 (Logs.Wire_Count'Last)];
      Tag := capCall (Item.Slot, Msg);
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
      if Result = OK then
         if Msg.words (0) = 0 or else Msg.words (2) not in
           Unsigned_64 (Logs.Header_Bytes) ..
             Unsigned_64 (Logs.Wire_Count'Last)
         then
            Result := Invalid_Request;
            return;
         end if;
         for I in Bytes'Range loop
            Bytes (I) := Item.Page (I);
         end loop;
         Decoded := Logs.Decode (Bytes, Logs.Wire_Count (Msg.words (2)));
         if Decoded.Success then
            Value := (Msg.words (0), Msg.words (3), Msg.words (1),
                      Decoded.Value);
         else
            Result := Invalid_Request;
         end if;
      elsif Result = Gap then
         Lost := Msg.words (0);
      end if;
   end Read_Next;

   procedure Close (Item : in out Reader; Result : out Status) is
      Msg : Message := Request (CuBit.Log_Protocol.Close);
      Tag : MessageTag;
   begin
      Msg.words (0) := Item.Subscription;
      Tag := capCall (Item.Slot, Msg);
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
      if Result = OK or Result = Denied then
         Item.Subscription := 0;
      end if;
   end Close;
end CuBit.Logging;
