pragma Ada_2022;
with CuBit.Channel_Protocol;
with CuBit.Log_Publish_Rings;
with CuBit.Log_Streams;
package body CuBit.Logging is
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

   --  Open the channel to logstore, once.
   procedure Attach (Item : in out Publisher);
   procedure Attach (Item : in out Publisher) is
      Result : Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Channels.Open (Item.Slot, Publish_Rings.CONTRACT, Channels.Producing, Item.Link, Result,
                     Ignore_Refusal);
      Item.State := (if Result = Channels.Opened then Ready else Disabled);
   end Attach;

   procedure Publish_Now
     (Value : Logs.Log_Record; Result : out Status; Kept_From : out Logs.Severity)
   is
      Submitted, Drained : Boolean;
   begin
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
      Slot : CuBit.Messages.CapabilitySlot := Control_Slot)
   is
      Msg : Message := Request (CuBit.Log_Protocol.Set_Minimum);
      Tag : MessageTag;
   begin
      Msg.words (0) := Logs.Severity'Pos (Level);
      Tag := capCall (Slot, Msg, CuBit.Messages.Wait_Forever);
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
      Tag := capCall (Slot, Msg, CuBit.Messages.Wait_Forever);
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
   procedure Emit
     (Item : in out Publisher; Value : Logs.Log_Record;
      Submitted : out Boolean)
   is
      Bytes : Logs.Wire_Buffer;
      Used : Logs.Wire_Count;
      Result : Channels.Put_Result;
   begin
      Submitted := False;
      if Item.Disconnecting or else Item.State = Disabled then
         Drop (Item);
         return;
      end if;
      if Item.State = Uninitialized then
         Attach (Item);
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
      Source : Unsigned_64 := Every_Source) is
      Msg : Message := Request (CuBit.Log_Protocol.Subscribe);
      Tag : MessageTag;
      Opened : Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Result := Unavailable;
      if not Item.Link.Active then
         Channels.Open (Item.Slot, CuBit.Log_Streams.CONTRACT, Channels.Consuming, Item.Link,
                        Opened, Ignore_Refusal);
         if Opened /= Channels.Opened then
            return;
         end if;
      end if;
      Msg.words := [Logs.Severity'Pos (Minimum), Source, Item.Link.Peer_Number, 0];
      Tag := capCall (Item.Slot, Msg, CuBit.Messages.Wait_Forever);
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
         Now := syscall (SYSCALL_GETTIME);
         if Now >= Item.Renewed_Ms + RENEW_MS then
            Subscribe (Item, Renewal, Item.Minimum, Item.Source);
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
      Result := (if Tag.label = 0 then Unavailable else Reply_Status (Msg));
      if Result = OK or Result = Denied then
         --  The channel goes too; a later Subscribe opens a fresh one.
         Item.Subscription := 0;
         Channels.Close (Item.Link);
      end if;
   end Close;
end CuBit.Logging;
