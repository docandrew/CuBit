with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Log_Protocol;
with CuBit.Log_Streams;

package body Stream_Writers is
   package Channels renames CuBit.Channels;
   package Streams renames CuBit.Log_Streams;
   use type CuBit.Log_Protocol.Status;
   use type Channels.Put_Result;
   use type Channels.Side;
   use type CuBit.Channel_Contracts.Contract;

   function Number_Of (Index : Positive) return Unsigned_64 is
     (First_Number + Unsigned_64 (Index));

   --  The stream numbered Number that From opened, or 0.
   function Find (Item : Table; Number : Unsigned_64; From : CuBit.Messages.ProcessID) return Natural is
     (if Number > First_Number and then Number - First_Number <= Unsigned_64 (Item'Last)
        and then Item (Natural (Number - First_Number)).Link.Active
        and then Item (Natural (Number - First_Number)).Owner = Unsigned_64 (From)
      then Natural (Number - First_Number) else 0);

   procedure Let_Go (S : in out Stream);
   procedure Let_Go (S : in out Stream) is
   begin
      Channels.Close (S.Link);
      S := (others => <>);
   end Let_Go;

   procedure Open
     (Item : in out Table; From : CuBit.Messages.ProcessID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message)
   is
      Is_Open, Valid : Boolean;
      Offered : CuBit.Channel_Contracts.Contract;
      Opener_Side : Channels.Side;
      Ignore_Connector : Unsigned_16;
   begin
      Channels.Decode_Open (Request, Is_Open, Valid, Offered, Opener_Side, Ignore_Connector);
      if not Valid or else Offered /= Streams.CONTRACT or else Opener_Side /= Channels.Consuming then
         Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.Unknown_Type);
         return;
      end if;
      for I in Item'Range loop
         if not Item (I).Link.Active then
            Channels.Accept_Open (From, Request, Number_Of (I), Item (I).Link, Reply);
            if Item (I).Link.Active then
               Item (I).Owner := Unsigned_64 (From);
               Item (I).Authority := Authority;
               Item (I).Handle := 0;
            end if;
            return;
         end if;
      end loop;
      Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.No_Room);
   end Open;

   procedure Bind
     (Item : in out Table; Number, Handle : Unsigned_64; From : CuBit.Messages.ProcessID;
      Authority : Unsigned_64; Bound : out Boolean)
   is
      Index : constant Natural := Find (Item, Number, From);
   begin
      Bound := Index /= 0 and then Item (Index).Authority = Authority
        and then (Item (Index).Handle = 0 or else Item (Index).Handle = Handle);
      if Bound then
         Item (Index).Handle := Handle;
      end if;
   end Bind;

   procedure Unbind (Item : in out Table; Handle : Unsigned_64) is
   begin
      for S of Item loop
         if S.Link.Active and then S.Handle = Handle then
            S.Handle := 0;
         end if;
      end loop;
   end Unbind;

   procedure Close (Item : in out Table; From : CuBit.Messages.ProcessID; Number : Unsigned_64) is
      Index : constant Natural := Find (Item, Number, From);
   begin
      if Index /= 0 then
         Let_Go (Item (Index));
      end if;
   end Close;

   procedure Ended (Item : in out Table; Event : CuBit.Control_Events.Event) is
   begin
      for S of Item loop
         if Channels.Ended (S.Link, Event) then
            Let_Go (S);
         end if;
      end loop;
   end Ended;

   procedure Drain (Item : in out Table; Store : in out Log_Fanout.Broker; Backlog : out Boolean) is
   begin
      Backlog := False;
      for S of Item loop
         if S.Link.Active and then S.Handle /= 0 then
            if not Log_Fanout.Active (Store, S.Handle) then
               --  Closed or expired: nothing more for it until it is bound again.
               S.Handle := 0;
            else
               declare
                  Value : CuBit.Log_Protocol.Event;
                  Lost : Unsigned_64;
                  Result : CuBit.Log_Protocol.Status;
                  Entry_Bytes : Streams.Entry_Buffer;
                  Length : Streams.Entry_Length;
                  Put : Channels.Put_Result;
               begin
                  loop
                     --  Room first: an event taken from the queue is never lost.
                     if Channels.Free_Bytes (S.Link) < Streams.Room_Needed then
                        Backlog := True;
                        exit;
                     end if;
                     Log_Fanout.Read_Next
                       (Store, S.Owner, S.Authority, S.Handle, Value, Lost, Result, Renew => False);
                     exit when Result not in CuBit.Log_Protocol.OK | CuBit.Log_Protocol.Gap;
                     if Result = CuBit.Log_Protocol.Gap then
                        Streams.Encode_Gap (Lost, Entry_Bytes, Length);
                     else
                        Streams.Encode_Event (Value, Entry_Bytes, Length);
                     end if;
                     Channels.Put (S.Link, Entry_Bytes'Address, Length, Put);
                     exit when Put /= Channels.Put;
                  end loop;
               end;
            end if;
         end if;
      end loop;
   end Drain;
end Stream_Writers;
