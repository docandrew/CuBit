------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Channel_Protocol;
with CuBit.Channels;
with CuBit.Desktop_Messages;
with Display_Frame_Client;
with Desktop_Logs;

package body Desktop_Frame_Ring is

   use type CuBit.Channels.Open_Result;
   use type Display_Frame_Client.Submit_Result;

   Channel : CuBit.Channels.Channel;
   Client  : Display_Frame_Client.Client;
   Opened_Once : Boolean := False;

   function Address_Of (Base : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Base)));

   --  The one place Desktop wakes Display (IPC-010: a shared futex here).
   procedure Wake_Display is
      Ignore : CuBit.Channels.Signal_Result;
   begin
      CuBit.Channels.Signal (Channel, Sync => False, Result => Ignore);
   end Wake_Display;

   procedure Open (Display : CuBit.Handles.Endpoint; Opened : out Boolean) is
      Result : CuBit.Channels.Open_Result;
      Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Opened := Display_Frame_Client.Attached (Client);
      if Opened then
         return;
      end if;
      CuBit.Channels.Open
        (Display, FR.Element, CuBit.Channels.Producing, Channel, Result, Refusal,
         Deadline => CuBit.Messages.Deadline_After (Open_Limit_Ms));
      if Result /= CuBit.Channels.Opened then
         Desktop_Logs.Warn ("desktop: frame ring to Display not opened (" &
                            (if Result = CuBit.Channels.Refused
                             then CuBit.Channel_Protocol.Open_Refusal'Image (Refusal)
                             else "no answer") & ")");
         return;
      end if;
      Display_Frame_Client.Attach
        (Client, Address_Of (Channel.Own_Base), Address_Of (Channel.Peer_Base));
      Desktop_Logs.Write ("desktop: frame ring to Display open" &
                          (if Opened_Once then " again" else "") & ASCII.LF);
      Opened_Once := True;
      Opened := True;
   end Open;

   function Is_Open return Boolean is (Display_Frame_Client.Attached (Client));

   procedure Submit (Item : FR.Present; Token, Frame : Unsigned_64; Accepted : out Boolean) is
      Result : Display_Frame_Client.Submit_Result;
      Ring : Boolean;
   begin
      Display_Frame_Client.Submit (Client, Item, Token, Frame, Result, Ring);
      Accepted := Result = Display_Frame_Client.Queued;
      if Ring then
         Wake_Display;
      end if;
   end Submit;

   procedure Reap (Completion : out CuBit.Messages.CompletionEntry; Got : out Boolean) is
      Item : Display_Frame_Client.Reaped;
      Failed_Token : Unsigned_64;
   begin
      Completion := CuBit.Messages.NULL_COMPLETION;
      Display_Frame_Client.Reap (Client, Item, Got);
      if Got then
         Completion.token := Item.Token;
         Completion.msg := CuBit.Desktop_Messages.From_Wire (Item.Result);
         Completion.status := CuBit.Messages.COMPLETION_OK;
         Completion.valid := True;
         return;
      end if;
      --  The ring ended: each present it held fails, once.
      Display_Frame_Client.Fail_Next (Client, Failed_Token, Got);
      if Got then
         Completion.token := Failed_Token;
         Completion.status := CuBit.Messages.COMPLETION_TARGET_DIED;
         Completion.valid := True;
      end if;
   end Reap;

   function Quiet return Boolean is (Display_Frame_Client.Arm (Client));

   procedure Disarm is
   begin
      Display_Frame_Client.Disarm (Client);
   end Disarm;

   procedure Take_Event (Event : CuBit.Control_Events.Event; Ended : out Boolean) is
   begin
      Ended := Display_Frame_Client.Attached (Client) and then CuBit.Channels.Ended (Channel, Event);
      if Ended then
         Display_Frame_Client.Detach (Client);
         CuBit.Channels.Close (Channel);
         Desktop_Logs.Warn ("desktop: frame ring to Display ended");
      end if;
   end Take_Event;

   function Strays return Unsigned_64 is (Display_Frame_Client.Strays (Client));

end Desktop_Frame_Ring;
